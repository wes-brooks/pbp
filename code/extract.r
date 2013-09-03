#Load the raw data and extract the part including 'mod-pbp', the play-by-play module.
raw = scan("http://scores.espn.go.com/ncf/playbyplay?gameId=332430275&period=0", what=character(0), sep='\n')
index = grep('mod-pbp', raw)
pbp = raw[index]

#Quarter breaks:
quarters = c('1st', '2nd', '3rd', '4th')
starts = sapply(quarters, function(x) {paste(x, "Quarter Play by Play")})
breaks = lapply(starts, function(x) {regexpr(x,pbp)})

#Get the play-by-play for the four quarters into a list
quarter_pbp = list()
for (k in 1:4) {
    start = breaks[[k]][1] + attr(breaks[[k]], 'match.length')
    if (k<4) {end = breaks[[k+1]][1] - 1}
    else {end = nchar(pbp)}
    
    quarter_pbp[[k]] = substr(pbp, start, end)
}

#Drive breaks:
#Divide each quarter of play-by-play into drives:
#(Note that the kickoffs to begin each half will be considered drives here.)
drives = list()
for (k in 1:4) {
    #Begin by identifying the beginning of each drive:
    drivebreaks = gregexpr("(?<team>[A-Za-z]+) at (?<min>\\d{1,2}):(?<sec>\\d\\d)", quarter_pbp[[k]], perl=TRUE, fixed=FALSE)[[1]]
    
    for (j in 1:length(drivebreaks)) {
        start = drivebreaks[j]
        end = start + attr(drivebreaks, 'capture.length')[j,'team'] - 1
        teamname = substr(quarter_pbp[[k]], start, end)

        #Minutes remaining in the quarter when the drive begins
        minstart = attr(drivebreaks, 'capture.start')[j,'min']
        minlength = attr(drivebreaks, 'capture.length')[j,'min'] - 1
        minutes = substr(quarter_pbp[[k]], minstart, minstart + minlength)

        #Seconds remaining in the quarter when the drive begins
        secstart = attr(drivebreaks, 'capture.start')[j,'sec']
        seclength = attr(drivebreaks, 'capture.length')[j,'sec'] - 1
        seconds = substr(quarter_pbp[[k]], secstart, secstart + seclength)

        #Game time (in seconds) remaining at the beginning of the drive:
        time = ((4-k)*15 + as.numeric(minutes))*60 + as.numeric(seconds)
        
        #Extract the play-by-play for this drive:
        if (j<length(drivebreaks)) {nextstart = drivebreaks[j+1] - 1}
        else {nextstart = nchar(quarter_pbp[[k]])}
        end = start + attr(drivebreaks, 'match.length')[j]
        drive_pbp = substr(quarter_pbp[[k]], end, nextstart)
        
        #One drive can span the 1st-2nd or 3rd-4th quarters:
        i = length(drives)
        if ((k==2 || k==4) && j==1 && teamname==drives[[i]][['team']]) {
            drives[[i]][['pbp']] = paste(drives[[i]][['pbp']], drive_pbp, esp='')
        }

        #If this is a new drive then add it to the list.
        else {drives[[i+1]] = list(team=teamname, time=time, pbp=drive_pbp)}        

        #Use the beginning time of this drive to compute the duration of the previous drive:
        if (i>0) {drives[[i]][['duration']] = drives[[i]][['time']] - time}
    }
}

#Set the duration of the final drive equal to the 
drives[[length(drives)]][['duration']] = drives[[length(drives)]][['time']]

#Get the unique team names:
teams = unique(sapply(drives, function(x) {x[['team']]}))

#Summarize each play and attach metadata
plays = list()

for (k in 1:length(drives)) {
    #Divide the drive into plays:
    playbreak_regex = "(?<down>1st|2nd|3rd|4th|1ST|2ND|3RD|4TH) (and|AND) (?<togo>\\d{1,2}|goal|Goal|GOAL) at (?<field>[A-Za-z]{3,4}) (?<yardline>\\d{1,2})</td>" 
    playbreaks = gregexpr(playbreak_regex, drives[[k]][['pbp']], perl=TRUE, fixed=FALSE)[[1]]
    nplay = length(playbreaks)

    for (j in 1:nplay) {
        #Get the play-by-play for this play.
        if (j==nplay) {end = nchar(drives[[k]][['pbp']])}
        else {end = playbreaks[j+1]}
        pbp = substr(drives[[k]][['pbp']], playbreaks[j], end)

        ###Parse the play:
        #First the down:
        downstart = attr(playbreaks, 'capture.start')[j,'down']
        downlength = attr(playbreaks, 'capture.length')[j,'down'] - 1
        down = substr(drives[[k]][['pbp']], downstart, downstart+downlength)

        #Then the yards to go:
        togostart = attr(playbreaks, 'capture.start')[j,'togo']
        togolength = attr(playbreaks, 'capture.length')[j,'togo'] - 1
        togo = substr(drives[[k]][['pbp']], togostart, togostart+togolength)

        #Guestimate the game time remaining:
        time = drives[[k]][['time']] - (j-1)/nplay * drives[[k]][['duration']]

        ##How far from the end zone did the play begin?
        #Figure out which side of the field the play began from:
        fieldstart = attr(playbreaks, 'capture.start')[j,'field']
        fieldlength = attr(playbreaks, 'capture.length')[j,'field'] - 1
        field = substr(drives[[k]][['pbp']], fieldstart, fieldstart+fieldlength)

        #Figure out which yard-line the play began from:
        linestart = attr(playbreaks, 'capture.start')[j,'yardline']
        linelength = attr(playbreaks, 'capture.length')[j,'yardline'] - 1
        line = as.numeric(substr(drives[[k]][['pbp']], linestart, linestart+linelength))
    
        #which team name is the field most like?
        off = drives[[k]][['team']]
        def = teams[teams != off]
        offense_unlike = adist(off, field, costs=list(insertions=2, deletions=0, substitutions=1), ignore.case=TRUE)
        defense_unlike = adist(def, field, costs=list(insertions=2, deletions=0, substitutions=1), ignore.case=TRUE)

        #Now compute the distance from the goal line:
        if (offense_unlike < defense_unlike) {dist = 100 - line}
        else {dist = line}
        
        #Replace 'goal' to go with the distance to the goal line:
        if (substr(togo[1],1,1)=='g' || substr(togo[1],1,1)=='G') {togo = dist}
        togo = as.numeric(togo)

        #Add this play to the list
        plays[[length(plays)+1]] = list(poss=drives[[k]][['team']], down=down, togo=togo, time=time, dist=dist, pbp=pbp)
    }
}

#Special teams plays:
kickoff_regex = "<td><a.*?>(?<kicker>[-a-zA-Z\\. ']+)</a> kickoff for (?<kickdist>\\d{1,3}) yards? (returned by <a.*?>(?<returner>[-a-zA-Z\\. ']+)</a> for (?<returndist>\\d{1,3}) yards|.*touchback).*</td>"
punt_regex = "<td><a.*?>(?<player>[-a-zA-Z\\. ']+)</a> punt for (?<dist>\\d{1,3}) yards?(.*touchback.*|.*out[- ]of[- ]bounds at|.*fair catch by (<a.*?>)?(?<catcher>[-a-zA-Z\\. ']+)(</a>)? at|.*returned by (<a.*?>)?(?<returner>[-a-zA-Z\\. ']+)(</a>)? for (?<return>no gain|\\d{1,3} yards) to)( the (?<side>[a-zA-Z]+) (?<yardline>\\d{1,2}))?"
fg_regex = "<td><a.*?>(?<kicker>[-a-zA-Z\\. ']+)</a> (?<kickdist>\\d{1,3}) yards? field goal (?<made>GOOD|MADE)|(?<missed>MISSED|NO GOOD).*</td>"

#Offensive plays
rush_regex = "<td><a.*?>(?<player>[-a-zA-Z\\. ']+)</a> rush [\\s\\w]*for ((?<gain>\\d+) yards?|(a )?loss of (?<loss>\\d+) yards?|(?<nogain>no gain)).*</td>"
pass_regex = "<td><a.*?>(?<QB>[-a-zA-Z\\. ']+)</a> pass ((?<complete>complete)|(?<incomplete>incomplete))( to <a.*?>(?<receiver>[-a-zA-Z\\. ']+)</a>(?(complete) for ((?<gain>\\d+) yards?|(a )?loss of (?<loss>\\d+) yards?|(?<nogain>no gain)).*</td>))?"

#Turnovers/timeouts/penalties:
fumble_regex = "fumbled?.*(forced by (<a.*?>)?(?<forcer>[-a-zA-Z\\. ']+)(</a>)?)?.*(recovered by (?<team>[a-zA-Z]+) (<a.*?>)?(?<recoverer>[-a-zA-Z\\. ']+)(</a>)?)?"
interception_regex = "intercept(ed|ion)? by (<a.*?>)?(?<intercepter>[-a-zA-Z\\. ']+)(</a>)? at the (?<side>[a-zA-Z]+) (?<yardline>\\d{1,2})[\\.,]?( returned for (?<return>\\d{1,3} yards|no gain))?"
penalty_regex = "<td>(?<team>[-a-zA-Z\\. ']+) penalty (?<dist>\\d{1,3}) yards? (?<penalty>[-a-zA-Z\\. ']+)( on (<a.*?>)?(?<player>[-a-zA-Z\\. ']+)(</a>)?)? (?<decision>accepted|declined).*</td>"
timeout_regex = "<td>Timeout (?<team>[-a-zA-Z\\. ']+).* (?<min>\\d{1,2})?:(?<sec>\\d\\d).*"


#Establish the columns:
rush = pass = punt = kickoff = fg = INT = fumble = pen = timeout = FALSE
poss = down = togo = time = dist = passer = carrier = kicker = returner = gain = retrn = made = intercepter = forced_by = recovered_by = penalized_team = penalized_player = NA
default_row = data.frame(poss, down, time, togo, dist, rush, pass, punt, kickoff, fg, INT, fumble, pen, timeout, passer, carrier, kicker, returner, gain, retrn, made, intercepter, forced_by, recovered_by, penalized_team, penalized_player)

play_table = data.frame(matrix(NA, ncol=length(default_row), nrow=0))  
colnames(play_table) = colnames(default_row)

for (k in 1:length(plays)) {
    #Get the already-established metadata:
    this = default_row
    this$poss = plays[[k]][['poss']]
    this$down = plays[[k]][['down']]
    this$time = plays[[k]][['time']]
    this$togo = plays[[k]][['togo']]
    this$dist = plays[[k]][['dist']]

    pbp = plays[[k]][['pbp']]
    
    if (length(grep(kickoff_regex, pbp, perl=TRUE, fixed=FALSE))>0) {
        match = gregexpr(kickoff_regex, pbp, perl=TRUE, fixed=FALSE)[[1]]
        
    }
    else if (length(grep(fg_regex, pbp, perl=TRUE, fixed=FALSE))>0) {
        print(8)
    }
    else if (length(grep(punt_regex, pbp, perl=TRUE, fixed=FALSE))>0) {
        print(4)
    }
    else if (length(grep(rush_regex, pbp, perl=TRUE, fixed=FALSE))>0) {
        print(2)
    }
    else if (length(grep(pass_regex, pbp, perl=TRUE, fixed=FALSE))>0) {
        print(3)
    }
    else if (length(grep(interception_regex, pbp, perl=TRUE, fixed=FALSE))>0) {
        print(6)
    }
    else if (length(grep(timeout_regex, pbp, perl=TRUE, fixed=FALSE))>0) {
        print(7)
    }
    
    if (length(grep(penalty_regex, pbp, perl=TRUE, fixed=FALSE))>0) {
        
    }
    if (length(grep(fumble_regex, pbp, perl=TRUE, fixed=FALSE))>0) {
        
    }
    
    play_table = cbind(play_table, this)
}




