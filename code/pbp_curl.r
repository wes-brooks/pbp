#Load the raw data and extract the part including 'mod-pbp', the play-by-play module.
library(RCurl)
library(XML)

url = "http://scores.espn.go.com/ncf/playbyplay?gameId=332430275&period=0"
tree <- htmlTreeParse(url, isURL=TRUE, useInternalNodes=TRUE)

pbp <- xpathSApply(tree, paste(
    "//table[contains(@class,'mod-pbp')]/child::tr/td[position()<3]",
    "//table[contains(@class,'mod-pbp')]/child::tbody/tr/child::td[position()<3]",
    "//table[contains(@class,'mod-pbp')]/child::thead/tr/th[1]",
    "//table[contains(@class,'mod-pbp')]/child::thead/tr/td", sep=" | "), xmlValue) 


#Quarter breaks:
quarters = c('1st', '2nd', '3rd', '4th')
starts = sapply(quarters, function(x) {paste(x, "Quarter Play by Play")})
breaks = lapply(starts, function(x) {grep(x, pbp, ignore.case=TRUE)})

#Get the play-by-play HTML for the four quarters into a list
quarter_pbp = list()
for (k in 1:4) {
    start = breaks[[k]]
    if (k<4) {end = breaks[[k+1]]-1}
    else {end = length(pbp)}
    
    quarter_pbp[[k]] = pbp[start:end]
}

#Drive breaks:
#Divide each quarter of play-by-play into drives:
#(Note that the kickoffs to begin each half will be considered drives here.)
drives = list()
for (k in 1:4) {
    #Begin by identifying the beginning of each drive:
    drivebreaks = grep("(?<team>[A-Za-z ]+) at (?<min>\\d{1,2}):(?<sec>\\d\\d)", quarter_pbp[[k]], perl=TRUE, fixed=FALSE, ignore.case=TRUE)
    
    for (j in 1:length(drivebreaks)) {
        head = quarter_pbp[[k]][drivebreaks[j]] 
        match = regex("(?<team>[A-Za-z ]+) at (?<min>\\d{1,2}):(?<sec>\\d\\d)", head) 
        
        #Drive-level metadata:
        teamname = match[1,'team']
        minutes = match[1,'min']
        seconds = match[1,'sec']

        #Game time (in seconds) remaining at the beginning of the drive:
        time = ((4-k)*15 + as.numeric(minutes))*60 + as.numeric(seconds)
        
        #Extract the play-by-play HTML for this drive:
        if (j<length(drivebreaks)) {nextstart = drivebreaks[j+1]}
        else {nextstart = length(quarter_pbp[[k]])}
        drive_pbp = quarter_pbp[[k]][drivebreaks[j]:(nextstart-1)]

        #One drive can span the 1st-2nd or 3rd-4th quarters:
        i = length(drives)
        if ((k==2 || k==4) && j==1 && teamname==drives[[i]][['team']]) {
            drives[[i]][['pbp']] = c(drives[[i]][['pbp']], drive_pbp)
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
    #Break the drive into plays:
    nplay = (length(drives[[k]][['pbp']])-1) %/% 2
    playmeta_regex = "(?<down>1st|2nd|3rd|4th|1ST|2ND|3RD|4TH) (and|AND) (?<togo>\\d{1,2}|goal|Goal|GOAL) at (?<field>[A-Za-z]{3,4}) (?<yardline>\\d{1,2})" 

    for (j in 1:nplay) {
        playmeta = regex(playmeta_regex, drives[[k]][['pbp']][2*j], perl=TRUE, fixed=FALSE, ignore.case=TRUE)
    
        #Get the play-by-play HTML for this play.
        pbp = drives[[k]][['pbp']][2*j+1] #substr(drives[[k]][['pbp']], playbreaks[['raw']][j], end)

        ###Play-level metadata:
        down = playmeta[1,'down']
        togo = playmeta[1,'togo']

        #Guestimate the game time remaining:
        time = drives[[k]][['time']] - (j-1)/nplay * drives[[k]][['duration']]

        ##How far from the end zone did the play begin?
        #Figure out which side of the field the play began from:
        field = playmeta[1,'field']

        if (!is.na(field)) {
            #Figure out which yard-line the play began from:
            line = as.numeric(playmeta[1,'yardline'])
    
            #which team name is the field most like?
            costs = list(insertions=2, deletions=0, substitutions=1)
            off = drives[[k]][['team']]
            def = teams[teams != off]
            offense_unlike = adist(off, field, costs=costs, ignore.case=TRUE)
            defense_unlike = adist(def, field, costs=costs, ignore.case=TRUE)

            #Now compute the distance from the goal line:
            if (offense_unlike < defense_unlike) {dist = 100 - line}
            else {dist = line}
        
            #Replace 'goal' to go with the distance to the goal line:
            if (substr(togo[1],1,1)=='g' || substr(togo[1],1,1)=='G') {togo = dist}
            togo = as.numeric(togo)
        } else {
            dist = NA
            togo=NA
        }        
        
        #Add this play to the list
        plays[[length(plays)+1]] = list(poss=drives[[k]][['team']], down=down, togo=togo, time=time, dist=dist, pbp=pbp)
    }
}

#Special teams plays:
kickoff_regex = "(?<kicker>[-a-zA-Z\\. ']+) kickoff for (?<kickdist>\\d{1,3}) yards? (returned by (?<returner>[-a-zA-Z\\. ']+) for ((?<retgain>\\d{1,3}) yards|(a )?loss of (?<retloss>\\d+) yards?|(?<retnogain>no gain))|.*(?<touchback>touchback)).*"
punt_regex = "(?<punter>[-a-zA-Z\\. ']+) punt for (?<kickdist>\\d{1,3}) yards?(.*(?<touchback>touchback).*|.*out[- ]of[- ]bounds at|.*fair catch by (?<catcher>[-a-zA-Z\\. ']+) at|.*returned by (?<returner>[-a-zA-Z\\. ']+) (for ((?<retgain>\\d{1,3}) yards|(a )?loss of (?<retloss>\\d+) yards?|(?<retnogain>no gain)))?)?"
fg_regex = "(?<kicker>[-a-zA-Z\\. ']+) (?<kickdist>\\d{1,3}) yards? field goal (?<made>GOOD|MADE)|(?<missed>MISSED|NO GOOD).*"
pat_regex = "(?<kicker>[-a-zA-Z\\. ']+) extra point (?<made>GOOD|MADE)|(?<missed>MISSED|NO GOOD)"

#Scrimmage plays
rush_regex = "(?<player>[-a-zA-Z\\. ']+) rush [\\s\\w]*for ((?<gain>\\d+) yards?|(a )?loss of (?<loss>\\d+) yards?|(?<nogain>no gain))"
pass_regex = "(?<QB>[-a-zA-Z\\. ']+) pass (((?<complete>complete)|(?<incomplete>incomplete))( to (?<receiver>[-a-zA-Z\\. ']+).*(?(complete) for ((?<gain>\\d+) yards?|(a )?loss of (?<loss>\\d+) yards?|(?<nogain>no gain))))?)?"

#Turnovers/timeouts/penalties:
fumble_regex = "fumbled?.*(forced by (?<forcer>[-a-zA-Z\\. ']+))?.*(recovered by (?<team>[a-zA-Z]+) (?<recoverer>[-a-zA-Z\\. ']+))?"
interception_regex = "intercept(ed|ion)? by (?<intercepter>[-a-zA-Z\\. ']+) at (the )?(?<side>[a-zA-Z]+) (?<yardline>\\d{1,2})[\\.,]?( returned for ((?<retgain>\\d{1,3}) yards|(a )?loss of (?<retloss>\\d+) yards?|(?<retnogain>no gain)))?"
penalty_regex = "(?<team>[-a-zA-Z\\. ']+) penalty (?<dist>\\d{1,3}) yards? (?<penalty>[-a-zA-Z\\. ']+)( on (?<player>[-a-zA-Z\\. ']+))? (?<decision>accepted|declined)"
timeout_regex = "Timeout (?<team>[-a-zA-Z\\. ']+).* (?<min>\\d{1,2})?:(?<sec>\\d\\d)"

#Results:
first_regex = "(?<first>1st down|first down)"
td_regex = "(?<touchdown>touchdown)"

#Establish the columns:
rush = pass = sack = td = first = punt = kickoff = fg = pat = INT = fumble = penalty = timeout = FALSE
poss = down = togo = time = dist = passer = carrier = kicker = returner = touchback = faircatch = kick_dist = kick_return = gain = fumble_return = int_return = made = intercepter = forced_by = recovered_by = penalized_team = penalized_player = penalty_dist = complete = NA
default_row = data.frame(poss, down, time, togo, dist, gain, rush, pass, complete, sack, td, first, punt, kickoff, fg, pat, INT, fumble, penalty, timeout, passer, carrier, kicker, returner, touchback, faircatch, kick_dist, kick_return, fumble_return, made, intercepter, int_return, forced_by, recovered_by, penalized_team, penalized_player, penalty_dist)

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
    
    if (length(grep(fg_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE))>0) {
        match = regex(fg_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
        this$fg = TRUE
        this$kicker = match[1,'kicker']
        this$dist = match[1,'kickdist']
        if (!is.na(match[1,'made'])) {this$made = TRUE}
        else {this$made = FALSE}
    } else if (length(grep(punt_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE))>0) {
        match = regex(punt_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
        this$punt = TRUE
        this$kicker = match[1,'punter']
        this$returner = match[1,'returner']
        this$kick_dist = match[1,'kickdist']
        
        if (!is.na(match[1,'touchback'])) {
            this$touchback = TRUE
            this$kick_return = 0
        }
        if (!is.na(match[1,'catcher'])) {
            this$faircatch = TRUE
            this$returner = match[1,'catcher']
            this$kick_return = 0
        }
        
        if (!is.na(match[1,'retgain'])) {this$kick_return = as.numeric(match[1,'retgain'])}
        else if (!is.na(match[1,'retloss'])) {this$kick_return = -as.numeric(match[1,'retloss'])}
        else if (!is.na(match[1,'retnogain'])) {this$kick_return = 0}
    } else if (length(grep(rush_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE))>0) {
        match = regex(rush_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
        this$rush = TRUE
        this$carrier = match[1,'player']
        if (!is.na(match[1,'gain'])) {this$gain = as.numeric(match[1,'gain'])}
        else if (!is.na(match[1,'loss'])) {this$gain = -as.numeric(match[1,'loss'])}
        else if (!is.na(match[1,'nogain'])) {this$gain = 0}
    } else if (length(grep(pass_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE))>0) {
        match = regex(pass_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
        this$pass = TRUE
        this$passer = match[1,'QB']
        this$carrier = match[1,'receiver']
        
        if (!is.na(match[1,'gain'])) {this$gain = as.numeric(match[1,'gain'])}
        else if (!is.na(match[1,'loss'])) {this$gain = -as.numeric(match[1,'loss'])}
        else if (!is.na(match[1,'nogain'])) {this$gain = 0}
        
        if (!is.na(match[1,'complete'])) {this$complete = TRUE}
        else {
            this$complete = FALSE
            this$gain = 0
        }
    } else if (length(grep(timeout_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE))>0) {
    }
    
    #Fumbles, penalties, touchdowns, first downs can happen on any play:
    if (length(grep(penalty_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE))>0) {
        match = regex(penalty_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
        this$penalty = TRUE
    }
    if (length(grep(fumble_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE))>0) {
        match = regex(fumble_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
        this$fumble = TRUE
    }
    if (length(grep(td_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE))>0) {
        this$td = TRUE
        match = regex(pat_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
        if (!is.na(match[1,'made'])) {
            this$pat = TRUE
            this$made = TRUE
            this$kicker = match[1,'kicker']
        } else if (!is.na(match[1,'missed'])) {
            this$pat = TRUE
            this$made = FALSE
            this$kicker = match[1,'kicker']
        }
    }
    if (length(grep(first_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE))>0) {
        this$first = TRUE
    }
    if (length(grep(kickoff_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE))>0) {
        match = regex(kickoff_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
        this$kickoff = TRUE
        this$kicker = match[1,'kicker']
        this$returner = match[1,'returner']
        this$kick_dist = match[1,'kickdist']
        
        if (!is.na(match[1,'touchback'])) {
            this$touchback = TRUE
            this$kick_return = 0
        }
        
        if (!is.na(match[1,'retgain'])) {this$kick_return = as.numeric(match[1,'retgain'])}
        else if (!is.na(match[1,'retloss'])) {this$kick_return = -as.numeric(match[1,'retloss'])}
        else if (!is.na(match[1,'retnogain'])) {this$kick_return = 0}
    }
    if (length(grep(interception_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE))>0) {
        match = regex(interception_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
        this$pass = TRUE
        this$INT = TRUE
        this$complete = FALSE
        this$intercepter = match[1,'intercepter']
        
        if (!is.na(match[1,'retgain'])) {this$int_return = as.numeric(match[1,'retgain'])}
        else if (!is.na(match[1,'retloss'])) {this$int_return = -as.numeric(match[1,'retloss'])}
        else if (!is.na(match[1,'retnogain'])) {this$int_return = 0}
    }
    
    play_table = rbind(play_table, this)
}


#Sacks should count against passing, not rushing. Consider anyone who threw at least two passes a QB:
QBs = vector()
for (qb in unique(play_table$passer)) {
    if (!is.na(qb) && sum(play_table$passer==qb, na.rm=TRUE)) {
        QBs = c(QBs, qb)
    }
}

#Now any non-positive rush for a QB should be a sack.
sack_indx = which(play_table$rush & play_table$gain<=0 & play_table$carrier %in% QBs)
play_table$rush[sack_indx] = FALSE
play_table$pass[sack_indx] = TRUE
play_table$complete[sack_indx] = FALSE
play_table$sack[sack_indx] = TRUE


