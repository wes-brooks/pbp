ExtractPlays = function(url) {
    tree <- htmlTreeParse(url, isURL=TRUE, useInternalNodes=TRUE)

    pbp <- xpathSApply(tree, paste(
        "//table[contains(@class,'mod-pbp')]/child::tr/td[position()<3]",
        "//table[contains(@class,'mod-pbp')]/child::tbody/tr/child::td[position()<3]",
        "//table[contains(@class,'mod-pbp')]/child::thead/tr/th[1]",
        "//table[contains(@class,'mod-pbp')]/child::thead/tr/td", sep=" | "), xmlValue) 
        
    scoreCols = xpathSApply(tree,
        "//table[contains(@class,'mod-pbp')]/thead/tr[@class='team-color-strip' and count(th)=3]/th[position()>1]",
        xmlValue)[1:2]
        
    scores <- xpathSApply(tree,
        "//table[contains(@class,'mod-pbp')]/child::tr[count(td)=4]/td[position()>=3 and position()<5]",
        xmlValue) 
    scores = matrix(as.integer(scores), length(scores)/2, 2, byrow=TRUE)
    scores = rbind(c(0,0), scores)
    for (i in 2:nrow(scores)) {
        if (all(is.na(scores[i,])))
            scores[i,] = scores[i-1,]
    }

    #Quarter breaks:
    quarters = c('1st', '2nd', '3rd', '4th')
    starts = sapply(quarters, function(x) {paste(x, "Quarter Play-by-Play")})
    breaks = lapply(starts, function(x) {grep(x, pbp, ignore.case=TRUE)})

    #Get the play-by-play HTML for the four quarters into a list
    quarter_pbp = list()
    for (k in 1:4) {
        start = breaks[[k]]

        #Snip off the end-of-quarter "plays"
        endq = paste("end of ", quarters[k], " quarter", sep="")
        endq = which(grepl(endq, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE))[1]
        
        if (!is.na(endq)) {
            end = endq
        } else if (k<4) {
            end = breaks[[k+1]]-1
        } else {end = length(pbp)}
    
        quarter_pbp[[k]] = pbp[start:end]
    }
    
    #This is a flag indicating a possible timekeeping error:
    fishy = FALSE

    #Drive breaks:
    #Divide each quarter of play-by-play into drives:
    #(Note that the kickoffs to begin each half will be considered drives here.)
    drives = list()
    for (k in 1:4) {
        #Begin by identifying the beginning of each drive:
        drivebreaks = grep("(?<team>[A-Za-z ]+) at (?<min>\\d{1,2}):(?<sec>\\d\\d)",
            quarter_pbp[[k]], perl=TRUE, fixed=FALSE, ignore.case=TRUE)
    
        for (j in 1:length(drivebreaks)) {
            head = quarter_pbp[[k]][drivebreaks[j]] 
            match = regex("(?<team>[A-Za-z ]+) at (?<min>\\d{1,2}):(?<sec>\\d\\d)", head) 
        
            #Drive-level metadata:
            teamname = match[1,'team']
            minutes = as.numeric(match[1,'min'])
            seconds = as.numeric(match[1,'sec'])

            #Game time (in seconds) remaining at the beginning of the drive:
            #(Second half always starts at 1800, but sometimes data is screwy)
            time = ((4-k)*15 + minutes)*60 + seconds
            if (k==3 && j==1) 
                time = 1800
        
            #Extract the play-by-play HTML for this drive:
            if (j<length(drivebreaks)) {
            nextstart = drivebreaks[j+1]
            } else {nextstart = length(quarter_pbp[[k]])}
            drive_pbp = quarter_pbp[[k]][drivebreaks[j]:(nextstart-1)]

            #One drive can span the 1st-2nd or 3rd-4th quarters:
            i = length(drives)
            if ((k==2 || k==4) && j==1 && teamname==drives[[i]]$team) {
                len = length(drives[[i]]$pbp)
                drives[[i]]$pbp = c(drives[[i]]$pbp[-len], drive_pbp[-1])
            }

            #If this is a new drive then add it to the list.
            else {
                drives[[i+1]] = list(team=teamname, time=time, pbp=drive_pbp)
            
                #Indicate the half
                drives[[i+1]]$half = ifelse(k %in% c(1,2), 1, 2)
            }

            #Calculate the duration of the previous drive:
            if (i>0) {drives[[i]]$duration = drives[[i]]$time - time}
            
            #Generally, drives should not begin with zero time remaining
            if (fishy) {
                drives[[i]]$time = time
                drives[[i-1]]$duration = drives[[i-1]]$time - drives[[i]]$time
                drives[[i]]$duration = drives[[i]]$time - time
                fishy = FALSE
            } 
            if (minutes==0 && seconds==0)
                fishy = TRUE
        }
    }

    #Set the duration of the final drive equal to the remaining game time
    drives[[length(drives)]]$duration = drives[[length(drives)]]$time

    #Get the unique team names:
    teams = unique(sapply(drives, function(x) {x$team}))
    
    #Make sure to label the scores properly:
    costs = list(insertions=2, deletions=0, substitutions=1)
    home_unlike1 = adist(teams[1], scoreCols[1], costs=costs, ignore.case=TRUE)
    home_unlike2 = adist(teams[2], scoreCols[1], costs=costs, ignore.case=TRUE)
    away_unlike1 = adist(teams[1], scoreCols[2], costs=costs, ignore.case=TRUE)
    away_unlike2 = adist(teams[2], scoreCols[2], costs=costs, ignore.case=TRUE)
    team1 = which.max(c(home_unlike1+away_unlike2, home_unlike2+away_unlike1)) 
    colnames(scores) = c(teams[-team1], teams[team1])

    #Summarize each play and attach metadata
    plays = list()

    for (k in 1:length(drives)) {
        off = drives[[k]]$team
        def = teams[teams != off]
                
        #Break the drive into plays:
        nplay = (length(drives[[k]]$pbp)-1) %/% 2
        playmeta_regex = paste("(?<down>1st|2nd|3rd|4th|1ST|2ND|3RD|4TH) (and|AND) ",
            "(?<togo>\\d{1,2}|goal|Goal|GOAL) at (?<field>[A-Za-z]{2,4})? ?",
            "(?<yardline>\\d{1,2})", sep='')

        for (j in 1:nplay) {
            playmeta = regex(playmeta_regex, drives[[k]]$pbp[2*j],
                perl=TRUE, fixed=FALSE, ignore.case=TRUE)
    
            #Get the play-by-play HTML for this play.
            pbp = drives[[k]]$pbp[2*j+1]

            ###Play-level metadata:
            down = playmeta[1,'down']
            togo = playmeta[1,'togo']

            #Guestimate the game time remaining:
            time = drives[[k]]$time - (j-1)/nplay * drives[[k]]$duration

            ##How far from the end zone did the play begin?
            
            #Figure out which yard-line the play began from:
            line = playmeta[1,'yardline']
            if (!is.na(line))
                line = as.numeric(line)
            
            #Figure out which side of the field the play began from:
            field = playmeta[1,'field']
            if (!is.na(field)) {    
                #which team name is the field most like?
                offense_unlike = adist(off, field, costs=costs, ignore.case=TRUE)
                defense_unlike = adist(def, field, costs=costs, ignore.case=TRUE)

                #Now compute the distance from the goal line:
                if (offense_unlike < defense_unlike) {dist = 100 - line}
                else {dist = line}
            } else {
                dist = line
            }        
        
            #Replace 'goal' to go with the distance to the goal line:
            if (!is.na(togo)) 
                if (substr(togo[1],1,1)=='g' || substr(togo[1],1,1)=='G') {togo = dist}
            togo = as.numeric(togo)
        
            #Add this play to the list
            plays[[length(plays)+1]] = list(poss=off, def=def, down=down,
                togo=togo, time=time, dist=dist, pbp=pbp, drive=k, playnum=j, drive=k)
        }
    }

    return(list(plays=plays, scores=scores))
}