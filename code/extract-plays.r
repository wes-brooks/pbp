ExtractPlays = function(url) {
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
        drivebreaks = grep("(?<team>[A-Za-z ]+) at (?<min>\\d{1,2}):(?<sec>\\d\\d)",
            quarter_pbp[[k]], perl=TRUE, fixed=FALSE, ignore.case=TRUE)
    
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

            #Calculate the duration of the previous drive:
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
        playmeta_regex = paste("(?<down>1st|2nd|3rd|4th|1ST|2ND|3RD|4TH) (and|AND) ",
            "(?<togo>\\d{1,2}|goal|Goal|GOAL) at (?<field>[A-Za-z]{3,4}) ",
            "(?<yardline>\\d{1,2})", sep='')

        for (j in 1:nplay) {
            playmeta = regex(playmeta_regex, drives[[k]][['pbp']][2*j],
                perl=TRUE, fixed=FALSE, ignore.case=TRUE)
    
            #Get the play-by-play HTML for this play.
            pbp = drives[[k]][['pbp']][2*j+1]

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
            plays[[length(plays)+1]] = list(poss=drives[[k]][['team']], down=down,
                togo=togo, time=time, dist=dist, pbp=pbp, drive=k)
        }
    }

    return(plays)
}