#' Check whether this play is a kick off and if so, interpret it accordingly.
#'
parse.kickoff = function(pbp, play) {
    play$kickoff = FALSE

    kickoff_regex = paste("(?<kicker>[-a-zA-Z\\. ']+) kickoff for (?<kickdist>\\d{1,3}) ",
        "(yd|yard)s? (returned by (?<returner>[-a-zA-Z\\. ']+) for ((?<retgain>\\d{1,3}) ",
        "(yd|yard)s?|(a )?loss of (?<retloss>\\d+) (yd|yard)s?|(?<retnogain>no gain))",
        "|.*(?<touchback>touchback))?", sep='')
        
    if (grepl(kickoff_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
        match = regex(kickoff_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
        play$kickoff = TRUE
        play$kicker = match[1,'kicker']
        play$returner = match[1,'returner']
        play$kick_dist = match[1,'kickdist']
        
        #Define the kicking team to have possession and remove kickoffs from drives
        #If first play of a drive is a kickoff, then flip possession.
        if (play$playnum == 1) {
            tmp = play$poss
            play$poss = play$def
            play$def = tmp
        }
    
        if (!is.na(match[1,'touchback'])) {
            play$touchback = TRUE
            play$kick_return = 0
        } else {play$touchback=FALSE}
    
        if (!is.na(match[1,'retgain'])) {play$kick_return = as.numeric(match[1,'retgain'])}
        else if (!is.na(match[1,'retloss'])) {play$kick_return = -as.numeric(match[1,'retloss'])}
        else if (!is.na(match[1,'retnogain'])) {play$kick_return = 0}
    }
    
    return(play)
}