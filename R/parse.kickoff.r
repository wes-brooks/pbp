#' Check whether this play is a kick off and if so, interpret it accordingly.
#'
parse.kickoff <- function(play) {
    play$kickoff <- FALSE

    kickoff_regex <- paste("(?<kicker>", name.pattern, ") kickoff for (?<kickdist>\\d{1,3}) ",
        "(yd|yard)s?\\s?,? (((?<returner1>", name.pattern, ") return|returned by (?<returner2>", name.pattern, ")) for ((?<retgain>\\d{1,3}) ",
        "(yd|yard)s?|(a )?loss of (?<retloss>\\d+) (yd|yard)s?|(?<retnogain>no gain))",
        "|.*(?<touchback>touchback))?", sep='')
        
    if (grepl(kickoff_regex, play$pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
        match <- regex(kickoff_regex, play$pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
        play$kickoff <- TRUE
        play$kicker <- format.name(match[1,'kicker'])
        play$returner <- ifelse(!is.na(match[1,'returner1']), format.name(match[1,'returner1']), format.name(match[1,'returner2']))
        play$kick_dist <- match[1,'kickdist']
        
        #Define the kicking team to have possession and remove kickoffs from drives
        #If first play of a drive is a kickoff, then flip possession.
        if (play$playnum == 1) {
            tmp <- play$poss
            play$poss <- play$def
            play$def <- tmp
        }
    
        if (!is.na(match[1,'touchback'])) {
            play$touchback <- TRUE
            play$kick_return <- 0
        } else play$touchback <- FALSE
    
        if (!is.na(match[1,'retgain'])) {play$kick_return <- as.numeric(match[1,'retgain'])}
        else if (!is.na(match[1,'retloss'])) {play$kick_return <- -as.numeric(match[1,'retloss'])}
        else if (!is.na(match[1,'retnogain'])) {play$kick_return <- 0}
    }
    
    return(play)
}