parse.rush <- function(pbp, play) {
    play$rush <- FALSE
    play$kneeldown <- FALSE

    # regular expresions to identify rushing plays
    rush_regex1 <- paste0("(?<player>", name.pattern, ") (run|rush) [\\s\\w]*for ((?<gain>\\d+) ",
                         "(yd|yard)s?|(a )?loss of (?<loss>\\d+) (yd|yard)s?|(?<nogain>no gain))")
    rush_regex2 <- paste0("(?<player>", name.pattern, ") (?<gain>\\d+) (yd|yard)s? (run|rush)")
    
    
    if (grepl(rush_regex1, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
        match <- regex(rush_regex1, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
        
        play$rush <- TRUE
        play$carrier <- format.name(match[1,'player'])
        
        if (!is.na(match[1,'gain'])) {play$gain <- as.numeric(match[1,'gain'])}
        else if (!is.na(match[1,'loss'])) {play$gain <- -as.numeric(match[1,'loss'])}
        else if (!is.na(match[1,'nogain'])) {play$gain <- 0}
    } else if (grepl(rush_regex2, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
        match <- regex(rush_regex2, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
        
        play$rush <- TRUE
        play$carrier <- format.name(match[1,'player'])
        
        if (!is.na(match[1,'gain'])) {play$gain <- as.numeric(match[1,'gain'])}
    }
    
    # kneeldowns are coded as "team rush"
    if (play$rush && play$carrier %in% c('TEAM', 'team', 'Team')) {
      play$rush <- FALSE
      play$kneeldown <- TRUE
    }

    return(play)
}