parse.rush <- function(pbp, play) {
    play$rush <- FALSE
    
    # rush_regex1 <- paste("(?<player>(\\d({1,2}-)?[-a-zA-Z,\\. ']+) (run|rush) [\\s\\w]*for ((?<gain>\\d+) ",
    #     "(yd|yard)s?|(a )?loss of (?<loss>\\d+) (yd|yard)s?|(?<nogain>no gain))", sep='')
    # rush_regex2 <- "(?<player>(\\d({1,2}-)?[-a-zA-Z,\\. ']+) (?<gain>\\d+) (yd|yard)s? (run|rush)"

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

    return(play)
}