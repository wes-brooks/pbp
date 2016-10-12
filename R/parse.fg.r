parse.fg = function(pbp, play) {
    play$FG = FALSE

    fg_regex = paste("(?<kicker>[-a-zA-Z,\\. ']+) (?<kickdist>\\d{1,3}) (yd|yard)s? (field goal|FG) ",
        "((?<made>GOOD|MADE)|(?<missed>MISSED|NO GOOD)).*", sep='')
        
    if (grepl(fg_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
        match = regex(fg_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
        
        play$FG = TRUE
        play$kicker = format.name(match[1,'kicker'])
        play$kick_dist = match[1,'kickdist']
        if (!is.na(match[1,'made'])) {play$made = TRUE}
        else {play$made = FALSE}
    }
    
    return(play)        
}