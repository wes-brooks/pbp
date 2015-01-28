parse.pass = function(pbp, play) {
    play$pass = FALSE
    play$complete = NA
    play$passer = NA

    pass_regex1 = paste("(?<QB>[-a-zA-Z\\. ']+) pass ((?<complete>complete)|",
        "(?<incomplete>incomplete))(( to (?<receiver>[-a-zA-Z\\. ']+).*(?(complete) for ",
        "((?<gain>\\d+) (yd|yard)s?|(a )?loss of (?<loss>\\d+) (yd|yard)s?|",
        "(?<nogain>no gain))))?)?", sep="")
        
    pass_regex2 = "(?<receiver>[-a-zA-Z\\. ']+) ((?<gain>\\d+) (yd|yard)s? )?pass .*from (?<QB>[-a-zA-Z\\. ']+)"
        
    if (grepl(pass_regex1, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
        match = regex(pass_regex1, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
        play$pass = TRUE
        play$passer = match[1,'QB']
        play$carrier = match[1,'receiver']
    
        if (!is.na(match[1,'gain'])) {play$gain = as.numeric(match[1,'gain'])}
        else if (!is.na(match[1,'loss'])) {play$gain = -as.numeric(match[1,'loss'])}
        else if (!is.na(match[1,'nogain'])) {play$gain = 0}
    
        if (!is.na(match[1,'complete'])) {play$complete = TRUE}
        else {
            play$complete = FALSE
            play$gain = 0
        }
    } else if (grepl(pass_regex2, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
        match = regex(pass_regex2, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
        play$pass = TRUE
        play$passer = match[1,'QB']
        play$carrier = match[1,'receiver']
    
        if (!is.na(match[1,'gain'])) {
            play$gain = as.numeric(match[1,'gain'])
            play$complete = TRUE    
        }
        #else if (!is.na(match[1,'loss'])) {play$gain = -as.numeric(match[1,'loss'])}
        #else if (!is.na(match[1,'nogain'])) {play$gain = 0}
    
        #if (!is.na(match[1,'complete'])) {play$complete = TRUE}
        #else {
        #    play$complete = FALSE
        #    play$gain = 0
        #}
    }
        
    return(play)
}