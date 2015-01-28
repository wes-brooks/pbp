parse.interception = function(pbp, play) {
    play$INT = NA
    play$intercepter = NA
    play$int_return = NA

    interception_regex = paste("intercept(ed|ion)? by (?<intercepter>[-a-zA-Z\\. ']+) ",
        "at (the )?(?<side>[a-zA-Z]+) (?<yardline>\\d{1,2})[\\.,]?( returned for ",
        "((?<retgain>\\d{1,3}) (yd|yard)s?|(a )?loss of (?<retloss>\\d+) ",
        "(yd|yard)s?|(?<retnogain>no gain)))?", sep='')    

    if (grepl(interception_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
        match = regex(interception_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
        play$pass = TRUE
        play$INT = TRUE
        play$complete = FALSE
        play$intercepter = match[1,'intercepter']
    
        if (!is.na(match[1,'retgain'])) {play$int_return = as.numeric(match[1,'retgain'])}
        else if (!is.na(match[1,'retloss'])) {play$int_return = -as.numeric(match[1,'retloss'])}
        else if (!is.na(match[1,'retnogain'])) {play$int_return = 0}
    }
    
    return(play)
}