parse.interception <- function(play) {
    play$INT <- NA
    play$intercepter <- NA
    play$int_return <- NA

    interception_regex <- paste0("pass intercept(ed|ion)?( by)? (?<intercepter>", name.pattern, ")",
        "( at (the )?(?<side>[a-zA-Z]+) (?<yardline>\\d{1,2}))?[\\.,]? return(ed)? for ",
        "((?<retgain>\\d{1,3}) (yd|yard)s?|(a )?loss of (?<retloss>\\d+) ",
        "(yd|yard)s?|(?<retnogain>no gain))")   
    
    interception_regex2 <- paste0("(?<intercepter>", name.pattern, ")( (?<retgain>\\d{1,3}) (yd|yard)s?)? interception return")   

    # test whether this play matches any off the pass patterns
    int.patterns <- c(interception_regex, interception_regex2)
    int.match <- sapply(int.patterns, function(p) grepl(pattern=p, x=play$pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE))
    
    
    if (any(int.match)) {
        match <- regex(int.patterns[which(int.match)[1]], play$pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
        play$pass <- TRUE
        play$INT <- TRUE
        play$complete <- FALSE
        play$intercepter <- format.name(match[1,'intercepter'])
    
        if (!is.na(match[1,'retgain'])) {play$int_return <- as.numeric(match[1,'retgain'])}
        else if (int.match[[2]] && !is.na(match[1,'retloss'])) {play$int_return <- -as.numeric(match[1,'retloss'])}
        else if (int.match[[2]] && !is.na(match[1,'retnogain'])) {play$int_return <- 0}
    }
    
    return(play)
}