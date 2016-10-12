parse.pat = function(pbp, play) {
    play$PAT = FALSE

    # pat_regex1 = paste0("(?<kicker>[-a-zA-Z,\\. ']+)",
    #     " (extra point|KICK|PAT)",
    #     "( (?<made>GOOD|MADE)| (?<missed>MISSED|NO GOOD|BLOCKED))")
    pat_regex = paste0("(TD|touchdown)[^[:alnum:]]* \\(?(?<kicker>[-a-zA-Z,\\. ']+)",
        " (extra point|KICK|PAT)",
        "( (?<made>GOOD|MADE)| (?<missed>MISSED|NO GOOD|BLOCKED))?\\)?")

    if (grepl(pat_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
        play$PAT <- TRUE
        play$made <- TRUE
        
        match <- regex(pat_regex1, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
        play$kicker <- format.name(match[1,'kicker'])     
           
        if (!is.na(match[1,'missed'])) {
            play$made <- FALSE
        }
    }
    
    return(play)
}