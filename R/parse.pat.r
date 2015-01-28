parse.pat = function(pbp, play) {
    play$PAT = FALSE

    pat_regex1 = paste("(?<kicker>[-a-zA-Z\\. ']+) (extra point|KICK)",
        " (?<made>GOOD|MADE)|(?<missed>MISSED|NO GOOD)", sep='')
    pat_regex2 = paste("\\((?<kicker>[-a-zA-Z\\. ']+) (extra point|KICK)",
        "( (?<made>GOOD|MADE)|(?<missed>MISSED|NO GOOD))?\\)", sep='')

    if (grepl(pat_regex1, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
        play$PAT = TRUE
        play$made = TRUE
        
        match = regex(pat_regex1, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
        play$kicker = match[1,'kicker']     
           
        if (!is.na(match[1,'missed'])) {
            play$made = FALSE
        }
    } else if (grepl(pat_regex2, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
        play$PAT = TRUE
        play$made = TRUE
        
        match = regex(pat_regex2, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
        play$kicker = match[1,'kicker']     
           
        if (!is.na(match[1,'missed'])) {
            play$made = FALSE
        }
    }
    
    return(play)
}