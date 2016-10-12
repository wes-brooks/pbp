parse.fumble = function(pbp, play) {
    play$fumble = FALSE
    
    fumble_regex = paste("fumbled?.*(forced by (?<forcer>[-a-zA-Z,\\. ']+))?.*",
        "(recovered by (?<team>[a-zA-Z]+) (?<recoverer>[-a-zA-Z,\\. ']+))?", sep='')
        
    if (grepl(fumble_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
        match = regex(fumble_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
        play$fumble = TRUE
    }
    
    return(play)
}