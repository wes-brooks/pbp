parse.fumble = function(pbp, play) {
    play$fumble = FALSE
    
    fumble_regex = paste0("fumbled?.*(forced by (?<forcer>", name.pattern, "))?.*",
        "(recovered by (?<team>[a-zA-Z]+) (?<recoverer>", name.pattern, "))?", sep='')
        
    if (grepl(fumble_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
        match = regex(fumble_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
        play$fumble = TRUE
    }
    
    return(play)
}