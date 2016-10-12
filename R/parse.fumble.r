parse.fumble <- function(play, meta) {
    play$fumble <- FALSE
    
    team.pattern <- paste(meta$home.school, meta$home.abbrev, meta$away.school, met$away.abbrev, sep='|')
    
    fumble_regex = paste0("fumbled?.*(forced by (?<forcer>", name.pattern, "))?.*",
        "(recovered by (?<team>", team.pattern, ") (?<recoverer>", name.pattern, "))?", sep='')
        
    if (grepl(fumble_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
        match = regex(fumble_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
        play$fumble = TRUE
    }
    
    return(play)
}