parse.touchdown <- function(play) {
    play$TD <- FALSE
    
    td_regex <- paste0("(?<touchdown>touchdown|TD)|\\(", name.pattern, "(kick|PAT)( (good|missed|blocked|no good))?\\)")
    
    if (grepl(td_regex, play$pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
        play$TD <- TRUE
    }
    
    return(play)
}