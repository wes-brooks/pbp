parse.first.down = function(pbp, play) {
    play$first.down = FALSE
    
    first_regex = "(?<first>1st down|first down)"
    
    if (grepl(first_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
        play$first.down = TRUE
    }
    
    return(play)    
}