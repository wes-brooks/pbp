parse.touchdown = function(pbp, play) {
    play$TD = FALSE
    
    td_regex = "(?<touchdown>touchdown|TD)"
    
    if (grepl(td_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
        play$TD = TRUE
    }
    
    return(play)
}