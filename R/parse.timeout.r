parse.timeout = function(pbp, play) {
    play$timeout = FALSE
    play$timeout_team = NA
    
    timeout_regex = "Timeout (?<team>[-a-zA-Z\\. ']+).* (?<min>\\d{1,2})?:(?<sec>\\d\\d)"
    
    if (grepl(timeout_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
        play$timeout = TRUE
        
        match = regex(timeout_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
                
        if (!is.na(match[1,'team'])) {
            teams = c(play$poss, play$def)
            costs = list(insertions=2, deletions=0, substitutions=1)
            t1.unlike = adist(teams[1], match[1,'team'], costs=costs, ignore.case=TRUE)
            t2.unlike = adist(teams[2], match[1,'team'], costs=costs, ignore.case=TRUE)
            if (t1.unlike != t2.unlike) {
                play$timeout_team = teams[which.min(c(t1.unlike, t2.unlike))]
            } else {play$timeout_team = match[1,'team']}
        }
    }
    
    return(play)
}