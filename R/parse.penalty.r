parse.penalty = function(pbp, play) {
    play$penalty = FALSE
    play$penalty_type = NA
    play$penalized_player = NA
    play$penalty_dist = NA
    play$penalized_team = NA

    penalty_regex = paste("(?<team>[-a-zA-Z\\. ']+) penalty( (?<dist>\\d{1,3}) (yd|yard)s? ",
        "(?<penalty>[-a-zA-Z\\. ']+?)( on (?<player>[-a-zA-Z\\. ']+))? ",
        "(?<decision>accepted|declined))?", sep='')

    if (grepl(penalty_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
        match = regex(penalty_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
        
        play$penalty = TRUE
        play$penalty_type = match[1,'penalty']
        play$penalized_player = match[1,'player']
        play$penalty_dist = match[1,'dist']
        
        #Identify the penalized team
        if (!is.na(match[1,'team'])) {
            teams = c(play$poss, play$def)
            costs = list(insertions=2, deletions=0, substitutions=1)
            t1.unlike = adist(teams[1], match[1,'team'], costs=costs, ignore.case=TRUE)
            t2.unlike = adist(teams[2], match[1,'team'], costs=costs, ignore.case=TRUE)
            if (t1.unlike != t2.unlike) {
                play$penalized_team = teams[which.min(c(t1.unlike, t2.unlike))]
            } else {play$penalized_team = match[1,'team']}
        }
    }

    return(play)        
}