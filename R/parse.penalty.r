parse.penalty = function(play, meta) {
    play$penalty = FALSE
    play$penalty_type = NA
    play$penalized_player = NA
    play$penalty_dist = NA
    play$penalized_team = NA
    
    pbp <- play$pbp
    team.pattern <- paste0(meta$home.school, '|', meta$home.abbrev, '|', meta$away.school, '|', meta$away.abbrev)
    
    penalty_regex0 = paste0("(?<team>", team.pattern, ") penalty,? ( \\(?-?(?<dist>\\d{1,3}) (yd|yard)s?\\)? ",
        "(?<penalty>[-a-zA-Z\\. ']+)?(( on)? \\(?(?<player>[-a-zA-Z,\\. ']+)\\)?)? ",
        "(?<decision>accepted|declined))?")
    
    penalty_regex = paste0("(?<team>", team.pattern, ") penalty,? (?<penalty>[-a-zA-Z\\. ']+) \\(-?(?<dist>\\d{1,3}) (yd|yard)s?\\)",
                           "( \\(?(?<player>[-a-zA-Z,\\. ']+)\\)?)?",
                           "( (?<decision>accepted|declined)?)?")
    
    penalty_regex2 = paste0("penalty,? (?<team>", team.pattern, ") (?<penalty>[-a-zA-Z\\. ']+) \\(-?(?<dist>\\d{1,3}) (yd|yard)s?\\)",
                           "( \\(?(?<player>[-a-zA-Z,\\. ']+)\\)?)?",
                           "( (?<decision>accepted|declined)?)?")
    
    penalty_regex3 = paste0("penalty,? (?<team>", team.pattern, ") (?<penalty>[-a-zA-Z\\. ']+)",
                            "( \\(?(?<player>[-a-zA-Z,\\. ']+)\\)?)?",
                            " \\(?-?(?<dist>\\d{1,3}) (yd|yard)s?\\)?",
                            "( (?<decision>accepted|declined)?)?")
    
    penalty_regex4 = paste0("(?<team>", team.pattern, ") penalty,? (?<penalty>[-a-zA-Z\\. ']+)",
                            "( \\(?(?<player>[-a-zA-Z,\\. ']+)\\)?)?",
                            "( \\(?-?(?<dist>\\d{1,3}) (yd|yard)s?\\)?)?",
                            "( (?<decision>accepted|declined)?)?")

    patterns <- c(penalty_regex, penalty_regex2, penalty_regex3, penalty_regex4, penalty_regex0)
    penalty <- sapply(patterns, function(p) grepl(pattern=p, x=pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) 
    
    if (any(penalty)) {
        match = regex(patterns[which(penalty)[1]], pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
        
        play$penalty = TRUE
        play$penalty_type = match[1,'penalty']
        play$penalized_player = format.name(match[1,'player'])
        play$penalty_dist = as.numeric(match[1,'dist'])
        
        #Identify the penalized team
        play$penalized_team <- assign.team(match[1, 'team'], meta$home.abbrev, meta$away.abbrev)
    }

    return(play)        
}