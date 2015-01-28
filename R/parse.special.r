parse.special = function(pbp, play) {
    play$kicker = NA
    play$made = NA
    play$returner = NA
    play$kick_dist = NA
    play$touchback = FALSE
    play$kick_return = NA
    
    play = parse.kickoff(pbp, play)
    play = parse.punt(pbp, play)
    play = parse.pat(pbp, play)
    play = parse.fg(pbp, play)
    
    return(play)
}