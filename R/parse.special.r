parse.special <- function(play) {
    play$kicker <- NA
    play$made <- NA
    play$returner <- NA
    play$kick_dist <- NA
    play$touchback <- FALSE
    play$kick_return <- NA
    
    play <- parse.kickoff(play)
    play <- parse.punt(play)
    play <- parse.pat(play)
    play <- parse.fg(play)
    
    return(play)
}