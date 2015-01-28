#' Scrape a game's play-by-play data
#'
#' Connects the the given \code{url}, scrapes the play-by-play data, and interprets it as football plays.
#'
#' @param url url of the website containing the data to parse, provided as a string
#' @return A \code{data.table} representing each play of the game as one row.
#' @examples
#' plays = parse.url("http://espn.go.com/ncf/playbyplay?gameId=400610325&period=0")
#' 
#' @export
parse.url = function(url) {

    pbp = ExtractPlays(url)
    
    plays = pbp$plays
    scores = pbp$scores
    teams = colnames(pbp$scores)

    options(stringsAsFactors = FALSE)
    play_table = data.frame()
    raw_pbp = vector()

    for (k in 1:length(plays)) {
        #Get the already-established metadata:
        play = list()
        play$poss = plays[[k]]$poss
        play$def = plays[[k]]$def
        play$time = plays[[k]]$time
        play$down = plays[[k]]$down
        play$togo = plays[[k]]$togo
        play$dist = plays[[k]]$dist
        play$drive = plays[[k]]$drive
        play$playnum = plays[[k]]$playnum
        play$carrier = NA
        play$gain = NA

        pbp = plays[[k]]$pbp
        
        play = parse.rush(pbp, play)
        play = parse.pass(pbp, play)
        play = parse.timeout(pbp, play)
    
        #Fumbles, penalties, touchdowns, first downs can happen on any play:
        play = parse.penalty(pbp, play)
        play = parse.fumble(pbp, play)
        play = parse.touchdown(pbp, play)
        play = parse.special(pbp, play)
        play = parse.first.down(pbp, play)
        play = parse.interception(pbp, play)
        
        #Put scores in the table
        play$margin = scores[k, play$poss] - scores[k, play$def]
        play$away = scores[k,1]
        play$home = scores[k,2]
        play$pbp = pbp
    
        play_table = rbind(play_table, play)
    }
    
    #Sacks should count against passing, not rushing. Consider anyone who threw at least two passes a QB:
    QBs = vector()
    for (qb in unique(play_table$passer)) {
        if (!is.na(qb) && sum(play_table$passer==qb, na.rm=TRUE)) {
            QBs = c(QBs, qb)
        }
    }
        
    #Now any non-positive rush for a QB should be a sack.
    play_table$sack = FALSE
    sack_indx = which(play_table$rush & play_table$gain<=0 & play_table$carrier %in% QBs)
    play_table$rush[sack_indx] = FALSE
    play_table$pass[sack_indx] = TRUE
    play_table$complete[sack_indx] = FALSE
    play_table$sack[sack_indx] = TRUE
    play_table$passer[sack_indx] = play_table$carrier[sack_indx]
    
    #Remove kickoffs, penalties, and other non-scrimmage plays from play numbering.
    scrimmage = which(play_table$rush | play_table$pass | play_table$FG | play_table$punt)
    PAT_play = which(play_table$PAT & !play_table$TD)
    play_table$playnum[-scrimmage] = NA
    play_table$playnum[PAT_play] = NA
    
    #Also make kickoffs the first play of the ensuing drive.
    koind = which(play_table$kickoff)
    for (l in rev(koind)) {
        if (l != nrow(play_table) && !is.na(play_table$drive[l+1]))
            play_table$drive[l] = play_table$drive[l+1]
    }
        
    #Correct play numbering:
    for (l in unique(play_table$drive[!is.na(play_table$drive)])) {
        indx = which(play_table$drive==l & !is.na(play_table$playnum))
        play_table$playnum[indx] = 1:length(indx)
    }
    
    #If a play has no down and distance, see if we can correct it:
    missing = which((play_table$rush | play_table$pass | play_table$FG | play_table$punt) & is.na(play_table$down))
    for (m in missing) {
        if (m>1) {
            if (play_table$timeout[m-1] | play_table$penalty[m-1]) {
                play_table$down[m] = ifelse(!is.na(play_table$down[m]), play_table$down[m], play_table$down[m-1])
                play_table$dist[m] = ifelse(!is.na(play_table$dist[m]), play_table$dist[m], play_table$dist[m-1])
                play_table$togo[m] = ifelse(!is.na(play_table$togo[m]), play_table$togo[m], play_table$togo[m-1])
            }
        }
    }
    
    return(play_table)
}
