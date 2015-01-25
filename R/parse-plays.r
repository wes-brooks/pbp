#' @export
ParsePlays = function(url) {

    pbp = ExtractPlays(url)
    
    plays = pbp$plays
    scores = pbp$scores
    teams = colnames(pbp$scores)

    #Special teams plays:
    kickoff_regex = paste("(?<kicker>[-a-zA-Z\\. ']+) kickoff for (?<kickdist>\\d{1,3}) ",
        "yards? (returned by (?<returner>[-a-zA-Z\\. ']+) for ((?<retgain>\\d{1,3}) ",
        "yards|(a )?loss of (?<retloss>\\d+) yards?|(?<retnogain>no gain))",
        "|.*(?<touchback>touchback)).*", sep='')
    punt_regex = paste("(?<punter>[-a-zA-Z\\. ']+) punt for (?<kickdist>\\d{1,3}) ",
        "yards?(.*(?<touchback>touchback).*|.*out[- ]of[- ]bounds at|.*fair catch by ",
        "(?<catcher>[-a-zA-Z\\. ']+) at|.*returned by (?<returner>[-a-zA-Z\\. ']+) ",
        "for (((?<retgain>\\d{1,3}) yards|(a )?loss of (?<retloss>\\d+) ",
        "yards?|(?<retnogain>no gain)))?)?", sep='')
    fg_regex = paste("(?<kicker>[-a-zA-Z\\. ']+) (?<kickdist>\\d{1,3}) yards? field goal ",
        "((?<made>GOOD|MADE)|(?<missed>MISSED|NO GOOD)).*", sep='')
    pat_regex = paste("(?<kicker>[-a-zA-Z\\. ']+) (extra point|KICK) ",
        "((?<made>GOOD|MADE)|(?<missed>MISSED|NO GOOD))", sep='')

    #Scrimmage plays
    rush_regex = paste("(?<player>[-a-zA-Z\\. ']+) rush [\\s\\w]*for ((?<gain>\\d+) ",
        "yards?|(a )?loss of (?<loss>\\d+) yards?|(?<nogain>no gain))", sep='')
    pass_regex = paste("(?<QB>[-a-zA-Z\\. ']+) pass (((?<complete>complete)|",
        "(?<incomplete>incomplete))( to (?<receiver>[-a-zA-Z\\. ']+).*(?(complete) for ",
        "((?<gain>\\d+) yards?|(a )?loss of (?<loss>\\d+) yards?|",
        "(?<nogain>no gain))))?)?", sep='')

    #Turnovers/timeouts/penalties:
    fumble_regex = paste("fumbled?.*(forced by (?<forcer>[-a-zA-Z\\. ']+))?.*",
        "(recovered by (?<team>[a-zA-Z]+) (?<recoverer>[-a-zA-Z\\. ']+))?", sep='')
    interception_regex = paste("intercept(ed|ion)? by (?<intercepter>[-a-zA-Z\\. ']+) ",
        "at (the )?(?<side>[a-zA-Z]+) (?<yardline>\\d{1,2})[\\.,]?( returned for ",
        "((?<retgain>\\d{1,3}) yards|(a )?loss of (?<retloss>\\d+) ",
        "yards?|(?<retnogain>no gain)))?", sep='')
    penalty_regex = paste("(?<team>[-a-zA-Z\\. ']+) penalty (?<dist>\\d{1,3}) yards? ",
        "(?<penalty>[-a-zA-Z\\. ']+?)( on (?<player>[-a-zA-Z\\. ']+))? ",
        "(?<decision>accepted|declined)", sep='')
    timeout_regex = "Timeout (?<team>[-a-zA-Z\\. ']+).* (?<min>\\d{1,2})?:(?<sec>\\d\\d)"

    #Results:
    first_regex = "(?<first>1st down|first down)"
    td_regex = "(?<touchdown>touchdown)"

    #Establish the columns:
    rush = pass = sack = TD = first = punt = kickoff = FG = PAT = INT = fumble = penalty = timeout = FALSE
    poss = def = margin = drive = playnum = down = togo = time = dist = passer = carrier = kicker = returner = touchback = faircatch = timeout_team = kick_dist = kick_return = gain = fumble_return = int_return = made = intercepter = forced_by = recovered_by = penalty_type = penalized_team = penalized_player = penalty_dist = complete = NA
    default_row = data.frame(poss, down, time, togo, dist, gain, rush, pass, complete, sack, TD, first, punt, kickoff, FG, PAT, INT, fumble, penalty, timeout, timeout_team, passer, carrier, kicker, returner, touchback, faircatch, kick_dist, kick_return, fumble_return, made, intercepter, int_return, forced_by, recovered_by, penalty_type, penalized_team, penalized_player, penalty_dist)

    play_table = data.frame(matrix(NA, ncol=length(default_row), nrow=0))  
    colnames(play_table) = colnames(default_row)

    for (k in 1:length(plays)) {
        #Get the already-established metadata:
        this = default_row
        this$poss = plays[[k]]$poss
        this$def = plays[[k]]$def
        this$down = plays[[k]]$down
        this$time = plays[[k]]$time
        this$togo = plays[[k]]$togo
        this$dist = plays[[k]]$dist
        this$drive = plays[[k]]$drive
        this$playnum = plays[[k]]$playnum
        this$pbp = plays[[k]]$pbp

        pbp = plays[[k]][['pbp']]
    
        if (grepl(fg_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
            match = regex(fg_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
            this$FG = TRUE
            this$kicker = match[1,'kicker']
            this$kick_dist = match[1,'kickdist']
            if (!is.na(match[1,'made'])) {this$made = TRUE}
            else {this$made = FALSE}
        } else if (grepl(punt_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
            match = regex(punt_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
            this$punt = TRUE
            this$kicker = match[1,'punter']
            this$returner = match[1,'returner']
            this$kick_dist = match[1,'kickdist']
        
            if (!is.na(match[1,'touchback'])) {
                this$touchback = TRUE
                this$kick_return = 0
            } else {this$touchback = FALSE}
            
            if (!is.na(match[1,'catcher'])) {
                this$faircatch = TRUE
                this$returner = match[1,'catcher']
                this$kick_return = 0
            } else {this$faircatch = FALSE}
        
            if (!is.na(match[1,'retgain'])) {this$kick_return = as.numeric(match[1,'retgain'])}
            else if (!is.na(match[1,'retloss'])) {this$kick_return = -as.numeric(match[1,'retloss'])}
            else if (!is.na(match[1,'retnogain'])) {this$kick_return = 0}
        } else if (grepl(rush_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
            match = regex(rush_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
            this$rush = TRUE
            this$carrier = match[1,'player']
            if (!is.na(match[1,'gain'])) {this$gain = as.numeric(match[1,'gain'])}
            else if (!is.na(match[1,'loss'])) {this$gain = -as.numeric(match[1,'loss'])}
            else if (!is.na(match[1,'nogain'])) {this$gain = 0}
        } else if (grepl(pass_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
            match = regex(pass_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
            this$pass = TRUE
            this$passer = match[1,'QB']
            this$carrier = match[1,'receiver']
        
            if (!is.na(match[1,'gain'])) {this$gain = as.numeric(match[1,'gain'])}
            else if (!is.na(match[1,'loss'])) {this$gain = -as.numeric(match[1,'loss'])}
            else if (!is.na(match[1,'nogain'])) {this$gain = 0}
        
            if (!is.na(match[1,'complete'])) {this$complete = TRUE}
            else {
                this$complete = FALSE
                this$gain = 0
            }
        } else if (grepl(timeout_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
            match = regex(timeout_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
            
            if (!is.na(match[1,'team'])) {
                costs = list(insertions=2, deletions=0, substitutions=1)
                t1.unlike = adist(teams[1], match[1,'team'], costs=costs, ignore.case=TRUE)
                t2.unlike = adist(teams[2], match[1,'team'], costs=costs, ignore.case=TRUE)
                if (t1.unlike != t2.unlike) {
                    this$timeout_team = teams[which.min(c(t1.unlike, t2.unlike))]
                } else {this$timeout_team = match[1,'team']}
            }
            this$timeout = TRUE
        }
    
        #Fumbles, penalties, touchdowns, first downs can happen on any play:
        if (grepl(penalty_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
            match = regex(penalty_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
            
            this$penalty = TRUE
            this$penalty_type = match[1,'penalty']
            this$penalized_player = match[1,'player']
            this$penalty_dist = match[1,'dist']
            
            #Identify the penalized team
            if (!is.na(match[1,'team'])) {
                costs = list(insertions=2, deletions=0, substitutions=1)
                t1.unlike = adist(teams[1], match[1,'team'], costs=costs, ignore.case=TRUE)
                t2.unlike = adist(teams[2], match[1,'team'], costs=costs, ignore.case=TRUE)
                if (t1.unlike != t2.unlike) {
                    this$penalized_team = teams[which.min(c(t1.unlike, t2.unlike))]
                } else {this$penalized_team = match[1,'team']}
            }
        }
        if (grepl(fumble_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
            match = regex(fumble_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
            this$fumble = TRUE
        }
        if (grepl(td_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
            this$TD = TRUE
        }
        if (grepl(pat_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
            this$PAT = TRUE
            match = regex(pat_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
            if (!is.na(match[1,'made'])) {
                this$made = TRUE
                this$kicker = match[1,'kicker']
            } else if (!is.na(match[1,'missed'])) {
                this$made = FALSE
                this$kicker = match[1,'kicker']
            }
        }
        if (grepl(first_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
            this$first = TRUE
        }
        if (grepl(kickoff_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
            match = regex(kickoff_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
            this$kickoff = TRUE
            this$kicker = match[1,'kicker']
            this$returner = match[1,'returner']
            this$kick_dist = match[1,'kickdist']
            
            #Define the kicking team to have possession and remove kickoffs from drives
            #If first play of a drive is a kickoff, then flip possession.
            if (plays[[k]]$playnum == 1) {
                tmp = this$poss
                this$poss = this$def
                this$def = tmp
            }
        
            if (!is.na(match[1,'touchback'])) {
                this$touchback = TRUE
                this$kick_return = 0
            } else {this$touchback=FALSE}
        
            if (!is.na(match[1,'retgain'])) {this$kick_return = as.numeric(match[1,'retgain'])}
            else if (!is.na(match[1,'retloss'])) {this$kick_return = -as.numeric(match[1,'retloss'])}
            else if (!is.na(match[1,'retnogain'])) {this$kick_return = 0}
        }
        if (grepl(interception_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
            match = regex(interception_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
            this$pass = TRUE
            this$INT = TRUE
            this$complete = FALSE
            this$intercepter = match[1,'intercepter']
        
            if (!is.na(match[1,'retgain'])) {this$int_return = as.numeric(match[1,'retgain'])}
            else if (!is.na(match[1,'retloss'])) {this$int_return = -as.numeric(match[1,'retloss'])}
            else if (!is.na(match[1,'retnogain'])) {this$int_return = 0}
        }
        
        #Put scores in the table
        this$margin = scores[k, this$poss] - scores[k, this$def]
        this$away = scores[k,1]
        this$home = scores[k,2]
    
        play_table = rbind(play_table, this)
    }


    #Sacks should count against passing, not rushing. Consider anyone who threw at least two passes a QB:
    QBs = vector()
    for (qb in unique(play_table$passer)) {
        if (!is.na(qb) && sum(play_table$passer==qb, na.rm=TRUE)) {
            QBs = c(QBs, qb)
        }
    }
        
    #Now any non-positive rush for a QB should be a sack.
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
