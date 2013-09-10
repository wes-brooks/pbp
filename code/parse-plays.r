ParsePlays = function(plays) {
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
        "(?<made>GOOD|MADE)|(?<missed>MISSED|NO GOOD).*", sep='')
    pat_regex = paste("(?<kicker>[-a-zA-Z\\. ']+) extra point ",
        "(?<made>GOOD|MADE)|(?<missed>MISSED|NO GOOD)", sep='')

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
        "(?<penalty>[-a-zA-Z\\. ']+)( on (?<player>[-a-zA-Z\\. ']+))? ",
        "(?<decision>accepted|declined)", sep='')
    timeout_regex = "Timeout (?<team>[-a-zA-Z\\. ']+).* (?<min>\\d{1,2})?:(?<sec>\\d\\d)"

    #Results:
    first_regex = "(?<first>1st down|first down)"
    td_regex = "(?<touchdown>touchdown)"

    #Establish the columns:
    rush = pass = sack = td = first = punt = kickoff = fg = pat = INT = fumble = penalty = timeout = FALSE
    poss = down = togo = time = dist = passer = carrier = kicker = returner = touchback = faircatch = kick_dist = kick_return = gain = fumble_return = int_return = made = intercepter = forced_by = recovered_by = penalized_team = penalized_player = penalty_dist = complete = NA
    default_row = data.frame(poss, down, time, togo, dist, gain, rush, pass, complete, sack, td, first, punt, kickoff, fg, pat, INT, fumble, penalty, timeout, passer, carrier, kicker, returner, touchback, faircatch, kick_dist, kick_return, fumble_return, made, intercepter, int_return, forced_by, recovered_by, penalized_team, penalized_player, penalty_dist)

    play_table = data.frame(matrix(NA, ncol=length(default_row), nrow=0))  
    colnames(play_table) = colnames(default_row)

    for (k in 1:length(plays)) {
        #Get the already-established metadata:
        this = default_row
        this$poss = plays[[k]][['poss']]
        this$down = plays[[k]][['down']]
        this$time = plays[[k]][['time']]
        this$togo = plays[[k]][['togo']]
        this$dist = plays[[k]][['dist']]

        pbp = plays[[k]][['pbp']]
    
        if (length(grep(fg_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE))>0) {
            match = regex(fg_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
            this$fg = TRUE
            this$kicker = match[1,'kicker']
            this$dist = match[1,'kickdist']
            if (!is.na(match[1,'made'])) {this$made = TRUE}
            else {this$made = FALSE}
        } else if (length(grep(punt_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE))>0) {
            match = regex(punt_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
            this$punt = TRUE
            this$kicker = match[1,'punter']
            this$returner = match[1,'returner']
            this$kick_dist = match[1,'kickdist']
        
            if (!is.na(match[1,'touchback'])) {
                this$touchback = TRUE
                this$kick_return = 0
            }
            if (!is.na(match[1,'catcher'])) {
                this$faircatch = TRUE
                this$returner = match[1,'catcher']
                this$kick_return = 0
            }
        
            if (!is.na(match[1,'retgain'])) {this$kick_return = as.numeric(match[1,'retgain'])}
            else if (!is.na(match[1,'retloss'])) {this$kick_return = -as.numeric(match[1,'retloss'])}
            else if (!is.na(match[1,'retnogain'])) {this$kick_return = 0}
        } else if (length(grep(rush_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE))>0) {
            match = regex(rush_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
            this$rush = TRUE
            this$carrier = match[1,'player']
            if (!is.na(match[1,'gain'])) {this$gain = as.numeric(match[1,'gain'])}
            else if (!is.na(match[1,'loss'])) {this$gain = -as.numeric(match[1,'loss'])}
            else if (!is.na(match[1,'nogain'])) {this$gain = 0}
        } else if (length(grep(pass_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE))>0) {
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
        } else if (length(grep(timeout_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE))>0) {
        }
    
        #Fumbles, penalties, touchdowns, first downs can happen on any play:
        if (length(grep(penalty_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE))>0) {
            match = regex(penalty_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
            this$penalty = TRUE
        }
        if (length(grep(fumble_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE))>0) {
            match = regex(fumble_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
            this$fumble = TRUE
        }
        if (length(grep(td_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE))>0) {
            this$td = TRUE
            match = regex(pat_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
            if (!is.na(match[1,'made'])) {
                this$pat = TRUE
                this$made = TRUE
                this$kicker = match[1,'kicker']
            } else if (!is.na(match[1,'missed'])) {
                this$pat = TRUE
                this$made = FALSE
                this$kicker = match[1,'kicker']
            }
        }
        if (length(grep(first_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE))>0) {
            this$first = TRUE
        }
        if (length(grep(kickoff_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE))>0) {
            match = regex(kickoff_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
            this$kickoff = TRUE
            this$kicker = match[1,'kicker']
            this$returner = match[1,'returner']
            this$kick_dist = match[1,'kickdist']
        
            if (!is.na(match[1,'touchback'])) {
                this$touchback = TRUE
                this$kick_return = 0
            }
        
            if (!is.na(match[1,'retgain'])) {this$kick_return = as.numeric(match[1,'retgain'])}
            else if (!is.na(match[1,'retloss'])) {this$kick_return = -as.numeric(match[1,'retloss'])}
            else if (!is.na(match[1,'retnogain'])) {this$kick_return = 0}
        }
        if (length(grep(interception_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE))>0) {
            match = regex(interception_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
            this$pass = TRUE
            this$INT = TRUE
            this$complete = FALSE
            this$intercepter = match[1,'intercepter']
        
            if (!is.na(match[1,'retgain'])) {this$int_return = as.numeric(match[1,'retgain'])}
            else if (!is.na(match[1,'retloss'])) {this$int_return = -as.numeric(match[1,'retloss'])}
            else if (!is.na(match[1,'retnogain'])) {this$int_return = 0}
        }
    
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

    return(play_table)
}
