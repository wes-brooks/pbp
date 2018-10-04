ExtractPlays <- function(url) {
    tree <- htmlTreeParse(url, isURL=TRUE, useInternalNodes=TRUE)

    #Make sure to label the scores properly:
    costs <- list(insertions=2, deletions=0, substitutions=1)
    
    img.id.regex <- "\\/(?<id>[^\\/]+)\\.png"
    meta <- list()
    
    # home team metadata
    meta$home.id <-home.id <- regex(img.id.regex, getNodeSet(tree, "//div[contains(@class, 'competitors')]/div[contains(@class, 'team') and contains(@class, 'home')]/descendant::img[contains(@class, 'team-logo')]/@src")[[1]])
    meta$home.school <-home.school <- xmlValue(getNodeSet(tree, "//div[contains(@class, 'competitors')]/div[contains(@class, 'team') and contains(@class, 'home')]/descendant::span[contains(@class, 'long-name')]")[[1]])
    meta$home.name <-home.name <- xmlValue(getNodeSet(tree, "//div[contains(@class, 'competitors')]/div[contains(@class, 'team') and contains(@class, 'home')]/descendant::span[contains(@class, 'short-name')]")[[1]])
    meta$home.abbrev <-home.abbrev <- xmlValue(getNodeSet(tree, "//div[contains(@class, 'competitors')]/div[contains(@class, 'team') and contains(@class, 'home')]/descendant::span[contains(@class, 'abbrev')]")[[1]])
    meta$home.score <-home.score <- as.integer(xmlValue(getNodeSet(tree, "//div[contains(@class, 'competitors')]/div[contains(@class, 'team') and contains(@class, 'home')]/descendant::div[contains(@class, 'score')]")[[1]]))
    
    # away team metadata
    meta$away.id <- away.id <- regex(img.id.regex, getNodeSet(tree, "//div[contains(@class, 'competitors')]/div[contains(@class, 'team') and contains(@class, 'away')]/descendant::img[contains(@class, 'team-logo')]/@src")[[1]])
    meta$away.school <- away.school <- xmlValue(getNodeSet(tree, "//div[contains(@class, 'competitors')]/div[contains(@class, 'team') and contains(@class, 'away')]/descendant::span[contains(@class, 'long-name')]")[[1]])
    meta$away.name <- away.name <- xmlValue(getNodeSet(tree, "//div[contains(@class, 'competitors')]/div[contains(@class, 'team') and contains(@class, 'away')]/descendant::span[contains(@class, 'short-name')]")[[1]])
    meta$away.abbrev <- away.abbrev <- xmlValue(getNodeSet(tree, "//div[contains(@class, 'competitors')]/div[contains(@class, 'team') and contains(@class, 'away')]/descendant::span[contains(@class, 'abbrev')]")[[1]])
    meta$away.score <- away.score <- as.integer(xmlValue(getNodeSet(tree, "//div[contains(@class, 'competitors')]/div[contains(@class, 'team') and contains(@class, 'away')]/descendant::div[contains(@class, 'score')]")[[1]]))
    
    team.patterns <- paste(meta$home.school, meta$home.abbrev, meta$away.school, meta$away.abbrev, sep='|')
    
    playmeta.regex = paste0("^(?<down>1st|2nd|3rd|4th|1ST|2ND|3RD|4TH) (and|AND|&) ",
                            "(?<togo>\\d{1,2}|goal|Goal|GOAL) at (?<field>", team.patterns, ")? ?",
                            "(?<yardline>\\d{1,2})$")
    
    postplay.regex <- "^\\(((?<OT>OT)|(?<min>\\d{1,2}):(?<sec>\\d\\d)\\s*-\\s*(?<quarter>1st|2nd|3rd|4th|1ST|2ND|3RD|4TH))\\)\\s(?<pbp>.*)\\s*$"
    
    
    
    # drive metadata
    ndrives <- getNodeSet(tree, "count(//article[contains(@class, 'play-by-play')]/descendant::div[@id='gamepackage-drives-wrap']/ul[contains(@class, 'css-accordion')]/li[contains(@class, 'accordion-item')])")
    halftime <- 1 + getNodeSet(tree, "count(//article[contains(@class, 'play-by-play')]/descendant::div[@id='gamepackage-drives-wrap']/ul[contains(@class, 'css-accordion')]/li[contains(@class, 'accordion-item') and contains(@class, 'half-time')]/preceding-sibling::*)")
    if (halftime == 1) {
      indx.drives <- 1:ndrives
    } else indx.drives <- (1:ndrives)[-halftime]
    
    # scores after each drive. ESPN's presentation has the ID of home/away reversed in the score column.
    drive.score.away <- as.integer(xpathSApply(tree, "//article[contains(@class, 'play-by-play')]/descendant::span[contains(@class, 'home')]/span[contains(@class,'team-score')]", xmlValue))
    drive.score.home <- as.integer(xpathSApply(tree, "//article[contains(@class, 'play-by-play')]/descendant::span[contains(@class, 'away')]/span[contains(@class,'team-score')]", xmlValue))
    
    drives <- list()
    
    # for each drive extract the team, half, pbp code, duration
    for (i in indx.drives) {
      # identify the team with possession for this drive
      src <- getNodeSet(tree, paste0("//article[contains(@class, 'play-by-play')]/descendant::div[@id='gamepackage-drives-wrap']/ul[contains(@class, 'css-accordion')]/li[contains(@class, 'accordion-item')][", i, "]/div[contains(@class, 'accordion-header')]/descendant::span[contains(@class, 'home-logo')]/img[contains(@class, 'team-logo')]/@src"))[[1]]
      poss <- regex(img.id.regex, src)
      if (poss[1] == away.id) { off <- away.abbrev; def <- home.abbrev; }
      if (poss[1] == home.id) { off <- home.abbrev; def <- away.abbrev; }
      
      
      # extract data for each play
      playdata <- trim(xpathSApply(tree, paste0("//article[contains(@class, 'play-by-play')]/descendant::div[@id='gamepackage-drives-wrap']/ul[contains(@class, 'css-accordion')]/li[contains(@class, 'accordion-item')][", i, "]/descendant::ul[contains(@class, 'drive-list')]/li[not(contains(@class, 'half-time')) and not(contains(@class, 'end-quarter'))]/p/span[contains(@class, 'post-play')]"), xmlValue))
      down.dist <- trim(xpathSApply(tree, paste0("//article[contains(@class, 'play-by-play')]/descendant::div[@id='gamepackage-drives-wrap']/ul[contains(@class, 'css-accordion')]/li[contains(@class, 'accordion-item')][", i, "]/descendant::ul[contains(@class, 'drive-list')]/li[not(contains(@class, 'half-time')) and not(contains(@class, 'end-quarter'))]/h3"), xmlValue))

      # get the gametime out of the "post-play" code
      plays1 <- as.data.frame(t(sapply(playdata, function(x) regex(postplay.regex, x))))
      plays2 <- as.data.frame(t(sapply(down.dist, function(x) regex(playmeta.regex, x))))
      
      # adjust the data objects
      colnames(plays1) <- c('OT', 'min', 'sec', 'quarter', 'pbp')
      rownames(plays1) <- NULL
      
      colnames(plays2) <- c('down', 'togo', 'field', 'yardline')
      rownames(plays2) <- NULL
      
      # adjust the types of each column
      plays <- as.data.frame(cbind(plays2, pbp=plays1$pbp))
      plays <- within(plays, {down <- as.character(down);
                              field <- as.character(field);
                              yardline <- as.integer(yardline);
                              pbp <- as.character(pbp)})
      plays1 <- within(plays1, {min <- as.integer(min);
                              sec <- as.integer(sec);
                              quarter <- as.character(quarter)})
      
      #################################
      # How far from the end zone did the play begin?
      
      # Figure out which side of the field the play began from
      offense_unlike <- ifelse(!is.na(plays$field), adist(off, plays$field, costs=costs, ignore.case=TRUE), NA)
      defense_unlike <- ifelse(!is.na(plays$field), adist(def, plays$field, costs=costs, ignore.case=TRUE), NA)
      
      #Now compute the distance from the goal line:
      plays$dist <- ifelse(plays$yardline == 50, 50, ifelse(!is.na(plays$field), ifelse(offense_unlike < defense_unlike, 100 - plays$yardline, plays$yardline), NA))
      
      #Replace 'goal' to go with the distance to the goal line:
      indx <- !is.na(plays$togo) & (substr(plays$togo, 1, 1) == 'g' | substr(plays$togo,1,1) == 'G')
      plays$togo[indx] <- plays$dist[indx]
      plays$togo <- as.numeric(plays$togo)
      
      ##########################
      # convert all times to seconds
      plays1$quarter <- ifelse(plays1$quarter %in% c('1st', '1ST'), 1,
                              ifelse(plays1$quarter %in% c('2nd', '2ND'), 2,
                                     ifelse(plays1$quarter %in% c('3rd', '3RD'), 3,
                                            ifelse(plays1$quarter %in% c('4th', '4TH'), 4, NA))))
      plays$time <- 60 * (15 * (4 - plays1$quarter) + plays1$min) + plays1$sec
      
      
      this.drive <- list(plays=plays)
      
      ####################
      # metadata for the drive
      this.drive$half <- ifelse(i < halftime[1], 1, 2)
      if (i > 1) { drives[[length(drives)]]$duration <- drives[[length(drives)]]$plays$time[1] - plays$time[1] }
      
      # get the home and away scores during this drive
      if (i == 1) {
        this.drive$plays$score.home <- this.drive$home.score <- 0
        this.drive$plays$score.away <- this.drive$away.score <- 0
      } else {
        this.drive$plays$score.home <- this.drive$home.score <- drive.score.home[which(indx.drives == i) - 1]
        this.drive$plays$score.away <- this.drive$away.score <- drive.score.away[which(indx.drives == i) - 1]
      }
      
      # attach metadata to the play table
      this.drive$plays$half <- this.drive$half
      this.drive$plays$poss <- off
      this.drive$plays$def <- def
      this.drive$plays$home <- home.abbrev
      this.drive$plays$away <- away.abbrev
      if (poss[1] == away.id) { this.drive$plays$score.offense <- this.drive$away.score; this.drive$plays$score.defense <- this.drive$home.score; }
      if (poss[1] == home.id) { this.drive$plays$score.defense <- this.drive$away.score; this.drive$plays$score.offense <- this.drive$home.score; }
      this.drive$plays$playnum <- 1:nrow(plays)
      this.drive$plays$drive <- which(indx.drives == i)
      
      rownames(this.drive$plays) <- NULL
      
      # add this drive to the list
      drives[[length(drives) + 1]] <- this.drive
    }
    
    # final drive takes up the remaining time
    drives[[length(drives)]]$duration <- drives[[length(drives)]]$plays$time[1]
    

    list(drives=drives, meta=meta)
}
