parse.sack <- function(pbp, play) {
  play$sack <- FALSE
  play$sack.credit <- NA
  
  sack_regex1 <- paste("(?<passer>[-a-zA-Z,\\. ']+) (sack|sacked)( by (?<sackcredit>[-a-zA-Z,\\. ']+))? for ((?<gain>\\d+) ",
                      "(yd|yard)s?|(a )?loss of (?<loss>\\d+) (yd|yard)s?|(?<nogain>no gain))", sep='')
  # sack_regex2 = "(?<passer>[-a-zA-Z\\. ']+) (?<gain>\\d+) (yd|yard)s? (sacked|sack)"
  
  if (grepl(sack_regex1, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
    match = regex(sack_regex1, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
    
    play$sack <- TRUE
    play$pass <- TRUE
    play$passer <- format.name(match[1,'passer'])
    play$sack.credit <- format.name(match[1, 'sackcredit'])
    
    if (!is.na(match[1,'gain'])) {play$gain <- as.numeric(match[1,'gain'])}
    else if (!is.na(match[1,'loss'])) {play$gain <- -as.numeric(match[1,'loss'])}
    else if (!is.na(match[1,'nogain'])) {play$gain <- 0}
  } #else if (grepl(sack_regex2, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
  #   match <- regex(sack_regex2, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
  #   
  #   play$sack <- TRUE
  #   play$pass <- TRUE
  #   play$passer <- match[1,'player']
  #   
  #   if (!is.na(match[1,'gain'])) {play$gain = as.numeric(match[1,'gain'])}
  # }
  
  return(play)
}