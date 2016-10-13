parse.sack <- function(pbp, play) {
  play$sack <- FALSE
  play$sack.credit <- NA
  
  sack_regex <- paste0("(?<passer>", name.pattern, ") (sack|sacked)( by (?<sackcredit>", name.pattern, "))? for ((?<gain>\\d+) ",
                      "(yd|yard)s?|(a )?loss of (?<loss>\\d+) (yd|yard)s?|(?<nogain>no gain))")

  if (grepl(sack_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
    match = regex(sack_regex, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
    
    play$sack <- TRUE
    play$pass <- TRUE
    play$passer <- format.name(match[1,'passer'])
    play$sack.credit <- format.name(match[1, 'sackcredit'])
    
    if (!is.na(match[1,'gain'])) {play$gain <- as.numeric(match[1,'gain'])}
    else if (!is.na(match[1,'loss'])) {play$gain <- -as.numeric(match[1,'loss'])}
    else if (!is.na(match[1,'nogain'])) {play$gain <- 0}
  }
  
  return(play)
}