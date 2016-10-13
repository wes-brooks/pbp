parse.pass <- function(pbp, play) {
    play$pass <- FALSE
    play$complete <- NA
    play$passer <- NA

    pass_regex1 <- paste0("(?<QB>", name.pattern, ") pass ((?<complete>complete)|",
        "(?<incomplete>incomplete))(( to (?<receiver>",name.pattern, ").*(?(complete) for ",
        "((?<gain>\\d+) (yd|yard)s?|(a )?loss of (?<loss>\\d+) (yd|yard)s?|",
        "(?<nogain>no gain))))?)?", sep="")
        
    pass_regex2 <- paste0("(?<receiver>", name.pattern, ") ((?<gain>\\d+) (yd|yard)s? )?pass( (?<complete>complete))? .*from (?<QB>", name.pattern, ")")
    
    # test whether this play matches any off the pass patterns
    pass.patterns <- c(pass_regex1, pass_regex2)
    pass.match <- sapply(pass.patterns, function(p) grepl(pattern=p, x=pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE))
    
    if (any(pass.match)) {
      match <- regex(pass.patterns[which(pass.match)[1]], pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
      
        # match = regex(pass_regex1, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
      play$pass <- TRUE
      play$passer <- format.name(match[1,'QB'])
      play$carrier <- format.name(match[1,'receiver'])
  
      if (!is.na(match[1,'gain'])) {play$gain <- as.numeric(match[1,'gain'])}
      else if (!is.na(match[1,'loss'])) {play$gain <- -as.numeric(match[1,'loss'])}
      else if (!is.na(match[1,'nogain'])) {play$gain <- 0}
  
      if (!is.na(match[1,'complete'])) {play$complete <- TRUE}
      else {
          play$complete <- FALSE
          play$gain <- 0
      }
    } # else if (grepl(pass_regex2, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)) {
    #     match = regex(pass_regex2, pbp, perl=TRUE, fixed=FALSE, ignore.case=TRUE)
    #     play$pass = TRUE
    #     play$passer = format.name(match[1,'QB'])
    #     play$carrier = format.name(match[1,'receiver'])
    # 
    #     if (!is.na(match[1,'gain'])) {
    #         play$gain = as.numeric(match[1,'gain'])
    #         play$complete = TRUE    
    #     }
    #     #else if (!is.na(match[1,'loss'])) {play$gain = -as.numeric(match[1,'loss'])}
    #     #else if (!is.na(match[1,'nogain'])) {play$gain = 0}
    # 
    #     #if (!is.na(match[1,'complete'])) {play$complete = TRUE}
    #     #else {
    #     #    play$complete = FALSE
    #     #    play$gain = 0
    #     #}
    # }
        
    return(play)
}