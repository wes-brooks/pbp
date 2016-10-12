# go from 'Last, First' to 'First Last' name format
format.name <- function(name) {
  if (is.na(name))
    return(NA)
  
  junior <- c('Jr', 'Jr.', 'JR', 'JR.', 'III')
  
  split <- strsplit(name, ',', fixed=TRUE)[[1]]
  split <- rev(trim(split))
  if (any(split %in% junior)) {
    indx <- split %in% junior
    split <- c(split[!indx], split[indx])
  }
  trim(paste(split, collapse=' '))
}