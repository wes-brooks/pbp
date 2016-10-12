# go from 'Last, First' to 'First Last' name format
format.name <- function(name) {
  if (is.na(name))
    return(NA)
  
  split <- strsplit(name, ',', fixed=TRUE)[[1]]
  trim(paste(rev(split), collapse=' '))
}