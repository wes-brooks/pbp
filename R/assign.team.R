# identify an event with one team or the other
assign.team <- function(string, A, B) {
  team <- NA
  
  if (!is.na(string)) {
    teams <- c(A, B)
    costs <- list(insertions=2, deletions=0, substitutions=1)
    A.unlike <- adist(teams[1], string, costs=costs, ignore.case=TRUE)
    B.unlike <- adist(teams[2], string, costs=costs, ignore.case=TRUE)
    if (A.unlike != B.unlike) {
      team <- teams[which.min(c(A.unlike, B.unlike))]
    } else {team <- string}
  }
  
  return(team)
}