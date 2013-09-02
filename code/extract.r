#Load the raw data and extract the part including 'mode-pbp', the play-by-play module.
raw = scan("http://scores.espn.go.com/ncf/playbyplay?gameId=332430275&period=0", what=character(0), sep='\n')
index = grep('mod-pbp', raw)
pbp = raw[index]

#Quarter breaks:
quarters = downs = c('1st', '2nd', '3rd', '4th')
starts = sapply(quarters, function(x) {paste(x, "Quarter Play by Play")})
breaks = lapply(starts, function(x) {regexpr(x,pbp)})

plays = list()

for (k in 1:4) {
    start = breaks[[k]][1] + attr(breaks[[k]], 'match.length')
    if (k<4) {end = breaks[[k+1]][1] - 1}
    else {end = nchar(pbp)}
    
    plays[[k]] = substr(pbp, start, end)
}



#Drive breaks:
drives = list()

#Divide each quarter of play-by-play into drives:
for (k in 1:4) {
    #Begin by identifying the beginning of each drive:
    drivebreaks = gregexpr("(?<team>[A-Za-z]+) at \\d{1,2}:\\d\\d", plays[[k]], perl=TRUE, fixed=FALSE)
    
    for (j in 1:length(drivebreaks[[1]])) {
        start = drivebreaks[[1]][j]
        end = start + attr(drivebreaks[[1]], 'capture.length')[j] - 1
        teamname = substr(plays[[k]], start, end)
        
        if (j<length(drivebreaks[[1]])) {nextstart = drivebreaks[[1]][j+1]}
        else {nextstart = nchar(plays[[k]])}
        end = start + attr(drivebreaks[[1]], 'match.length')[j]
        drive_pbp = substr(plays[[k]], end, nextstart)
        
        drives = c(drives, list(team=teamname, pbp=drive_pbp))
    }
}


#Divide each drive into plays:
playbreak_regex = "(?<down>1st|2nd|3rd|4th|1ST|2ND|3RD|4TH) (and|AND) \\d{1,2} at (?<field>[A-Za-z]{3,4}) (?<yardline>\\d{1,2})</td>" 

playbreaks = gregexpr(playbreak_regex, drives[[k]], perl=TRUE, fixed=FALSE)

rush_regex = "<td><a.*?>(?<player>[-a-zA-Z\\. ']+)</a>"
pass_regex = "<td><a.*?>(?<QB>[-a-zA-Z\\. ']+)</a> pass (?<completion>"
kick_regex = "<td><a.*?>(?<player>[-a-zA-Z\\. ']+)</a>"