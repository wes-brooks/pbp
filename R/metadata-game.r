#@' export
GameMetadata = function(url) {
    tree <- htmlTreeParse(url, isURL=TRUE, useInternalNodes=TRUE)

    gamemeta = list()
    gamemeta[['team-visitor']] = xpathSApply(tree, paste(
        "//div[contains(@class,'matchup')]",
        "/child::div[contains(@class,'team away')]",
        "/child::div/h3/a", sep=''), xmlValue)
    gamemeta[['team-home']] = xpathSApply(tree, paste(
        "//div[contains(@class,'matchup')]",
        "/child::div[contains(@class,'team home')]",
        "/child::div/h3/a", sep=''), xmlValue)     
    gamemeta[['datetime']] = xpathSApply(tree, paste(
        "//div[contains(@class,'game-time-location')]",
        "/child::p[1]", sep=''), xmlValue) 
    gamemeta[['location']] = xpathSApply(tree, paste(
        "//div[contains(@class,'game-time-location')]",
        "/child::p[2]", sep=''), xmlValue) 


    #What season is this?
    year = tail(strsplit(gamemeta[['datetime']], '\\s')[[1]], 1)
    month = tail(strsplit(gamemeta[['datetime']], '\\s'), 3)
    if (tolower(month) %in% tolower(c("January", "February"))){
        season = as.numeric(year) - 1
    } else {season = as.numeric(year)}
    gamemeta[['season']] = season


    #What week of the season for each team?
    visitor_record = xpathSApply(tree, paste(
        "//div[contains(@class,'matchup')]",
        "/child::div[contains(@class,'team away')]",
        "/child::div/p", sep=''), xmlValue)
    gamemeta[['week-visitor']] = sum(as.numeric(
        strsplit(visitor_record, "[-\\s\\(,]")[[1]][2:3]))
    
    home_record = xpathSApply(tree, paste(
        "//div[contains(@class,'matchup')]",
        "/child::div[contains(@class,'team home')]",
        "/child::div/p", sep=''), xmlValue)
    gamemeta[['week-home']] = sum(as.numeric(
        strsplit(home_record, "[-\\s\\(,]")[[1]][2:3]))


    #Get the final score:
    gamemeta[['score-visitor']] = as.numeric(tail(xpathSApply(tree, paste(
        "//div[contains(@class,'matchup')]",
        "/child::div[contains(@class,'team away')]",
        "/child::div/h3/span", sep=''), xmlValue),1))
    gamemeta[['score-home']] = as.numeric(tail(xpathSApply(tree, paste(
        "//div[contains(@class,'matchup')]",
        "/child::div[contains(@class,'team home')]",
        "/child::div/h3/span", sep=''), xmlValue),1))   

    return(gamemeta)
}