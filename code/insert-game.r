#Connect to the database
drv <- dbDriver("SQLite")
con = dbConnect(drv, dbfile)

tables = dbListTables(con)

t = 'Wisconsin'
tt = list(team_name=t)

if (!("teams" %in% tables)) {
    dbWriteTable(con, 'teams', as.data.frame(tt), row.names=FALSE)
}

dbWriteTable(con, t[['name']], t[['dataframe']])
team_row = dbGetQuery(con, paste("SELECT rowid, team_name FROM teams WHERE team_name=='", t, "'",  sep=""))

if (nrow(teamrow)==0) {
    dbWriteTable(con, 'teams', as.data.frame(tt), row.names=FALSE)
    team_row = dbGetQuery(con, "select last_insert_rowid()")
}

team_id = team_row[1]



#Add the game to the games table
dbWriteTable(con, "games", as.data.frame(gamemeta), row.names=FALSE)
game_id = dbGetQuery(con, "select last_insert_rowid()")

for (i in 1:length(drives)) {
    drives[[i]][['pbp']] <- NULL
    drives[[i]][['game_id']] = game_id
}

