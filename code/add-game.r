AddGame = function(dbfile="~/git/pbp/db/pbp_db.sqlite", tables) {
    #Connect to the database
    drv <- dbDriver("SQLite")
    con = dbConnect(drv, dbfile)

    for (t in tables) {
        #Create the tables that don't already exist
        if (!dbExistsTable(con, t[['name']]) {
            dbWriteTable(con, t[['name']], t[['dataframe']])
        }
    }
    
    #Disconnect from the database:
    dbDisconnect()
}
