#Load the raw data and extract the part including 'mod-pbp', the play-by-play module.
library(RCurl)
library(XML)

url = "http://scores.espn.go.com/ncf/playbyplay?gameId=332430275&period=0"

source("~/git/pbp/code/metadata-game.r")
source("~/git/pbp/code/regex.r")
source("~/git/pbp/code/extract-plays.r")
source("~/git/pbp/code/parse-plays.r")

gamemeta = GameMetadata(url)
plays = ExtractPlays(url)
plays = ParsePlays(plays)






