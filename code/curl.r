library(RCurl)
library(XML)

url = "http://scores.espn.go.com/ncf/playbyplay?gameId=332430275&period=0"
tree <- htmlTreeParse(url, isURL=TRUE, useInternalNodes=TRUE)

pbp <- xpathSApply(tree, paste(
    "//table[contains(@class,'mod-pbp')]/child::tr/td[position()<3]",
    "//table[contains(@class,'mod-pbp')]/child::tbody/tr/child::td[position()<3]",
    "//table[contains(@class,'mod-pbp')]/child::thead/tr/th[1]",
    "//table[contains(@class,'mod-pbp')]/child::thead/tr/td", sep=" | "), xmlValue) 
