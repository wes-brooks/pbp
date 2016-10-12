regex <- function(pattern, str, perl=TRUE, fixed=FALSE, ignore.case=TRUE) {
    #Process the regex
    match = gregexpr(pattern, str, perl=perl, fixed=fixed, ignore.case=ignore.case)[[1]]
    
    #Get the named capture groups
    capts = attr(match, 'capture.names')
    starts = attr(match, 'capture.start')
    lengths = attr(match, 'capture.length')
    
    #Remove unnamed captures:
    capts = capts[capts != ""]
    
    #Initialize the table of results
    result = matrix(NA, nrow=0, ncol=length(capts))
    
    #Produce a table of results where each row is a complete match
    for (j in 1:length(match)) {
        row = vector()
        
        #Loop through the possible capture groups and find those that matched.
        for (capt in capts) {
            start = starts[j,capt]
            length = lengths[j,capt]
            
            #Uncaptured groups are returned NA.
            if (length<=0) {row = c(row, NA)}
            else {
                #Remove leading and trailing whitespace:
                item = substr(str, start, start+length-1)
                item = gsub("[\\s\\.]*$","", item, perl=TRUE)
                item = gsub("^[\\s\\.]*","", item, perl=TRUE)
                row = c(row, item)
            }
        }
        
        #Add this match to the table
        result = rbind(result, row)
    }
    
    #Annotate the table and return it
    colnames(result) = capts
    return(result)
}