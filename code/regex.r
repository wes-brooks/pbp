extract = function(pattern, str, perl=TRUE, fixed=FALSE) {
    match = gregexpr(pattern, str, perl=perl, fixed=fixed)[[1]]
    
    caps = attr(match, 'capture.names')
    starts = attr(match, 'capture.start')
    lengths = attr(match, 'capture.length')
    
    #Remove unnamed captures:
    caps = caps[caps!=""]
    
    result = data.frame(matrix(NA, nrow=0, ncol=length(caps)))
    
    for (j in length(match)) {
        row = vector()
        
        for (cap in caps) {
            start = starts[j,cap]
            length = lengths[j,cap] - 1
            
            if (length==0) {
                row = c(row, NA)
            }
            else {
                row = c(row, substr(str, start, start+length))
            }
        }
        result = rbind(result, row)
    }
    colnames(result) = caps
    
    return(result)
}