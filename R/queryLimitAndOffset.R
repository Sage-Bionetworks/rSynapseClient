## Extract limit and offset from a synapse query
## 
## Author: J. Christopher Bare <chris.bare@sagebase.org>
###############################################################################

## Extract limit and offset from a synapse query.
## Returns a list containing the modified query statement and, if specified, limit and offset.
.queryLimitAndOffset <-
  function(queryStatement, default.limit=as.integer(NA), default.offset=1L) {

  result <- list(limit=default.limit, offset=default.offset)

  # check for limit in the query
  re.limit <- ".*limit\\s+(\\d+).*"
  if (grepl(re.limit, queryStatement, ignore.case=TRUE)) {

    # retrieve limit from query statement
    result$limit <- as.integer(sub(re.limit, "\\1", queryStatement, ignore.case=TRUE))
    
    # remove limit from query
    queryStatement <- sub("\\s*limit\\s+\\d+\\s*"," ",queryStatement)
  
  }
  
  # check for offset in the query
  re.offset <- ".*offset\\s+(\\d+).*"
  if (grepl(re.offset, queryStatement, ignore.case=TRUE)) {
    
    # take offset out of query
    result$offset <- as.integer(sub(re.offset, "\\1", queryStatement, ignore.case=TRUE))

    # remove offset from query
    queryStatement <- sub("\\s*offset\\s+\\d+\\s*"," ",queryStatement, ignore.case=TRUE)
    
  }

  # clean up trailing whitespace in queries
  queryStatement <- sub("\\s+$", "", queryStatement)

  result$query <- queryStatement

  return(result)
}