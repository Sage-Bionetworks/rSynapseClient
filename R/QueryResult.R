## Reference class to page through the results of a Synapse Query
## 
## Author: J. Christopher Bare <chris.bare@sagebase.org>
###############################################################################

## Reference class for paging through results of a Synapse Query
QueryResult <-
  setRefClass(
    Class="QueryResult",
    fields=list(
      queryStatement="character",
      blockSize="integer",
      limit="integer",
      offset="integer",
      totalNumberOfResults="integer",
      results="data.frame",
      .fetchedRows = "integer"
    )
  )

QueryResult$methods(
  initialize =
    function(queryStatement, blockSize=100L) {
      'Fetch query results one blocks of records at a time.'

      # try to extract limit and offset from the query statement
      processed.query <- .queryLimitAndOffset(queryStatement)
      
      queryStatement <<- processed.query$query
      limit <<- as.integer(processed.query$limit)
      offset <<- as.integer(processed.query$offset)
      blockSize <<- as.integer(blockSize)
      totalNumberOfResults <<- as.integer(NA)
      results <<- data.frame()
      .fetchedRows <<- as.integer(NA)
    },

  fetch =
    function(n=blockSize) {
      'Retrieve the next block of results or an empty data.frame if there are no more results.'

      # dunno if it makes sense to enforce the limit specified in the query statement
      if (!is.na(limit))
        n <- min(n, limit-offset+1)

      if (n<=0) {
        result <- data.frame()
        attr(result, "totalNumberOfResults") <- totalNumberOfResults
      }
      else {
        query <- paste(queryStatement, "limit", n, "offset", offset)
        result <- synapseQuery(query)
        offset <<- as.integer(offset + nrow(result))
        totalNumberOfResults <<- as.integer(attr(result, "totalNumberOfResults"))
      }

      if (!is.na(limit))
        attr(result, "limit") <- limit
      attr(result, "offset") <- offset
      .fetchedRows <<- nrow(result)

      return(result)
    },
  
  collect =
    function(n=blockSize) {
      'Fetch and accumulate another block of rows. Repeated calls to fetch will return larger and larger data.frames'
      results <<- .mergeDataFrames(results, fetch(n))
      return(results)
    },
  
  collectAll =
    function() {
      'Retrieve all remaining results (up to the limit, if one was set in the original query).'

      repeat {
        collect()
        if (is.na(.fetchedRows) || .fetchedRows==0L) break
      }
      return(results)
    },

  reset =
    function(offset=1) {
      'Move the offset back to 1 or some other offset. Restart iterating through results.'
      offset <<- offset
    },

  show =
    function() {
      cat("QueryResults object:\n")
      cat("  query: ", queryStatement, "\n")
      cat("  blockSize: ", blockSize, "\n")
    }
)
