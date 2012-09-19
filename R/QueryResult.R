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
    function(n=blockSize, accumulate=FALSE) {
      'Retrieve the next block of results, replacing any accumulated results
      with the current block or accumulating more results if accumulate is set
      to TRUE. Returns the block of results as a data.frame or an empty
      data.frame if there are no results'

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

      if (accumulate) {
        # accumulate cached results
        results <<- .mergeDataFrames(results, result)
      }
      else {
        # replace cached results
        results <<- result
      }

      return(invisible(result))
    },
  
  collect =
    function(n=blockSize) {
      'Fetch and accumulate another block of rows. Repeated calls to collect will store query results
      in the results field of the QueryResults object, which can later be obtained by a call to
      as.data.frame(qr). Invisibly returns the currently fetched block of results.'

      return(invisible(fetch(n, accumulate=TRUE)))
    },
  
  collectAll =
    function() {
      'Retrieve and accumulate all remaining results (up to the limit, if one was set in the original query)
      invisibly returning them as a data.frame.'

      repeat {
        collect()
        if (is.na(.fetchedRows) || .fetchedRows==0L) break
      }
      return(invisible(results))
    },

  reset =
    function(offset=1) {
      'Start iterating through results from a new offset. Move the offset back to 1 (by default) or to some other offset.'
      offset <<- offset
    },

  as.data.frame =
    function() {
      return(results)
    },

  names =
    function() {
      'Column names of the accumulated query results'
      colnames(results)
    },

  length =
    function() {
      'Number of accumulated results'
      nrow(results)
    },

  show =
    function() {
      cat("QueryResults object:\n")
      cat("      query: ", queryStatement, "\n")
      cat("  blockSize: ", blockSize, "\n")
      cat("     offset: ", offset, "\n")
      cat("      limit: ", limit, "\n")
      cat("  totalNumberOfResults: ", totalNumberOfResults, "\n")
    }
)
