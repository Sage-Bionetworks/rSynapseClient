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
      totalNumberOfResults="integer"
    )
  )

QueryResult$methods(
  initialize =
    function(queryStatement, blockSize=100L) {
      'Fetch query results one blocks of records at a time.'

      # try to extract limit and offset from the query statement
      processed.query <- .queryLimitAndOffset(queryStatement)
      
      .self$queryStatement <<- processed.query$query
      .self$limit <<- as.integer(processed.query$limit)
      .self$offset <<- as.integer(processed.query$offset)
      .self$blockSize <<- as.integer(blockSize)
      .self$totalNumberOfResults <<- as.integer(NA)
    },
  
  fetch =
    function(n=blockSize) {
      'Retrieve the next block of results or an empty data.frame if there are no more results.'

      # dunno if it makes sense to enforce the limit specified in the query statement
      if (!is.na(limit))
        n <- min(n, limit-offset+1)

      if (n<=0) {
        result <- data.frame()
        if (!is.na(limit))
          attr(result, "limit") <- limit
        attr(result, "offset") <- offset
        attr(result, "totalNumberOfResults") <- totalNumberOfResults
      }
      else {
        query <- paste(queryStatement, "limit", n, "offset", offset)
        result <- synapseQuery(query)
        if (!is.na(limit))
          attr(result, "limit") <- limit
        attr(result, "offset") <- offset
        offset <<- as.integer(offset + nrow(result))
        totalNumberOfResults <<- as.integer(attr(result, "totalNumberOfResults"))
      }

      return(result)
    },
  
  fetchAll =
    function() {
      'Retrieve all remaining results (up to the limit, if one was set in the original query).'

      results <- .self$fetch()
      if (nrow(results)==0) return(results)
      repeat {
        nextResult <- .self$fetch()
        if (nrow(nextResult)==0) break

        # There is no guarantee of getting the same schema in each
        # block, so we make sure to add any missing columns, before
        # we do rbind.

        # Note: This might be a duplication of what's in rowMerge, so we
        #       might want to remove one or the other.

        # add new columns to existing results
        results <- .mergeColumns(results, names(nextResult))

        # add missing columns to new results
        nextResult <- .mergeColumns(nextResult, names(results))

        results <- rbind(results, nextResult)
      }
      return(results)
    }
)
