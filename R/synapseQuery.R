## Execute a Synapse Query
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

synapseQuery <- 
  function(queryStatement, blockSize=NULL)
{
  ## Constants
  kPath <- "/query?query="
  ## end constants
  
  if(!is.character(queryStatement)){
    stop("a query statement must be supplied of R type character")
  }
  
  uri <- paste(kPath, curlEscape(queryStatement), sep="")

  if (is.null(blockSize)) {
    result <- synapseGet(uri=uri, anonymous=FALSE)

    if(result$totalNumberOfResults == 0){
      return(NULL)
    }

    ## Parse response and prepare return value
    return.val <- .jsonListToDataFrame(result$results)
    attr(return.val, "totalNumberOfResults") <- result$totalNumberOfResults

    return(return.val)
  }
  else {

    if (!is.numeric(blockSize)) {
      stop("blockSize must be an integer")
    }

    return(QueryResult$new(queryStatement, blockSize=blockSize))
  }
  
}


synQuery <- 
  function(queryStatement, blockSize=NULL)
{
  synapseQuery(queryStatement, blockSize)
}
