## Convert a JSON list to a data.frame
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

.jsonListToDataFrame <- function(jsonList){
  returnVal <- data.frame()
  for(i in seq(along=jsonList)){
    thisRow <- .parseSingleRow(jsonList[[i]])
    if(is.null(returnVal)){
      returnVal <- thisRow
    }else{
      returnVal <- .rowMerge(returnVal, thisRow)
    }		
  }
  return(returnVal)
}
