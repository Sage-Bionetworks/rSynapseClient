## Utility function for merging tables by row
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
##############################################################################

.rowMerge <- 
  function(table1, newRow)
{
  rowNumber <- nrow(table1) + 1
  for(colName in names(newRow)){
    table1[rowNumber, colName] <- newRow[1,colName]
  }
  return(table1)
}
