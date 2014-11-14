# convenience functions for converting data frame into a list of TableColumns
# 
# Author: brucehoff
###############################################################################

setMethod(
  f = "as.tableColumns",
  signature = signature("data.frame"),
  definition = function(source) {
    dataframe<-source
    n<-length(dataframe)
    result<-list()
    if (n<1) return(result)
    for (i in 1:length(dataframe)) {
      dfColumnType<-class(dataframe[[i]])[1]
      columnType<-getTableColumnTypeForDataFrameColumnType(dfColumnType)[1]
      columnName<-names(dataframe[i])
      if (dfColumnType=="factor") {
        levels<-levels(dataframe[[i]])
        tableColumn<-TableColumn(name=columnName, columnType=columnType, enumValues=do.call("CharacterList", as.list(levels)))
      } else {
        tableColumn<-TableColumn(name=columnName, columnType=columnType)
      }
      result<-append(result, tableColumn)
    }
    result
  }
)

setMethod(
  f = "as.tableColumns",
  signature = signature("character"),
  definition = function(source) {
    as.tableColumns(read.csv(source))
  }
)

# returns the Synapse types which can hold the given R type, with the first one being the preferred
getTableColumnTypeForDataFrameColumnType<-function(dfColumnType) {
  map<-list(integer=c("INTEGER","DOUBLE","FILEHANDLEID"), 
      factor=c("STRING","FILEHANDLEID","ENTITYID","LINK"), 
      character=c("STRING","FILEHANDLEID","ENTITYID","LINK"), 
      numeric=c("DOUBLE","INTEGER","FILEHANDLEID"), 
      logical=c("BOOLEAN","STRING"),
      Date=c("DATE","STRING"),
      POSIXct=c("DATE","STRING"))
  result<-map[[dfColumnType]]
  if (is.null(result)) stop(sprintf("No column type for %s", dfColumnType))
  result
}

