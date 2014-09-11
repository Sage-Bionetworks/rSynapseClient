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
      dfColumnType<-class(dataframe[[i]])
      columnType<-getTableColumnTypeForDataFrameColumnType(dfColumnType)
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

getTableColumnTypeForDataFrameColumnType<-function(dfColumnType) {
  map<-c(integer="INTEGER", factor="STRING", character="STRING", numeric="DOUBLE", logical="BOOLEAN")
  result<-map[dfColumnType]
  if (is.na(result)) stop(sprintf("No column type for %s", dfColumnType))
  names(result)<-NULL # otherwise the dfColumnType labels the result
  result
}

