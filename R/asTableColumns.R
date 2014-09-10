# convenience functions for converting data frame into a list of TableColumns
# 
# Author: brucehoff
###############################################################################

setMethod(
  f = "as.TableColumns",
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
        tableColumn<-TableColumn(name=columnName, columnType=columnType, enumValues=CharacterList(levels))
      } else {
        tableColumn<-TableColumn(name=columnName, columnType=columnType)
      }
      result<-append(result, tableColumn)
    }
    result
  }
)

setMethod(
  f = "as.TableColumns",
  signature = signature("character"),
  definition = function(source) {
    as.TableColumns(read.csv(source))
  }
)

getTableColumnTypeForDataFrameColumnType<-function(dfColumnType) {
  map<-c(integer="INTEGER", factor="STRING", character="STRING", numeric="DOUBLE", logical="BOOLEAN")
  result<-map[dfColumnType]
  if (is.na(result)) stop(sprintf("No column type for %s", dfColumnType))
  result
}

