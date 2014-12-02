# convenience functions for converting csv file or data frame into a list of TableColumns
# 
# Author: brucehoff
###############################################################################

setMethod(
  f = "as.tableColumns",
  signature = signature("data.frame"),
  definition = function(source) {
    file<-tempfile()
    writeDataFrameToCSV(dataFrame=source, filePath=file)
    as.TableColumns(file)
  }
)

setMethod(
  f = "as.tableColumns",
  signature = signature("character"),
  definition = function(source) {
    filePath<-source
    s3FileHandle<-chunkedUploadFile(filePath)
    request<-UploadToTablePreviewRequest(uploadFileHandleId=s3FileHandle$id)
    asyncJobId<-createS4ObjectFromList(
      synRestPOST("/table/upload/csv/preview/async/start", createListForS4Object(request)))
    responseBodyAsList<-trackProgress(sprintf("/table/upload/csv/preview/async/get/%s", asyncJobId@token), verbose=FALSE)
    responseBody<-createS4ObjectFromList(responseBodyAsList, "UploadToTablePreviewResult")
    list(fileHandleId=s3FileHandle$id, tableColumns=responseBody$suggestedColumns)
  }
)


