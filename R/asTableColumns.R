# convenience functions for converting csv file or data frame into a list of TableColumns
# 
# Author: brucehoff
###############################################################################

setMethod(
  f = "as.tableColumns",
  signature = signature("data.frame"),
  definition = function(source, doFullFileScan=TRUE) {
    file<-tempfile()
    writeDataFrameToCSV(dataFrame=source, filePath=file)
    as.tableColumns(file, doFullFileScan=doFullFileScan)
  }
)

setMethod(
  f = "as.tableColumns",
  signature = signature("character"),
  definition = function(source, linesToSkip=as.integer(0), quoteCharacter=character(0),
    escapeCharacter=character(0), separator=character(0), lineEnd=character(0), doFullFileScan=TRUE) {
    filePath<-source
    s3FileHandleId<-chunkedUploadFile(filePath)
    request<-UploadToTablePreviewRequest(
      uploadFileHandleId=as.character(s3FileHandleId),
      linesToSkip=linesToSkip,
      doFullFileScan=doFullFileScan,
      csvTableDescriptor=CsvTableDescriptor(
        quoteCharacter=quoteCharacter,
        isFirstLineHeader=TRUE,
        escapeCharacter=escapeCharacter,
        separator=separator,
        lineEnd=lineEnd)
    )
    asyncJobId<-createS4ObjectFromList(
      synRestPOST("/table/upload/csv/preview/async/start", createListFromS4Object(request)),
      "AsyncJobId")
    responseBodyAsList<-trackProgress(sprintf("/table/upload/csv/preview/async/get/%s", asyncJobId@token), verbose=FALSE)
    responseBody<-createS4ObjectFromList(responseBodyAsList, "UploadToTablePreviewResult")
    list(fileHandleId=as.integer(s3FileHandle$id), tableColumns=responseBody$suggestedColumns@content)
  }
)


