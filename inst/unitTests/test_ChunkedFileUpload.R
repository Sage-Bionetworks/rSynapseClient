# Teest expontential backoff/retry
# 
# Author: brucehoff
###############################################################################

.setUp <- function() {
  synapseClient:::.mock("createChunkedFileUploadToken", function(filepath, s3UploadDestination, contentType) {list()})
  synapseClient:::.setCache("chunkUrlCount", 0)
  
  synapseClient:::.mock("createChunkedFileUploadChunkURL", function(chunkRequest) {
      count<-synapseClient:::.getCache("chunkUrlCount")
      result<-sprintf("http://foo/bar/%s", count)
      synapseClient:::.setCache("chunkUrlCount", count+1)
      result
  })
  synapseClient:::.mock("completeChunkFileUpload", function(chunkedFileToken, chunkResults) {list()})
  synapseClient:::.mock(".getURLIntern", function(url,postfields,customrequest,httpheader,curl,debugfunction,.opts) {
      if (url=="http://foo/bar/0") {
        stop("Connection reset by peer")
      } else {
        # nonsenscial response
        "HTTP/1.1 200 OK\r\nContent-Type: application/json\r\n\r\n{\"foo\":\"bar\"}"
      }
  })
  synapseClient:::.mock(".checkCurlResponse", function(object, response, call.=FALSE, logErrorToSynapse=FALSE) {TRUE})
}

.tearDown <- function() {
  synapseClient:::.unmockAll()
}


unitTestChunkedUpload <- function() {
  # create a file to upload
  content<-"this is a test"
  filePath<- tempfile()
  connection<-file(filePath)
  writeChar(content, connection, eos=NULL)
  close(connection)  
  
  synapseClient:::chunkedUploadFile(filePath)
  checkEquals(2, synapseClient:::.getCache("chunkUrlCount"))
}