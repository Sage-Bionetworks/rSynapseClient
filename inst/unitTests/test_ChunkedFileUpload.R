# Test expontential backoff/retry
# 
# Author: brucehoff
###############################################################################

set<-function(key, value) {synapseClient:::.setCache(key, value)}
get<-function(key) {synapseClient:::.getCache(key)}

contains<-function(s, sub) {regexpr(sub, s)[1]>=0}

.setUp <- function() {
	synapseClient:::.mock("functionname", function() {})
}

.tearDown <- function() {
  synapseClient:::.unmockAll()
}

unitTestChunkedUpload <- function() {
  # create a file to upload
  content<-"this is a test"
  filePath<- tempfile(fileext = ".txt")
  connection<-file(filePath)
  writeChar(content, connection, eos=NULL)
  close(connection)  
	
	synapseClient:::.mock("file.info", function(filePath) {list(size=as.integer(5242880*2.5))})
	synapseClient:::.mock("readBin", function(conn,what,n) {"some file content"})
	synapseClient:::.mock("seek", function(conn,n) {n})
	
	synapseClient:::.mock("synapsePost", function(...) {
				if (uri=="/file/multipart") {
					checkEquals(entity$fileName, basename(filePath))
					checkEquals(entity$fileSize, as.integer(5242880*2.5))
					list(uploadId="101", partsState="100") # first chunk has already been uploaded
				} else if (contains(uri, "/presigned/url/batch")) { # TODO need a regex here
					list(partPresignedUrls=list(
									list(partNumber=as.integer(1), uploadPresignedUrl="/url11"),
									list(partNumber=as.integer(2), uploadPresignedUrl="/url22"),
									list(partNumber=as.integer(3), uploadPresignedUrl="/url33")
								)
						)
				} else {
					stop("unexpected uri: ", uri)
				}
			})
  
	synapseClient:::.mock("synapsePut", function(...) {
				if (contains(uri, "add")) {
					list(addPartState="ADD_SUCCESS")
				} else if (contains(uri, "complete")) {
					list(state="COMPLETED")
				} else {
					stop("unexpected uri: ", uri)
				}
			})
	
	synapseClient:::.mock("synapseGet", function(...) {
				if (uri=="/fileHandle") {
					list(addPartState="ADD_SUCCESS")
				} else if (uri=="/file/multipart/%s/complete") {
					list(state="COMPLETED")
				} else {
					stop("unexpected uri: ", uri)
				}
			})
	
	synapseClient:::.mock("curlStringUpload", function(...) {""})
	
	
	fileHandle <- synapseClient:::chunkedUploadFile(filePath)
  
	# TODO check correctness
}
