## Test expontential backoff/retry
## 
## Author: brucehoff
################################################################################
#
#set<-function(key, value) {synapseClient:::.setCache(key, value)}
#get<-function(key) {synapseClient:::.getCache(key)}
#
#.setUp <- function() {
#	synapseClient:::.mock("functionname", function() {})
#}
#
#.tearDown <- function() {
#  synapseClient:::.unmockAll()
#}
#
#unitTestChunkedUpload <- function() {
#  # create a file to upload
#  content<-"this is a test"
#  filePath<- tempfile(fileext = ".txt")
#  connection<-file(filePath)
#  writeChar(content, connection, eos=NULL)
#  close(connection)  
#	
#	synapseClient:::.mock("file.info", function(filePath) {list(size=as.integer(5242880*2.5))})
#	synapseClient:::.mock("synapsePost", function(...) {
#				if (uri=="/file/multipart") {
#					checkEquals(entity$fileName, basename(filePath))
#					checkEquals(entity$fileSize, as.integer(5242880*2.5))
#					list(uploadId="101", partsState="100") # first chunk has already been uploaded
#				} else if (uri=="/file/multipart/%s/presigned/url/batch") {
#					list(partPresignedUrls=c("/url11", "/url22", "/url33"))
#				} else {
#					stop("unexpected uri: ", uri)
#				}
#			})
#
#  
#  fileHandle <- synapseClient:::chunkedUploadFile(filePath)
#  
#	# TODO check correctness
#}
