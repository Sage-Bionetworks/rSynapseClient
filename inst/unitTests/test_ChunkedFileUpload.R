# Test expontential backoff/retry
# 
# Author: brucehoff
###############################################################################
library(rjson)

# this is a set of convenience functions for global variables
# global getter
g<-function(key) {synapseClient:::.getCache(key)}
# global getter, turning nulls to zeros
gInt<-function(key) {
	result<-g(key)
	if (is.null(result)) result<-as.integer(0)
	result
}
s<-function(key, value) {
	synapseClient:::.setCache(key, value)
	allCtrs<-g("test_ChunkedFileUpload_counters")
	allCtrs<-unique(c(allCtrs, key))
	synapseClient:::.setCache("test_ChunkedFileUpload_counters", allCtrs)
}
clearAllCtrs<-function() {
	toClear<-synapseClient:::.getCache("test_ChunkedFileUpload_counters")
	for (key in toClear) synapseClient:::.setCache(key, NULL)
	synapseClient:::.setCache("test_ChunkedFileUpload_counters", NULL)
}

inc<-function(key) {
	value<-gInt(key)
	s(key, value+1)
	value+1
}

# return TRUE iff the string 's' contains the substring 'sub'
contains<-function(s, sub) {regexpr(sub, s)[1]>=0}

.setUp <- function() {
	clearAllCtrs()
}

.tearDown <- function() {
	clearAllCtrs()
  synapseClient:::.unmockAll()
}

unitTestChunkedUpload <- function() {
  # create a file to upload
  content<-"this is a test"
  filePath<- tempfile(fileext = ".txt")
  connection<-file(filePath)
  writeChar(content, connection, eos=NULL)
  close(connection)  
	
	# mock a file which will have three 'chunks'
	synapseClient:::.mock("syn.file.info", function(...) {list(size=as.integer(5242880*2.5))})
	synapseClient:::.mock("syn.readBin", function(conn,what,n) {"some file content"})
	synapseClient:::.mock("syn.seek", function(conn,n) {n})
	
	synapseClient:::.mock("synapsePost", function(uri, entity, ...) {
				if (uri=="/file/multipart") {
					checkEquals(entity$fileName, basename(filePath))
					checkEquals(entity$fileSize, as.integer(5242880*2.5))
					key<-"post_/file/multipart"
					cntr<-gInt(key) # how many times has this mock been called?
					inc(key)
					if (cntr==0) {
						# first time, let's say two of three chunks need to be uploaded
						list(uploadId="101", partsState="100")
					} else {
						# shouldn't call a second time
						stop(sprintf("Unexpected value for %s %d.", key, cntr))
					}
				} else if (contains(uri, "/presigned/url/batch")) {
					key<-"post_/presigned/url/batch"
					cntr<-gInt(key) # how many times has this mock been called?
					inc(key)
					if (cntr==0) {
						list(partPresignedUrls=list(
									list(partNumber=as.integer(2), uploadPresignedUrl="/url22"),
									list(partNumber=as.integer(3), uploadPresignedUrl="/url33")
								)
						)
					} else {
						# shouldn't call a second time
						stop(sprintf("Unexpected value for %s %d.", key, cntr))
					}
				} else {
					stop("unexpected uri: ", uri)
				}
			})
  
	synapseClient:::.mock("synapsePut", function(uri, ...) {
				if (contains(uri, "add")) {
					key<-"put_add"
					cntr<-gInt(key) # how many times has this mock been called?
					inc(key)
					# there are just two parts to put
					if (cntr>=2) stop(sprintf("Unexpected value for %s %d.", key, cntr))
					list(addPartState="ADD_SUCCESS")
				} else if (contains(uri, "complete")) {
					key<-"put_complete"
					cntr<-gInt(key) # how many times has this mock been called?
					inc(key)
					# should just be called once (so counter is zero)
					if (cntr>=1) stop(sprintf("Unexpected value for %s %d.", key, cntr))
					list(state="COMPLETED", resultFileHandleId="202")
				} else {
					stop("unexpected uri: ", uri)
				}
			})
	
	synapseClient:::.mock("synapseGet", function(uri, ...) {
				if (contains(uri, "/fileHandle")) {
					key<-"get_fileHandle"
					cntr<-gInt(key) # how many times has this mock been called?
					inc(key)
					# should just be called once (so counter is zero)
					if (cntr>=1) stop(sprintf("Unexpected value for %s %d.", key, cntr))
					list(addPartState="ADD_SUCCESS")
				} else {
					stop("unexpected uri: ", uri)
				}
			})
	
	synapseClient:::.mock("curlStringUpload", function(...) {
				key<-"curlStringUpload"
				cntr<-gInt(key) # how many times has this mock been called?
				inc(key)
				# should just be called twice
				if (cntr>=2) stop(sprintf("Unexpected value for %s %d.", key, cntr))
				""
		})
	
	
	fileHandle <- synapseClient:::chunkedUploadFile(filePath)
 
	# should have called 'POST /file/multipart' twice
	checkEquals(gInt("post_/file/multipart"), as.integer(1))
	
	# check that /presigned/url/batch was called once
	checkEquals(gInt("post_/presigned/url/batch"), as.integer(1))
	
	# check that two parts were added
	checkEquals(gInt("put_add"), as.integer(2))
	
	# check that 'complete' was called once
	checkEquals(gInt("put_complete"), as.integer(1))
	
	# check that GET /fileHandle was called once
	checkEquals(gInt("get_fileHandle"), as.integer(1))
	
	# check that two parts were added
	checkEquals(gInt("curlStringUpload"), as.integer(2))
	
}
