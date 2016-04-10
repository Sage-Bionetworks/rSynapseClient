# Upload a local file to Synapse using the 'chunked file' service.
# 
# implements the client side of the Chunked File Upload API defined here:
# https://sagebionetworks.jira.com/wiki/display/PLFM/Robust+File+Upload
# 
#  Upload a file to be stored in Synapse, dividing large files into chunks.
#  
#  filepath: the file to be uploaded
#  curlHandle
#  chunksize: chop the file into chunks of this many bytes. The default value is
#  5MB, which is also the minimum value.
#  
#  returns an S3FileHandle
#  Author: brucehoff
###############################################################################

chunkedUploadFile<-function(filepath, uploadDestination=S3UploadDestination(), curlHandle=getCurlHandle(), contentType=NULL) {
	if (!file.exists(filepath))
		stop(sprintf('File not found: %s',filepath))
	
	# guess mime-type - important for confirmation of MD5 sum by receiver
	if (is.null(contentType)) {
		contentType<-getMimeTypeForFile(basename(filepath))
	}
	
	md5 <- tools::md5sum(path.expand(filepath))
	names(md5)<-NULL
	
	fileSize<-file.info(filepath)$size
	chunksizeBytes <- max(5242880, ceiling(fileSize/10000))
	
	curlHandle<-getCurlHandle()

	MAX_UPLOAD_RETRIES<-7
	uploadRetryCounter<-0
	
	debug<-.getCache("debug")
	if (is.null(debug)) debug<-FALSE
	
	
	multipartUploadRequest<-MultipartUploadRequest(
			contentMD5Hex=md5, 
			fileName=basename(filepath), 
			generatePreview=TRUE,
			contentType=contentType,
			partSizeBytes=chunksizeBytes,
			fileSizeBytes=fileSize,
			storageLocationId=uploadDestination@storageLocationId
	)
	responseAsList<-synapsePost(uri="/file/multipart", 
			entity=createListFromS4Object(multipartUploadRequest), 
			endpoint=synapseServiceEndpoint("FILE"),
			extraRetryStatusCodes=NULL)
	uploadStatus<-createS4ObjectFromList(responseAsList, "MultipartUploadStatus")
	uploadId<-uploadStatus@uploadId
	
	# within this loop we don't want to throw exceptions when we retry
  # we just want to let the overall process replay (at least for MAX_UPLOAD_RETRIES iterations)
	while (uploadRetryCounter<MAX_UPLOAD_RETRIES && length(uploadStatus@resultFileHandleId)==0) {
		uploadRetryCounter<-uploadRetryCounter+1

		ps<-uploadStatus@partsState
		partsToUpload<-as.integer(which(sapply(1:nchar(ps), function(i){substring(ps,i,i)=="0"})))
		
		if (length(partsToUpload)==0) { # nothing left to upload!
			uploadStatus<-finalizeUpload(uploadId, curlHandle)
			next
		}
		
		cat("ps: ", ps, "\n")
		cat("partsToUpload: ", toJSON(partsToUpload), "\n")
		
		batchPartUploadURLRequest<-BatchPresignedUploadUrlRequest(
				uploadId=uploadId, 
				contentType=contentType,
				partNumbers=partsToUpload)
		

		responseAsList<-synapsePost(uri=sprintf("/file/multipart/%s/presigned/url/batch", uploadId), 
				entity=createListFromS4Object(batchPartUploadURLRequest), 
				endpoint=synapseServiceEndpoint("FILE"),
				extraRetryStatusCodes=NULL,
				checkHttpStatus=FALSE,
				curlHandle=curlHandle)
		if (isErrorResponseStatus(getStatusCode(curlHandle))) {
			next
		}
		
		batchPartUploadUrlResponse<-createS4ObjectFromList(responseAsList, "BatchPresignedUploadUrlResponse")
		partNumberToUrlMap<-list()
		for (part in batchPartUploadUrlResponse@partPresignedUrls@content) {
			partNumberToUrlMap[[as.character(part@partNumber)]]<-part@uploadPresignedUrl
		}
		if (uploadRetryCounter>1) cat("Upload needs to be repated for ", length(partNumberToUrlMap) , " file parts.\n")
		
		connection<-file(filepath, open="rb")
		chunkCount<-0
		tryCatch({
					for (chunkNumber in names(partNumberToUrlMap)) {
						chunkCount<-chunkCount+1
						
						addMultiPartResponse<-uploadOneChunk(uploadId, connection, chunkNumber, chunksizeBytes, partNumberToUrlMap, curlHandle, contentType)
						if (debug) message(sprintf('\nChunk %d.\n', chunkCount))
						
						
						if (!is.null(addMultiPartResponse)) {
							percentUploaded <- chunkCount/length(partNumberToUrlMap)*100
							# print progress, but only if there's more than one chunk
							if (chunkCount>1 | percentUploaded<100) {
								cat(sprintf("Uploaded %.1f%%\n", percentUploaded))
							}	
						}
					}
					
		}, finally=close(connection))
		
		uploadStatus<-finalizeUpload(uploadId, curlHandle)
		if (isErrorResponseStatus(getStatusCode(curlHandle))) {
			next
		}
		
	}
	
	if (length(uploadStatus@resultFileHandleId)==0) {
		stop("File upload failed.")
	}
	
	# now return the full file handle, retrieved using the new ID
	synapseGet(
			uri=sprintf("/fileHandle/%s", uploadStatus@resultFileHandleId),
			endpoint=synapseServiceEndpoint("FILE"), 
			curlHandle=getCurlHandle())
}

finalizeUpload<-function(uploadId, curlHandle) {
	responseAsList<-synapsePut(uri=sprintf("/file/multipart/%s/complete", uploadId), 
			entity=list(),
			endpoint=synapseServiceEndpoint("FILE"),
			extraRetryStatusCodes=NULL,
			checkHttpStatus=FALSE,
			curlHandle=curlHandle)
	createS4ObjectFromList(responseAsList, "MultipartUploadStatus")
}

uploadOneChunk<-function(uploadId, connection, chunkNumber, chunksizeBytes, partNumberToUrlMap, curlHandle, contentType) {
	## S3 wants 'content-type' and 'content-length' headers. S3 doesn't like
	## 'transfer-encoding': 'chunked', which requests will add for you, if it
	## can't figure out content length. The errors given by S3 are not very
	## informative:
	## If a request mistakenly contains both 'content-length' and
	## 'transfer-encoding':'chunked', you get [Errno 32] Broken pipe.
	## If you give S3 'transfer-encoding' and no 'content-length', you get:
	## 501 Server Error: Not Implemented
	## A header you provided implies functionality that is not implemented
	headers <- list('Content-Type'=contentType)

	debug<-.getCache("debug")
	if (is.null(debug)) debug<-FALSE
	
	startPositionForChunk<-(as.integer(chunkNumber)-1)*chunksizeBytes
	seek(connection, startPositionForChunk)
	chunk <- readBin(con=connection, what="raw", n=chunksizeBytes)
	
	## get the signed S3 URL
	uploadUrl <- partNumberToUrlMap[[chunkNumber]]
	
	if (is.null(uploadUrl)) return(NULL)
	
	result<-webRequestWithRetries(
			fcn=function(curlHandle) {
				if (debug) message(sprintf('url= %s\n', uploadUrl))
				
				httpResponse<-.getURLIntern(uploadUrl, 
						postfields=chunk, # the request body
						customrequest="PUT", # the request method
						httpheader=headers, # the headers
						curl=curlHandle, 
						debugfunction=NULL,
						.opts=.getCache("curlOpts"))
				# return the http response
				httpResponse$body
			}, 
			curlHandle,
			extraRetryStatusCodes=400 #SYNR-967
	)
	
	responseAsList<-synapsePut(uri=sprintf("/file/multipart/%s/add/%s?partMD5Hex=%s", 
					uploadId, chunkNumber, stringMd5(chunk)), 
			entity=list(),
			endpoint=synapseServiceEndpoint("FILE"),
			extraRetryStatusCodes=NULL,
			checkHttpStatus=FALSE,
			curlHandle=curlHandle)
	if (isErrorResponseStatus(getStatusCode(curlHandle))) {
		return(NULL)
	}
	addMultiPartResponse<-createS4ObjectFromList(responseAsList, "AddPartResponse")
	addMultiPartResponse
}


