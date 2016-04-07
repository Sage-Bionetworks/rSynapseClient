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
	
	debug<-.getCache("debug")
	if (is.null(debug)) debug<-FALSE
	
	# guess mime-type - important for confirmation of MD5 sum by receiver
	if (is.null(contentType)) {
		contentType<-getMimeTypeForFile(basename(filepath))
	}
	
	md5 <- tools::md5sum(path.expand(filepath))
	
	
	fileSize<-file.info(filepath)$size
	chunksizeBytes <- max(5242880, ceiling(fileSize/10000))
	
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
	
	curlHandle<-getCurlHandle()

	MAX_UPLOAD_RETRIES<-7
	uploadRetryCounter<-0
	fileHandleId<-NULL
	
	multipartUploadRequest<-MultipartUploadRequest(
			contentMD5Hex=md5, 
			fileName=basename(filepath), 
			generatePreview=TRUE,
			contentType=contentType,
			partSizeBytes=chunksizeBytes,
			fileSizeBytes=fileSize,
			storageLocationId=uploadDestination@storageLocationId
	)
	responseAsList<-synapsePost(uri="/file/multipart/upload", 
			entity=multipartUploadRequest, 
			endpoint=synapseServiceEndpoint("FILE"),
			extraRetryStatusCodes=NULL)
	uploadStatus<-createS4ObjectFromList(responseAsList, "MultipartUploadStatus")
	uploadId<-uploadStatus@uploadId
	
	while (uploadRetryCounter<MAX_UPLOAD_RETRIES && is.null(fileHandleId)) {
		uploadRetryCounter<-uploadRetryCounter+1

		batchPartUploadURLRequest<-BatchPartUploadURLRequest()
		# TODO retry but don't stop if failure
		responseAsList<-synapsePost(uri=sprintf("/file/multipart/%s/presignedurl/batch", uploadId), 
				entity=batchPartUploadURLRequest, 
				endpoint=synapseServiceEndpoint("FILE"),
				extraRetryStatusCodes=NULL)
		batchPartUploadUrlResponse<-createS4ObjectFromList(responseAsList, "BatchPartUploadURLResponse")
		partNumberToUrlMap<-list()
		for (part in batchPartUploadUrlResponse@partPresignedUrls@content) {
			partNumberToUrlMap[[as.character(part@partNumber)]]<-part@uploadPresignedUrl
		}
		connection<-file(filepath, open="rb") # TODO wrap in try/finally and close the connection in the 'finally'
		chunkCount<-1
		for (chunkNumber in names(partNumberToUrlMap)) {
			startPositionForChunk<-(as.integer(chunkNumber)-1)*chunksizeBytes
			seek(connection, startPositionForChunk)
			chunk <- readBin(con=connection, what="raw", n=chunksizeBytes)
	
			if (debug) message(sprintf('\nChunk %d. size %d\n', chunkCount, length(chunk)))
			
			## get the signed S3 URL
			uploadUrl <- partNumberToUrlMap[[chunkNumber]]
			
			if (!is.null(uploadUrl)) {
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
	
				# TODO retry but don't stop if failure
				responseAsList<-synapsePut(uri=sprintf("/file/multipart/%s/add/%s?partMD5Hex=%s", 
								uploadId, chunkNumber, stringMd5(chunk)), 
					endpoint=synapseServiceEndpoint("FILE"),
					extraRetryStatusCodes=NULL)
				addMultiPartResponse<-createS4ObjectFromList(responseAsList, "AddMultipartResponse")
				
				# TODO review this computation
				totalUploadedBytes <- totalUploadedBytes + length(chunk)
				percentUploaded <- totalUploadedBytes*100/(length(partNumberToUrlMap)*chunksizeBytes)
				# print progress, but only if there's more than one chunk
				if (chunkCount>1 | percentUploaded<100) {
					cat(sprintf("Uploaded %.1f%%\n", percentUploaded))
				}
			}
			chunkCount<-chunkCount+1
		}
		
		# TODO retry but don't stop if failure	
    # TODO retry on state!="COMPLETED"
		responseAsList<-synapsePut(uri=sprintf("/file/multipart/%s/complete", uploadId), 
				endpoint=synapseServiceEndpoint("FILE"),
				extraRetryStatusCodes=NULL)
		multipartUploadStatus<-createS4ObjectFromList(responseAsList, "MultipartUploadStatus")
		fileHandleId<-multipartUploadStatus@resultFileHandleId
	}
	
	fileHandleId
}


