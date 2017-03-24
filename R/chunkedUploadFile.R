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
	
	fileSize<-syn.file.info(filepath)$size
	chunksizeBytes <- max(5242880, ceiling(fileSize/10000))
	
	curlHandle<-getCurlHandle()

	MAX_UPLOAD_RETRIES<-7
	uploadRetryCounter<-1
	
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
	multipartUploadRequestAsList<-createListFromS4Object(multipartUploadRequest)
	
	# in the previous step of the loop, how many parts did we need to upload
	lastPartsToUploadCount<-NULL
	
	# within this loop we don't want to throw exceptions when we retry
  # we just want to let the overall process replay (at least for MAX_UPLOAD_RETRIES iterations)
	while (uploadRetryCounter<MAX_UPLOAD_RETRIES) {
		responseAsList<-synapsePost(uri="/file/multipart", 
				entity=multipartUploadRequestAsList, 
				endpoint=synapseServiceEndpoint("FILE"),
				extraRetryStatusCodes=NULL)
		uploadStatus<-createS4ObjectFromList(responseAsList, "MultipartUploadStatus")
		uploadId<-uploadStatus@uploadId
		if (length(uploadStatus@resultFileHandleId)>0)	{
			# upload is done!
			break
		}
		
		ps<-uploadStatus@partsState
		partsToUpload<-as.integer(which(sapply(1:nchar(ps), function(i){substring(ps,i,i)=="0"})))
		
		# the retry counter doesn't increment unless no progress is being made
		partsToUploadCount<-length(partsToUpload)
		if (!is.null(lastPartsToUploadCount) && lastPartsToUploadCount==partsToUploadCount) {
			uploadRetryCounter<-uploadRetryCounter+1
		}
		lastPartsToUploadCount<-partsToUploadCount
		
		if (length(partsToUpload)==0) { # nothing left to upload!
			uploadStatus<-finalizeUpload(uploadId, curlHandle)
			if (!is.null(uploadStatus) && length(uploadStatus@resultFileHandleId)>0)	{
				# upload is done!
				break
			} else {
				next
			}
		}
		
		batchPartUploadURLRequest<-BatchPresignedUploadUrlRequest(
				uploadId=uploadId, 
				contentType=contentType,
				partNumbers=partsToUpload)

		batchRequestTime<-Sys.time()
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
		if (uploadRetryCounter>1) cat("Upload needs to be repeated for ", length(partNumberToUrlMap) , " file part(s).\n")
		
		connection<-file(filepath, open="rb")
		chunkCount<-0
		tryCatch({
			for (chunkNumber in names(partNumberToUrlMap)) {
				chunkCount<-chunkCount+1
				
				# 15 min time limit on upload URLs
				# http://hud.rel.rest.doc.sagebase.org.s3-website-us-east-1.amazonaws.com/POST/file/multipart/uploadId/presigned/url/batch.html
				#
				if (Sys.time()-batchRequestTime>=as.difftime(batchUrlTimeLimit())) {
					break
				}
				
				## get the signed S3 URL
				uploadUrl <- partNumberToUrlMap[[chunkNumber]]
				
				if (is.null(uploadUrl)) {
					next
				}
				
				startPositionForChunk<-(as.integer(chunkNumber)-1)*chunksizeBytes
				syn.seek(connection, startPositionForChunk)
				chunk <- syn.readBin(con=connection, what="raw", n=chunksizeBytes)
				
				# if unsuccessful we simply go on to the next chunk
				# outermost loop will retry any missing chunks
				uploadSuccess <- uploadOneChunk(chunk, uploadUrl, uploadId, chunkNumber, contentType)
				
				if (uploadSuccess) {
					percentUploaded <- chunkCount/length(partNumberToUrlMap)*100
					# print progress, but only if there's more than one chunk
					if (chunkCount>1 | percentUploaded<100) {
						cat(sprintf("Uploaded %.1f%%\n", percentUploaded))
					}	
				}
			}
					
		}, finally=close(connection))
		
		uploadStatus<-finalizeUpload(uploadId, curlHandle)
		if (!is.null(uploadStatus) && length(uploadStatus@resultFileHandleId)>0)	{
			# upload is done!
			break
		}
	} # end while
	
	if (length(uploadStatus@resultFileHandleId)==0) {
		stop("File upload failed.")
	}
	
	# now return the full file handle, retrieved using the new ID
	synapseGet(
			uri=sprintf("/fileHandle/%s", uploadStatus@resultFileHandleId),
			endpoint=synapseServiceEndpoint("FILE"), 
			curlHandle=getCurlHandle())
}


# some mockable interfaces
syn.file.info<-function(...) {file.info(...)}
syn.readBin<-function(...) {readBin(...)}
syn.seek<-function(...) {seek(...)}
batchUrlTimeLimit<-function() {"00:15:00"}

# return TRUE if successful, FALSE otherwise
uploadOneChunk<-function(chunk, uploadUrl, uploadId, chunkNumber, contentType) {
	md5<-stringMd5(chunk)	
	
	## S3 wants 'content-type' and 'content-length' headers. S3 doesn't like
	## 'transfer-encoding': 'chunked', which requests will add for you, if it
	## can't figure out content length. The errors given by S3 are not very
	## informative:
	## If a request mistakenly contains both 'content-length' and
	## 'transfer-encoding':'chunked', you get [Errno 32] Broken pipe.
	## If you give S3 'transfer-encoding' and no 'content-length', you get:
	## 501 Server Error: Not Implemented
	## A header you provided implies functionality that is not implemented
	headers <- c('Content-Type'=contentType)
	
	debug<-.getCache("debug")
	if (is.null(debug)) debug<-FALSE
	
	if (debug) message(sprintf('url= %s\n', uploadUrl))
	
	stringUploadCurlHandle<-getCurlHandle()
	curlRawUploadSuccess<-
			curlRawUpload(url=uploadUrl, chunk=chunk, curlHandle=stringUploadCurlHandle, header=headers)
	
	if (!curlRawUploadSuccess) {
		return(FALSE)
	}
	
	curlHandle<-getCurlHandle()
	responseAsList<-synapsePut(uri=sprintf("/file/multipart/%s/add/%s?partMD5Hex=%s", 
					uploadId, chunkNumber, md5), 
			entity=list(),
			endpoint=synapseServiceEndpoint("FILE"),
			extraRetryStatusCodes=NULL,
			checkHttpStatus=FALSE,
			curlHandle=curlHandle)
	
	# return true if successful, false otherwise
	if(isErrorResponseStatus(getStatusCode(curlHandle))) {
		return(FALSE)
	}
	
	addMultiPartResponse<-createS4ObjectFromList(responseAsList, "AddPartResponse")			
	
	uploadSuccess<-!isErrorResponseStatus(getStatusCode(curlHandle)) &&
			addMultiPartResponse@addPartState=="ADD_SUCCESS"
	
	if (debug) {
		if (uploadSuccess) {uploadResult<-"SUCCESSFUL"} else {uploadResult<-"NOT successful"}
		message(sprintf('\nUpload of Chunk %s was %s.\n', chunkNumber, uploadResult))
	}
	
	uploadSuccess
}

curlRawUpload <-function(url, chunk, method="PUT", 
				curlHandle = getCurlHandle(), header, opts = .getCache("curlOpts"))
{
	opts$noprogress <- 0L
	# I have not idea why, but to get 'curlPerform' to move a file to another file (see test_curlUploadDownload)
	# it is not sufficient to set customrequestmethod to POST, you also have to set opts$put
	if (method=="PUT") opts$put <- 1L
	
	chunkSize<-length(chunk)
	opts$infilesize <- as.integer(chunkSize)
	responseWriteFunction<-basicTextGatherer()
	
	rc<-try(curlPerform(
			URL=url, 
			customrequest=method, 
			readfunction=chunk, 
			curl=curlHandle, 
			httpHeader=header, 
			.opts = opts, 
			writefunction=responseWriteFunction$update), silent=TRUE)
	
		!is(rc, "try-error") && rc==0 && !isErrorResponseStatus(getStatusCode(curlHandle))
}

finalizeUpload<-function(uploadId, curlHandle) {
	responseAsList<-synapsePut(uri=sprintf("/file/multipart/%s/complete", uploadId), 
			entity=list(),
			endpoint=synapseServiceEndpoint("FILE"),
			extraRetryStatusCodes=NULL,
			checkHttpStatus=FALSE,
			curlHandle=curlHandle)
	if (isErrorResponseStatus(getStatusCode(curlHandle))) {
		NULL
	} else {
		createS4ObjectFromList(responseAsList, "MultipartUploadStatus")
	}
}

