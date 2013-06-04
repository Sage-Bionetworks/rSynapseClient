# Upload a local file to Synapse using the 'chunked file' service.
# 
# implements the client side of the Chunked File Upload API defined here:
# https://sagebionetworks.jira.com/wiki/pages/viewpage.action?pageId=31981623
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

chunkedUploadFile<-function(filepath, curlHandle=getCurlHandle(), chunksizeBytes=5*1024*1024) {
  if (chunksizeBytes < 5*1024*1024)
      stop('Minimum chunksize is 5 MB.')
  
  if (!file.exists(filepath))
    stop(sprintf('File not found: %s',filepath))
  
  debug<-.getCache("debug")
  if (is.null(debug)) debug<-FALSE
  
  chunkNumber <- 1 # service requires that chunk number be >0
  
  # guess mime-type - important for confirmation of MD5 sum by receiver
  mimetype<-getMimeTypeForFile(basename(filepath))
  
  ## S3 wants 'content-type' and 'content-length' headers. S3 doesn't like
  ## 'transfer-encoding': 'chunked', which requests will add for you, if it
  ## can't figure out content length. The errors given by S3 are not very
  ## informative:
  ## If a request mistakenly contains both 'content-length' and
  ## 'transfer-encoding':'chunked', you get [Errno 32] Broken pipe.
  ## If you give S3 'transfer-encoding' and no 'content-length', you get:
  ## 501 Server Error: Not Implemented
  ## A header you provided implies functionality that is not implemented
  headers <- list('Content-Type'=mimetype)
  
  ## get token
  token <- createChunkedFileUploadToken(filepath, mimetype)
  if (debug) {
    tokenAsString<-paste(lapply(names(token), function(n,x){sprintf("%s=%s",n, x[[n]])}, token), collapse=",")
    message(sprintf('\n\ntoken: %s\n', tokenAsString))
  }
  
  ## define the retry policy for uploading chunks
  # with_retry = RetryRequest(retry_status_codes=[502,503], retries=4, wait=1, back_off=2, verbose=verbose)
  chunkResults<-list()
  connection<-file(filepath, open="rb")
  repeat {
    #chunk <- readChar(connection, chunksizeBytes)
    chunk <- readBin(con=connection, what="raw", n=chunksizeBytes)
    if (length(chunk)==0) break
    # get the ith chunk from the file
    if (debug) message(sprintf('\nChunk %d. size %d\n', chunkNumber, length(chunk)))
    
    ## get the signed S3 URL
    chunkRequest <- list(chunkNumber=chunkNumber, chunkedFileToken=token)
    chunkUploadUrl <- createChunkedFileUploadChunkURL(chunkRequest)
    if (debug) message(sprintf('url= %s\n', chunkUploadUrl))
    
    ## PUT the chunk to S3
    response <- getURLWithRetries(chunkUploadUrl,
      postfields = chunk, # the request body
      customrequest="PUT", # the request method
      httpheader=headers, # the headers
      opts=.getCache("curlOpts")
    )
    
    chunkResult <- addChunkToFile(chunkRequest)
    chunkResults[[length(chunkResults)+1]]<-chunkResult
    chunkNumber <- chunkNumber + 1
  }
  close(connection)
  ## finalize the upload and return a fileHandle
  completeChunkFileUpload(token, chunkResults)
}

createChunkedFileUploadToken<-function(filepath, mimetype) {
  md5 <- tools::md5sum(filepath)
  if (is.na(md5)) stop(sprintf("Unable to compute md5 for %s", filepath))
  names(md5)<-NULL # Needed to make toJSON work right

  chunkedFileTokenRequest<-list(
    fileName=basename(filepath),
    contentType=mimetype,
    contentMD5=md5
  )
  
  synapsePost(uri='/createChunkedFileUploadToken', 
    entity=chunkedFileTokenRequest, 
    endpoint=synapseServiceEndpoint("FILE"))
}

# returns the URL for uploading the specified chunk
createChunkedFileUploadChunkURL<-function(chunkRequest) {
  synapsePost(uri='/createChunkedFileUploadChunkURL', 
    entity=chunkRequest, 
    endpoint=synapseServiceEndpoint("FILE"))
}

addChunkToFile<-function(chunkRequest) {
  ## We occasionally get an error on addChunkToFile:
  ## 500 Server Error: Internal Server Error
  ## {u'reason': u'The specified key does not exist.'}
  ## This might be because S3 hasn't yet finished propagating the
  ## addition of the new chunk. So, retry_request will wait and retry.

  # TODO add retry for 500 error, but just for this case, not for all POSTs
  synapsePost(uri='/addChunkToFile', 
    entity=chunkRequest, 
    endpoint=synapseServiceEndpoint("FILE"))
}

completeChunkFileUpload<-function(chunkedFileToken, chunkResults) {
  S3FileHandle(
    synapsePost(uri='/completeChunkFileUpload', 
      entity=list(
        chunkedFileToken=chunkedFileToken, 
        chunkResults=chunkResults), 
      endpoint=synapseServiceEndpoint("FILE"))
  )
}
