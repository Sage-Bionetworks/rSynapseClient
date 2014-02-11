# Utility to use chunked file upload service to load a string as a single 'chunk'
# Returns a file handle
# Author: brucehoff
###############################################################################

uploadStringToFile<-function(content, contentType="text/plain") {
  if (is.null(content) || nchar(content)==0) stop("Missing content.")
  
  MINIMUM_CHUNK_SIZE_BYTES<-5*1024*1024
  if (nchar(content)>=MINIMUM_CHUNK_SIZE_BYTES) 
    stop("String must be less than "+MINIMUM_CHUNK_SIZE_BYTES+" bytes.")
  
  contentMD5 <- stringMd5(content)
  
  chunkedFileTokenRequest<-list(
    fileName="content",
    contentType=contentType,
    contentMD5=contentMD5
  )
  
  chunkedFileUploadToken<-synapsePost(uri='/createChunkedFileUploadToken', 
    entity=chunkedFileTokenRequest, 
    endpoint=synapseServiceEndpoint("FILE"))
  
  chunkNumber<-1
  ## get the signed S3 URL
  chunkRequest <- list(chunkNumber=chunkNumber, chunkedFileToken=chunkedFileUploadToken)
  chunkUploadUrl <- createChunkedFileUploadChunkURL(chunkRequest)
  
  headers <- list('Content-Type'=contentType)
  
  ## PUT the chunk to S3
  response <- getURLWithRetries(chunkUploadUrl,
    postfields = charToRaw(content), # the request body
    customrequest="PUT", # the request method
    httpheader=headers, # the headers
    opts=.getCache("curlOpts")
  )
  
  chunkResults<-list(chunkNumber)
 
  completeChunkFileUpload(chunkedFileUploadToken, chunkResults)   
}

