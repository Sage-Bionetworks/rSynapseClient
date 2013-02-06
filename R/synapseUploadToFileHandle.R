##
## upload a file to the FileHandle service
## this service is for files under 100MB in size
## 
## Author:Bruce Hoff <bruce.hoff@sagebase.org>
###############################################################################

synapseUploadToFileHandle<-function(fileName, curlHandle=getCurlHandle()) {
  ## check that fileName exists and can be read
  if (file.access(fileName, 0)==-1) stop (sprintf("%s does not exist.", fileName))
  if (file.access(fileName, 4)==-1) stop (sprintf("Read permission required for %s.", fileName))
  ## check that file is <=100MB
  MAX_FILE_SIZE <- 100 * 1024 * 1024
  fileSize <- file.info(fileName)$size
  if (fileSize > MAX_FILE_SIZE) 
    stop(sprintf("File %s has size %d which exceeds maximum file size of %d."), fileName, fileSize, MAX_FILE_SIZE)
  
  # check own version, stopping if blacklisted
  checkBlackList()
  
  # make sure permanent redirects have been resolved
  resolvePermanentRedirects("FILE")
  
  # the url for the fileHandle service
  fileHandleUri<-"/fileHandle"
  fileHandleUrl<-sprintf("%s%s", synapseFileServiceEndpoint(), fileHandleUri)
  
  # we start with the common header info, as used for other requests
  header <- .getCache("curlHeader")
  # we add in the authentication info
  header <- switch(authMode(),
      auth = .stuffHeaderAuth(header),
      hmac = .stuffHeaderHmac(header, sprintf("%s%s", .getFileEndpointPrefix(), fileHandleUri)),
      stop("Unknown auth mode: %s. Could not build header", authMode())
    )		
  # file upload to /fileHandle is a 'multipart' request
  header['Content-Type']<-"multipart/form-data"

  # we start wit the common request options
  opts <- .getCache("curlOpts")
  
  # 'postForm' doesn't have a httpheader param, but does allow the headers to be specified in the options param
  opts$httpheader <- header
  
  # to invoke multipart upload the 'style' param is set to 'HTTPPOST'
  # unfortunately it looks like 'postForm' doesn't have an option NOT to raise an error for a non-2xx response status,
  # so we have to catch their error and handle it ourselves
  response<-tryCatch(postForm(uri=fileHandleUrl, "fileData" = fileUpload(fileName), curl=curlHandle, .opts=opts, style="HTTPPOST"),
    HTTPError = function(e) {
      .checkCurlResponse(curlHandle)
    }
  )
      
  # check 'curl' for error
  .checkCurlResponse(curlHandle, response)
  
  # translate response body from JSON to R object
  responseObject<-fromJSON(response)
  
  # this object is a list of file handles.  we expect exactly one
  if (is.null(responseObject$list)) stop(sprintf("expected list but found %s", response))
  if (length(responseObject$list)!=1) stop (sprintf("expected list of length 1 but found %s"), responseObject$list)
  
  responseObject$list[[1]]
}