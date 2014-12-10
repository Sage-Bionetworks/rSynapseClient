## Add objects to an entity
## 
## Author: Martin Morgan <mtmorgan@fhcrc.org>
###############################################################################

.curlReaderOpen <-
  function(filename)
{
  if (!is.character(filename) || 1L != length(filename))
    stop("'filename' must be character(1)")
  if (!file.exists(filename) || file.info(filename)$isdir)
    stop("'filename' does not exist or is a directory")
  
  .Call("reader_open", filename)
}

.curlReaderClose <-
  function(ext)
{
  .Call("reader_close", ext)
}

.curlReaderUpload <-
  function(url, srcfile, header, method="PUT", curlHandle = getCurlHandle(), readFunction=.getCache('curlReader'), opts = .getCache("curlOpts"))
{
  parsedUrl <- .ParsedUrl(url)
  if(tolower(parsedUrl@protocol) == "file"){
    if(file.exists(parsedUrl@path))
      file.remove(parsedUrl@path)
    file.create(parsedUrl@path)
  }
  ext <- .curlReaderOpen(srcfile)
  on.exit(.curlReaderClose(ext))
  opts$noprogress <- 0L
  # I have not idea why, but to get 'curlPerform' to move a file to another file (see test_curlUploadDownload)
  # it is not sufficient to set customrequestmethod to POST, you also have to set opts$put
  if (method=="PUT") opts$put <- 1L

  opts$infilesize <- file.info(srcfile)$size
  responseWriteFunction<-basicTextGatherer()
  if(missing(header)){
    curlPerform(URL=url, customrequest=method, readfunction=readFunction,readdata=ext, curl=curlHandle, .opts = opts, writefunction=responseWriteFunction$update)
  }else{
    curlPerform(URL=url, customrequest=method, readfunction=readFunction,readdata=ext, curl=curlHandle, httpHeader=header, .opts = opts, writefunction=responseWriteFunction$update)
  }
  .checkCurlResponse(object=curlHandle, response=responseWriteFunction$value(), logErrorToSynapse=TRUE)
  responseWriteFunction$value()
}
