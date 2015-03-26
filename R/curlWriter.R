## Add objects to an entity
## 
## Author: Martin Morgan <mtmorgan@fhcrc.org>
###############################################################################

.curlWriterOpen <-
  function(filename)
{
  if (!is.character(filename) || 1L != length(filename))
    stop("'filename' must be character(1)")
  dir <- dirname(filename)
  if (!file.exists(dir) || !file.info(dir)$isdir)
    stop("'dirname(filename)' does not exist or is not a directory")
  filename <- file.path(normalizePath(dir), basename(filename))
  if (file.exists(filename))
    stop("'filename' must not already exist")
  
  .Call("writer_open", filename)
}

.curlWriterClose <-
  function(ext)
{
  .Call("writer_close", ext)
}

.curlWriterDownload <-
  function(url, destfile=tempfile(), curlHandle = getCurlHandle(), writeFunction=.getCache('curlWriter'), opts = .getCache("curlOpts"))
{
	if(!is.null(.getCache("debug")) && .getCache("debug")) {
		message("DOWNLOADING FROM: ", url);
	}
  ext <- .curlWriterOpen(destfile)
  on.exit(.curlWriterClose(ext))
  opts$noprogress <- 0L
  
  opts$header<-FALSE # capture the response header
  
  headerHandler<-dynCurlReader(curl=curlHandle)
  
  curlPerform(URL=url, writefunction=writeFunction,
		  writedata=ext, .opts = opts, curl = curlHandle)
#  curlPerform(URL=url, writefunction=writeFunction,
#		  writedata=ext, .opts = opts, curl = curlHandle, headerfunction = headerHandler$update)
  
  if (!is.null(.getCache("debug")) && .getCache("debug")) {
	message("curlWriterDownload response headers:\n", headerHandler$header())
  }
  .checkCurlResponse(object=curlHandle, logErrorToSynapse=TRUE)
  destfile
}
