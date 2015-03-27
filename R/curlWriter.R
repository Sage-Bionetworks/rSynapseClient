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
	
  writeBody <- .curlWriterOpen(destfile)
  on.exit(.curlWriterClose(writeBody))
  
  opts$noprogress <- 0L
  opts$followlocation<-FALSE # don't follow redirects
  
  # capture header information
  captureHeaderInfo<-FALSE
  if (captureHeaderInfo) {
  	opts$header<-TRUE # capture the response header
	headerfile<-tempfile()
	writeHeader <- .curlWriterOpen(headerfile)
	on.exit(.curlWriterClose(writeHeader))
	#h = basicTextGatherer()
	curlPerform(URL=url, 
			writefunction=writeFunction, writedata=writeBody, 
		#	headerfunction=h$update, 
				writeheader=writeHeader,
			.opts = opts, curl = curlHandle)	
#  	if (!is.null(.getCache("debug")) && .getCache("debug")) {
		message("curlWriterDownload response headers:\n", readFile(headerfile))
#  	}
  } else {
	  curlPerform(URL=url, 
			  writefunction=writeFunction, writedata=writeBody, 
			  .opts = opts, curl = curlHandle)
  }
  

  .checkCurlResponse(object=curlHandle, logErrorToSynapse=TRUE)
  destfile
}
