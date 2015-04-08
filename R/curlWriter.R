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

# Download from the given URL to a temporary file in the given directory. 
# Return the path to the downloaded file as well as the file name (either
# from the Content-Disposition header or the tail of the URL).
.curlWriterDownload <-
  function(url, destdir=tempdir(), curlHandle = getCurlHandle(), writeFunction=.getCache('curlWriter'), opts = .getCache("curlOpts"))
{
  destfile<-tempfile(tmpdir=destdir)
  
  ext <- .curlWriterOpen(destfile)
  on.exit(.curlWriterClose(ext))
  
  opts$noprogress <- 0L
  
  h = basicTextGatherer()
  curlPerform(URL=url, writefunction=writeFunction, headerfunction=h$update, 
			writedata=ext, .opts = opts, curl = curlHandle)	
	
	
  .checkCurlResponse(object=curlHandle, logErrorToSynapse=TRUE)
  fileName<-fileNameFromHeaders(h$value())
  list(downloadedFile=destfile, fileName=fileName)
}

# looks for a header of the form:
#	Content-Disposition: ... filename=<filename>
# If found, returns <filename> else NULL
fileNameFromHeaders<-function(headers) {
	for (header in strsplit(headers, "\r\n", fixed=T)[[1]]) {
		if (1==regexpr("^Content-Disposition:", header)[1]) {
			pieces<-strsplit(header, "filename=")[[1]]
			if (length(pieces)==2) return(pieces[2])
		}
	}
	NULL
}
