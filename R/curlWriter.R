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
  
  .Call("writer_open", filename)
}

.curlWriterClose <-
  function(ext)
{
  .Call("writer_close", ext)
}

# Download from the given URL to the given temporary file. 
# Return the file name (either from the Content-Disposition header or the tail of the URL).
.curlWriterDownload <-
  function(url, destfile, curlHandle, writeFunction=.getCache('curlWriter'), opts = .getCache("curlOpts"))
{
  ext <- .curlWriterOpen(destfile)
  on.exit(.curlWriterClose(ext))
  
  opts$noprogress <- 0L
  
	if (file.exists(destfile)) {
		filesize<-file.info(destfile)$size
		opts<-c(opts, range=sprintf("%s-", filesize))
		
	}
  h = basicTextGatherer()
  curlPerform(URL=url, writefunction=writeFunction, headerfunction=h$update, 
			writedata=ext, .opts = opts, curl = curlHandle)	
	
  fileName<-fileNameFromHeaders(h$value())
  if (is.null(fileName)) {
	  parsedUrl<-.ParsedUrl(url)
	  fileName<-parsedUrl@file
  }
  fileName
}

# looks for a header of the form:
#	Content-Disposition: ... filename=<filename>
# If found, returns <filename> else NULL
fileNameFromHeaders<-function(headers) {
	for (header in strsplit(headers, "\r\n", fixed=T)[[1]]) {
		if (1==regexpr("^Content-Disposition:", header)[1]) {
			# per https://regex101.com/r/hJ7tS6/28
			return(gsub("^Content-Disposition:.*filename[^;\n=]*=(['\"])*(.*)(?(1)\\1|)", "\\2", header, perl=TRUE))
		}
	}
	NULL
}
