## Download a file from Synapse
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

legalFilePath<-function(filePath) {
	gsub("[()`'<>\"|?*]", "_", filePath)
}

synapseDownloadFile  <- 
  function (url, checksum, curlHandle = getCurlHandle(), cacheDir = synapseCacheDir(), opts = .getCache("curlOpts"), versionId = NULL)
{
	if (is.null(cacheDir)) stop(paste("cacheDir is required. synapseCacheDir() returns ", synapseCacheDir()))
	
  ## Download the file to the cache
  destfile <- .generateCacheDestFile(url, versionId)
  
  ## temporary hack for github url that does not contain file extension
  parsedUrl <- .ParsedUrl(url)
  if( parsedUrl@host=="github.com" ){
    splits <- strsplit(parsedUrl@pathPrefix, "/")
    if( splits[[1]][length(splits[[1]])] == "zipball" )
      destfile <- paste(destfile, ".zip", sep="")
    if( splits[[1]][length(splits[[1]])] == "tarball" )
      destfile <- paste(destfile, ".tar", sep="")
  }
  
  synapseDownloadFileToDestination(url=url, destfile=destfile, opts=opts)
}

# download file from source which may involve one of a variety of protocols
synapseDownloadFileToDestination  <- 
  function (url, destfile, curlHandle = getCurlHandle(), opts = .getCache("curlOpts"), extraRetryStatusCodes)
{
  parsedUrl<-.ParsedUrl(url)
  protocol<-tolower(parsedUrl@protocol)
  if (protocol=="http" || protocol=="https" || protocol=="file" || protocol=="ftp") {
    synapseDownloadHttpFileToDestination(url, destfile, curlHandle, opts, extraRetryStatusCodes)
  } else if (protocol=="sftp") {
    synapseDownloadSftpFileToDestination(url, destfile)
  } else {
    stop(sprintf("Unsupported protocol %s", protocol))
  }
}

# download file from source which is HTTP/HTTPS
synapseDownloadHttpFileToDestination  <- 
    function (url, destfile, curlHandle = getCurlHandle(), opts = .getCache("curlOpts"), extraRetryStatusCodes)
  {
    ## Download the file to a specified location
  splits <- strsplit(destfile, .Platform$file.sep)
  downloadDir <- path.expand(paste(splits[[1]][-length(splits[[1]])], collapse=.Platform$file.sep))
  downloadFileName <- splits[[1]][length(splits[[1]])]
  if(!file.exists(downloadDir)){
    dir.create(downloadDir, recursive=TRUE)
  }
  
  ## download to temp file first so that the existing local file (if there is one) is left in place
  ## if the download fails
  tmpFile <- tempfile()
  # TODO wrap this in webRequestWithRetries
  tryCatch(
    downloadResult<-.curlWriterDownload(url=url, destfile=tmpFile, opts = opts, curlHandle = curlHandle),
    error = function(ex){
      file.remove(tmpFile)
      stop(ex)
    }
  )
  fileName<-downloadResult$fileName # TODO use this for the file name
  
  ## copy then delete. this avoids a cross-device error encountered
  ## on systems with multiple hard drives when using file.rename
  if(!file.copy(tmpFile, destfile, overwrite = TRUE)){
    file.remove(tmpFile)
    stop("COULD NOT COPY: ", tmpFile, " TO: ", destfile)
  }
  file.remove(tmpFile)
  return(destfile)
}

synapseDownloadSftpFileToDestination  <- 
  function (url, destfile)
{
  if (!(RsftpPackageIsAvailable() && require("Rsftp"))) 
    stop("File is hosted on SFTP server but Rsftp package not installed/available.  Please install Rsftp and try again.")
  parsedUrl<-.ParsedUrl(url)
  credentials<-getCredentialsForHost(parsedUrl)
  urlDecodedPath<-URLdecode(parsedUrl@path)
  success<-sftpDownload(parsedUrl@host, credentials$username, credentials$password, urlDecodedPath, destfile)
  if (!success) {
    message<-sprintf("Failed to download %s from %s", urlDecodedPath, parsedUrl@host)
    logErrorToSynapse(label=sprintf("sftp get %s", parsedUrl@host), message=)
    stop(message)
  }
}

