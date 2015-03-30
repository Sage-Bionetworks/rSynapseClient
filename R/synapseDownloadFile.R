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
  
  synapseDownloadFileToDestination(url=url, destdir=dirname(destfile), opts=opts)
}

# download file from source which may involve one of a variety of protocols
synapseDownloadFileToDestination  <- 
  function (url, destdir, curlHandle = getCurlHandle(), opts = .getCache("curlOpts"), extraRetryStatusCodes)
{
  parsedUrl<-.ParsedUrl(url)
  protocol<-tolower(parsedUrl@protocol)
  if (protocol=="http" || protocol=="https" || protocol=="file" || protocol=="ftp") {
    result<-synapseDownloadHttpFileToDestination(url, destdir, curlHandle, opts, extraRetryStatusCodes)
  } else if (protocol=="sftp") {
    result<-synapseDownloadSftpFileToDestination(url, destdir)
  } else {
    stop(sprintf("Unsupported protocol %s", protocol))
  }
  result
}

# download file from source which is HTTP/HTTPS
synapseDownloadHttpFileToDestination  <- 
    function (url, destdir, curlHandle = getCurlHandle(), opts = .getCache("curlOpts"), extraRetryStatusCodes)
  {
  	if (!file.exists(destdir)){
   	 dir.create(destdir, recursive=TRUE)
  	}
  
  # TODO wrap this in webRequestWithRetries
  tryCatch(
    downloadResult<-.curlWriterDownload(url=url, destdir=destdir, opts = opts, curlHandle = curlHandle),
    error = function(ex){
      file.remove(tmpFile)
      stop(ex)
    }
  )
  downloadResult
}

synapseDownloadSftpFileToDestination  <- 
  function (url, destdir)
{
  if (!(RsftpPackageIsAvailable() && require("Rsftp"))) 
    stop("File is hosted on SFTP server but Rsftp package not installed/available.  Please install Rsftp and try again.")
  parsedUrl<-.ParsedUrl(url)
  credentials<-getCredentialsForHost(parsedUrl)
  urlDecodedPath<-URLdecode(parsedUrl@path)
  filePath<-tempfile(tmpdir=destdir)
  success<-sftpDownload(parsedUrl@host, credentials$username, credentials$password, urlDecodedPath, filePath)
  if (!success) {
    message<-sprintf("Failed to download %s from %s", urlDecodedPath, parsedUrl@host)
    logErrorToSynapse(label=sprintf("sftp get %s", parsedUrl@host), message=)
    stop(message)
  }
  list(downloadedFile=filePath, fileName=URLdecode(parsedUrl@file))
}

