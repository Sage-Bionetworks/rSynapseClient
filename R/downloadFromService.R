##
## downloadFromService
##
## for a wiki page attachment, downloadUri would be:
## /{ownerObjectType}/{ownerObjectId}/wiki/{wikiId}/attachment?fileName={attachmentFileName}
## for a File entity the downloadUri would be:
## 		/entity/{enityId}/file
## or if a version is specified:
## 		/entity/{entityId}/version/{versionNumber}/file
##
## returns the destfile
##
##
## author:  bruce.hoff@sagebase.org
##

## Download a file from a path provided via redirect from a Synapse service
# This function assumes that there will be a redirect to the actual download URL.
# We capture the URL then pass it to the next function which directs it to the right
# handler for the protocol it has (e.g. https vs. sftp).
# NOTE:  downloadUri must contain redirect=FALSE request parameter
downloadFromService<-function(
  downloadUri, 
  endpointName="REPO", 
  destdir, 
  extraRetryStatusCodes=NULL) {
  # check own version, stopping if blacklisted
  checkBlackList()
  
	if (!file.exists(destdir)){
		dir.create(destdir, recursive=TRUE)
	}
	destfile<-tempfile(tmpdir=destdir)
	
	curlHandle<-getCurlHandle()
		
	backoff<-initialBackOffSeconds()
	startTime<-Sys.time()
	
	# The error message that follow come from libcurl as enumerated here
	# http://curl.askapache.com/c/libcurl-errors.html
	# ideally we'd include an exhaustive list of transient outage conditions
	# from the libcurl list.  Unfortunately RCurl neither exposes the numeric
	# value of the error condition nor generates the string in a predictable way
	# So we retry on everything and provide a place to list messages for which
	# we should not retry
	errorMessagesNotToRetry <- character(0)
	
	while (!maxWaitTimeExceeded(startTime, Sys.time())) {
		# first, get the redirect URL.  
		# we have to do this for each retry because the URL 
		# is time limited and may have become invalid
		opts = .getCache("curlOpts")
		# we don't want to follow redirects, we just want to return the redirectURL
		opts$followlocation<-NULL
		url<-synapseGet(downloadUri, 
				endpoint=synapseServiceEndpoint(endpointName),   
				opts = opts)
		
		fcnResult<-try({
					# now download, in a 'protocol-specific' way
					if (isCurlDownload(url)) {
						result<-.curlWriterDownload(url=url, destfile=destfile, curlHandle = curlHandle, opts = opts)
					} else if (isSftpDownload(url)) {
						result<-downloadSftpFile(url, destfile)
					} else {
						stop(sprintf("Unsupported protocol for url: %s", url))
					}
					result
				}, silent=T)
		
		if (isCurlDownload(url)) {
			if (is(fcnResult, "try-error")) {
				# return true if there is no error message specifically tagged not to retry
				isRetryable<-!any(sapply(errorMessagesNotToRetry, function(pattern){regexpr(pattern, fcnResult[[1]], fixed=T)[1]>=0}))
			} else {
				httpStatus<-.getCurlInfo(curlHandle)$response.code
				# return true if status is >=500 or 429 or in the list of statuses to retry
				isRetryable<-httpStatus>=500 || httpStatus>=429 || any(httpStatus==extraRetryStatusCodes)
		}
		} else if (isSftpDownload(url)) {
			isRetryable<-is(fcnResult, "try-error")
		} else {
			stop(sprintf("Unsupported protocol for url: %s", url))
		}
		
		if (isRetryable) {
			# retry
			if (is(fcnResult, "try-error")) {
				reportableResult<-fcnResult[[1]]
			} else {
				reportableResult<-fcnResult
			}
			sleepTime<-sleepTime(startTime, Sys.time(), backoff)
			if (sleepTime>0) {
				message(sprintf("Error encountered: %s. Will wait for %.0f seconds then retry. Press CTRL+C to quit.", 
								reportableResult, sleepTime))
				Sys.sleep(sleepTime)
			}
			backoff <- increaseBackoff(backoff)
		} else {
			break
		}
	} # end for loop
	
	if (isCurlDownload(url)) {
		if (is(fcnResult, "try-error")) {
			.logErrorToSynapse("", fcnResult[[1]])
			stop(fcnResult[[1]])
		} else {
			.checkCurlResponse(object=curlHandle)
			list(downloadedFile=destfile, fileName=fcnResult)
		}
	} else if (isSftpDownload(url)) {
		if (is(fcnResult, "try-error")) {
			.logErrorToSynapse("", fcnResult[[1]])
			stop(fcnResult[[1]])
		} else {
			list(downloadedFile=destfile, fileName=fcnResult)
		}
	} else {
		stop(sprintf("Unsupported protocol for url: %s", url))
	}	
}

downloadSftpFile  <- 
		function (url, filePath)
{
	if (!(RsftpPackageIsAvailable() && require("Rsftp"))) 
		stop("File is hosted on SFTP server but Rsftp package not installed/available.  Please install Rsftp and try again.")
	parsedUrl<-.ParsedUrl(url)
	credentials<-getCredentialsForHost(parsedUrl)
	urlDecodedPath<-URLdecode(parsedUrl@path)
	success<-sftpDownload(parsedUrl@host, credentials$username, credentials$password, urlDecodedPath, filePath)
	if (!success) {
		message<-sprintf("Failed to download %s from %s", urlDecodedPath, parsedUrl@host)
		logErrorToSynapse(label=sprintf("sftp get %s", parsedUrl@host), message=)
		stop(message)
	}
	URLdecode(parsedUrl@file)
}

isCurlDownload<-function(url) {
	parsedUrl<-.ParsedUrl(url)
	protocol<-tolower(parsedUrl@protocol)
	protocol=="http" || protocol=="https" || protocol=="file" || protocol=="ftp"
}

isSftpDownload<-function(url) {
	parsedUrl<-.ParsedUrl(url)
	protocol<-tolower(parsedUrl@protocol)
	protocol=="sftp"
}
