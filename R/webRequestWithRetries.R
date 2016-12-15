# Execute a web request with retries
# fcn has just one param, curlHandle
# extraRetryStatusCodes is a vector of status codes to retry in addition
# to the standard ones (502,503,504 see SYNR-296)
# returns a list having two elements (1) the response of the web request,
# (2) the returned http status code.  
# The result is in the form list(result, httpStatus)
#
# Author: brucehoff
###############################################################################

webRequestWithRetries<-function(fcn,
		curlHandle,
		extraRetryStatusCodes=NULL,
		logErrorsToSynapse=TRUE) {
	
	isRetryable<-function(tryReturnValue) {
		# The error message that follow come from libcurl as enumerated here
		# http://curl.askapache.com/c/libcurl-errors.html
		# ideally we'd include an exhaustive list of transient outage conditions
		# from the libcurl list.  Unfortunately RCurl neither exposes the numeric
		# value of the error condition nor generates the string in a predictable way
		# So we retry on everything and provide a place to list messages for which
		# we should not retry
		errorMessagesNotToRetry <- character(0)
		
		if (is(tryReturnValue, "try-error")) {
			# return true if there is no error message specifically tagged not to retry
			!any(sapply(errorMessagesNotToRetry, function(pattern){regexpr(pattern, tryReturnValue[[1]], fixed=T)[1]>=0}))
		} else {
			httpStatus<-.getCurlInfo(curlHandle)$response.code
			# return true if status is >=500 or 429 or in the list of statuses to retry
			httpStatus>=500 || httpStatus==429 || any(httpStatus==extraRetryStatusCodes)
		}
	}
	
	finalizeResult<-function(tryReturnValue) {
		if (is(tryReturnValue, "try-error")) {
			if (logErrorsToSynapse) .logErrorToSynapse("", tryReturnValue[[1]])
			stop(tryReturnValue[[1]])
		} else {
			httpStatus<-.getCurlInfo(curlHandle)$response.code
			# Note:  We do NOT want to stop() here for non-2xx statuses. 
			# It's the caller's business to decide how to proceed
			list(result=tryReturnValue, httpStatus=httpStatus)
		}
	}
	
	withRetries(fcn=function(){fcn(curlHandle)},
			isRetryable=isRetryable,
			finalizeResult=finalizeResult)
}

# this is added for unit testing purposes, providing a function to override
.logErrorToSynapse<-function(label, message) {
  	logErrorToSynapse(label, message)
}