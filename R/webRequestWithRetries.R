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
			# return true if status is >=500 or in the list of statuses to retry
			httpStatus>=500 || any(httpStatus==extraRetryStatusCodes)
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

# webRequestWithRetries<-function(fcn,
#   curlHandle,
#   extraRetryStatusCodes=NULL,
#   logErrorsToSynapse=TRUE) {
#   INITIAL_BACKOFF_SECONDS <- 1
#   BACKOFF_MULTIPLIER <- 2 # i.e. the back off time is (initial)*[multiplier^(# retries)]
# 
#   maxTries<-.getCache("webRequestMaxTries")
#   if (is.null(maxTries) || maxTries<1) stop(sprintf("Illegal value for maxTries %d.", maxTries))
# 
#   backoff<-INITIAL_BACKOFF_SECONDS
# 
#   # The error message that follow come from libcurl as enumerated here
#   # http://curl.askapache.com/c/libcurl-errors.html
#   # ideally we'd include an exhaustive list of transient outage conditions
#   # from the libcurl list.  Unfortunately RCurl neither exposes the numeric
#   # value of the error condition nor generates the string in a predictable way
#   # So we retry on everything and provide a place to list messages for which
#   # we should not retry
#   errorMessagesNotToRetry <- character(0)
# 
#   for (retry in 1:maxTries) {
#     rawResponse<-try(fcn(curlHandle), silent=T)
# 
#     if (class(rawResponse)=="try-error") {
#       # if any of the strings in 'errorMessagesNotToRetry' appear anywhere in 'rawResponse[[1]]'...
#       if (any(sapply(errorMessagesNotToRetry, function(pattern){regexpr(pattern, rawResponse[[1]], fixed=T)[1]>=0}))) {
#         # ... it's an error for which we specifically don't retry
#         if (logErrorsToSynapse) .logErrorToSynapse("", rawResponse[[1]])
#         stop(rawResponse[[1]])
#       } else {
#         # ... retry
#         Sys.sleep(backoff)
#         backoff <- backoff * BACKOFF_MULTIPLIER
#       }
#     } else {
#       httpStatus<-.getCurlInfo(curlHandle)$response.code
#       if (httpStatus>=500 || any(httpStatus==extraRetryStatusCodes)) {
#         # then we retry
#         Sys.sleep(backoff)
#         backoff <- backoff * BACKOFF_MULTIPLIER
#       } else {
#         break
#       }
#     }
#   }
#   if (class(rawResponse)=="try-error") {
#     # then we gave up on the exponential retry
#     if (logErrorsToSynapse) .logErrorToSynapse("", rawResponse[[1]])
#     stop(rawResponse[[1]])
#   }
# 
#   # note, if we continue to get 502s or 503s after max exponential retries, then we just return the 502 or 503 result
#   list(result=rawResponse, httpStatus=httpStatus)
# }

# this is added for unit testing purposes, providing a function to override
.logErrorToSynapse<-function(label, message) {
  	logErrorToSynapse(label, message)
}