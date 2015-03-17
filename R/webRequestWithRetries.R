# Execute a web request with retries
# fcn has just one param, curlHandle
# extraRetryStatusCodes is a vector of status codes to retry in addition
# to the standard ones (502,503,504 see SYNR-296)
# returns a list having two elements (1) the response of the web request,
# (2) the returned http status code
#
# Author: brucehoff
###############################################################################



webRequestWithRetries<-function(fcn,
  curlHandle,
  extraRetryStatusCodes=NULL) {
  INITIAL_BACKOFF_SECONDS <- 1
  BACKOFF_MULTIPLIER <- 2 # i.e. the back off time is (initial)*[multiplier^(# retries)]
  
  maxTries<-.getCache("webRequestMaxTries")
  if (is.null(maxTries) || maxTries<1) stop(sprintf("Illegal value for maxTries %d.", maxTries))
  
  backoff<-INITIAL_BACKOFF_SECONDS
  errorMessages <- c("connect() timed out",
    "Connection reset by peer",
    "Failure when receiving data from the peer",
    "Empty reply from server",
    "SSL read: error:00000000",
    "Unknown SSL protocol error",
    "couldn't connect to host")
  
  retryStatusCodes<-append(c(502,503,504), extraRetryStatusCodes)
  
  for (retry in 1:maxTries) {
    rawResponse<-try(fcn(curlHandle), silent=T)
    
    if (class(rawResponse)=="try-error") {
      # if any of the strings in 'errorMessages' appear anywhere in 'rawResponse[[1]]'...
      if (any(sapply(errorMessages, function(pattern){regexpr(pattern, rawResponse[[1]], fixed=T)[1]>=0}))) {
        # ...then it's a time out
        Sys.sleep(backoff)
        backoff <- backoff * BACKOFF_MULTIPLIER
      } else {
        # ... otherwise it's some other error
        .logErrorToSynapse("", rawResponse[[1]])
        stop(rawResponse[[1]])
      }
    } else {
      httpStatus<-.getCurlInfo(curlHandle)$response.code
      if (any(httpStatus==retryStatusCodes)) {
        # then we retry
        Sys.sleep(backoff)
        backoff <- backoff * BACKOFF_MULTIPLIER
      } else {
        break
      }
    }
  }
  if (class(rawResponse)=="try-error") {
    # then we gave up on the exponential retry
    .logErrorToSynapse("", rawResponse[[1]])
    stop(rawResponse[[1]])
  }
  
  # note, if we continue to get 502s or 503s after max exponential retries, then we just return the 502 or 503 result
  list(result=rawResponse, httpStatus=httpStatus)
}

# this is added for unit testing purposes, providing a function to override
.logErrorToSynapse<-function(label, message) {
  logErrorToSynapse(label, message)
}