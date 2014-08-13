
getURLWithRetries<-function(url,
  postfields = NULL, # the request body
  customrequest="GET", # the request method
  httpheader=NULL, # the headers
  curl=getCurlHandle(), # the curl handle
  debugfunction = NULL,
  opts
) {
  # for exponential retries, used for 502, 503 errors and timeouts (see SYNR-296)
  INITIAL_BACKOFF_SECONDS <- 1
  BACKOFF_MULTIPLIER <- 2 # i.e. the back off time is (initial)*[multiplier^(# retries)]
  
  maxTries<-.getCache("webRequestMaxTries")
  if (is.null(maxTries) || maxTries<1) stop(sprintf("Illegal value for maxTries %d.", maxTries))
  
  optsWithHeader<-opts
  optsWithHeader$header<-TRUE
  
  
  backoff<-INITIAL_BACKOFF_SECONDS
  for (retry in 1:maxTries) {
    if (is.null(postfields)) {
      rawResponse<-try(.getURLIntern(url, 
          customrequest=customrequest, 
          httpheader=httpheader, 
          curl=curl, 
          debugfunction=debugfunction,
          .opts=optsWithHeader
        ), silent=T)
    } else {
      rawResponse<-try(.getURLIntern(url, 
          postfields=postfields, 
          customrequest=customrequest, 
          httpheader=httpheader, 
          curl=curl, 
          debugfunction=debugfunction,
         .opts=optsWithHeader
        ), silent=T)
    }
    
    errorMessages <- c("connect() timed out",
      "Connection reset by peer",
      "Failure when receiving data from the peer",
      "Empty reply from server",
      "SSL read: error:00000000",
      "Unknown SSL protocol error")
    
    if (class(rawResponse)=="try-error") {
      # if any of the strings in 'errorMessages' appear anywhere in 'rawResponse[[1]]'...
      if (any(sapply(errorMessages, function(pattern){regexpr(pattern, rawResponse[[1]], fixed=T)[1]>=0}))) {
        # ...then it's a time out
        Sys.sleep(backoff)
        backoff <- backoff * BACKOFF_MULTIPLIER
      } else {
        # ... otherwise it's some other error
        stop(rawResponse[[1]])
      }
    } else {
      httpStatus<-.getCurlInfo(curl)$response.code
      if (httpStatus==504 || httpStatus==503 || httpStatus==502) {
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
    stop(rawResponse[[1]])
  }
  
  # note, if we continue to get 502s or 503s after max exponential retries, then we just return the 502 or 503 result

  #message(rawResponse)
  response<-parseHttpResponse(rawResponse)
  
  list(response=response, httpStatus=httpStatus)
}


# this is added for unit testing purposes, providing a function to override
.getURLIntern<-function(url, 
  postfields,
  customrequest, 
  httpheader, 
  curl, 
  debugfunction,
  .opts
) {
  if (missing(postfields)) {
    getURL(url=url, 
      customrequest=customrequest, 
      httpheader=httpheader, 
      curl=curl, 
      debugfunction=debugfunction,
      .opts=.opts
    )
  } else {
    getURL(url=url, 
      postfields=postfields, 
      customrequest=customrequest, 
      httpheader=httpheader, 
      curl=curl, 
      debugfunction=debugfunction,
      .opts=.opts
    )
  }
}

# this is added for unit testing purposes, providing a function to override
.getCurlInfo<-function(curlHandle) {
  getCurlInfo(curlHandle)
}


