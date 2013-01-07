# getURLFollowingRedirect
# wraps getURL in RCurl adding logic for following redirects
# Note: getURL also has redirect following logic, but doesn't support
# the option to suppress mapping 'POST' to 'GET' when redirecting a request
# Therefore we implement the redirect logic ourselves.
# 
# Author: brucehoff
###############################################################################


getURLFollowingRedirect<-function(
  uri, # omitting the endpoint
  isRepoRequest=TRUE, # if FALSE then it's an auth request
  postfields = NULL, # the request body
  customrequest, # the request method
  httpheader, # the headers
  curl, # the curl handle
  debugfunction = NULL,
  .opts, 
  maxTries # the number of tries when timeout or 503 is encountered.  1=no retries
) {
  noRedirOpts<-.opts
  noRedirOpts$followlocation<-NULL # do NOT include 'followlocation'
  noRedirOpts$header<-TRUE
  MAX_REDIRECTS <- 3
  
  # for exponential retries, used for 503 errors and timeouts (see SYNR-296)
  INITIAL_BACKOFF_SECONDS <- 1
  BACKOFF_MULTIPLIER <- 2 # i.e. the back off time is (initial)*[multiplier^(# retries)]
  if (maxTries<1) stop(sprintf("Illegal parameter maxTries %d.", maxTries))
  
  for (redirs in 1:MAX_REDIRECTS) { 
    if (isRepoRequest) {
      url <- paste(synapseRepoServiceEndpoint(), uri, sep="")
    } else {
      url <- paste(synapseAuthServiceEndpoint(), uri, sep="")
    }
    backoff<-INITIAL_BACKOFF_SECONDS
    for (retry in 1:maxTries) {
      if (is.null(postfields)) {
        rawResponse<-try(.getURLIntern(url, 
          customrequest=customrequest, 
          httpheader=httpheader, 
          curl=curl, 
          debugfunction=debugfunction,
          .opts=noRedirOpts
          ), silent=T)
      } else {
        rawResponse<-try(.getURLIntern(url, 
          postfields=postfields, 
          customrequest=customrequest, 
          httpheader=httpheader, 
          curl=curl, 
          debugfunction=debugfunction,
          .opts=noRedirOpts
          ), silent=T)
      }
      
      if (class(rawResponse)=="try-error") {
        if (regexpr("connect() timed out", rawResponse[[1]], fixed=T)[1]>=0) {
          # then it's a time out
          Sys.sleep(backoff)
          backoff <- backoff * BACKOFF_MULTIPLIER
        } else {
          # then it's some other error
          stop(rawResponse[[1]])
        }
      } else {
        httpStatus<-.getCurlInfo(curl)$response.code
        if (httpStatus==503) {
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
    
    # note, if we continue to get 503s after max exponential retries, then we just return the 503 result
    response<-parseHttpResponse(rawResponse)
    
    if (httpStatus==301) {
      redirectLocation<-response$headers[["Location"]]
      if (is.null(redirectLocation)) stop("received redirect status but no redirect location")
      # uri should be at the end of the redirect location
      uriStart <- regexpr(uri, redirectLocation, fixed=T)
      if (uriStart<0) stop(sprintf("%s does not appear in %s", uri, redirectLocation))
      if (uriStart+nchar(uri)!=1+nchar(redirectLocation)) stop(sprintf("%s does not come at the end of %s", uri, redirectLocation))
      redirectEndpoint<-substr(redirectLocation, 1, uriStart-1)
      if (!isRepoRequest) {
        synapseAuthServiceEndpoint(redirectEndpoint)
      } else {
        synapseRepoServiceEndpoint(redirectEndpoint)
      }
    } else {
      return(response$body)
    }
  }
  
  
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


