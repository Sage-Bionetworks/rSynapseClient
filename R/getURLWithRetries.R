# execute web request with exponential back-off for failures
#
# returned value is list(body=<body>, parsedHeaders=list(statusCode, statusString, headers))

getURLWithRetries<-function(url,
  postfields = NULL, # the request body
  customrequest="GET", # the request method
  httpheader=NULL, # the headers
  curl=getCurlHandle(), # the curl handle
  debugfunction = NULL,
  opts,
  logErrorsToSynapse=TRUE,
  extraRetryStatusCodes=NULL
) {
  
  # result has the form list(result=list(headers,body), httpStatus=<status>)
  result<-webRequestWithRetries(
      fcn=function(curlHandle) {
        if (is.null(postfields)) {
          .getURLIntern(url, 
              customrequest=customrequest, 
              httpheader=httpheader, 
              curl=curlHandle, 
              debugfunction=debugfunction,
			  .opts=opts
            )
        } else {
          .getURLIntern(url, 
              postfields=postfields, 
              customrequest=customrequest, 
              httpheader=httpheader, 
              curl=curlHandle, 
              debugfunction=debugfunction,
			  .opts=opts
            )
        }
		# returns list(headers,body)
      },
      curlHandle=curl,
	  extraRetryStatusCodes=extraRetryStatusCodes,
	  logErrorsToSynapse
  )
  parsedHeaders<-parseHttpHeaders(result$result$headers)
  list(body=result$result$body, parsedHeaders=parsedHeaders)
}


# this is added for unit testing purposes, providing a function to override
# returned value is list(headers, body), neither headers nor body have been parsed
.getURLIntern<-function(url, 
  postfields,
  customrequest, 
  httpheader, 
  curl, 
  debugfunction,
  .opts
) {
	if(!is.null(.getCache("debug")) && .getCache("debug")) {
		message(".getURLIntern: url:", url)
	}
	
	if (length(url)!=1) stop("Expected length(url) to be 1 found found ", (length(url)))
	
	h = basicTextGatherer()
	
  if (missing(postfields)) {
	  body<-getURL(url=url, 
      customrequest=customrequest, 
      httpheader=httpheader, 
      curl=curl, 
      debugfunction=debugfunction,
	  headerfunction=h$update,
      .opts=.opts
    )
  } else {
	  body<-getURL(url=url, 
      postfields=postfields, 
      customrequest=customrequest, 
      httpheader=httpheader, 
      curl=curl, 
      debugfunction=debugfunction,
	  headerfunction=h$update,
      .opts=.opts
    )
  }
  list(headers=h$value(), body=body)
}

# this is added for unit testing purposes, providing a function to override
.getCurlInfo<-function(curlHandle) {
  getCurlInfo(curlHandle)
}


