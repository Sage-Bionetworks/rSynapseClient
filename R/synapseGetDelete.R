## Send a get or delete request to synapse
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

.synapseGetDelete <- 
  function(
    uri, 
    endpoint, 
    requestMethod, 
    curlHandle=getCurlHandle(), 
    anonymous = .getCache("anonymous"), 
    opts = .getCache("curlOpts"), 
    checkHttpStatus=T,
	logErrorsToSynapse=TRUE
)
{
  
  if(is.null(uri) || length(uri)==0)
    stop("uri is required")

  path<-endpoint$endpointPrefix
  
  if(is.null(path) || length(path)==0)
    stop("path is required")

  ## constants
  kValidMethods <- c("GET", "DELETE")
  ## end constants
  
  if(!(requestMethod %in% kValidMethods)){
    stop("invalid request method")
  }
  
  if(!is.character(uri)){
    stop("a uri must be supplied of R type character")
  }
  
  ## uris formed by the service already have their servlet prefix
  pathIndex<-regexpr(path, uri, fixed=T)
  if(pathIndex>0) {
    uri<-substr(uri, pathIndex+nchar(path), nchar(uri))
  }
  paramIndex<-regexpr("?", uri, fixed=T)
  if(paramIndex>0) {
    uriWithoutParams<-substr(uri, 1, paramIndex-nchar("?"))
  } else {
    uriWithoutParams<-uri
  }
  
  if(length(path) > 1)
    stop("put", paste(length(path), path))
    
  ## Prepare the header. If not an anonymous request, stuff API key or session token into the header
  header <- .getCache("curlHeader")
  if(is.null(anonymous) || !anonymous) {
    header <- .stuffHeader(header, paste(path, uriWithoutParams, sep=""))
  }
  
  ## Submit request and check response code
  d = debugGatherer()
  
  if(!is.null(.getCache("debug")) && .getCache("debug")) {
    message("----------------------------------")
    message("REQUEST: ", requestMethod, " ", uri)
    message("HEADERS: ", listToString(header))
  }
  
  # check own version, stopping if blacklisted
  # but only do this check if the request is not for "/version" 
  # which is part of the blacklist check itself
  if (!(uri=="/version")) {
    checkBlackList()
  }
  
  ##curlSetOpt(opts,curl=curlHandle)
  response<-synapseRequest(
      uri,
      endpoint,
      postfields = NULL, # the request body
      customrequest = requestMethod,
      httpheader = header,
      curl = curlHandle, # the curl handle
      debugfunction=d$update,
      .opts=opts,
	  logErrorsToSynapse
      )
  
  if(!is.null(.getCache("debug")) && .getCache("debug")) {
	message("RESPONSE_HEADERS:: ", response$headers)
    message("RESPONSE_BODY: ", response$body)
  }
  
  if (checkHttpStatus) .checkCurlResponse(object=curlHandle, response=response$body)
  
  if("GET" == requestMethod) {
    parseResponseBody(response$headers, response$body)
  }
}

