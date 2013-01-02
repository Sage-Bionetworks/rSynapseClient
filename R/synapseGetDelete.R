## Send a get or delete request to synapse
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

.synapseGetDelete <- 
  function(uri, isRepoRequest, requestMethod, curlHandle=getCurlHandle(), 
    anonymous = .getCache("anonymous"), opts = .getCache("curlOpts"), entity=NULL, checkHttpStatus=T)
{
  
  if(is.null(uri))
    stop("uri cannot be null")

  if (isRepoRequest) {
    path = .getRepoEndpointPrefix()
  } else { # is auth request
    path = .getAuthEndpointPrefix()
  }
  
  if(is.null(path))
    stop("path cannot be null")

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
  
  if(length(path) > 1)
    stop("put", paste(length(path), path))
  ## Prepare the header. If not an anonymous request, stuff the
  ## sessionToken into the header
  header <- .getCache("curlHeader")
  if(is.null(anonymous) || !anonymous) {
    header <- switch(authMode(),
      auth = .stuffHeaderAuth(header),
      hmac = .stuffHeaderHmac(header, paste(path, uri, sep="")),
      stop("Unknown auth mode: %s. Could not build header", authMode())
    )		
  }
  
  ## Submit request and check response code
  d = debugGatherer()
  
  if(!is.null(.getCache("debug")) && .getCache("debug")) {
    message("----------------------------------")
    message("REQUEST: ", requestMethod, " ", url)
  }
  
  # check own version, stopping if blacklisted
  # but only do this check if the request is not for "/version" 
  # which is part of the blacklist check itself
  if (!(uri=="/version")) {
    checkBlackList()
  }
  
  ##curlSetOpt(opts,curl=curlHandle)
  if(is.null(entity)){
    response<-getURLFollowingRedirect(
      uri,
      isRepoRequest,
      postfields = NULL, # the request body
      customrequest = requestMethod,
      httpheader = header,
      curl = curlHandle, # the curl handle
      debugfunction=d$update,
      .opts=opts
      )
  }else{
    ## convert integers to characters
    for(ii in 1:length(entity)){
      if(all(checkInteger(entity[[ii]])))
        entity[[ii]] <- as.character(as.integer(entity[[ii]]))
    }	
    
    httpBody <- toJSON(entity)
    if(.getCache("debug")) {
      message("REQUEST_BODY: ", httpBody)
    }
    
    response<-getURLFollowingRedirect(
      uri,
      isRepoRequest,
      postfields = httpBody,
      customrequest = requestMethod,
      httpheader = header,
      curl = curlHandle, # the curl handle
      debugfunction=d$update,
      .opts=opts
    )
  }
  
  if(!is.null(.getCache("debug")) && .getCache("debug")) {
    message("RESPONSE_BODY: ", response)
  }
  
  if (checkHttpStatus) .checkCurlResponse(curlHandle, response)
  
  if("GET" == requestMethod) {
    ## Parse response and prepare return value
    as.list(fromJSON(response))
  }
}

