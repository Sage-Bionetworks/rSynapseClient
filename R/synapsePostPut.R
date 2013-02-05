## Send a post or put request to Synapse
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

.synapsePostPut <- 
  function(
    uri, 
    entity, 
    service, 
    requestMethod, 
    curlHandle = getCurlHandle(), 
    anonymous = FALSE, 
    opts = .getCache("curlOpts")
)
{
  ## constants
  kValidMethods <- c("POST", "PUT", "DELETE")
  ## end constants

  if(is.null(uri))
    stop("uri cannot be null")

  if (service=="REPO") {
    path <- .getRepoEndpointPrefix()
  } else if (service=="AUTH") {
    path <- .getAuthEndpointPrefix()
  } else if (service=="FILE") {
    path <- .getFileEndpointPrefix()
  } else {
    stop(sprintf("Unexpected service: %s.", service))
  }
  
  if(is.null(path))
    stop("path cannot be null")

  
  if(!(requestMethod %in% kValidMethods)){
    stop("invalid request method")
  }
  
  if(!is.character(uri)){
    stop("a uri must be supplied of R type character")
  }
  
  if (missing(entity) || is.null(entity)) {
	  stop("no content for post/put")
  }
  
  if(!is.list(entity)) {
    stop("an entity must be supplied of R type list")
  }
  
  if((any(names(entity) == "") || is.null(names(entity))) && length(entity) > 0){
    stop("all entity elements must be named")
  }
  
  if(length(entity) == 0)
    entity <- emptyNamedList
  
  ## change dates to characters
  indx <- grep("date", tolower(names(entity)))
  indx <- indx[as.character(entity[indx]) != "NULL"]
  indx <- indx[names(entity)[indx] != "dateAnnotations"]
  for(ii in indx)
    entity[ii] <- as.character(entity[ii])
  
  
  ## convert integers to characters
  if(length(entity) > 0){
    for(ii in 1:length(entity)){
      if(all(checkInteger(entity[[ii]])))
        entity[[ii]] <- as.character(as.integer(entity[[ii]]))
    }
  }
  
  httpBody <- toJSON(entity)
  
  ## uris formed by the service already have their servlet prefix
  pathIndex<-regexpr(path, uri, fixed=T)
  if(pathIndex>0) {
    uri<-substr(uri, pathIndex+nchar(path), nchar(uri))
  }
  
  
  
  ## Prepare the header. If not an anonymous request, stuff the
  ## sessionToken into the header
  header <- .getCache("curlHeader")
  if(is.null(anonymous) || !anonymous) {
    header <- switch(authMode(),
      auth = .stuffHeaderAuth(header),
      hmac = .stuffHeaderHmac(header, , paste(path, uri, sep="")),
      stop("Unknown auth mode: %s. Could not build header", authMode())
    )		
  }
  if("PUT" == requestMethod) {
    # Add the ETag header
    header <- c(header, ETag = entity$etag)
  }
  
  if(length(path) > 1)
    stop("put", paste(length(path), path))
  ## Submit request and check response code
  d = debugGatherer()
  
  ##curlSetOpt(opts,curl=curlHandle)
  
  if(!is.null(.getCache("debug")) && .getCache("debug")) {
    message("----------------------------------")
    message("REQUEST: ", requestMethod, " ", uri)
    message("REQUEST_BODY: ", httpBody)
  }
  
  # check own version, stopping if blacklisted
  checkBlackList()
  
  response<-synapseRequestFollowingAllRedirects(
    uri,
    service,
    postfields = httpBody,
    customrequest = requestMethod,
    httpheader = header,
    curl = curlHandle,
    debugfunction=d$update,
    .opts=opts
  )
  
  if(!is.null(.getCache("debug")) && .getCache("debug")) {
    message("RESPONSE_BODY:: ", response)
  }
  
  .checkCurlResponse(curlHandle, response)
  
  ## Parse response and prepare return value
  tryCatch(
    as.list(fromJSON(response)),
    error = function(e){NULL}
  )
}
