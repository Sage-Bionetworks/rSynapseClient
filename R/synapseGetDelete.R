.synapseGetDelete <- 
		function(uri, requestMethod, host = .getRepoEndpointLocation(), curlHandle=getCurlHandle(), 
				anonymous = .getCache("anonymous"), path = .getRepoEndpointPrefix(), opts = .getCache("curlOpts"), entity=NULL)
{

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
	if(grepl(path, uri)) {
		uri <- paste(host, uri, sep="")
	}else {
		uri <- paste(host, path, uri, sep="")
	}
	
	## Add the provenance parameter, if applicable
	step <- .getCache("currentStep")
	if(!is.null(step)) {
		if(grepl("?", uri, fixed=TRUE)) {
			uri <- paste(uri, "&stepId=", propertyValue(step, "id"), sep="")
		} else {
			uri <- paste(uri, "?stepId=", propertyValue(step, "id"), sep="")			
		}
	}
	
	if(length(path) > 1)
		stop("put", paste(length(path), path))
	## Prepare the header. If not an anonymous request, stuff the
	## sessionToken into the header
	header <- .getCache("curlHeader")
	if(!anonymous) {
		header <- switch(authMode(),
						auth = .stuffHeaderAuth(header),
						hmac = .stuffHeaderHmac(header, uri),
						stop("Unknown auth mode: %s. Could not build header", authMode())
		)		
	}
	
	## Submit request and check response code
	d = debugGatherer()

	if(.getCache("debug")) {
		message("----------------------------------")
		message("REQUEST: ", requestMethod, " ", uri)
	}
	
	##curlSetOpt(opts,curl=curlHandle)
	if(is.null(entity)){
		response <- getURL(uri, 
				customrequest = requestMethod, 
				httpheader = header, 
				curl = curlHandle, 
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
		response <- getURL(uri, 
				postfields = httpBody, 
				customrequest = requestMethod, 
				httpheader = header, 
				curl = curlHandle, 
				debugfunction=d$update,
				.opts=opts
		)
	}
	
	if(.getCache("debug")) {
		message("RESPONSE_BODY: ", response)
	}
	
	.checkCurlResponse(curlHandle, response)
	
	if("GET" == requestMethod) {
		## Parse response and prepare return value
		as.list(fromJSON(response))
	}
}

