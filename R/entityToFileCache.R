#
#
# This function downloads a Synapse entity into the local file cache:
# - downloads entity itself, as JSON, and creates a local file, 
#   <cache>/entity/<id>/entity.json, or <cache>/entity/<id>/<version>/entity.json
# - downloads the entity's annotations, into a local file
#   <cache>/entity/<id>/annotations.json, or <cache>/entity/<id>/<version>/annotations.json
#
entityToFileCache <- function(id, version=NULL)
{

	uri <- paste("/entity/", id, sep="")
	if (!is.null(version)) uri <- paste(uri, "/version/", version, sep="")
	requestMethod <- "GET"
	host <- .getRepoEndpointLocation()
	curlHandle <- getCurlHandle()
	anonymous <- .getCache("anonymous")
	path <- .getRepoEndpointPrefix()
	opts <- .getCache("curlOpts")

	## uris formed by the service already have their servlet prefix
	if(grepl(path, uri)) {
		uri <- paste(host, uri, sep="")
	}else {
		uri <- paste(host, path, uri, sep="")
	}
	
	
	## Prepare the header. If not an anonymous request, stuff the
	## sessionToken into the header
	header <- .getCache("curlHeader")
	if(!is.null(anonymous) && !anonymous) {
		header <- switch(authMode(),
				auth = .stuffHeaderAuth(header),
				hmac = .stuffHeaderHmac(header, uri),
				stop("Unknown auth mode: %s. Could not build header", authMode())
		)		
	}
	
	
	entityString <- getURL(uri, 
			customrequest = requestMethod, 
			httpheader = header, 
			curl = curlHandle, 
			debugfunction=NULL,
			.opts=opts
	)
	.checkCurlResponse(curlHandle, entityString)
	
	cacheRoot <- .getCache("cacheRoot")
	if (is.null(cacheRoot)) cacheRoot <- "~/.synapseCache"

	if (is.null(version)) {
		entityFileFolder <- paste(cacheRoot, "entity", id,  sep="/")
	} else {
		entityFileFolder <- paste(cacheRoot, "entity", id, version, sep="/")
	}
	if (!file.exists(file=entityFileFolder)) dir.create(entityFileFolder, recursive=TRUE)
	entityFile<-file(paste(entityFileFolder, "entity.json", sep="/"))
	writeLines(entityString, entityFile)
	close(entityFile)
	
	annotationsString <- getURL(paste(uri, "annotations", sep="/"), 
			customrequest = requestMethod, 
			httpheader = header, 
			curl = curlHandle, 
			debugfunction=NULL,
			.opts=opts
	)
	.checkCurlResponse(curlHandle, annotationsString)
	
	
	annotationsFile<-file(paste(entityFileFolder, "annotations.json", sep="/"))
	writeLines(annotationsString, annotationsFile)
	close(annotationsFile)
}