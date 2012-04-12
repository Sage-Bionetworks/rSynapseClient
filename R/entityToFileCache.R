#
#
# This function downloads a Synapse entity into the local file cache:
# - downloads entity itself, as JSON, and creates a local file, 
#   <cache>/entity/<id>/entity.json, or <cache>/entity/<id>/<version>/entity.json
# - downloads the entity's annotations, into a local file
#   <cache>/entity/<id>/annotations.json, or <cache>/entity/<id>/<version>/annotations.json
#
synapseEntityToFileCache <- function(id, version=NULL)
{
	repoEndpointMethod <- .getRepoEndpointProtocol()
	
	entityCachePath <- .entityFileCachePath(id, version)
	entityUrl = paste(repoEndpointMethod, "://", entityCachePath, sep="")
	
	.downloadToFile(entityUrl, entityCachePath, "entity.json")
	
	annotationsUrl = paste(entityUrl, "/annotations", sep="")
	
	.downloadToFile(annotationsUrl, entityCachePath, "annotations.json")
}

#
# Returns a JSON object for the given entity ID (and optional version), 
# retrieved from the local file cache
#
getEntityFromFileCache<- function(id, version=NULL) {
	entityCachePath <- .getAbsoluteFileCachePath(.entityFileCachePath(id, version))
	content <- readFile(paste(entityCachePath, "entity.json", sep="/"))
	toJSON(content)
}

#
# Returns a JSON object for the annotations of a given entity ID (and optional version), 
# retrieved from the local file cache
#
getEntityAnnotationsFromFileCache<- function(id, version=NULL) {
	entityCachePath <- .getAbsoluteFileCachePath(.entityFileCachePath(id, version))
	content <- readFile(paste(entityCachePath, "annotations.json", sep="/"))
	toJSON(content)
}

# note 'filePath' omits the cache root, which is prepended by this function
.downloadToFile <- function(url, filePath, fileName) {
	requestMethod <- "GET"
	curlHandle <- getCurlHandle()
	anonymous <- .getCache("anonymous")
	opts <- .getCache("curlOpts")
	
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
	
	content <- getURL(url, 
			customrequest = requestMethod, 
			httpheader = header, 
			curl = curlHandle, 
			debugfunction=NULL,
			.opts=opts
	)
	.checkCurlResponse(curlHandle, content)
	
	
	
	folder <- .getAbsoluteFileCachePath(filePath)
	
	if (!file.exists(file=folder)) dir.create(folder, recursive=TRUE)
	targetFile<-file(paste(folder, fileName, sep="/"))
	writeLines(content, targetFile)
	close(targetFile)
	
}

.getCacheRoot<-function() {
	cacheRoot <- .getCache("cacheRoot")
	if (is.null(cacheRoot)) cacheRoot <- "~/.synapseCache"
	cacheRoot
}

.getAbsoluteFileCachePath<-function(relativeFileCachePath) {
	paste(.getCacheRoot(), relativeFileCachePath, sep="/")
}

.entityFileCachePath<- function(id, version=NULL) {
	uri <- paste("/entity/", id, sep="")
	if (!is.null(version)) uri <- paste(uri, "/version/", version, sep="")
	host<- .getRepoEndpointHost()
	uriPrefix <- .getRepoEndpointPrefix()
	paste(host, uriPrefix, uri, sep="")
}

readFile<-function(fileName) {
	if (!file.exists(fileName)) stop(paste("file", fileName, "doen't exist."))
	sourceFile<-file(fileName)
	content<-readLines(sourceFile)
	close(sourceFile)
	content
}

