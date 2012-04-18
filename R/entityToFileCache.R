

# This function downloads a Synapse entity into the local file cache:
# - downloads entity itself, as JSON, and creates a local file, 
#   <cache>/entity/<id>/entity.json, or <cache>/entity/<id>/<version>/entity.json
#
getEntityFromSynapse <- function(id, version=NULL)
{
	entityCachePath <- .entityFileCachePath(id, version)
	.downloadToFile(.getEntityUri(id, version), entityCachePath, ENTITY_FILE_NAME)
}

# This function downloads the annotations of a Synapse entity into the local file cache:
# - downloads the entity's annotations, into a local file
#   <cache>/entity/<id>/annotations.json, or <cache>/entity/<id>/<version>/annotations.json
#
getAnnotationsFromSynapse <- function(id, version=NULL)
{
	entityCachePath <- .entityFileCachePath(id, version)
	.downloadToFile(.getAnnotationsUri(id, version), entityCachePath, ANNOTATIONS_FILE_NAME)
}

#
# Returns a SynapseEntity for the given entity ID (and optional version), 
# retrieved from the local file cache
#
getEntityFromFileCache<- function(id, version=NULL) {
	entityCachePath <- .getAbsoluteFileCachePath(.entityFileCachePath(id, version))
	SynapseEntity(as.list(fromJSON(paste(entityCachePath, ENTITY_FILE_NAME, sep="/"))))
}

#
# Returns a SynapseAnnotations object for the annotations of a given entity ID (and optional version), 
# retrieved from the local file cache
#
getAnnotationsFromFileCache<- function(id, version=NULL) {
	entityCachePath <- .getAbsoluteFileCachePath(.entityFileCachePath(id, version))
	SynapseAnnotations(as.list(fromJSON(paste(entityCachePath, ANNOTATIONS_FILE_NAME, sep="/"))))
}

# note 'filePath' omits the cache root, which is prepended by this function
.downloadToFile <- function(uri, filePath, fileName) {
	content <- .synapseGetDelete(uri, "GET")
	if (is.null(content)) stop(paste("No result for GET", uri))
	.writeToFile(gsub("[\r\n]", "", toJSON(content)), filePath, fileName)
}

# note 'filePath' omits the cache root, which is prepended by this function
.writeToFile<-function(content, filePath, fileName) {
	folder <- .getAbsoluteFileCachePath(filePath)
	
	if (!file.exists(file=folder)) dir.create(folder, recursive=TRUE)
	targetFile<-file(paste(folder, fileName, sep="/"))
	if (is.null(content)) stop("No content to write.")
	writeLines(content, targetFile)
	close(targetFile)
	
}

.getCacheRoot<-function() {
	synapseCacheDir()
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

.getEntityUri<-function(id, version=NULL) {
	uri <- paste("/entity/", id, sep="")
	if (!is.null(version)) uri <- paste(uri, "/version/", version, sep="")
	uri
}

.getAnnotationsUri<-function(id, version=NULL) {
	uri <- paste("/entity/", id, sep="")
	if (!is.null(version)) uri <- paste(uri, "/version/", version, sep="")
	paste(uri, "/annotations", sep="")
}

# maps an Entity to JSON and POST to Synapse
# Note: 'entity' must be a SimplePropertyOwner
# TODO support creating a new *version* of an existing entity
createEntity<-function(entity) {
	# translate entity to list, then serialize to String
	content <- as.list.SimplePropertyOwner(entity)
	SynapseEntity(.synapsePostPut("/entity", content, "POST"))
}

# Note: 'entity' must be a SimplePropertyOwner
# TODO support updating a specific version of an entity
updateEntityInFileCache<-function(entity) {
	entityId <- entity@properties[["id"]]
	if (is.null(entityId)) stop("Missing entity id")
	content <- toJSON(as.list.SimplePropertyOwner(entity))
	entityCachePath <- .entityFileCachePath(entityId, NULL)
	.writeToFile(gsub("[\r\n]", "", content), entityCachePath, ENTITY_FILE_NAME)
}

updateEntityInSynapse<-function(id, version=NULL) {
	uri <- .getEntityUri(id, version)
	filePath <- paste(.getAbsoluteFileCachePath(.entityFileCachePath(id, version)), 
			ENTITY_FILE_NAME, sep="/")
	content <- as.list(fromJSON(readLines(filePath)))
	.synapsePostPut(uri, content, "PUT")
}

deleteEntityFromSynapse<-function(id, version=NULL) {
	.synapseGetDelete(.getEntityUri(id, version), "DELETE")
}

deleteEntityFromFileCache<-function(id, version=NULL) {
	file <- paste(.getAbsoluteFileCachePath(.entityFileCachePath(id, version)), 
			ENTITY_FILE_NAME, sep="/")
	if (file.exists(file)) unlink(file)
}
		
# Note: 'annots' must be a SynapseAnnotations
# TODO support updating a specific version of an Annot
updateAnnotationsInFileCache<-function(annots) {
	entityId <- propertyValue(annots, "id")
	if (is.null(entityId)) stop("Missing entity id")
	
	content <- toJSON(as.list(annots))
	entityCachePath <- .entityFileCachePath(entityId, NULL)
	.writeToFile(gsub("[\r\n]", "", content), entityCachePath, ANNOTATIONS_FILE_NAME)
}

updateAnnotationsInSynapse<-function(id, version=NULL) {
	.synapsePostPut(.getAnnotationsUri(id, version), 
			as.list(fromJSON(readLines(paste(.getAbsoluteFileCachePath(.entityFileCachePath(id, version)), 
							ANNOTATIONS_FILE_NAME, sep="/")))), "PUT")
}

deleteAnnotationsFromFileCache<-function(id, version=NULL) {
	.synapseGetDelete(.getAnnotationsUri(id, version), "DELETE")
}

deleteAnnotationsFromSynapse<-function(id, version=NULL) {
	file <- paste(.getAbsoluteFileCachePath(.entityFileCachePath(id, version)), 
			ANNOTATIONS_FILE_NAME, sep="/")
	if (file.exists(file)) unlink(file)
}
