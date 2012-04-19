## TODO: add description
## 
## Author: Bruce Hoff <bruce.hoff@sagebase.org
###############################################################################


createSynapseEntity<- function(synapseEntity) {
  # create the entity in Synapse and get back the id
  id <- propertyValue(doCreateEntity(synapseEntity), "id")
  # now download the annotations to the local cache
  getAnnotationsFromSynapse(id)
  # read annotations from local cache into memory
  annots <- getAnnotationsFromFileCache(id)
  # merge annotations from input variable into 'annots'
  for (n in annotationNames(synapseEntity)) {
    annotValue(annots, n)<-annotValue(synapseEntity, n)
  }
  # now persist to file cache and to Synapse
  updateAnnotationsInFileCache(annots)
  updateAnnotationsInSynapse(id)
  
  # at this point the entity and its annotations are created
  # it remains to ensure the file cache is up-to-date
  # (in particular the 'update' will increment the etag) and
  # to construct the latest version of the object
  getSynapseEntity(id)
}



# This function downloads a Synapse entity into the local file cache:
# - downloads entity itself, as JSON, and creates a local file, 
#   <cache>/entity/<id>/entity.json, or <cache>/entity/<id>/<version>/entity.json
#
getEntityFromSynapse <- function(id, version=NULL)
{
	entityCachePath <- .entityFileCachePath(id, version)
	.downloadToFile(.getEntityUri(id, version), entityCachePath, .getCache("ENTITY_FILE_NAME"))
}

# This function downloads the annotations of a Synapse entity into the local file cache:
# - downloads the entity's annotations, into a local file
#   <cache>/entity/<id>/annotations.json, or <cache>/entity/<id>/<version>/annotations.json
#
getAnnotationsFromSynapse <- function(id, version=NULL)
{
	entityCachePath <- .entityFileCachePath(id, version)
	.downloadToFile(.getAnnotationsUri(id, version), entityCachePath, .getCache("ANNOTATIONS_FILE_NAME"))
}

#
# Returns a SynapseEntity for the given entity ID (and optional version), 
# retrieved from the local file cache
#
getEntityFromFileCache<- function(id, version=NULL) {
	entityCachePath <- .getAbsoluteFileCachePath(.entityFileCachePath(id, version))

	#SynapseEntity(as.list(fromJSON(paste(entityCachePath, .getCache("ENTITY_FILE_NAME"), sep="/"))))

    getEntityInstance(as.list(fromJSON(paste(entityCachePath, .getCache("ENTITY_FILE_NAME"), sep="/"))))

}

#
# Returns a SynapseAnnotations object for the annotations of a given entity ID (and optional version), 
# retrieved from the local file cache
#
getAnnotationsFromFileCache<- function(id, version=NULL) {
	entityCachePath <- .getAbsoluteFileCachePath(.entityFileCachePath(id, version))
	SynapseAnnotations(as.list(fromJSON(paste(entityCachePath, .getCache("ANNOTATIONS_FILE_NAME"), sep="/"))))
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
	targetFile
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
doCreateEntity <- function(entity) {
	# translate entity to list, then serialize to String
	content <- as.list.SimplePropertyOwner(entity)
	getEntityInstance(.synapsePostPut("/entity", content, "POST"))
}

# Note: 'entity' must be a SimplePropertyOwner
# TODO support updating a specific version of an entity
updateEntityInFileCache<-function(entity) {
	entityId <- entity@properties[["id"]]
	if (is.null(entityId)) stop("Missing entity id")
	content <- toJSON(as.list.SimplePropertyOwner(entity))
	entityCachePath <- .entityFileCachePath(entityId, NULL)
	.writeToFile(gsub("[\r\n]", "", content), entityCachePath, .getCache("ENTITY_FILE_NAME"))
}

updateEntityInSynapse<-function(id, version=NULL) {
	uri <- .getEntityUri(id, version)
	filePath <- paste(.getAbsoluteFileCachePath(.entityFileCachePath(id, version)), 
      .getCache("ENTITY_FILE_NAME"), sep="/")
	content <- as.list(fromJSON(readLines(filePath)))
	.synapsePostPut(uri, content, "PUT")
}

deleteEntityFromSynapse<-function(id, version=NULL) {
	.synapseGetDelete(.getEntityUri(id, version), "DELETE")
}

deleteEntityFromFileCache<-function(id, version=NULL) {
	file <- paste(.getAbsoluteFileCachePath(.entityFileCachePath(id, version)), 
      .getCache("ENTITY_FILE_NAME"), sep="/")
	if (file.exists(file)) unlink(file)
}
		
# Note: 'annots' must be a SynapseAnnotations
# TODO support updating a specific version of an Annot
updateAnnotationsInFileCache<-function(annots) {
	entityId <- propertyValue(annots, "id")
	if (is.null(entityId)) stop("Missing entity id")
	
	content <- toJSON(as.list(annots))
	entityCachePath <- .entityFileCachePath(entityId, NULL)
	.writeToFile(gsub("[\r\n]", "", content), entityCachePath, .getCache("ANNOTATIONS_FILE_NAME"))
}

updateAnnotationsInSynapse<-function(id, version=NULL) {
	.synapsePostPut(.getAnnotationsUri(id, version), 
			as.list(fromJSON(readLines(paste(.getAbsoluteFileCachePath(.entityFileCachePath(id, version)), 
                      .getCache("ANNOTATIONS_FILE_NAME"), sep="/")))), "PUT")
}


deleteAnnotationsFromFileCache<-function(id, version=NULL) {
	file <- paste(.getAbsoluteFileCachePath(.entityFileCachePath(id, version)), 
      .getCache("ANNOTATIONS_FILE_NAME"), sep="/")
	if (file.exists(file)) unlink(file)
}

# NOTE:  This method is not used
# 
# localSynapseEntity -- a copy of the SynapseEntity with local modifications
# remoteSynapseEntity -- a recently retrieved copy of the SynapseEntity
# returns the merged entity, i.e. a copy of the remoteEntity (especially having the
# remote entity's e-tag) with the local entity's data superimposed)
#
# Note:  In this initial implementation no sophisticated merging is attempted
# Rather the local values simply replace the remote values
#
# NOTE:  This method is not used
#
mergeSynapseEntities<- function(localSynapseEntity, remoteSynapseEntity) {
	if (is.null(propertyValue(remoteSynapseEntity, "id"))) stop("entity id is required")
	
	# first 'clone' the remote entity
	result<-new("SynapseEntity")
	for (n in propertyNames(remoteSynapseEntity)) {
		propertyValue(result, n)<-propertyValue(remoteSynapseEntity, n)
	}
	for (n in annotationNames(remoteSynapseEntity)) {
		annotValue(result, n)<-annotValue(remoteSynapseEntity, n)
	}
	result@synapseEntityKind <- remoteSynapseEntity@synapseEntityKind
	result@synapseWebUrl<-remoteSynapseEntity@synapseWebUrl
	
	# now copy the local properties (except id and etag)
	for (n in propertyNames(localSynapseEntity)) {
		if (n=="id" || n=="etag") continue
		propertyValue(result, n)<-propertyValue(localSynapseEntity, n)
	}
	for (n in annotationNames(localSynapseEntity)) {
		annotValue(result, n)<-annotValue(localSynapseEntity, n)
	}
	
	result
}

# takes a new SynapseEntity, not yet persisted, and sends it to Synapse 
# along with its annotations.  Then retreives the entity along with its
# annotations to the local cache and finally returns the complete SynapseEntity
# object with id and annotations
createSynapseEntity<- function(synapseEntity) {
	# create the entity in Synapse and get back the id
	id <- propertyValue(doCreateEntity(synapseEntity), "id")
	# now download the annotations to the local cache
	getAnnotationsFromSynapse(id)
	# read annotations from local cache into memory
	annots <- getAnnotationsFromFileCache(id)
	# merge annotations from input variable into 'annots'
	for (n in annotationNames(synapseEntity)) {
		annotValue(annots, n)<-annotValue(synapseEntity, n)
	}
	# now persist to file cache and to Synapse
	updateAnnotationsInFileCache(annots)
	updateAnnotationsInSynapse(id)
	
	# at this point the entity and its annotations are created
	# it remains to ensure the file cache is up-to-date
	# (in particular the 'update' will increment the etag) and
	# to construct the latest version of the object
	getSynapseEntity(id)
}

# given an entity id, retreives the entity and its annotations into
# the local file cache, then returns the object as a SynapseEntity
getSynapseEntity<- function(id) {
	getEntityFromSynapse(id)
	entity<-getEntityFromFileCache(id)
	getAnnotationsFromSynapse(id)
	annots<-getAnnotationsFromFileCache(id)
	
	entity@annotations<-annots
	
	entity
}

# updates the content of the given SynapseEntity in the local file
# cache and in Synapse.   Returns the updated entity
updateSynapseEntity<-function(synapseEntity) {
	id<-propertyValue(synapseEntity, "id")
	if (is.null(id)) stop ("entity id is required")

	updateEntityInFileCache(synapseEntity)
	updateEntityInSynapse(id)
	
	# now download the annotations to the local cache
	getAnnotationsFromSynapse(id)
	# read annotations from local cache into memory
	annots <- getAnnotationsFromFileCache(id)
	# merge annotations from input variable into 'annots'
	# TODO this logic *merges* only.  Need to allow local copy to *delete* annotations
	for (n in annotationNames(synapseEntity)) {
		annotValue(annots, n)<-annotValue(synapseEntity, n)
	}
	# now persist to file cache and to Synapse
	updateAnnotationsInFileCache(annots)
	updateAnnotationsInSynapse(id)
	
	getSynapseEntity(id)
}

# deletes the given SynapseEntity from the local cache and from Synapse
deleteSynapseEntity<-function(id) {
	deleteAnnotationsFromFileCache(id)
	deleteEntityFromFileCache(id)
	deleteEntityFromSynapse(id)
}
