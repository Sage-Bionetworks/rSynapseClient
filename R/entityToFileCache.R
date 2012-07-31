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
	content <- .downloadToFile(.getEntityUri(id, version), entityCachePath, .getCache("ENTITY_FILE_NAME"))
	SynapseEntity(content)
}

# This function downloads the annotations of a Synapse entity into the local file cache:
# - downloads the entity's annotations, into a local file
#   <cache>/entity/<id>/annotations.json, or <cache>/entity/<id>/<version>/annotations.json
#
getAnnotationsFromSynapse <- function(id, version=NULL)
{
	entityCachePath <- .entityFileCachePath(id, version)
	content <- .downloadToFile(.getAnnotationsUri(id, version), entityCachePath, .getCache("ANNOTATIONS_FILE_NAME"))
	SynapseAnnotations(content)
}

#
# Returns a SynapseEntity for the given entity ID (and optional version),
# retrieved from the local file cache
#
getEntityFromFileCache<- function(id, version=NULL) {
  entityCachePath <- .getAbsoluteFileCachePath(.entityFileCachePath(id, version))
  getEntityInstance(as.list(fromJSON(paste(entityCachePath, .getCache("ENTITY_FILE_NAME"), sep="/"), simplifyWithNames=FALSE)))
}

#
# Returns a SynapseAnnotations object for the annotations of a given entity ID (and optional version),
# retrieved from the local file cache
#
getAnnotationsFromFileCache<- function(id, version=NULL) {
	entityCachePath <- .getAbsoluteFileCachePath(.entityFileCachePath(id, version))
	SynapseAnnotations(as.list(fromJSON(paste(entityCachePath, .getCache("ANNOTATIONS_FILE_NAME"), sep="/"), simplifyWithNames=FALSE)))
}

# note 'filePath' omits the cache root, which is prepended by this function
.downloadToFile <- function(uri, filePath, fileName) {
	content <- .synapseGetDelete(uri, "GET")
	if (is.null(content)) stop(paste("No result for GET", uri))
	.writeToFile(gsub("[\r\n]", "", toJSON(content)), filePath, fileName)
	content
}

# note 'filePath' omits the cache root, which is prepended by this function
.writeToFile<-function(content, filePath, fileName) {
	folder <- .getAbsoluteFileCachePath(filePath)

	if (!file.exists(file=folder)) dir.create(folder, recursive=TRUE)
	targetFileName<-paste(folder, fileName, sep="/")
	targetFile<-file(targetFileName)
	if (is.null(content)) stop("No content to write.")
	writeLines(content, targetFile)
	close(targetFile)
	targetFileName
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
	content <- as.list(fromJSON(readLines(filePath), simplifyWithNames=FALSE))
	getEntityInstance(.synapsePostPut(uri, content, "PUT"))
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
  annotations <- as.list(fromJSON(readLines(paste(.getAbsoluteFileCachePath(.entityFileCachePath(id, version)),
          .getCache("ANNOTATIONS_FILE_NAME"), sep="/")), simplifyWithNames=FALSE))

  ## make sure all scalars are converted to lists since the service expects all
  ## annotation values to be arrays instead of scalars
  for(key in names(annotations)){
    ## This is one of our annotation buckets
    if(is.list(annotations[[key]])) {
      for(annotKey in names(annotations[[key]])) {
        if(is.scalar(annotations[[key]][[annotKey]])) {
          annotations[[key]][[annotKey]] <- list(annotations[[key]][[annotKey]])
        }
      }
    }
  }

  SynapseAnnotations(.synapsePostPut(.getAnnotationsUri(id, version),
			annotations, "PUT"))
}


deleteAnnotationsFromFileCache<-function(id, version=NULL) {
	file <- paste(.getAbsoluteFileCachePath(.entityFileCachePath(id, version)),
      .getCache("ANNOTATIONS_FILE_NAME"), sep="/")
	if (file.exists(file)) unlink(file)
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
#
# The steps for updating an entity and its annotations are as follows:
# 1. We start with a local copy of, which has an etag
# 2. Push the Entity part (the properties) to Synapse.
#	 This will either return an updated entity with a new ('intermediate')
#	 etag or throw a ConcurrentUpdateException.
#	 If no error we know that NEITHER the entity NOR its annotations have
#	 been updated since we originally fetched them.  Now:
# 3. Put the new etag in the local copy of the annotations and update in Synapse
#    This will either return an exception or a new annotations object with new etag
# 4. Combine the properties returned in 2, the annotations and etag returned in 3,
#	 into a new object to return from this method.
#    Note:  When the 'dust settles', if successful, the returned object
#	 will have had its etag incremented by _2_.
#
#
updateSynapseEntity<-function(synapseEntity) {
	id<-propertyValue(synapseEntity, "id")
	if (is.null(id)) stop ("entity id is required")

	updateEntityInFileCache(synapseEntity)
	newEntity <- updateEntityInSynapse(id) # note, this omits annotations and has the 'intermediate' etag

	annots <- synapseEntity@annotations
	propertyValue(annots, "etag")<-propertyValue(newEntity, "etag")
	propertyValue(annots, "id")<-propertyValue(newEntity, "id")
	updateAnnotationsInFileCache(annots)
	newAnnots <- updateAnnotationsInSynapse(id)
	propertyValue(newAnnots, "id")<-propertyValue(newEntity, "id")
	updateAnnotationsInFileCache(newAnnots)

	propertyValue(newEntity, "etag") <- propertyValue(newAnnots, "etag")
	newEntity@annotations <- newAnnots
	updateEntityInFileCache(newEntity)

	newEntity

#	# now download the annotations to the local cache
#	getAnnotationsFromSynapse(id)
#	# read annotations from local cache into memory
#	annots <- getAnnotationsFromFileCache(id)
#
#    # merge annotations from input variable into 'annots'
#    # TODO this logic *merges* only.  Need to allow local copy to *delete* annotations
#    for (n in annotationNames(synapseEntity)) {
#      annotValue(annots, n)<-annotValue(synapseEntity, n)
#    }
#
#    ## make sure all scalars are converted to lists since the service expects all
#    ## annotation values to be arrays instead of scalars
#    for(key in names(annots)){
#      ## This is one of our annotation buckets
#      if(is.list(annots[[key]])) {
#        for(annotKey in names(annots[[key]])) {
#          if(is.scalar(annotations[[key]][[annotKey]])) {
#            annots[[key]][[annotKey]] <- list(annots[[key]][[annotKey]])
#          }
#        }
#      }
#    }
#
#	# now persist to file cache and to Synapse
#	updateAnnotationsInFileCache(annots)
#	updateAnnotationsInSynapse(id)
#
#	getSynapseEntity(id)
}

# deletes the given SynapseEntity from the local cache and from Synapse
deleteSynapseEntity<-function(id) {
	deleteAnnotationsFromFileCache(id)
	deleteEntityFromFileCache(id)
	deleteEntityFromSynapse(id)
}
