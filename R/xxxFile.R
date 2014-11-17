#
# class and method definitions for File
#
# This error was fixed by renaming "File.R" to "FileX.R"
#  Error in function (classes, fdef, mtable)  : 
#    unable to find an inherited method for function getFileCache for signature "character", "character", "FileCacheFactory"
# This error was fixed by renaming "FileX.R" to "xxxFile.R"
# Error in `$<-`(`*tmp*`, "entityType", value = "org.sagebionetworks.repo.model.FileEntity") : 
#  no method for assigning subsets of this S4 class

initializeProperties<-function(synapseType, hasConcreteType) {
  propertyTypes<-getEffectivePropertyTypes(synapseType)
  properties<-SynapseProperties(propertyTypes)
  if (hasConcreteType) properties$concreteType <- synapseType
  properties
}

setClassUnion("environmentOrNull", c("environment", "NULL"))


setClass(
  Class = "File",
  contains = "Entity",
  representation = representation(
    # fields:
    # filePath: full path to local file. Before an "external" file is created in Synapse, this is the external URL
    filePath = "character",
    # synapseStore: logical T if file is stored in Synapse, F if only the url is stored
    synapseStore = "logical",
    # uploadHost: character(0) to use default value for parent container
    uploadHost = "character",
    # fileHandle (generated from JSON schema, empty before entity is created)
    fileHandle = "list",
    # objects to be serialized/deserialized
    objects = "environmentOrNull"
  ),
  # This is modeled after defineEntityClass in AAAschema
  prototype = prototype(
    synapseEntityKind = "File",
    properties = initializeProperties("org.sagebionetworks.repo.model.FileEntity", TRUE),
    synapseStore = TRUE,
    uploadHost=character(0),
    objects = NULL
  )
)

getFileLocation <- function(file) {file@filePath}

synAnnotSetMethod<-function(object, which, value) {
  if(any(which==propertyNames(object))) {
    propertyValue(object, which)<-value
  } else {
    annotValue(object, which)<-value
  }
  object
}

setMethod(
  f = "synAnnot<-",
  signature = signature("Entity", "character", "ANY"),
  definition = function(object, which, value){
    synAnnotSetMethod(object, which, value)
  }
)

synAnnotGetMethod<-function(object, which) {
  if(any(which==propertyNames(object))) {
    propertyValue(object, which)
  } else {
    annotValue(object, which)
  }
}

setMethod(
  f = "synAnnot",
  signature = signature("Entity", "character"),
  definition = function(object, which) {
    synAnnotGetMethod(object, which)
  }
)

## File contructor: path="/path/to/file", synapseStore=T, name="foo", ...
File<-function(path, synapseStore=T, uploadHost=NULL, ...) {
    file <- new("File")
    if (is.null(list(...)$parentId)) {
        stop("parentId is required.")
    }
    if (missing(path)) {
        entityParams<-list(...)
    } else {
        possibleName <- .ParsedUrl(path.expand(path))@file
        entityParams<-modifyList(list(name=possibleName), list(...))
        file@filePath <- path
        if (!mockable.file.exists(file@filePath) && synapseStore) {
            stop(sprintf("'synapseStore' may not be true when %s does not exist.", file@filePath))
        }
    }
    if (is.null(uploadHost)) {
      file@uploadHost<-character(0)
    } else {
      file@uploadHost<-uploadHost
    }
    for (key in names(entityParams)) file<-synAnnotSetMethod(file, key, entityParams[[key]])
    file@synapseStore <- synapseStore
    file
}

mockable.file.exists <- function (filepath) {
    base::file.exists(filepath)
}

# this is the required constructor for a metadata Entity, taking a list of properties
# the logic is based on definedEntityConstructors in AAAschema
setMethod(
    f = "createFileFromProperties",
    signature = signature("list"),
    definition = function(propertiesList) {
        file <- new("File")
        for (prop in names(propertiesList))
          file<-synAnnotSetMethod(file, prop, propertiesList[[prop]])
        
        propertyValue(file, "concreteType") <- "org.sagebionetworks.repo.model.FileEntity"
        
        file
      }
)

isExternalFileHandle<-function(fileHandle) {length(fileHandle)>0 && fileHandle$concreteType=="org.sagebionetworks.repo.model.file.ExternalFileHandle"}
fileHasFileHandleId<-function(file) {!is.null(file@fileHandle$id)}
fileHasFilePath<-function(file) {length(file@filePath)>0 && nchar(file@filePath)>0}

# ensure that fileHandle info is consistent with synapseUpload field
validateFile<-function(file) {
  if (!fileHasFileHandleId(file)) return
  isExternal <- isExternalFileHandle(file@fileHandle)
  if (isExternal==FALSE & file@synapseStore==FALSE) stop("synapseStore=F but file is not external")
}

# returns the last modified timestamp or NA if the file does not exist
lastModifiedTimestampNonexistentOK<-function(filePath) {
  file.info(filePath)$mtime
}

lastModifiedTimestamp<-function(filePath) {
  if (!file.exists(filePath)) stop(sprintf("%s does not exist.", filePath))
  lastModifiedTimestampNonexistentOK(filePath)
}

fanoutDir<-function(fileHandleId) {
  fileHandleIdAsNumber<-as.numeric(fileHandleId)
  if (is.na(fileHandleIdAsNumber)) stop(sprintf("Expected number for fileHandleId but found %s", fileHandleId))
  fileHandleIdAsNumber %% 1000
}

defaultDownloadLocation<-function(fileHandleId) {
  file.path(synapseCacheDir(), fanoutDir(fileHandleId), fileHandleId)
}

cacheMapFilePath<-function(fileHandleId) {
  file.path(defaultDownloadLocation(fileHandleId), ".cacheMap")
}

getCacheMapFileContent<-function(fileHandleId) {
  cacheMapFile<-cacheMapFilePath(fileHandleId)
  if (!file.exists(cacheMapFile)) return(list())
  cacheRecordJson<-readFile(cacheMapFile)
  if (nchar(cacheRecordJson)==0) return (list())
  as.list(fromJSON(cacheRecordJson))
}

# return the last-modified time stamp for the given fileHandleId and filePath
# or NULL if there is no entry
getFromCacheMap<-function(fileHandleId, filePath) {
  cacheMapFile<-cacheMapFilePath(fileHandleId)
  lockFile(cacheMapFile)
  mapForFileHandleId<-getCacheMapFileContent(fileHandleId)
  unlockFile(cacheMapFile)
  # this is necessary to allow Windows paths to work with toJSON/fromJSON
  filePath<-normalizePath(filePath, winslash="/")
  for (key in names(mapForFileHandleId)) {
    if (key==filePath) return(mapForFileHandleId[[key]]) # i.e. return time stamp
  }
  NULL
}

addToCacheMap<-function(fileHandleId, filePath, timestamp=NULL) {
  if (is.null(fileHandleId)) stop("fileHandleId is required")
  if (is.null(filePath)) stop("filePath is required")
  if (is.null(timestamp)) timestamp<-lastModifiedTimestamp(filePath)
  cacheMapFile<-cacheMapFilePath(fileHandleId)
  lockExpiration<-lockFile(cacheMapFile)
  mapForFileHandleId<-getCacheMapFileContent(fileHandleId)
  record<-list()
  # this is necessary to allow Windows paths to work with toJSON/fromJSON
  filePath<-normalizePath(filePath, winslash="/")
  record[[filePath]]<-.formatAsISO8601(timestamp)
  mapForFileHandleId<-modifyList(mapForFileHandleId, as.list(record))
  cacheRecordJson<-toJSON(mapForFileHandleId)
  writeFileAndUnlock(cacheMapFile, cacheRecordJson, lockExpiration)
}

# is local file UNchanged
# given a FileHandleId and a file path
# returns TRUE if there is an entry in the Cache Map for the FileHandleId and the File Path
# and the 'last modified" timestamp for the file equals the value in the Cache Map
# returns FALSE if the timestamps differ OR if there is no entry in the Cache Map
localFileUnchanged<-function(fileHandleId, filePath) {
  downloadedTimestamp<-getFromCacheMap(fileHandleId, filePath)
  !is.null(downloadedTimestamp) && .formatAsISO8601(lastModifiedTimestamp(filePath))==downloadedTimestamp
}

uploadAndAddToCacheMap<-function(filePath, uploadDestination, contentType=NULL) {
  lastModified<-lastModifiedTimestamp(filePath)
  fileHandle<-uploadFileToEntity(filePath=filePath, uploadDestination=uploadDestination, curlHandle=getCurlHandle(), contentType=contentType)
  if (lastModified!=lastModifiedTimestamp(filePath)) stop(sprintf("During upload, %s was modified by another process.", filePath))
  addToCacheMap(fileHandle$id, filePath, lastModified)
  fileHandle
}

hasObjects<-function(file) {!is.null(file@objects)}

# returns TRUE iff the file can be loaded.  
# Would be ideal to answer the question WITHOUT loading what might be a large file,
# but there isn't a well-defined way to do so
isLoadable<-function(filePath) {
  tempenv<-new.env(parent=emptyenv())
  originalWarnLevel<-options()$warn
  options(warn=2)
  loadable<-TRUE
  tryCatch(
    load(file=filePath, envir = tempenv),
    error = function(e) {
      loadable<<-FALSE
    }
  )
  options(warn=originalWarnLevel)
  loadable
}


# if File has associated objects, then serialize them into a file
serializeObjects<-function(file) {
  if (fileHasFilePath(file)) {
    filePath<-file@filePath
    # Check that either (1) file does not exist or (2) file is already has binary data (and therefore is being updated)
    if (file.exists(filePath)) {
      if (!isLoadable(filePath)) stop(sprintf("File contains in-memory objects, but %s is not a serialized object file.", filePath))
    }
  } else {
    filePath<-tempfile(fileext=".rbin")
  }
  # now serialize the content into the file
  save(list=ls(file@objects), file=filePath, envir=file@objects)
  filePath
}

synStoreFile <- function(file, createOrUpdate=T, forceVersion=T, contentType=NULL) {
  if (hasObjects(file)) file@filePath<-serializeObjects(file)
  if (!fileHasFileHandleId(file)) { # if there's no existing Synapse File associated with this object...
    if (!fileHasFilePath(file)) { # ... and there's no local file staged for upload ...
      # meta data only
    } else { # ... we are storing a new file
      if (file@synapseStore) { # ... we are storing a new file which we are also uploading
        # determine upload destination
        uploadDestination<-selectUploadDestination(file@uploadHost, getUploadDestinations(propertyValue(file, "parentId")))
        if (is.null(uploadDestination)) stop("Selected upload host is not allowed for the file's parent container.");
        fileHandle<-uploadAndAddToCacheMap(filePath=file@filePath, uploadDestination=uploadDestination, contentType=contentType)
      } else { # ... we are storing a new file which we are linking, but not uploading
        # link external URL in Synapse, get back fileHandle	
        fileName <- basename(file@filePath)
        if (is.null(contentType)) {
          contentType<-getMimeTypeForFile(fileName)
        }
        fileHandle<-synapseLinkExternalFile(file@filePath, fileName, contentType)
        # note, there's no cache map entry to create
      }
      #	save fileHandle in slot, put id in entity properties
      file@fileHandle <- fileHandle
      propertyValue(file, "dataFileHandleId")<-file@fileHandle$id
    }
  } else { # fileHandle is not null, i.e. we're updating a Synapse File that's already created
    #	if fileHandle is inconsistent with filePath or synapseStore, raise an exception 
    validateFile(file)
    if (file@synapseStore) {
      if (!fileHasFilePath(file) || localFileUnchanged(file@fileHandle$id, file@filePath)) {
        # since local file matches Synapse file (or was not actually retrieved) nothing to store
      } else {
        uploadDestination<-selectUploadDestination(getUploadHostForFileHandle(file@fileHandle), getUploadDestinations(propertyValue(file, "parentId")))
        # TODO test this case
        if (is.null(uploadDestination)) stop("None of the container's upload destinations matches that of the existing file.")
        #	load file into Synapse, get back fileHandle (save in slot, put id in properties)       
        fileHandle<-uploadAndAddToCacheMap(filePath=file@filePath, uploadDestination=uploadDestination, contentType=contentType)
        file@fileHandle<-fileHandle
        propertyValue(file, "dataFileHandleId")<-file@fileHandle$id
      }
    } else { # file@synapseStore==F
      # file is considered 'read only' and will not be uploaded
    }
  }
  file
}

# choose the upload destination, taking into account:
# (1) the uploadDestinations for the container entity;
# (2) the uploadHost for the file (if any)
selectUploadDestination<-function(uploadHost, containerDestinations) {
  if (length(uploadHost)==0) {
    return(containerDestinations[[1]])
  } else {
    for (dest in containerDestinations@content) {
      if (uploadHost=="S3") {
        if (is(dest, "S3UploadDestination")) {
          return(dest)
        }
      } else if (is(dest, "ExternalUploadDestination")) {
        if (matchHostToURL(uploadHost, dest@url)) {
          return (dest)
        }
      }
    }
  }
  NULL
}

# match protocol, host and port
matchHostToURL<-function(userHost, containerUrl) {
  parsedUserHost<-.ParsedUrl(userHost)
  parsedContainerUrl<-.ParsedUrl(containerUrl)
  parsedUserHost@protocol==parsedContainerUrl@protocol &&
    parsedUserHost@host==parsedContainerUrl@host
}

uploadHostMatchesUploadDestination<-function(uploadHost, containerEntityId) {
  uploadDestinations<-getUploadDestinations(containerEntityId)
  !is.null(selectUploadDestination(uploadHost, uploadDestinations))
}

getUploadDestinations<-function(containerEntityId) {
  uploadDestinationResponse<-synRestGET(sprintf("/entity/%s/uploadDestinations", containerEntityId), endpoint=synapseFileServiceEndpoint())
  uploadDestinations<-createTypedListFromList(uploadDestinationResponse$list, "UploadDestinationList")
  if (length(uploadDestinations)==0) stop(sprintf("Entity %s has no upload destinations to choose from.", containerEntityId))
  uploadDestinations
}

getUploadHostForFileHandle<-function(fileHandle) {
  if (fileHandle$concreteType=="org.sagebionetworks.repo.model.file.S3FileHandle") {
    return("S3")
  } else if (fileHandle$concreteType=="org.sagebionetworks.repo.model.file.ExternalFileHandle") {
    parsedExtUrl<-.ParsedUrl(fileHandle$externalURL)
    return(sprintf("%s://%s", parsedExtUrl@protocol, parsedExtUrl@host))
  } else {
    stop(sprintf("Unexpected file handle type %s", fileHandle$concreteType))
  }
}

# we define these functions to allow mocking during testing
.hasAccessRequirement<-function(entityId) {
  currentAccessRequirements<-synRestGET(sprintf("/entity/%s/accessRequirement", entityId))
  currentAccessRequirements$totalNumberOfResults>0
}

.createLockAccessRequirement<-function(entityId) {
  synRestPOST(sprintf("/entity/%s/lockAccessRequirement", entityId), list())
}

fileExists<-function(folder, filename) {
  file.exists(file.path(folder, filename))
}

generateUniqueFileName<-function(folder, filename) {
  if (!fileExists(folder, filename)) return(filename)
  extension<-getExtension(filename)
  if (nchar(extension)==0) {
    prefix<-filename
  } else {
    # we know there's a '.' before the extension
    extension<-sprintf(".%s", extension)
    prefix<-substring(filename, 1, nchar(filename)-nchar(extension))
  }
  # we're not going to try forever, but we will try a number of times
  for (i in 1:100) {
    filenameVariation<-sprintf("%s(%d)%s",prefix,i,extension)
    if (!fileExists(folder, filenameVariation)) return(filenameVariation)
  }
  stop(sprintf("Cannot generate unique file name variation in folder %s for file %s", folder, filename))
}

downloadFromSynapseOrExternal<-function(
  downloadLocation, 
  filePath, 
  isExternalUrl, 
  downloadUri, 
  endpointName, 
  externalURL, 
  fileHandle) {
  dir.create(downloadLocation, recursive=T, showWarnings=F)
  if (isExternalUrl==FALSE) {
    synapseDownloadFromServiceToDestination(downloadUri, endpointName, destfile=filePath, extraRetryStatusCodes=404)
  } else {
    synapseDownloadFileToDestination(externalURL, filePath)
  }
  addToCacheMap(fileHandle$id, filePath)
}

getFileHandle<-function(entity) {
  fileHandleId<-propertyValue(entity, "dataFileHandleId")
  if (is.null(fileHandleId)) stop(sprintf("Entity %s (version %s) is missing its FileHandleId", propertyValue(entity, "id"), propertyValue(entity, "version")))
  handlesUri<-sprintf(
    "/entity/%s/version/%s/filehandles", 
    propertyValue(entity, "id"),
    propertyValue(entity, "versionNumber"))
  fileHandlesArray<-synapseGet(handlesUri)
  fileHandles<-fileHandlesArray$list
  for (fileHandle in fileHandles) {
    if (fileHandle['id']==fileHandleId) return(as.list(fileHandle))
  }
  stop(sprintf("Could not find file handle for entity %s, fileHandleId %s", propertyValue(entity, "id"), fileHandleId))
}

# return TRUE iff there are unfulfilled access requirements
# Note this is 'broken out' from 'synGetFile' to allow mocking 
# during integration test
hasUnfulfilledAccessRequirements<-function(id) {
  unfulfilledAccessRequirements<-synapseGet(sprintf("/entity/%s/accessRequirementUnfulfilled", id))
  unfulfilledAccessRequirements$totalNumberOfResults>0
}


synGetFile<-function(file, downloadFile=T, downloadLocation=NULL, ifcollision="keep.both", load=F) {
  if (class(file)!="File") stop("'synGetFile' may only be invoked with a File parameter.")
  id<-propertyValue(file, "id")
  
  # if I lack access due to a restriction...
  if (downloadFile && hasUnfulfilledAccessRequirements(id)) {
    # ...an error message is displayed which include the 'onweb' command to open the entity page, 
    # where a user can address the access requirements.
    message <- sprintf("Please visit the web page for this entity (onWeb(\"%s\")) to review and fulfill its download requirement(s).", id)
    stop(message)
  }
  
  fileHandle<-getFileHandle(file)
  file@fileHandle<-fileHandle
  
  if (is.null(propertyValue(file, "versionNumber"))) {
    downloadUri<-sprintf("/entity/%s/file", id)
  } else {
    downloadUri<-sprintf("/entity/%s/version/%s/file", id, propertyValue(file, "versionNumber"))
  }
  
  filePath<-synGetFileAttachment(
    downloadUri,
    "REPO",
    fileHandle,
    downloadFile,
    downloadLocation,
    ifcollision,
    load
    )
    
  # now construct File from 'result', which has filePath, synapseStore 
  if (!is.null(filePath)) file@filePath<-filePath
  # if the file location matches one of the upload destinations for the container then we can store it
  # back to that location later
  file@uploadHost<-getUploadHostForFileHandle(fileHandle)
  file@synapseStore<-uploadHostMatchesUploadDestination(file@uploadHost, propertyValue(file, "parentId"))
  if (load) {
    if (is.null(file@objects)) file@objects<-new.env(parent=emptyenv())
    # Note: the following only works if 'path' is a file system path, not a URL
    load(file=file@filePath, envir = as.environment(file@objects))
  }  
  file
}

synGetFileAttachment<-function(downloadUri, endpointName, fileHandle, downloadFile=T, downloadLocation=NULL, ifcollision="keep.both", load=F) {
  if (isExternalFileHandle(fileHandle)) {
    isExternalURL<-TRUE
    externalURL<-fileHandle$externalURL
    if (is.null(externalURL)) stop(sprintf("URL missing from External File Handle for %s", fileHandle$fileName))
  } else {
    isExternalURL<-FALSE
    externalURL<-NULL
  }
  
  if (downloadFile) {
    if (is.null(downloadLocation)) {
      downloadLocation<-defaultDownloadLocation(fileHandle$id)
    } else {
      if (file.exists(downloadLocation) && !file.info(downloadLocation)$isdir) stop(sprintf("%s is not a folder", downloadLocation))
    }
    filePath<-file.path(downloadLocation, fileHandle$fileName)
    if (file.exists(filePath)) {
      if (localFileUnchanged(fileHandle$id, filePath)) {
        # no need to download, 'filePath' is now the path to the local copy of the file
      } else {
  			if (ifcollision=="overwrite.local") {
  				# download file from Synapse to downloadLocation
          # we first capture the existing file.  If the download fails, we'll move it back
          temp<-file.path(downloadLocation, sample(999999999, 1))
          if (!file.rename(filePath, temp)) stop(sprintf("Failed to back up %s before downloading new version.", filePath))
          tryCatch(
            downloadFromSynapseOrExternal(downloadLocation, filePath, isExternalURL, downloadUri, endpointName, externalURL, fileHandle),
            error = function(e) {file.rename(temp, filePath); stop(e)}
          )
          unlink(temp)
        } else if (ifcollision=="keep.local") {
  				# nothing to do
        } else if (ifcollision=="keep.both") {
          #download file from Synapse to distinct filePath
          uniqueFileName <- generateUniqueFileName(downloadLocation, fileHandle$fileName)
          filePath <- file.path(downloadLocation, uniqueFileName)
          downloadFromSynapseOrExternal(downloadLocation, filePath, isExternalURL, downloadUri, endpointName, externalURL, fileHandle) 
        } else {
  				stop(sprintf("Unexpected value for ifcollision: %s.  Allowed settings are 'overwrite.local', 'keep.local', 'keep.both'", ifcollision))
        }
      }
    } else { # filePath does not exist
      downloadFromSynapseOrExternal(downloadLocation, filePath, isExternalURL, downloadUri, endpointName, externalURL, fileHandle) 
    }
  } else { # !downloadFile
    filePath<-externalURL # url from fileHandle (could be web-hosted URL or file:// on network file share)
  }
  if (load) {
    if(is.null(filePath)) {
      if (!isExternalURL(fileHandle) && !downloadFile) {
        stop("Cannot load Synapse file which has not been downloaded.  Please try again, with downloadFile=T.")
      }
      # shouldn't reach this statement.  something has gone very wrong.
      stop(sprintf("Cannot load file %s, there is no local file path.", fileHandle$fileName))
    }
  }
  filePath
}


#
# Wrappers for legacy functions
#

setMethod(
  f = "updateEntity",
  signature = signature("File"),
  definition = function(entity) {synStore(entity, activity=generatedBy(entity), forceVersion=F)}
)

setMethod(
  f = "createEntity",
  signature = signature("File"),
  definition = function(entity) {synStore(entity, activity=generatedBy(entity), forceVersion=F)}
)

setMethod(
  f = "storeEntity",
  signature = signature("File"),
  definition = function(entity) {synStore(entity, activity=generatedBy(entity), forceVersion=F)}
)

setMethod(
  f = "downloadEntity",
  signature = signature("File","missing"),
  definition = function(entity){
    synGet(propertyValue(entity, "id"))
  }
)

# This is a weird legacy method in which the caller may specify a version
# different than the one in the 'entity'.
# We extract the id, combine it with the specified version, and call 'synGet'
# to download the metadata anew.
setMethod(
  f = "downloadEntity",
  signature = signature("File","character"),
  definition = function(entity, versionId){
      synGet(propertyValue(entity, "id"), versionId, downloadFile=T)
  }
)

setMethod(
  f = "downloadEntity",
  signature = signature("File","numeric"),
  definition = function(entity, versionId){
    downloadEntity(entity, as.character(versionId))
  }
)

setMethod(
  f = "loadEntity",
  signature = signature("File","missing"),
  definition = function(entity){
    synGet(propertyValue(entity, "id"), version=NULL, downloadFile=T, load=T)
  }
)

# This is a weird legacy method in which the caller may specify a version
# different than the one in the 'entity'.
# We extract the id, combine it with the specified version, and call 'synGet'
# to download the metadata anew.
setMethod(
  f = "loadEntity",
  signature = signature("File","character"),
  definition = function(entity, versionId){
    synGet(id=propertyValue(entity, "id"), version=versionId, downloadFile=T, load=T)
  }
)

setMethod(
  f = "loadEntity",
  signature = signature("File","numeric"),
  definition = function(entity, versionId){
    loadEntity(entity, as.character(versionId))
  }
)

setMethod(
  f = "moveFile",
  signature = signature("File", "character", "character"),
  definition = function(entity, src, dest){
    stop("Unsupported operation for Files.")
  }
)

setMethod(
  f = "deleteFile",
  signature = signature("File", "character"),
  definition = function(entity, file){
    stop("Unsupported operation for Files.")
  }
)

setMethod(
  f = "addFile",
  signature = signature("File", "character", "character"),
  definition = function(entity, file, path) { # Note 'entity' is a File, not an Entity
    addFile(entity, file.path(path, file))
  }
)

setMethod(
  f = "addFile",
  signature = signature("File", "character", "missing"),
  definition = function(entity, file) { # Note 'entity' is a File, not an Entity
    if (length(entity@filePath)>0) stop("A File may only contain a single file.")
    entity@filePath = file
    if (is.null(propertyValue(entity, "name"))) propertyValue(entity, "name")<-basename(entity@filePath)
    entity
  }
)

addObjectMethod<-function(owner, object, name) {
  if (is.null(owner@objects)) owner@objects<-new.env(parent=emptyenv())
  assign(x=name, value=object, envir=owner@objects)
  invisible(owner)
}

setMethod(
  f = "addObject",
  signature = signature("File", "ANY", "missing", "missing"),
  definition = function(owner, object) {
    name <- deparse(substitute(object, env=parent.frame()))
    name <- gsub("\\\"", "", name)
    addObjectMethod(owner, object, name)
  }
)


setMethod(
  f = "addObject",
  signature = signature("File", "ANY", "character", "missing"),
  definition = function(owner, object, name) {
    addObjectMethod(owner, object, name)
  }
)

deleteObjectMethod<-function(owner, which) {
  remove(list=which, envir=owner@objects)
  if (length(owner@objects)==0) owner@objects<-NULL
  invisible(owner)
}

setMethod(
  f = "deleteObject",
  signature = signature("File", "character"),
  definition = function(owner, which) {
    deleteObjectMethod(owner, which)
  }
)

setMethod(
  f = "getObject",
  signature = signature("File", "character"),
  definition = function(owner, which) {
    get(x=which, envir=owner@objects)
  }
)

getObjectMethod<-function(owner) {
  if (is.null(owner@objects)) stop("Owner contains no objects.")
  numberOfObjects<-length(ls(owner@objects))
  if (numberOfObjects>1) stop(sprintf("Owner contains %s objects.  Use 'getObject(owner,objectName)' to select one.",numberOfObjects))
  objectName<-ls(owner@objects)[1]
  get(x=objectName, envir=owner@objects)
  
}

setMethod(
  f = "getObject",
  signature = signature("File", "missing"),
  definition = function(owner) {
    getObjectMethod(owner)
  }
)

renameObjectMethod<-function(owner, which, name) {
  object<-get(x=which, envir=owner@objects)
  assign(x=name, value=object, envir=owner@objects)
  remove(list=which, envir=owner@objects)   
  invisible(owner)
  
}

setMethod(
  f = "renameObject",
  signature = signature("File", "character", "character"),
  definition = function(owner, which, name) {
    renameObjectMethod(owner, which, name)
  }
)

listObjects<-function(file) {
  if (is.null(file@objects)) {
    list()
  } else {
    ls(file@objects)
  }
}






