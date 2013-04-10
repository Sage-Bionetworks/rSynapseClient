#
# class and method definitions for File
#
# This error was fixed by renaming "File.R" to "FileX.R"
#  Error in function (classes, fdef, mtable)  : 
#    unable to find an inherited method for function ‘getFileCache’ for signature ‘"character", "character", "FileCacheFactory"’
# This error was fixed by renaming "FileX.R" to "xxxFile.R"
# Error in `$<-`(`*tmp*`, "entityType", value = "org.sagebionetworks.repo.model.FileEntity") : 
#  no method for assigning subsets of this S4 class



initializeFileProperties<-function() {
  synapseType<-"org.sagebionetworks.repo.model.FileEntity"
  properties<-SynapseProperties(getEffectivePropertyTypes(synapseType))
  properties$entityType <- synapseType
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
    # fileHandle (generated from JSON schema, empty before entity is created)
    fileHandle = "list",
    # objects to be serialized/deserialized
    objects = "environmentOrNull"
  ),
  # This is modeled after defineEntityClass in AAAschema
  prototype = prototype(
    synapseEntityKind = "File",
    properties = initializeFileProperties(),
    synapseStore = TRUE,
    objects = NULL
  )
)

synAnnotSetMethod<-function(object, which, value) {
  if(any(which==propertyNames(object))) {
    propertyValue(object, which)<-value
  } else {
    annotValue(object, which)<-value
  }
  object
}

synAnnotGetMethod<-function(object, which) {
  if(any(which==propertyNames(object))) {
    propertyValue(object, which)
  } else {
    annotValue(object, which)
  }
}

fileConstructorMethod<-function(path, synapseStore, ...) {
  file <- new("File")
  entityParams<-modifyList(list(name=basename(path)), list(...))
  for (key in names(entityParams)) file<-synAnnotSetMethod(file, key, entityParams[[key]])
  file@filePath <- path
  file@synapseStore <- synapseStore
  file
}

##
## File contructor: path="/path/to/file", synapseStore=T, name="foo", ...

##
setMethod(
  f = "File",
  signature = signature("character", "missing"),
  definition = function(path, ...) {fileConstructorMethod(path, synapseStore=TRUE, ...)}
)

setMethod(
  f = "File",
  signature = signature("character", "logical"),
  definition = fileConstructorMethod
)

## File contructor: obj=<obj ref>, ...
setMethod(
  f = "File",
  signature = signature("ANY", "missing"),
  definition = function(path, synapseStore) {
    file <- new("File")
    
    name <- deparse(substitute(path, env=parent.frame()))
    name <- gsub("\\\"", "", name)
    file<-addObjectMethod(file, path, name)
    
#    entityParams<-list(...)
#    for (key in names(entityParams)) file<-synAnnotSetMethod(file, key, entityParams[[key]])
    file@synapseStore <- TRUE
    
    file
  }
)

# this is the required constructor for a metadata Entity, taking a list of properties
# the logic is based on definedEntityConstructors in AAAschema
setMethod(
  f = "FileListConstructor",
  signature = signature("list"),
  definition = function(propertiesList) {
    # the param is not actually a file path.  Using the same param names in all constructors is an S-4 requirement  
    #propertiesList<-path   
    file <- new("File")
    for (prop in names(propertiesList))
      file<-synAnnotSetMethod(file, prop, propertiesList[[prop]])
    
    propertyValue(file, "entityType") <- "org.sagebionetworks.repo.model.FileEntity"

    file
  }
)

isExternalFileHandle<-function(fileHandle) {length(fileHandle)>0 && fileHandle$concreteType=="ExternalFileHandle"}
fileHasFileHandleId<-function(file) {!is.null(file@fileHandle$id)}
fileHasFilePath<-function(file) {length(file@filePath)>0 && nchar(file@filePath)>0}

# TODO:  shall we allow 'synapseStore' to be switched in an existing File?
# ensure that fileHandle info is consistent with synapseUpload field
validdateFile<-function(file) {
  if (!fileHasFileHandleId(file)) return
  isExternal <- isExternalFileHandle(file@fileHandle)
  if (isExternal & file@synapseStore) stop("synapseStore==T but file is external")
  if (isExternal==FALSE & file@synapseStore==FALSE) stop("synapseStore=F but file is not external")
}

lastModifiedTimestamp<-function(filePath) {
  if (!file.exists(filePath)) stop(sprintf("%s does not exist.", filePath))
  file.info(filePath)$mtime
}

defaultDownloadLocation<-function(fileHandleId) {
  # TODO:  insert an intermediate subfolder?
  sprintf("%s/%s", synapseCacheDir(), fileHandleId)
}

cacheMapFilePath<-function(fileHandleId) {
  sprintf("%s/.cacheMap", defaultDownloadLocation(fileHandleId))
}

getCacheMapFileContent<-function(fileHandleId) {
  cacheMapFile<-cacheMapFilePath(fileHandleId)
  if (!file.exists(cacheMapFile)) return(list())
  cacheRecordJson<-readFile(cacheMapFile)
  as.list(fromJSON(cacheRecordJson))
}

# return the last-modified time stamp for the given fileHandleId and filePath
# or NULL if there is no entry
getFromCacheMap<-function(fileHandleId, filePath) {
  mapForFileHandleId<-getCacheMapFileContent(fileHandleId)
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

uploadAndAddToCacheMap<-function(filePath) {
  lastModified<-lastModifiedTimestamp(filePath)
  fileHandle<-synapseUploadToFileHandle(filePath)
  if (lastModified!=lastModifiedTimestamp(filePath)) stop(sprintf("During upload, %s was modified by another process.", filePath))
  addToCacheMap(fileHandle$id, filePath, lastModified)
  fileHandle
}

hasObjects<-function(file) {!is.null(file@objects)}

# if File has associated objects, then serialize them into a file
serializeObjects<-function(file) {
  if (fileHasFilePath(file)) {
    filePath<-file@filePath
  } else {
    filePath<-sprintf("%s.rbin", tempfile())
  }
  # now serialize the content into the file
  save(list=ls(file@objects), file=filePath, envir=file@objects)
  filePath
}

synStore <- function(entity, activity=NULL, used=NULL, executed=NULL, activityName=NULL, activityDescription=NULL, createOrUpdate=T, forceVersion=T) {  
  if (class(entity)=="File" || class(entity)=="Record") {
    entity<-synStoreFile(file=entity, createOrUpdate, forceVersion)
    # TODO: Handle Record
  }
  # Now save the metadata
  if (!is.null(activity)) {
    generatedBy(entity)<-activity
  } else if (!is.null(used) || !is.null(executed)) {
    activity<-Activity(list(name=activityName, description=activityDescription))
    usedAndExecuted<-list()
    if (!is.null(used)) usedAndExecuted<-c(usedAndExecuted, lapply(X=used, FUN=usedListEntry, wasExecuted=F))
    if (!is.null(executed)) usedAndExecuted<-c(usedAndExecuted, lapply(X=executed, FUN=usedListEntry, wasExecuted=T))
    if (length(usedAndExecuted)>0) propertyValue(activity, "used") <- usedAndExecuted
    generatedBy(entity)<-activity
  }
  if (is.null(propertyValue(entity, "id"))) {
    # get the superclass createEntity method, which just stores the metadata
    superCreateEntity<-getMethod("createEntity", "Entity") # TODO createOrUpdate
    storedEntity<-superCreateEntity(entity)
  } else {
    storedEntity<-updateEntityMethod(entity, forceVersion)
  }
  if (class(entity)=="File" || class(entity)=="Record") {
    # now copy the class-specific fields into the newly created object
    if (fileHasFilePath(entity)) storedEntity@filePath <- entity@filePath
    storedEntity@synapseStore <- entity@synapseStore
    storedEntity@fileHandle <- entity@fileHandle
    storedEntity@objects <- entity@objects
  }
  storedEntity
}

synStoreFile <- function(file, createOrUpdate=T, forceVersion=T) {
  if (hasObjects(file)) file@filePath<-serializeObjects(file)
  if (!fileHasFileHandleId(file)) { # if there's no existing Synapse File associated with this object...
    if (!fileHasFilePath(file)) { # ... and there's no local file staged for upload ...
      # then we will only be storing the meta data
    } else { # ... we are storing a new file
      if (file@synapseStore) { # ... we are storing a new file which we are also uploading
        fileHandle<-uploadAndAddToCacheMap(file@filePath)
      } else { # ... we are storing a new file which we are linking, but not uploading
        # link external URL in Synapse, get back fileHandle	
        fileName <- basename(file@filePath)
        contentType<-getMimeTypeForFile(fileName)
        fileHandle<-synapseLinkExternalFile(file@filePath, fileName, contentType)
        # note, there's no cache map entry to create
      }
      #	save fileHandle in slot, put id in entity properties
      file@fileHandle <- fileHandle
      propertyValue(file, "dataFileHandleId")<-file@fileHandle$id
    }
  } else { # fileHandle is not null, i.e. we're updating a Synapse File that's already created
    #	if fileHandle is inconsistent with filePath or synapseStore, raise an exception 
    validdateFile(file)
    if (file@synapseStore) {
      if (!fileHasFilePath(file) || localFileUnchanged(file@fileHandle$id, file@filePath)) {
        # since local file matches Synapse file (or was not actually retrieved) nothing to store
      } else {
        #	load file into Synapse, get back fileHandle (save in slot, put id in properties)
        fileHandle<-uploadAndAddToCacheMap(file@filePath)
        file@fileHandle<-fileHandle
        propertyValue(file, "dataFileHandleId")<-file@fileHandle$id
      }
    } else { # synapseStore==F
      # file is considered 'read only' and will not be uploaded
    }
  }
  file
}

createFilePath<-function(folder, filename) {sprintf("%s/%s", folder, filename)}

fileExists<-function(folder, filename) {
  file.exists(createFilePath(folder, filename))
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

downloadFromSynapseOrExternal<-function(downloadLocation, filePath, synapseStore, downloadUri, externalURL, fileHandle) {
  dir.create(downloadLocation, recursive=T, showWarnings=F)
  if (synapseStore) {
    synapseDownloadFromRepoServiceToDestination(downloadUri, destfile=filePath)
  } else {
    synapseDownloadFileToDestination(externalURL, filePath)
  }
  addToCacheMap(fileHandle$id, filePath)
  
}

synGet<-function(id, version=NULL, downloadFile=T, downloadLocation=NULL, ifcollision="keep.both", load=F) {
  if (is.null(version)) {
    file<-getEntity(id)
  } else {
    file<-getEntity(id, version=version)
  }   
  if ((class(file)=="File" || class(file)=="Record") && (downloadFile || load)) {
    synGetFile(file, downloadFile, downloadLocation, ifcollision, load)
    # TODO: Handle Record
  } else {
    file
  }
}

getFileHandle<-function(entity) {
  fileHandleId<-propertyValue(entity, "dataFileHandleId")
  if (is.null(fileHandleId)) stop(sprintf("Entity %s (version %s) is missing its FileHandleId", propertyValue(entity, "id"), propertyValue(entity, "version")))
  handlesUri<-sprintf(
    "/entity/%s/version/%s/filehandles", 
    propertyValue(entity, "id"),
    propertyValue(entity, "versionNumber"))
  fileHandlesArray<-synapseGet(handlesUri, service="REPO")
  fileHandles<-fileHandlesArray[[1]] # not sure why it's wrapped in an extra layer of array, just an artifact of the JSON structure
  for (fileHandle in fileHandles) {
    if (fileHandle$id==fileHandleId) return(fileHandle)
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
  if (hasUnfulfilledAccessRequirements(id)) {
    # ...an error message is displayed which include the 'onweb' command to open the entity page, 
    # where a user can address the access requirements.
    message <- sprintf("Please visit the web page for this entity (onWeb(\"%s\")) to review and fulfill its download requirement(s).", id)
    stop(message)
  }
  
  fileHandle<-getFileHandle(file)

  if (isExternalFileHandle(fileHandle)) {
    synapseStore<-FALSE
    externalURL<-fileHandle$externalURL
    if (is.null(externalURL)) stop(sprintf("URL missing from External File Handle for %s", fileHandle$fileName))
  } else {
    synapseStore<-TRUE
    externalURL<-NULL
    downloadUri<-sprintf("/entity/%s/file", id) # /entity/{enityId}/file
  }
  
  if (downloadFile) {
    if (is.null(downloadLocation)) {
      downloadLocation<-defaultDownloadLocation(fileHandle$id)
    } else {
      if (file.exists(downloadLocation) && !file.info(downloadLocation)$isdir) stop(sprintf("%s is not a folder", downloadLocation))
    }
    filePath<-createFilePath(downloadLocation, fileHandle$fileName)
    if (file.exists(filePath)) {
      if (localFileUnchanged(fileHandle$id, filePath)) {
        # no need to download, 'filePath' is now the path to the local copy of the file
      } else {
  			if (ifcollision=="overwrite.local") {
  				# download file from Synapse to downloadLocation
          downloadFromSynapseOrExternal(downloadLocation, filePath, synapseStore, downloadUri, externalURL, fileHandle) 
        } else if (ifcollision=="keep.local") {
  				# nothing to do
        } else if (ifcollision=="keep.both") {
          #download file from Synapse to distinct filePath
          uniqueFileName <- generateUniqueFileName(downloadLocation, fileHandle$fileName)
          filePath <- sprintf("%s/%s", downloadLocation, uniqueFileName)
          downloadFromSynapseOrExternal(downloadLocation, filePath, synapseStore, downloadUri, externalURL, fileHandle) 
        } else {
  				stop(sprintf("Unexpected value for ifcollision: %s.  Allowed settings are 'overwrite.local', 'keep.local', 'keep.both'", ifcollision))
        }
      }
    } else { # filePath does not exist
      downloadFromSynapseOrExternal(downloadLocation, filePath, synapseStore, downloadUri, externalURL, fileHandle) 
    }
  } else { # !downloadFile
    filePath<-externalURL # url from fileHandle (could be web-hosted URL or file:// on network file share)
  }
  if (downloadFile) file@filePath<-filePath
  file@synapseStore<-synapseStore
  file@fileHandle<-fileHandle
  if (load) {
    if(is.null(filePath)) {
      if (!isExternalURL(fileHandle) && !downloadFile) {
        stop("Cannot load Synapse file which has not been downloaded.  Please call synGet again, with downloadFile=T.")
      }
      # shouldn't reach this statement.  something has gone very wrong.
      stop(sprintf("Cannot load file %s, there is no local file path.", fileHandle$fileName))
    }
    # TODO handle .Rbin, .R, or text data file differently (Use the contentType field in the FileHandle?)
    if (is.null(file@objects)) file@objects<-new.env(parent=emptyenv())
    load(file=filePath, envir = as.environment(file@objects))
  }
  file
}


#
# Wrappers for legacy functions
#

setMethod(
  f = "updateEntity",
  signature = signature("File"),
  definition = function(entity) {synStore(entity, forceVersion=F)}
)

setMethod(
  f = "createEntity",
  signature = signature("File"),
  definition = function(entity) {synStore(entity, forceVersion=F)}
)

setMethod(
  f = "storeEntity",
  signature = signature("File"),
  definition = function(entity) {synStore(entity, forceVersion=F)}
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
    addFile(entity, sprintf("%s/%s", path, file))
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

setMethod(
  f = "deleteObject",
  signature = signature("File", "character"),
  definition = function(owner, which) {
    remove(list=which, envir=owner@objects)
    if (length(owner@objects)==0) owner@objects<-NULL
    invisible(owner)
  }
)

setMethod(
  f = "getObject",
  signature = signature("File", "character"),
  definition = function(owner, which) {
    get(x=which, envir=owner@objects)
  }
)

setMethod(
  f = "renameObject",
  signature = signature("File", "character", "character"),
  definition = function(owner, which, name) {
    object<-get(x=which, envir=owner@objects)
    assign(x=name, value=object, envir=owner@objects)
    remove(list=which, envir=owner@objects)   
    invisible(owner)
  }
)




