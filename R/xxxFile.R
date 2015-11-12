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
    # synapseStore: logical T if file is stored in Synapse or in external storage managed by Synapse, F if only the url is stored
    synapseStore = "logical",
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
File<-function(path, synapseStore=T, ...) {
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
# TODO once all file handle's storageLocationIds are back-filled (PLFM-3327):
# isExternalFileHandle<-function(fileHandle) {length(fileHandle)>0 && is.null(fileHandle$storageLocationId)}

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
  as.list(synFromJson(cacheRecordJson))
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

fileMatchesTimestamp<-function(filePath, timestamp) {
	!is.null(timestamp) && .formatAsISO8601(lastModifiedTimestamp(filePath))==timestamp
}

# is local file UNchanged
# given a FileHandleId and a file path
# returns TRUE if there is an entry in the Cache Map for the FileHandleId and the File Path
# and the 'last modified" timestamp for the file equals the value in the Cache Map
# returns FALSE if the timestamps differ OR if there is no entry in the Cache Map
localFileUnchanged<-function(fileHandleId, filePath) {
  downloadedTimestamp<-getFromCacheMap(fileHandleId, filePath)
  fileMatchesTimestamp(filePath, downloadedTimestamp)
}

startswith<-function(str, prefix) {
	spl<-strsplit(str, prefix)[[1]]
	return(length(spl)==2 && nchar(spl[1])==0 && nchar(spl[2])>0)
}

# Searches for a file for the given handle id which is already downloaded in the
# given location (directory). Looks both for an _unchanged_ file (i.e. not modified
# since download) and a downloaded file, whether changed or not.  Returns the first
# two matching files found in the slots 'unchanged' and 'any' of the returned list.
# if no match is found, the corresponding slot is empty.
getCachedInLocation<-function(fileHandleId, downloadLocation) {
	cacheMapFile<-cacheMapFilePath(fileHandleId)
	lockFile(cacheMapFile)
	mapForFileHandleId<-getCacheMapFileContent(fileHandleId)
	unlockFile(cacheMapFile)
	# this is necessary to allow Windows paths to work with toJSON/fromJSON
	downloadLocation<-normalizePath(downloadLocation, winslash="/")
	result<-list()
	for (key in names(mapForFileHandleId)) {
		if (startswith(key,downloadLocation) && file.exists(key)) {
			if (is.null(result$any)) result$any<-key
			if (fileMatchesTimestamp(key, mapForFileHandleId[[key]])) {
				if (is.null(result$unchanged)) result$unchanged<-key
			}
		}
	}
	result
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
    },
    finally = options(warn=originalWarnLevel)
  )
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
        uploadDestination<-getUploadDestinations(propertyValue(file, "parentId"))[[1]]
        fileHandle<-uploadAndAddToCacheMap(filePath=file@filePath, uploadDestination=uploadDestination, contentType=contentType)
      } else { # ... we are storing a new file which we are linking, but not uploading
        # link external URL in Synapse, get back fileHandle	
        fileHandle<-synapseLinkExternalFile(file@filePath, contentType, storageLocationId=NULL)
        # note, there's no cache map entry to create
      }
      #	save fileHandle in slot, put id in entity properties
      file@fileHandle <- fileHandle
      propertyValue(file, "dataFileHandleId")<-file@fileHandle$id
    }
  } else { # fileHandle is not null, i.e. we're updating a Synapse File that's already created
    # if fileHandle is inconsistent with filePath or synapseStore, raise an exception 
    validateFile(file)
    if (file@synapseStore) {
      if (!fileHasFilePath(file) || localFileUnchanged(file@fileHandle$id, file@filePath)) {
        # since local file matches Synapse file (or was not actually retrieved) nothing to store
      } else {
        uploadDestinations<-getUploadDestinations(propertyValue(file, "parentId"))
        uploadDestination<-selectUploadDestination(file, uploadDestinations)
        if(is.null(uploadDestination)) {
          stop("File's container lacks upload destination for the file's storage location.")
        }
        #	load file into Synapse, get back fileHandle (save in slot, put id in properties)       
        fileHandle<-uploadAndAddToCacheMap(filePath=file@filePath, uploadDestination=uploadDestination, contentType=contentType)
        file@fileHandle<-fileHandle
        propertyValue(file, "dataFileHandleId")<-file@fileHandle$id
      }
    } else { # file@synapseStore==F
      # we have an external file handle, NOT an externally managed file
      externalURL<-file@fileHandle$externalURL
      if (fileHasFilePath(file)) {
        filePath<-file@filePath
        # may need to update the external file handle
        if (filePath!=externalURL) {
          # update the file handle
          file@fileHandle<-synapseLinkExternalFile(filePath, contentType, storageLocationId=NULL)
          propertyValue(file, "dataFileHandleId")<-file@fileHandle$id
        }
      } else {
        # file handle will not be updated
      }
    }
  }
  file
}

# containerDestinations has type UploadDestinationList, a TypeList of UploadDestination
selectUploadDestination<-function(file, containerDestinations) {
  fileStorageLocationId<-file@fileHandle$storageLocationId
	for (dest in containerDestinations@content) {
			# NOTE: legacy S3 file handles have null fileStorageLocationId
			# which matches dest@storageLocationId==as.integer(1)
			if ((is.null(fileStorageLocationId) && !isExternalFileHandle(file@fileHandle) && dest@storageLocationId==as.integer(1)) ||
					(!is.null(fileStorageLocationId) && fileStorageLocationId==dest@storageLocationId)) {
				return(dest)
			}
	}
	
	# remove the following once PLFM-3327 is done
	# if the above fails to match an upload destination, try matching on URL
	if (isExternalFileHandle(file@fileHandle)) {
		fileUrl<-file@fileHandle$externalURL
		if (length(fileUrl)==0) {
			stop(sprintf("No file handle url for file handle %s", file@fileHandle$id))
		} 
		
		for (dest in containerDestinations@content) {
			if (is(dest, "ExternalUploadDestination") && 
					matchURLHosts(fileUrl, dest@url)) {
				return (dest)
			}
		}
	}

  	NULL
}

matchURLHosts<-function(url1, url2) {
	 parsedUrl1<-.ParsedUrl(url1)
	 parsedUrl2<-.ParsedUrl(url2)
	 parsedUrl1@protocol==parsedUrl2@protocol &&
			 parsedUrl1@host==parsedUrl2@host
}


fileHandleMatchesUploadDestination<-function(file, containerEntityId) {
  uploadDestinations<-getUploadDestinations(containerEntityId)
  !is.null(selectUploadDestination(file, uploadDestinations))
}

getUploadDestinations<-function(containerEntityId) {
  uploadDestinationResponse<-synRestGET(sprintf("/entity/%s/uploadDestinations", containerEntityId), endpoint=synapseFileServiceEndpoint())
  uploadDestinations<-createTypedListFromList(uploadDestinationResponse$list, "UploadDestinationList")
  if (length(uploadDestinations)==0) stop(sprintf("Entity %s has no upload destinations to choose from.", containerEntityId))
  uploadDestinations
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
  unfulfilledAccessRequirements<-synapseGet(sprintf("/entity/%s/accessRequirementUnfulfilled?accessType=DOWNLOAD", id))
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
    downloadUri<-sprintf("/entity/%s/file?redirect=FALSE", id)
  } else {
    downloadUri<-sprintf("/entity/%s/version/%s/file?redirect=FALSE", id, propertyValue(file, "versionNumber"))
  }
  
  filePath<-retrieveAttachedFileHandle(
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
  # if the file handle is not an external one or if it is external but the storageLocation
  # is 'recognized' by the file's parent project, then we can store it back later
  file@synapseStore<-!isExternalFileHandle(fileHandle) ||
		  fileHandleMatchesUploadDestination(file, propertyValue(file, "parentId"))
  if (load) {
    if (is.null(file@objects)) file@objects<-new.env(parent=emptyenv())
    if (length(grep(".zip$", tolower(file@filePath)))>0) {
		# this special case handles zipped R binaries transferred from the old "Locationable"
		# attachments to the current File entities.  If the user asks to 'load' such a zip
		# file, we unzip it and load its contents.  See SYNR-897.
		unzipped<-unzip(file@filePath, exdir=dirname(file@filePath))
		for (f in unzipped) {
			load(file=f, envir = as.environment(file@objects))
			unlink(f)
		}
	} else {
		load(file=file@filePath, envir = as.environment(file@objects))
	}
  }  
  file
}

retrieveAttachedFileHandle<-function(downloadUri, endpointName, fileHandle, downloadFile=T, downloadLocation=NULL, ifcollision="keep.both", load=F) {
  if (isExternalFileHandle(fileHandle)) {
    isExternalURL<-TRUE
    externalURL<-fileHandle$externalURL
    if (is.null(externalURL)) stop(sprintf("URL missing from External File Handle for %s", fileHandle$fileName))
  } else {
    isExternalURL<-FALSE
    externalURL<-NULL
  }
  
  if (downloadFile) {
	  filePath<-downloadFromServiceWithCaching(downloadUri, endpointName, fileHandle$id, downloadLocation, ifcollision)
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

downloadFromServiceWithCaching<-function(downloadUri, endpointName, fileHandleId, downloadLocation=NULL, ifcollision="keep.both") {
	if (is.null(downloadLocation)) {
		downloadLocation<-defaultDownloadLocation(fileHandleId)
	} else {
		if (file.exists(downloadLocation) && !file.info(downloadLocation)$isdir) stop(sprintf("%s is not a folder", downloadLocation))
	}
	
	# if there is already a downloaded, unmodified version of the file in the desired directory, then return it
	downloaded<-getCachedInLocation(fileHandleId, downloadLocation)
	if (!is.null(downloaded$unchanged)) return(downloaded$unchanged)
	if (!is.null(downloaded$any) && ifcollision=="keep.local") {
		# there's no need to download
		return(downloaded$any)
	}
	
	# OK, we need to download it
	downloadResult<-downloadFromService(downloadUri, endpointName, destdir=downloadLocation, extraRetryStatusCodes=404)
	# result is list(downloadedFile, fileName) where 
	# 'downloadedFile' is a temp file in the target location and fileName is the desired file name
	
	if (is.null(downloadResult$fileName)) stop(sprintf("download of %s failed to return file name.", downloadUri))
	filePath<-file.path(downloadLocation, downloadResult$fileName)
	if (file.exists(filePath)) {
		if (ifcollision=="overwrite.local") {
			# here we are reverting to the original, overwriting local changes to the file
		} else if (ifcollision=="keep.local") {
			# this is a weird edge case in which a file with the target name exists, but
			# wasn't downloaded by us.  Since the user asked to keep the local copy we will do so.
			unlink(downloadResult$downloadedFile)
			return(filePath)
		} else if (ifcollision=="keep.both") {
			#download file from Synapse to distinct filePath
			uniqueFileName <- generateUniqueFileName(downloadLocation, downloadResult$fileName)
			filePath <- file.path(downloadLocation, uniqueFileName)
		} else {
			stop(sprintf("Unexpected value for ifcollision: %s.  Allowed settings are 'overwrite.local', 'keep.local', 'keep.both'", ifcollision))
		}
	}
	copySuccess<-file.copy(downloadResult$downloadedFile, filePath, overwrite=TRUE)
	if (!copySuccess) stop(sprintf("Failed to copy %s to %s.", downloadResult$downloadedFile, filePath))
	addToCacheMap(fileHandleId, filePath)
	unlink(downloadResult$downloadedFile)
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






