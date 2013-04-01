#
# class and method definitions for File
#

setClass(
  Class = "File",
  representation = representation(
    # fields:
    # metadata
    metadata = "Entity",
    # filePath: full path to local file. Before an "external" file is created in Synapse, this is the external URL
    filePath = "character",
    # synapseStore: logical T if file is stored in Synapse, F if only the url is stored
    synapseStore = "logical",
    # fileHandle (generated from JSON schema, empty before entity is created)
    fileHandle = "list"
  )
)

fileConstructorMethod<-function(filePathParam, synapseStoreParam, ...) {
  file <- new("File")
  #TODO need to separate out properties from annotations
  entityParams<-modifyList(list(name=basename(filePathParam)), list(...))
  file@metadata<-FileEntity(entityParams)
  file@filePath <- filePathParam
  file@synapseStore <- synapseStoreParam
  file
}
##
## File contructor: path="/path/to/file", synapseStore=T, name="foo", ...
## TODO: can also take: obj=<obj ref>, synapseStore=T, ...
##
setMethod(
  f = "File",
  signature = signature("character", "logical"),
  definition = fileConstructorMethod
)

isExternalFileHandle<-function(fileHandle) {fileHandle$concreteType=="ExternalFileHandle"}
fileHasFileHandleId<-function(file) {!is.null(file@fileHandle$id)}
fileHasFilePath<-function(file) {length(file@filePath)>0}

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

# TODO test content, including that record key is file path
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
  for (filePath in names(mapForFileHandleId)) {
    if (filePath==filePath) return(mapForFileHandleId[[filePath]]) # i.e. return time stamp
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
  record[[filePath]]<-as.character(timestamp)
  mapForFileHandleId<-modifyList(mapForFileHandleId, list(record))
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
  !is.null(downloadedTimestamp) && lastModifiedTimestamp(filePath)==downloadedTimestamp
}

uploadAndAddToCacheMap<-function(filePath) {
  lastModified<-lastModifiedTimestamp(filePath)
  fileHandle<-synapseUploadToFileHandle(filePath)
  if (lastModified!=lastModifiedTimestamp(filePath)) stop(sprintf("During upload, %s was modified by another process.", filePath))
  addToCacheMap(fileHandle$id, filePath, lastModified)
  fileHandle
}

# TODO:  synStore must work for Entity and Record as well as File
synStore <- function(file, used=NULL, executed=NULL, activityName=NULL, activityDescription=NULL, createOrUpdate=T, forceVersion=T){
  if (!fileHasFileHandleId(file)) { # if there's no existing Synapse File associated with this object...
    if (!fileHasFilePath(file)) { # ... and there's no local file staged for upload ...
      stop("filePath is required") # ... then we reject the call to 'synStore'
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
    }
    #	save fileHandle in slot, put id in entity properties
    file@fileHandle <- fileHandle
  } else { # fileHandle is not null, i.e. we're updating a Synapse File that's already created
    #	if fileHandle is inconsistent with filePath or synapseStore, raise an exception 
    validdateFile(file)
    if (file@synapseStore) {
      if (is.null(file@filePath) || localFileUnchanged(file@fileHandle$id, file@filePath)) {
        # since local file matches Synapse file (or was not actually retrieved) nothing to store
      } else {
        #	load file into Synapse, get back fileHandle (save in slot, put id in properties)
        fileHandle<-uploadAndAddToCacheMap(file@filePath)
        file@fileHandle<-fileHandle
      }
    } else { # synapseStore==F
      # file is considered 'read only' and will not be uploaded
    }
  }
  # Now save the metadata
  # TODO take care of the provenance info
  activity<-NULL
  if (!is.null(used) || !is.null(executed)) {
    # create the provenance record and put in entity
  }
  propertyValue(file@metadata, "dataFileHandleId")<-file@fileHandle$id
  if (is.null(propertyValue(file@metadata, "id"))) {
    storedMetadata<-createEntity(file@metadata) #  TODO createOrUpdate
  } else {
    storedMetadata<-updateEntityMethod(file@metdata, forceVersion)
  }
  file@metadata<-storedMetadata
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
  # first, get the metadata and the fileHandle
  if (is.null(version)) {
    metadata<-getEntity(id)
  } else {
    metadata<-getEntity(id, version=version)
  }
  fileHandleId<-propertyValue(metadata, "dataFileHandleId")
  if (is.null(fileHandleId)) stop(sprintf("Entity %s (version %s) is missing its FileHandleId", id, version))
  handleUri<-sprintf("/fileHandle/%s", fileHandleId)
  fileHandle<-synapseGet(handleUri, service="FILE")	

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
          filePath <- generateUniqueFileName(downloadLocation, fileHandle$fileName)
  				#download file from Synapse to distinct filePath
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
  file<-File(filePath, synapseStore, NULL, NULL)
  file@metadata<-metadata
  file@fileHandle<-fileHandle
  if (load) {
    if(is.null(filePath)) {
      if (!isExternalURL(fileHandle) && !downloadFile) {
        stop("Cannot load Synapse file which has not been downloaded.  Please call synGet again, with downloadFile=T.")
      }
      # shouldn't reach this statement.  something has gone very wrong.
      stop(sprintf("Cannot load file %s, there is no local file path.", fileHandle$fileName))
    }
    # TODO source into workspace (different for binary, .R, or text data file)
  }
  file
}

