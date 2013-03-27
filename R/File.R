
setClass(
  Class = "File",
  representation = representation(
    # fields:
    # metadata:  the Meta-data part of the File (the "entity")
    metadata = "Entity",
    # filePath: full path to local file. Before an "external" file is created in Synapse, this is the external URL
    filePath = "character",
    # synapseStore: logical T if file is stored in Synapse, F if only the url is stored
    synapseStore = "logical",
    # fileHandle (generated from JSON schema, null before entity is created)
    fileHandle = "list"
  ),
  prototype = prototype(
    metadata = NULL,
    filePath = NULL,
    synapseStore = NULL,
    fileHandle = NULL
  )
)

# if name is available, use it, else use the file name part of the file path
getNameFromParamOrFile<-function(nameParam, filePathParam) {
  if (!missing(nameParam) && !is.null(nameParam)) {
    nameParam
  }
  if (missing(filePathParam) || is.null(filePathParam)) stop("File path required.")
  basename(filePathParam)
}

##
## File contructor: path="/path/to/file", synapseStore=T, name="foo", parentId="syn101", ...
## TODO: can also take: obj=<obj ref>, synapseStore=T, name="foo", parentId="syn101", ...
##
setMethod(
  f = "File",
  signature = signature("character", "logical", "character", "character"),
  definition = function(filePathParam, synapseStoreParam, nameParam, parentIdParam, ...){
    file <- new("File")
    parentId<-NULL
    if (!missing("parentIdParam")) parentId=parentIdParam
    file@metadata<-FileEntity(name=getNameFromParamOrFile(nameParam, filePathParam), parentId=parentId)
    #TODO put '...' as annotations on file@metadata
    file@filePath <- filePathParam
    file@synapseStore <- synapseStoreParam
    file@fileHandle<-NULL
    file
  }
)

isExternalFileHandle<-function(fileHandle) {fileHandle$concreteType=="ExternalFileHandle"}

# TODO:  shall we allow 'synapseStore' to be switched in an existing File?
# ensure that fileHandle info is consistent with synapseUpload field
validdateFile<-function(file) {
  if (is.null(file@fileHandle)) return
  isExternal <- isExternalFileHandle(file@fileHandle)
  if (isExternal & synapseStore) stop("synapseStore==T but file is external")
  if (isExternal==FALSE & synapseStore==FALSE) stop("synapseStore=F but file is not external")
}

lastModifiedTimestamp<-function(filePath) {
  file.info(filePath)$mtime
}

addToCacheMap<-function(fileHandleID, filePath, timestamp=NULL) {
  if (is.null(fileHandleID)) stop("fileHandleID is required")
  if (is.null(filePath)) stop("filePath is required")
  if (is.null(timestamp)) timestamp<-lastModifiedTimestamp(filePath)
  # TODO: add record for fileHandleID->{filePath,timestamp}
}

# is local file UNchanged
# given a FileHandleID and a file path
# returns TRUE if there is an entry in the Cache Map for the FileHandleID and the File Path
# and the 'last modified" timestamp for the file equals the value in the Cache Map
# returns FALSE if the timestamps differ OR if there is no entry in the Cache Map
localFileUnchanged<-function(fileHandleID, filePath) {
  cacheMapEntry<-getCacheMapEntry(fileHandleID, filePath)
  !is.null(cacheMapEntry) && lastModifiedTimestamp(filePath)==cacheMapEntry$lastModified
}


synStore <- function(file, used=NULL, executed=NULL, activityName=NULL, activityDescription=NULL, createOrUpdate=T, forceVersion=T){
  if (is.null(file@fileHandle)) { # if there's no existing Synapse File associated with this object...
    if (is.null(file@filePath)) { # ... and there's no local file staged for upload ...
      stop("filePath is required") # ... then we reject the call to 'synStore'
    } else { # ... we are storing a new file
      if (file@synapseStore) { # ... we are storing a new file which we are also uploading
        lastModified<-lastModifiedTimestamp(file@filePath)
        fileHandle<-synapseUploadToFileHandle(file@filePath)
        if (lastModified!=lastModifiedTimestamp(file@filePath)) stop(sprintf("During upload, %s was modified by another process.", file@filePath))
        addToCacheMap(fileHandle$id, file@filePath, lastModified)
      } else { # ... we are storing a new file which we are linking, but not uploading
        # link external URL in Synapse, get back fileHandle	
        fileName <- basename(file@filePath)
        mimeTypeMap<-getMimeTypeForFile(fileName)
        fileHandle <- synapseLinkExternalFile(file@filePath, fileName, contentType)
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
        # TODO this duplicates four lines above.  Consolidate them?
        lastModified<-lastModifiedTimestamp(file@filePath)
        fileHandle<-synapseUploadToFileHandle(file@filePath)
        if (lastModified!=lastModifiedTimestamp(file@filePath)) stop(sprintf("During upload, %s was modified by another process.", file@filePath))
        addToCacheMap(fileHandle$id, file@filePath, lastModified)
        file@fileHandle<-fileHandle
      }
    } else { # synapseStore==F
      # file is considered 'read only' and will not be uploaded
    }
  }
  # Now save the metadata
  # TODO take care of the provenance info
  activity<-NULL
  if (used!=NULL || executed!=NULL || activityName!=NULL || activityDescription==NULL) {
    # create the provenance record and put in entity
  }
  propertyValue(file@metadata, "dataFileHandleId")<-file@fileHandle$id
  if (is.null(propertyValue(file@metadata, "id"))) {
    storedMetadata<-createEntity(file@metadata) #  TODO createOrUpdate
  } else {
    storedMetadata<-updateEntityMethod(file@metdata, forceVersion)
  }
  file
}


synGet<-function(downloadFile=T, downloadLocation=NULL, ifcollision="keep.both", load=T) {
  # first, get the metadata and the fileHandle
  # if fileHandle is external
  #		synapseStore<-F 
  #	else 
  #		synapseStore<-T
  #		
  # if (downloadFile==T)
  #		if downloadLocation==NULL
  #			get md5 from fileHandle and look in RO cache location
  #			if (RO Cache file exists)
  #				nothing to download
  #			else
  #				download to RO Cache location
  #			filePath <- NULL (Alternatively, file path could be the path to the cached location)
  #		else
  #			if filePath exists already
  #				if md5 of local file matches that in fileHandle
  #					filePath <- downloadLocation
  #				else
  #					if ifcollision=="overwrite"
  #						download file from Synapse to downloadLocation
  #						filePath <- downloadLocation
  #					else if ifcollision=="keep.original"
  #						filePath <- downloadLocation
  #					else if ifcollision=="keep.both"
  #						distinctFilePath <- generate new file name in the given location
  #						download file from Synapse to distinctFilePath
  #						filePath <- distinctFilePath
  #					else
  #						raise error
  #			else (filePath does not exist)
  #				download file from Synapse to downloadLocation
  #				filePath <- downloadLocation
  # else (downloadFile==F)
  #		filePath <- url from fileHandle (could be web-hosted URL or file:// on network file share)
  # 
  # if (load==TRUE)
  #		figure out path to local or remote file
  #		source into workspace (different from binary, .R, or text data file)
}

