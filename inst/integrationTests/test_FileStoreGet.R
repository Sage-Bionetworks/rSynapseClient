#
# 
#

.setUp <- function() {
  ## create a project to fill with entities
  project <- createEntity(Project())
  synapseClient:::.setCache("testProject", project)
}

.tearDown <- function() {
  ## delete the test project
  deleteEntity(synapseClient:::.getCache("testProject"))
}

integrationTestCacheMapRoundTrip <- function() {
  fileHandleId<-"TEST_FHID"
  filePath<- system.file("NAMESPACE", package = "synapseClient")
  filePath2<- system.file("DESCRIPTION", package = "synapseClient")
  
  synapseClient:::addToCacheMap(fileHandleId, filePath)
  synapseClient:::addToCacheMap(fileHandleId, filePath2)
  content<-synapseClient:::getCacheMapFileContent(fileHandleId)
  checkEquals(2, length(content))
  checkTrue(any(filePath==names(content)))
  checkTrue(any(filePath2==names(content)))
  checkEquals(as.character(file.info(filePath)$mtime), synapseClient:::getFromCacheMap(fileHandleId, filePath))
  checkEquals(as.character(file.info(filePath2)$mtime), synapseClient:::getFromCacheMap(fileHandleId, filePath2))
  checkTrue(synapseClient:::localFileUnchanged(fileHandleId, filePath))
  checkTrue(synapseClient:::localFileUnchanged(fileHandleId, filePath2))
  
  # now clean up
  unlink(synapseClient:::defaultDownloadLocation(fileHandleId), recursive=TRUE)
}

integrationTestMetadataRoundTrip <- function() {
  # create a Project
  project <- synapseClient:::.getCache("testProject")
  checkTrue(!is.null(project))
  
  # create a file to be uploaded
  filePath<- system.file("NAMESPACE", package = "synapseClient")
  synapseStore<-TRUE
  file<-File(filePath, synapseStore, parentId=propertyValue(project, "id"))
  checkTrue(!is.null(propertyValue(file, "name")))
  checkEquals(propertyValue(project, "id"), propertyValue(file, "parentId"))
  
  # now store it
  storedFile<-synStore(file)

  metadataOnly<-synGet(propertyValue(storedFile, "id"),downloadFile=F)
  metadataOnly<-synapseClient:::synAnnotSetMethod(metadataOnly, "annot", "value")
  storedMetadata<-synStore(metadataOnly, forceVersion=F) # TODO check with forceVersion=T
  
  checkEquals("value", synapseClient:::synAnnotGetMethod(storedMetadata, "annot"))
}

#
# This code exercises the file services underlying upload/download to/from an entity
#
integrationTestRoundtrip <- function()
{
  # create a Project
  project <- synapseClient:::.getCache("testProject")
  checkTrue(!is.null(project))
  
  # create a file to be uploaded
  filePath<- system.file("NAMESPACE", package = "synapseClient")
  synapseStore<-TRUE
  file<-File(filePath, synapseStore, parentId=propertyValue(project, "id"))
  checkTrue(!is.null(propertyValue(file, "name")))
  checkEquals(propertyValue(project, "id"), propertyValue(file, "parentId"))
  
  # now store it
  storedFile<-synStore(file)
  
  # check that it worked
  checkTrue(!is.null(storedFile))
  id<-propertyValue(storedFile, "id")
  checkTrue(!is.null(id))
  checkEquals(propertyValue(project, "id"), propertyValue(storedFile, "parentId"))
  checkEquals(propertyValue(file, "name"), propertyValue(storedFile, "name"))
  checkEquals(filePath, storedFile@filePath)
  checkEquals(synapseStore, storedFile@synapseStore)
  
  # check that cachemap entry exists
  cachePath<-sprintf("%s/.cacheMap", synapseClient:::defaultDownloadLocation(storedFile@fileHandle$id))
  checkTrue(file.exists(cachePath))
  
  downloadedFile<-synGet(id)
  checkEquals(id, propertyValue(downloadedFile, "id"))
  checkEquals(propertyValue(project, "id"), propertyValue(downloadedFile, "parentId"))
  checkEquals(synapseStore, downloadedFile@synapseStore)
  checkTrue(!is.null(downloadedFile@filePath))
  
  # compare MD-5 checksum of filePath and downloadedFile@filePath
  origChecksum<- as.character(tools::md5sum(filePath))
  downloadedChecksum <- as.character(tools::md5sum(downloadedFile@filePath))
  checkEquals(origChecksum, downloadedChecksum)
  
  checkEquals(storedFile@fileHandle, downloadedFile@fileHandle)
  
  # delete the file
  deleteEntity(downloadedFile)
  # clean up downloaded file
  handleUri<-sprintf("/fileHandle/%s", storedFile@fileHandle$id)
  synapseClient:::synapseDelete(handleUri, service="FILE")
  
  # clean up cache
  file.remove(downloadedFile@filePath)
  file.remove(sprintf("%s/.cacheMap", dirname(downloadedFile@filePath)))
  file.remove(dirname(downloadedFile@filePath))
}


# test that legacy *Entity based methods work on File objects
integrationTestAddToNewFILEEntity <-
  function()
{
  project <- synapseClient:::.getCache("testProject")
  filePath<- system.file("NAMESPACE", package = "synapseClient")
  file<-File(list(parentId=propertyValue(project, "id")))
  file<-addFile(file, filePath)
  storedFile<-storeEntity(file)
  
  checkTrue(!is.null(storedFile))
  id<-propertyValue(storedFile, "id")
  checkTrue(!is.null(id))
  checkEquals(propertyValue(project, "id"), propertyValue(storedFile, "parentId"))
  checkEquals(propertyValue(file, "name"), propertyValue(storedFile, "name"))
  checkEquals(filePath, storedFile@filePath)
  checkEquals(TRUE, storedFile@synapseStore) # this is the default
  checkTrue(!is.null(propertyValue(storedFile, "dataFileHandleId")))
  
  gotEntity<-getEntity(storedFile) # get metadata, don't download file
  
  checkTrue(!is.null(gotEntity))
  id<-propertyValue(gotEntity, "id")
  checkTrue(!is.null(id))
  checkEquals(propertyValue(project, "id"), propertyValue(gotEntity, "parentId"))
  checkEquals(propertyValue(file, "name"), propertyValue(gotEntity, "name"))
  checkTrue(!is.null(propertyValue(gotEntity, "dataFileHandleId")))
  checkTrue(length(gotEntity@filePath)==0) # empty since it hasn't been downloaded
  
  # test update of metadata
  annotValue(gotEntity, "foo")<-"bar"
  updatedEntity<-updateEntity(gotEntity)
  gotEntity<-getEntity(updatedEntity)
  checkEquals("bar", annotValue(gotEntity, "foo"))
  
  downloadedFile<-downloadEntity(id)
  checkEquals(id, propertyValue(downloadedFile, "id"))
  checkEquals(propertyValue(project, "id"), propertyValue(downloadedFile, "parentId"))
  checkEquals(TRUE, downloadedFile@synapseStore) # this is the default
  checkTrue(!is.null(downloadedFile@filePath))
  
  # compare MD-5 checksum of filePath and downloadedFile@filePath
  origChecksum<- as.character(tools::md5sum(filePath))
  downloadedChecksum <- as.character(tools::md5sum(downloadedFile@filePath))
  checkEquals(origChecksum, downloadedChecksum)
  
  checkEquals(storedFile@fileHandle, downloadedFile@fileHandle)
  
  # check that downloading a second time doesn't retrieve again
  timeStamp<-synapseClient:::lastModifiedTimestamp(downloadedFile@filePath)
  Sys.sleep(1.0)
  downloadedFile<-downloadEntity(id)
  checkEquals(timeStamp, synapseClient:::lastModifiedTimestamp(downloadedFile@filePath))
 
  # delete the file
  deleteEntity(downloadedFile)
  # clean up downloaded file
  handleUri<-sprintf("/fileHandle/%s", storedFile@fileHandle$id)
  synapseClient:::synapseDelete(handleUri, service="FILE")
  
  # clean up cache
  file.remove(downloadedFile@filePath)
  file.remove(sprintf("%s/.cacheMap", dirname(downloadedFile@filePath)))
  file.remove(dirname(downloadedFile@filePath))
  
}

# test that legacy *Entity based methods work on File objects, cont.
integrationTestReplaceFile<-function() {
    project <- synapseClient:::.getCache("testProject")
    filePath<- system.file("NAMESPACE", package = "synapseClient")
    file<-File(list(parentId=propertyValue(project, "id")))
    file<-addFile(file, filePath)
    # replace storeEntity with createEntity
    storedFile<-createEntity(file)
    
    # now getEntity, add a different file, store, retrieve
    gotEntity<-getEntity(storedFile) # get metadata, don't download file
    newFile<-system.file("DESCRIPTION", package = "synapseClient")
    gotEntity<-addFile(gotEntity, newFile)
    newStoredFile<-storeEntity(gotEntity)
    
    downloadedFile<-downloadEntity(newStoredFile)
 
    # compare MD-5 checksum of filePath and downloadedFile@filePath
    origChecksum<- as.character(tools::md5sum(newFile))
    downloadedChecksum <- as.character(tools::md5sum(downloadedFile@filePath))
    checkEquals(origChecksum, downloadedChecksum)
    
    checkEquals(newStoredFile@fileHandle, downloadedFile@fileHandle)
    
    # delete the file
    deleteEntity(downloadedFile)
    # clean up downloaded file
    handleUri<-sprintf("/fileHandle/%s", newStoredFile@fileHandle$id)
    synapseClient:::synapseDelete(handleUri, service="FILE")
    
    # clean up cache
    file.remove(downloadedFile@filePath)
    file.remove(sprintf("%s/.cacheMap", dirname(downloadedFile@filePath)))
    file.remove(dirname(downloadedFile@filePath))
}



integrationTestLoadEntity<-function() {
  project <- synapseClient:::.getCache("testProject")
  filePath<- system.file("NAMESPACE", package = "synapseClient")
  file<-File(list(parentId=propertyValue(project, "id")))
  dataObject<-list(a="A", b="B", c="C")
  file<-addObject(file, dataObject, "dataObjectName")
  storedFile<-createEntity(file)
  
  loadedEntity<-loadEntity(propertyValue(storedFile, "id"))
  
  checkEquals(dataObject, getObject(loadedEntity, "dataObjectName"))
  
  # can load from an entity as well as from an ID
  loadedEntity2<-loadEntity(storedFile)
  checkEquals(dataObject, getObject(loadedEntity2, "dataObjectName"))
  
  # delete the file
  deleteEntity(loadedEntity)
  # clean up downloaded file
  handleUri<-sprintf("/fileHandle/%s", loadedEntity2@fileHandle$id)
  synapseClient:::synapseDelete(handleUri, service="FILE")
  
  # clean up cache
  file.remove(loadedEntity@filePath)
  file.remove(sprintf("%s/.cacheMap", dirname(loadedEntity@filePath)))
  file.remove(dirname(loadedEntity2@filePath))
}

# first pass
# TODO test synGet of existing File, (1) to default location, (2) to existing location, (3) to new location
# TODO test synStore of retrieved entity
# TODO test storage/retrieval of provenance info, incl. two files having the same activity
#			what should the default behavior be for 'synStore' if prov' info exists but is not specified:
#			leave existing info intact or clear info?
# TODO test governance restriction (unfulfilled access requirement)
# TODO test retrieval of specific version
# TODO test serialization of binary / deserialization ("load=TRUE")

# Second pass
# TODO test update of existing File/Folder
# TODO test automatic/explicit revision
# TODO test synGet / synStore of metadata only (i.e. downloadFile==FALSE)
# TODO test synGet / synStore of external link
# TODO test three different 'ifcollision' modes


