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
  
  ## in the case that we have 'mocked' hasUnfulfilledAccessRequirements, this restores the original function
  if (!is.null(synapseClient:::.getCache("hasUnfulfilledAccessRequirementsIsOverRidden"))) {
    assignInNamespace("hasUnfulfilledAccessRequirements", attr(synapseClient:::hasUnfulfilledAccessRequirements, "origDef"), "synapseClient")
    synapseClient:::.setCache("hasUnfulfilledAccessRequirementsIsOverRidden", NULL)
  }
  
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
  checkEquals(synapseClient:::.formatAsISO8601(file.info(filePath)$mtime), synapseClient:::getFromCacheMap(fileHandleId, filePath))
  checkEquals(synapseClient:::.formatAsISO8601(file.info(filePath2)$mtime), synapseClient:::getFromCacheMap(fileHandleId, filePath2))
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
  storedMetadata<-synStore(metadataOnly, forceVersion=F)
  
  checkEquals("value", synapseClient:::synAnnotGetMethod(storedMetadata, "annot"))
  
  checkEquals(1, propertyValue(metadataOnly, "versionNumber"))
  
  # now store again, but force a version update
  storedMetadata<-synStore(storedMetadata) # default is forceVersion=T
  
  retrievedMetadata<-synGet(propertyValue(storedFile, "id"),downloadFile=F)
  checkEquals(2, propertyValue(retrievedMetadata, "versionNumber"))
  
  # of course we should still be able to get the original version
  originalVersion<-synGet(propertyValue(storedFile, "id"), version=1, downloadFile=F)
  checkEquals(1, propertyValue(originalVersion, "versionNumber"))
  # ...whether or not we download the file
  originalVersion<-synGet(propertyValue(storedFile, "id"), version=1, downloadFile=T)
  checkEquals(1, propertyValue(originalVersion, "versionNumber"))
  
}

integrationTestGovernanceRestriction <- function() {
  project <- synapseClient:::.getCache("testProject")
  checkTrue(!is.null(project))
  
  # create a File
  filePath<- tempfile()
  file.copy(system.file("NAMESPACE", package = "synapseClient"), filePath)
  synapseStore<-TRUE
  file<-File(filePath, synapseStore, parentId=propertyValue(project, "id"))
  storedFile<-synStore(file)
  id<-propertyValue(storedFile, "id")
  
  # mock Governance restriction
  myHasUnfulfilledAccessRequirements<-function(id) {TRUE} # return TRUE, i.e. yes, there are unfulfilled access requiremens
  attr(myHasUnfulfilledAccessRequirements, "origDef") <- synapseClient:::hasUnfulfilledAccessRequirements
  assignInNamespace("hasUnfulfilledAccessRequirements", myHasUnfulfilledAccessRequirements, "synapseClient")
  synapseClient:::.setCache("hasUnfulfilledAccessRequirementsIsOverRidden", TRUE)
  
  # try synGet with downloadFile=F, load=F, should be OK
  synGet(id, downloadFile=F, load=F)
  
  # try synGet with downloadFile=T, should NOT be OK
  result<-try(synGet(id, downloadFile=T, load=F))
  checkEquals("try-error", class(result))
  
  # try synGet with load=T, should NOT be OK
  result<-try(synGet(id, downloadFile=F, load=T))
  checkEquals("try-error", class(result))

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
  filePath<- tempfile()
  file.copy(system.file("NAMESPACE", package = "synapseClient"), filePath)
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
  fileHandleId<-storedFile@fileHandle$id
  cachePath<-sprintf("%s/.cacheMap", synapseClient:::defaultDownloadLocation(fileHandleId))
  checkTrue(file.exists(cachePath))
  modifiedTimeStamp<-synapseClient:::getFromCacheMap(fileHandleId, filePath)
  checkTrue(!is.null(modifiedTimeStamp))
  
  # now download it.  This will pull a new copy into the cache
  downloadedFile<-synGet(id)
  downloadedFilePathInCache<-downloadedFile@filePath
  checkEquals(id, propertyValue(downloadedFile, "id"))
  checkEquals(propertyValue(project, "id"), propertyValue(downloadedFile, "parentId"))
  checkEquals(synapseStore, downloadedFile@synapseStore)
  checkTrue(!is.null(downloadedFile@filePath))
  
  # compare MD-5 checksum of filePath and downloadedFile@filePath
  origChecksum<- as.character(tools::md5sum(filePath))
  downloadedChecksum <- as.character(tools::md5sum(downloadedFile@filePath))
  checkEquals(origChecksum, downloadedChecksum)
  
  checkEquals(storedFile@fileHandle, downloadedFile@fileHandle)
  
  # test synStore of retrieved entity, no change to file
  modifiedTimeStamp<-synapseClient:::getFromCacheMap(fileHandleId, downloadedFilePathInCache)
  checkTrue(!is.null(modifiedTimeStamp))
  Sys.sleep(1.0)
  updatedFile <-synStore(downloadedFile, forceVersion=F)
  # the file handle should be the same
  checkEquals(fileHandleId, propertyValue(updatedFile, "dataFileHandleId"))
  # there should be no change in the time stamp.
  checkEquals(modifiedTimeStamp, synapseClient:::getFromCacheMap(fileHandleId, downloadedFilePathInCache))
  # we are still on version 1
  checkEquals(1, propertyValue(updatedFile, "versionNumber"))

  #  test synStore of retrieved entity, after changing file
  # modify the file
  connection<-file(downloadedFilePathInCache)
  result<-paste(readLines(connection), collapse="\n")
  close(connection)
  connection<-file(downloadedFilePathInCache)
  writeLines(result, connection)
  close(connection)  
  # check that we indeed modified the time stamp on the file
  newTimestamp<-synapseClient:::.formatAsISO8601(synapseClient:::lastModifiedTimestamp(downloadedFilePathInCache))
  checkTrue(newTimestamp!=modifiedTimeStamp)
  
  updatedFile2 <-synStore(updatedFile, forceVersion=F)
  # fileHandleId is changed
  checkTrue(fileHandleId!=propertyValue(updatedFile2, "dataFileHandleId"))
  # we are now on version 2
  checkEquals(2, propertyValue(updatedFile2, "versionNumber"))
  
  # of course we should still be able to get the original version
  originalVersion<-synGet(propertyValue(storedFile, "id"), version=1, downloadFile=F)
  checkEquals(1, propertyValue(originalVersion, "versionNumber"))
  # ...whether or not we download the file
  originalVersion<-synGet(propertyValue(storedFile, "id"), version=1, downloadFile=T)
  checkEquals(1, propertyValue(originalVersion, "versionNumber"))
  
  
  # delete the file
  deleteEntity(downloadedFile)
  # clean up downloaded file
  handleUri<-sprintf("/fileHandle/%s", storedFile@fileHandle$id)
  synapseClient:::synapseDelete(handleUri, service="FILE")
  handleUri<-sprintf("/fileHandle/%s", updatedFile2@fileHandle$id)
  synapseClient:::synapseDelete(handleUri, service="FILE")

  # clean up cache
  unlink(dirname(downloadedFile@filePath), recursive=TRUE)
  unlink(dirname(updatedFile2@filePath), recursive=TRUE)
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

