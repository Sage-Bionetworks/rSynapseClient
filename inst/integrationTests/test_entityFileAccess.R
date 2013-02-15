
#
# This code exercises file upload/download to/from an entity
# Ultimately we may change this to use public methods to manipulate
# files for entities
#
integrationTestEntityFileAccess <-
  function()
{
  # Note we cannot enable this integration test until the wiki service is on Staging
  if (FALSE) {
    # create a Project
    project<-Project()
    project<-createEntity(project)
    
    # create a file attachment which will be used in the wiki page
    # upload a file and receive the file handle
    fileName<-"NAMESPACE"
    filePath<- system.file(fileName, package = "synapseClient")
    fileHandle<-synapseClient:::synapseUploadToFileHandle(filePath)
    
    # create an entity with the file
    entity <- list(
      entityType="org.sagebionetworks.repo.model.FileEntity", # doesn't work for 'Data'
      name="foo", 
      parentId=propertyValue(project, "id"), 
      dataFileHandleId=fileHandle$id)
    entity <- synapseClient:::synapsePost("/entity", entity)
            
    # download the file
    # /entity/{enityId}/file
    downloadUri<-sprintf("/entity/%s/file", entity$id)
    # download into a temp file
    downloadedFile<-synapseClient:::synapseDownloadFromRepoService(downloadUri)
    origChecksum<- as.character(tools::md5sum(filePath))
    downloadedChecksum <- as.character(tools::md5sum(downloadedFile))
    checkEquals(origChecksum, downloadedChecksum)
    
    # delete the entity
    deleteEntity(entity$id)
    
#    # Now delete the wiki page
#    #/{ownertObjectType}/{ownerObjectId}/wiki/{wikiId}
#    synapseClient:::synapseDelete(wikiUri)
    
    # delete the file handle
    handleUri<-sprintf("/fileHandle/%s", fileHandle$id)
    synapseClient:::synapseDelete(handleUri, service="FILE")
    
    handleUri<-sprintf("/fileHandle/%s", fileHandle$id)
    synapseClient:::synapseDelete(handleUri, service="FILE")
  }
}