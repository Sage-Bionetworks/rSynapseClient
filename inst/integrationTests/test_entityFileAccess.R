
#
# This code exercises the file services underlying upload/download to/from an entity
#
integrationTestEntityFileAccess <-
  function()
{
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
    downloadedFile<-synapseClient:::synapseDownloadFromRepoServiceToDestination(downloadUri)
    origChecksum<- as.character(tools::md5sum(filePath))
    downloadedChecksum <- as.character(tools::md5sum(downloadedFile))
    checkEquals(origChecksum, downloadedChecksum)
    
    # delete the entity
    deleteEntity(entity$id)
    
    
    # delete the file handle
    handleUri<-sprintf("/fileHandle/%s", fileHandle$id)
    synapseClient:::synapseDelete(handleUri, service="FILE")
    
    handleUri<-sprintf("/fileHandle/%s", fileHandle$id)
    synapseClient:::synapseDelete(handleUri, service="FILE")
}