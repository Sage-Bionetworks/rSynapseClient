#
# this tests the file services underlying the wiki CRUD for entities
#
integrationTestWikiService <-
  function()
{
    # create a Project
    project<-Project()
    project<-createEntity(project)
    
    # create a file attachment which will be used in the wiki page
    # upload a file and receive the file handle
    filePath<- tempfile()
    fileName<-basename(filePath)
    connection<-file(filePath)
    writeChar("this is a test", connection, eos=NULL)
    close(connection)  
  
    fileHandle<-synapseClient:::synapseUploadToFileHandle(filePath)
    
    # create a wiki page
    wikiContent<-list(title="wiki title", markdown="some stuff", attachmentFileHandleIds=list(fileHandle$id))
    # /{ownertObjectType}/{ownerObjectId}/wiki
    ownerUri<-sprintf("/entity/%s/wiki", propertyValue(project, "id"))
    wiki<-synapseClient:::synapsePost(ownerUri, wikiContent)
    
    # see if we can get the wiki from its ID
    wikiUri<-sprintf("%s/%s", ownerUri, wiki$id)
    wiki2<-synapseClient:::synapseGet(wikiUri)
    checkEquals(wiki, wiki2)
    
    # check that fileHandle is in the wiki
    checkEquals(fileHandle$id, wiki2$attachmentFileHandleIds[1])
    
    # get the file handles
    # /{ownerObjectType}/{ownerObjectId}/wiki/{wikiId}/attachmenthandles
    fileHandles<-synapseClient:::synapseGet(sprintf("%s/attachmenthandles", wikiUri))
    checkEquals(fileHandle, fileHandles$list[[1]])
    
    # download the raw file attachment
    # /{ownerObjectType}/{ownerObjectId}/wiki/{wikiId}/attachment?fileName={attachmentFileName}
    downloadUri<-sprintf("%s/attachment?fileName=%s", wikiUri, fileName)
    # download into a temp file
    downloadedFile<-synapseClient:::synapseDownloadFromRepoServiceToDestination(downloadUri)
    origChecksum<- as.character(tools::md5sum(filePath))
    downloadedChecksum <- as.character(tools::md5sum(downloadedFile))
    checkEquals(origChecksum, downloadedChecksum)
    
    # Now delete the wiki page
    #/{ownertObjectType}/{ownerObjectId}/wiki/{wikiId}
    synapseClient:::synapseDelete(wikiUri)
    
    # delete the file handle
    handleUri<-sprintf("/fileHandle/%s", fileHandle$id)
    synapseClient:::synapseDelete(handleUri, endpoint=synapseFileServiceEndpoint())
    
    # Finally, delete the Project
    deleteEntity(project)
}