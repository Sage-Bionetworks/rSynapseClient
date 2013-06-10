#
#  integration tests for services around Wikis
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

createFile<-function() {
  filePath<- tempfile()
  fileName<-basename(filePath)
  connection<-file(filePath)
  writeChar("this is a test", connection, eos=NULL)
  close(connection) 
  filePath
}



#
# this tests the file services underlying the wiki CRUD for entities
#
integrationTestWikiService <-
  function()
{
    project <- synapseClient:::.getCache("testProject")
    checkTrue(!is.null(project))
    
    # create a file attachment which will be used in the wiki page
    filePath<-createFile()
    fileName<-basename(filePath)
    fileHandle<-synapseClient:::chunkedUploadFile(filePath)
    
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
}

# This repeats the basic CRUD of the previous test but using WikiPage, synStore, etc.
integrationTestWikiCRUD <-
  function()
{
  project <- synapseClient:::.getCache("testProject")
  checkTrue(!is.null(project))
  
  # create file attachments which will be used in the wiki page
  filePath1<-createFile()
  filePath2<-createFile()
  
  wikiPage<-WikiPage(
    parent=project, 
    title="wiki title", 
    markdown="some stuff", 
    attachments=list(filePath1, filePath2)
  )
  
  wikiPage<-synStore(wikiPage)
    
  # see if we can get the wiki from its parent
  wikiPage2<-synGetWiki(project)
  
  checkEquals(wikiPage, wikiPage2)
  
  # check that fileHandle is in the wiki
  fileHandleIds<-propertyValue(wikiPage2, "attachmentFileHandleIds")
  checkEquals(2, length(fileHandleIds))
  
  # Now delete the wiki page
  #/{ownertObjectType}/{ownerObjectId}/wiki/{wikiId}
  synDelete(wikiPage2)
  
  # delete the file handles
  for (fileHandleId in fileHandleIds) {
    handleUri<-sprintf("/fileHandle/%s", fileHandleId)
    synapseClient:::synapseDelete(handleUri, endpoint=synapseFileServiceEndpoint())
  }
}