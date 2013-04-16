
# this tests the file services underlying storeEntity
integrationTestFileHandle <-
  function()
{
    # upload a file and receive the file handle
    filePath<- tempfile()
    connection<-file(filePath)
    writeChar("this is a test", connection, eos=NULL)
    close(connection)  
    fileHandle<-synapseClient:::synapseUploadToFileHandle(filePath)
    checkEquals(basename(filePath), fileHandle$fileName)
    checkEquals(synapseClient:::getMimeTypeForFile(basename(filePath)), fileHandle$contentType)
    # now try to retrieve the file handle given the id
    handleUri<-sprintf("/fileHandle/%s", fileHandle$id)
    fileHandle2<-synapseClient:::synapseGet(handleUri, service="FILE")
    checkEquals(fileHandle, fileHandle2)
    # now delete the handle
    synapseClient:::synapseDelete(handleUri, service="FILE")
    # now we should not be able to get the handle
    fileHandle3<-synapseClient:::synapseGet(handleUri, service="FILE", checkHttpStatus=F)
    checkEquals("The resource you are attempting to access cannot be found", fileHandle3$reason)
}

integrationTestExternalFileHandle <- function() {
  externalURL<-"http://google.com"
  fileName<-"testFile"
  contentType<-"text/html"
  fileHandle <- synapseClient:::synapseLinkExternalFile(externalURL, fileName, contentType)
  checkTrue(!is.null(fileHandle$id))
  checkEquals("org.sagebionetworks.repo.model.file.ExternalFileHandle", fileHandle$concreteType)
  checkEquals(externalURL, fileHandle$externalURL)
  checkEquals(fileName, fileHandle$fileName)
  checkEquals(contentType, fileHandle$contentType)
}