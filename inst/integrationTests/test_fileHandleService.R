
# this tests the file services underlying storeEntity
integrationTestFileHandle <-
  function()
{
    # upload a file and receive the file handle
    filePath<- system.file("NAMESPACE", package = "synapseClient")
    fileHandle<-synapseClient:::synapseUploadToFileHandle(filePath)
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