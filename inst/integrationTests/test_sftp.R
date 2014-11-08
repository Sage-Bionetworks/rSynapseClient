# Round-trip test of sftp upload/download
# 
# Author: brucehoff
###############################################################################
library(Rssh)

.setUp <- function() {
  ## create a project to fill with entities
  project <- createEntity(Project())
  synapseClient:::.setCache("testProject", project)
}

.tearDown <- function() {
  ## delete the test projects
  deleteEntity(synapseClient:::.getCache("testProject"))
}



integrationTestSFTPRoundTrip <- function() {
  project<-synapseClient:::.getCache("testProject")
  projectId<-propertyValue(project, "id")
  
  # create the upload destination setting
  euds<-synapseClient:::ExternalUploadDestinationSetting()
  euds@url<-URLencode("sftp://ec2-54-212-85-156.us-west-2.compute.amazonaws.com/rClientIntegrationTest")
  euds@supportsSubfolders<-TRUE
  euds@concreteType<-"org.sagebionetworks.repo.model.project.ExternalUploadDestinationSetting"
  euds@uploadType<-"SFTP"
  euds@banner<-"*** A BIG ANNOUNCEMENT ***"
  
  uds<-synapseClient:::UploadDestinationListSetting()
  uds@projectId<-projectId
  uds@settingsType<-"upload"
  uds@concreteType<-"org.sagebionetworks.repo.model.project.UploadDestinationListSetting"
  uds@destinations<-synapseClient:::UploadDestinationSettingList(euds)
  
  response<-synRestPOST("/projectSettings", synapseClient:::createListFromS4Object(uds))
  
  uds<-synapseClient:::createS4ObjectFromList(response, "UploadDestinationListSetting")
  
  testFile<-createFile()
  originalMD5<-tools::md5sum(testFile)
  # Note, we purposely put some non-URL-safe text in the file name
  file<-File(testFile, name="test file.txt", parentId=projectId)
  
  file<-synStore(file)
  
  checkEquals("org.sagebionetworks.repo.model.file.ExternalFileHandle", file@fileHandle$concreteType)
  externalURL<-file@fileHandle$externalURL
  # TODO check that the URL in the external file handle starts with euds@url
  
  fileEntityId<-propertyValue(file, "id")
  
  retreived<-synGet(fileEntityIddownloadLocation=tempdir())
  checkEquals(tools::md5sum(retrieved@filePath), originalMD5)
  
  #udsResponse<-synRestGET(sprintf("/entity/%s/uploadDestinations", projectId), endpoint=synapseFileServiceEndpoint())
  #uploadDestinations<-synapseClient:::createTypedListFromList(udsResponse$list, "UploadDestinationList")
  
  # TODO complete the next line to delete the remote file using the Rssh package
  # TODO a better approach is to clean it up in 'tearDown'
  sftpDeleteFile(host, username, password, remotepath)
  
  # TODO test saving a revision of the file
  # testing that you can retrieve either revision
  
  # TODO test the case in which the upload destination is not the first in the list
  
  
  # This is not strictly necessary since we delete the whole project in tearDown
  # but it does check that deletion works on the the project settings
  synRestDELETE(sprintf("/projectSettings/%s", uds@id))
  
}