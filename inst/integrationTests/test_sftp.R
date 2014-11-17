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

createFile<-function(content, filePath) {
  if (missing(content)) content<-"this is a test"
  if (missing(filePath)) filePath<- tempfile()
  connection<-file(filePath)
  writeChar(content, connection, eos=NULL)
  close(connection)  
  filePath
}

integrationTestSFTPRoundTrip <- function() {
  # NOTE:  The following values must be set up external to the test suite
  host<-synapseClient:::.getCache("test_sftp_host")
  username<-synapseClient:::.getCache("test_sftp_username")
  password<-synapseClient:::.getCache("test_sftp_password")
  
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
  # TODO why does externalURL end with the actual file name rather than the one we gave it four lines above?
  # TODO check that the URL in the external file handle starts with euds@url
  # TODO check that the URL is URL encoded (i.e. that the " " is now "%20")
  
  fileEntityId<-propertyValue(file, "id")
  
  retrieved<-synGet(fileEntityId, downloadLocation=tempdir())
  checkEquals(tools::md5sum(retrieved@filePath), originalMD5)
  
  # TODO change the retrieved file and 'synstore' it 
  # TODO check that there's a new version and a new URL
  
  #udsResponse<-synRestGET(sprintf("/entity/%s/uploadDestinations", projectId), endpoint=synapseFileServiceEndpoint())
  #uploadDestinations<-synapseClient:::createTypedListFromList(udsResponse$list, "UploadDestinationList")
  
  # TODO complete the next line to delete the remote file using the Rssh package
  # TODO a better approach is to clean it up in 'tearDown'
  remotepath<-synapseClient:::.ParsedUrl(externalURL)@path
  checkTrue(sftpDeleteFile(host, username, password, remotepath))
  
  # TODO test saving a revision of the file
  # testing that you can retrieve either revision
  
  # TODO test the case in which the upload destination is not the first in the list
  
  
  # This is not strictly necessary since we delete the whole project in tearDown
  # but it does check that deletion works on the the project settings
  synRestDELETE(sprintf("/projectSettings/%s", uds@id))
  
}

# TODO
integrationTestChangeContainer<-function() {
  # create a regular Synapse file
  # now move it into a folder having a non-S3 upload destination
  # call synStore
}