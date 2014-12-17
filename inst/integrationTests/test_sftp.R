# Round-trip test of sftp upload/download
# 
# Author: brucehoff
###############################################################################
library(Rsftp)

.setUp <- function() {
  ## create a project to fill with entities
  # Note:  we add white space to test URL encoding, below
  project <- createEntity(Project(name=sprintf("test_sftp %s", sample(1000,1))))
  synapseClient:::.setCache("testProject", project)
}

.tearDown <- function() {
  ## delete the test projects
  deleteEntity(synapseClient:::.getCache("testProject"))
  
  sftpFilesToDelete<-synapseClient:::.getCache("sftpFilesToDelete")
  host<-synapseClient:::.getCache("test_sftp_host")
  if (!is.null(sftpFilesToDelete) && length(sftpFilesToDelete)>0 && !is.null(host)) {
    credentials<-synapseClient:::.getCache(sprintf("sftp://%s_credentials", host))
    username<-credentials$username
    password<-credentials$password
    for (path in sftpFilesToDelete) {
      success<-sftpDeleteFile(host, username, password, path)
      # delete directories too
      if (!success) message(sprintf("test_sftp.tearDown: Unable to delete %s", path))
      dirsToDelete<-synapseClient:::getDirectorySequence(dirname(path))
      for (dir in dirsToDelete[length(dirsToDelete):1]) {
        sftpRemoveDirectory(host, username, password, dir)
      }
    }
  }
  synapseClient:::.setCache("sftpFilesToDelete", NULL)
}

createFile<-function(content, filePath) {
  if (missing(content)) content<-"this is a test"
  if (missing(filePath)) filePath<- tempfile()
  connection<-file(filePath)
  writeChar(content, connection, eos=NULL)
  close(connection)  
  filePath
}

scheduleExternalURLForDeletion<-function(externalURL) {
  toDelete<-URLdecode(synapseClient:::.ParsedUrl(externalURL)@path)
  key<-"sftpFilesToDelete"
  filesToDelete<-synapseClient:::.getCache(key)
  if (is.null(filesToDelete)) {
    synapseClient:::.setCache(key,toDelete)
  } else {
    synapseClient:::.setCache(key,c(filesToDelete,toDelete))
  }
}

createSFTPUploadSettings<-function(projectId) {
  euds<-synapseClient:::ExternalUploadDestinationSetting()
  euds@url<-URLencode("sftp://ec2-54-212-85-156.us-west-2.compute.amazonaws.com/rClientIntegrationTest")
  euds@supportsSubfolders<-TRUE
  euds@concreteType<-"org.sagebionetworks.repo.model.project.ExternalUploadDestinationSetting"
  euds@uploadType<-"SFTP"
  euds@banner<-"*** A BIG ANNOUNCEMENT ***"
  
  euds2<-synapseClient:::ExternalUploadDestinationSetting()
  euds2@url<-URLencode("sftp://some.other.host.com/root")
  euds2@supportsSubfolders<-TRUE
  euds2@concreteType<-"org.sagebionetworks.repo.model.project.ExternalUploadDestinationSetting"
  euds2@uploadType<-"SFTP"
  euds2@banner<-"*** This is not a real host ***"
  
  uds<-synapseClient:::UploadDestinationListSetting()
  uds@projectId<-projectId
  uds@settingsType<-"upload"
  uds@concreteType<-"org.sagebionetworks.repo.model.project.UploadDestinationListSetting"
  uds@destinations<-synapseClient:::UploadDestinationSettingList(euds, euds2)
  
  response<-synRestPOST("/projectSettings", synapseClient:::createListFromS4Object(uds))
  
  uds<-synapseClient:::createS4ObjectFromList(response, "UploadDestinationListSetting")
}

integrationTestSFTPRoundTrip <- function() {
  # NOTE:  The following values must be set up external to the test suite
  host<-synapseClient:::.getCache("test_sftp_host")
  credentials<-synapseClient:::.getCache(sprintf("sftp://%s_credentials", host))
  username<-credentials$username
  password<-credentials$password
  
  project<-synapseClient:::.getCache("testProject")
  projectId<-propertyValue(project, "id")
  
  # create the upload destination setting
  uds<-createSFTPUploadSettings(projectId)
  
  testFile<-createFile()
  originalMD5<-tools::md5sum(testFile)
  names(originalMD5)<-NULL
  file<-File(testFile, name="testfile.txt", parentId=projectId)
  
  file<-synStore(file)
  
  checkTrue(!is.null(file@fileHandle$id))
  
  scheduleExternalURLForDeletion(file@fileHandle$externalURL)
  
  checkEquals("org.sagebionetworks.repo.model.file.ExternalFileHandle", file@fileHandle$concreteType)
  externalURL<-file@fileHandle$externalURL
  # check that the URL is URL encoded (i.e. that the " " is now "%20")
  checkTrue(grepl("test_sftp%20", externalURL))
  
  urlDecoded<-URLdecode(externalURL)
  # check that it starts with our sftp server
  checkTrue(grepl(sprintf("^%s", "sftp://ec2-54-212-85-156.us-west-2.compute.amazonaws.com/rClientIntegrationTest"), urlDecoded))
  # check that it ends with our file name
  checkTrue(grepl(sprintf("%s$", basename(testFile)), urlDecoded))

  fileEntityId<-propertyValue(file, "id")
  
  # delete the file cache record to force re-download
  unlink(synapseClient:::defaultDownloadLocation(file@fileHandle$id), recursive=TRUE)
  retrieved<-synGet(fileEntityId)
  downloadedMD5<-tools::md5sum(retrieved@filePath)
  names(downloadedMD5)<-NULL
  checkEquals(downloadedMD5, originalMD5)
  
  # change the retrieved file and 'synStore' it 
  # Our file time stamps have 1-sec accuracy, so we have to sleep for 1 sec to ensure the mtime changes
  Sys.sleep(1.1)
  createFile("some modified content", retrieved@filePath)
  checkTrue(!synapseClient:::localFileUnchanged(retrieved@fileHandle$id, retrieved@filePath))
  updated<-synStore(retrieved)
  scheduleExternalURLForDeletion(updated@fileHandle$externalURL)
  # check that there's a new version and a new URL
  checkTrue(updated@fileHandle$id!=retrieved@fileHandle$id)
  checkTrue(updated@fileHandle$externalURL!=retrieved@fileHandle$externalURL)
  checkEquals(2, propertyValue(updated, "versionNumber"))
  
  # This is not strictly necessary since we delete the whole project in tearDown
  # but it does check that deletion works on the the project settings
  synRestDELETE(sprintf("/projectSettings/%s", uds@id))
}


integrationTestMoveSFTPFileToS3Container<-function() {
  project<-synapseClient:::.getCache("testProject")
  projectId<-propertyValue(project, "id")
  
  # give the project a non-S3 upload destination
  uds<-createSFTPUploadSettings(projectId)
  
  # create an SFTP Synapse file
  testFile<-createFile()
  file<-File(testFile, name="testfile.txt", parentId=projectId)
  file<-synStore(file)
  scheduleExternalURLForDeletion(file@fileHandle$externalURL)
  
  # now remove the project settings, causing the project to revert to using S3
  synRestDELETE(sprintf("/projectSettings/%s", uds@id))  
  
  # check that exception occurs when synStore is called
  # change the file and 'synStore' it 
  createFile("some modified content", file@filePath)
  result<-try(synStore(file), silent=TRUE)
  checkEquals(class(result), "try-error")
}
