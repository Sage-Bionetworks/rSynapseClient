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
}

#
# This code exercises the file services underlying upload/download to/from an entity
#
integrationTestRoundtrip <-
  function()
{
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
  
  # check that it worked
  checkTrue(!is.null(storedFile))
  id<-propertyValue(storedFile, "id")
  checkTrue(!is.null(id))
  checkEquals(propertyValue(project, "id"), propertyValue(storedFile, "parentId"))
  checkEquals(propertyValue(file, "name"), propertyValue(storedFile, "name"))
  checkEquals(filePath, storedFile@filePath)
  checkEquals(synapseStore, storedFile@synapseStore)
  
  # TODO: check that cachemap entry exists
  
  downloadedFile<-synGet(id)
  checkEquals(id, propertyValue(downloadedFile, "id"))
  checkEquals(propertyValue(project, "id"), propertyValue(downloadedFile, "parentId"))
  checkEquals(synapseStore, downloadedFile@synapseStore)
  checkTrue(!is.null(downloadedFile@filePath))
  
  # compare MD-5 checksum of filePath and downloadedFile@filePath
  origChecksum<- as.character(tools::md5sum(filePath))
  downloadedChecksum <- as.character(tools::md5sum(downloadedFile@filePath))
  checkEquals(origChecksum, downloadedChecksum)
  
  checkEquals(storedFile@fileHandle, downloadedFile@fileHandle)
  
  # delete the file
  deleteEntity(downloadedFile)
  # clean up downloaded file
  handleUri<-sprintf("/fileHandle/%s", storedFile@fileHandle$id)
  synapseClient:::synapseDelete(handleUri, service="FILE")
}

# first pass
# TODO test synGet of existing File, (1) to default location, (2) to existing location, (3) to new location
# TODO test synStore of retrieved entity
# TODO test storage/retrieval of provenance info, incl. two files having the same activity
#			what should the default behavior be for 'synStore' if prov' info exists but is not specified:
#			leave existing info intact or clear info?
# TODO test governance restriction
# TODO test retrieval of specific version
# TODO test serialization of binary / deserialization ("load=TRUE")

# Second pass
# TODO test update of existing File/Folder
# TODO test automatic/explicit revision
# TODO test synGet / synStore of metadata only (i.e. downloadFile==FALSE)
# TODO test synGet / synStore of external link
# TODO test three different 'ifcollision' modes


