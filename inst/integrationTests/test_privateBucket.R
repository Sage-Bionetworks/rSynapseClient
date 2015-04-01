# Integration tests for private S3 bucket
# 
# Author: bhoff
###############################################################################

.setUp <- function() {
	## create a project to fill with entities
	project <- createEntity(Project())
	synapseClient:::.setCache("testProject", project)
	projectId<-propertyValue(project, "id")
	
	sl<-new("ExternalS3StorageLocationSetting")
	sl@bucket<-"rclient-test-private-bucket.sagebase.org"
	sl@concreteType<-"org.sagebionetworks.repo.model.project.ExternalS3StorageLocationSetting"
	sl@uploadType<-"S3"
	sl@banner<-"*** A BIG ANNOUNCEMENT ***"
	response<-synRestPOST("/storageLocation", synapseClient:::createListFromS4Object(sl))
	sl<-synapseClient:::createS4ObjectFromList(response, "ExternalStorageLocationSetting")
	
	uds<-synapseClient:::UploadDestinationListSetting()
	uds@projectId<-projectId
	uds@settingsType<-"upload"
	uds@concreteType<-"org.sagebionetworks.repo.model.project.UploadDestinationListSetting"
	uds@locations<-c(sl@storageLocationId)
	
	response<-synRestPOST("/projectSettings", synapseClient:::createListFromS4Object(uds))
	
	uds<-synapseClient:::createS4ObjectFromList(response, "UploadDestinationListSetting")	
	synapseClient:::.setCache("testProjectSettings", uds)
}

.tearDown <- function() {
	projectSettings<-synapseClient:::.getCache("testProjectSettings")
	synRestDELETE(sprintf("/projectSettings/%s", projectSettings@id))
	## delete the test project
	deleteEntity(synapseClient:::.getCache("testProject"))
}


integrationTestPrivateBucketRoundTrip <- function() {
	project<-synapseClient:::.getCache("testProject")
	projectId<-propertyValue(project, "id")
	# upload to private bucket
	filePath<- tempfile()
	connection<-file(filePath)
	writeChar(sprintf("this is a test %s", sample(999999999, 1)), connection, eos=NULL)
	close(connection)  
	originalMd5<-as.character(tools::md5sum(filePath))
	file<-File(path=filePath(), parentId=projectId)
	file<-synStore(file)
	# TODO how can I check that it was uploaded to the right place??
	checkEquals(storageLocationId, file@fileHandle$storageLocationId)
	
	# download file
	file<-synGet(propertyValue(file, "id"))
	checkEquals(originalMd5, as.character(tools::md5sum(getFileLocation(file))))
	
	# update file
	
}