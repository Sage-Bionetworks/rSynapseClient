#  This tests entityToFileCache, the utility that downloads an Entity and its Annotations to a local cache

require(RJSONIO)

.setUp <- 
		function()
{
	#synapseClient:::.setCache("debug", TRUE)
	
	sessionToken<-sessionToken()
	if (is.null(sessionToken)) stop("Failed to log in")
	# note, the repo' service endpoint is set up by the framework that calls the interation test suite
	
	# now create a Project
	entityList <- list(
			name = paste("myProject", gsub(':', '_', date())),
			entityType = "org.sagebionetworks.repo.model.Project"
	)
	entityList <- synapseClient:::.synapsePostPut("/entity", entityList, "POST")
	
	# save the created entity for use in the test(s)
	synapseClient:::.setCache("testProject", entityList)
	
	# set up the file cache
	testCacheRoot <- paste(tempdir(), ".synapseCache", sep="/")
	if (file.exists(testCacheRoot)) unlink(testCacheRoot, recursive=TRUE)
	
	synapseCacheDir(testCacheRoot)

}

.tearDown <-
		function()
{
	entityList <- synapseClient:::.getCache("testProject")
	synapseClient:::.synapseGetDelete(paste("/entity", entityList$id, sep="/"), "DELETE")
	
	unlink(synapseCacheDir(), recursive=T)
}


integrationTestGetEntity <- function()
{
	testProject <- synapseClient:::.getCache("testProject")
	entityId <- testProject$id
	if (is.null(entityId)) stop("Failed to get entity id from object")
	getEntityFromSynapse(entityId)
	
	checkTrue(!is.null(getEntityFromFileCache(entityId)))
}

integrationTestGetAnnotations <- function()
{
	testProject <- synapseClient:::.getCache("testProject")
	entityId <- testProject$id
	if (is.null(entityId)) stop("Failed to get entity id from object")
	getAnnotationsFromSynapse(entityId)
	
	checkTrue(!is.null(getAnnotationsFromFileCache(entityId)))
}

integrationTestCreateUpdateDeleteEntity <- function() {
	# create a new Entity
	entityName<-paste("testName_", gsub(':', '_', date()))
	entityType<-"org.sagebionetworks.repo.model.Project"
	s <- SynapseEntity(list(name=entityName, entityType=entityType))
	s <- createEntity(s)
	id <- s@properties[["id"]]
	checkTrue(!is.null(id))
	# get the entity, to the file cache
	fileDir <- synapseClient:::.getAbsoluteFileCachePath(synapseClient:::.entityFileCachePath(id))
	filePath <- paste(fileDir, "entity.json", sep="/")
	checkTrue(!file.exists(filePath))
	getEntityFromSynapse(id)
	# make sure it's there
	checkTrue(file.exists(filePath))
	# pull from the file cache
	entity<-getEntityFromFileCache(id)
	checkTrue(!is.null(entity))
	# check the contents
	eTag <- entity@properties[["etag"]]
	checkTrue(!is.null(eTag))
	checkEquals(id, entity@properties[["id"]])
	checkEquals(entityName, entity@properties[["name"]])
	checkEquals(entityType, entity@properties[["entityType"]])
	# update the entity in the file cache
	newName <- paste(entityName, "modified")
	entity@properties[["name"]]<-newName
	updateEntityInFileCache(entity)
	# make sure the file has been updated
	checkEquals(newName, getEntityFromFileCache(id)@properties[["name"]])
	# push update to Synapse
	updateEntityInSynapse(id)
	# let's hold on to the original file...
	origFilePath <- paste(fileDir, "origEntity.json", sep="/")
	file.rename(filePath, origFilePath)
	# get the entity and make sure it's updated
	getEntityFromSynapse(id)
	entity<-getEntityFromFileCache(id)
	checkTrue(!is.null(entity))
	# check the contents
	checkEquals(id, entity@properties[["id"]])
	checkEquals(entityType, entity@properties[["entityType"]])
	checkTrue(eTag != entity@properties[["etag"]])
	checkEquals(newName, getEntityFromFileCache(id)@properties[["name"]])
	# delete the entity
	deleteEntityFromSynapse(id)
	# make sure it's deleted.  GETting the id should now create a 404 http status (Not Found)
	errorMessage <- try(getEntityFromSynapse(id))
	checkTrue(any(grep("404", errorMessage)) || typeof(errorMessage)=='try-error')
}

integrationTestCreateUpdateDeleteAnnotations <- function() {
	# create a new Entity
	entityName<-paste("annotTestName_", gsub(':', '_', date()))
	entityType<-"org.sagebionetworks.repo.model.Project"
	s <- SynapseEntity(list(name=entityName, entityType=entityType))
	s <- createEntity(s)
	id <- s@properties[["id"]]
	checkTrue(!is.null(id))
	
	# get the annotations, to the file cache
	fileDir <- synapseClient:::.getAbsoluteFileCachePath(synapseClient:::.entityFileCachePath(id))
	filePath <- paste(fileDir, "annotations.json", sep="/")
	checkTrue(!file.exists(filePath))
	getAnnotationsFromSynapse(id)
	# make sure it's there
	checkTrue(file.exists(filePath))
	# pull from the file cache
	annots<-getAnnotationsFromFileCache(id)
	checkTrue(!is.null(annots))
	# check the contents
	eTag <- propertyValue(annots, "etag")
	checkTrue(!is.null(eTag))
	checkEquals(id, propertyValue(annots, "id"))
	fooList<-annotValue(annots, "foo")
	checkTrue(is.null(fooList))
	
	# update the annotations in the file cache
	newArray <- c("bas1", "bas2")
	annotValue(annots, "foo") <- newArray

	updateAnnotationsInFileCache(annots)
	# make sure the file has been updated

	fooList <- annotValue(getAnnotationsFromFileCache(id), "foo")
	checkTrue(!is.null(fooList))
	checkEquals(2, length(fooList))
	checkEquals(newArray, fooList)
	# push update to Synapse
	updateAnnotationsInSynapse(id)
	# let's hold on to the original file...
	origFilePath <- paste(fileDir, "origAnnotations.json", sep="/")
	file.rename(filePath, origFilePath)
	# get the annotations and make sure it's updated
	getAnnotationsFromSynapse(id)
	annots<-getAnnotationsFromFileCache(id)
	checkTrue(!is.null(annots))
	# check the contents
	checkEquals(id, propertyValue(annots, "id"))
	fooList <- annotValue(annots, "foo")
	checkTrue(!is.null(fooList))
	checkEquals(2, length(fooList))
	checkEquals(newArray, fooList)
	checkTrue(eTag != propertyValue(annots, "etag"))
	
	# delete the entity (i.e. to delete the annotations we delete the parent entity
	deleteEntityFromSynapse(id)
	# make sure it's deleted.  GETting the id should now create a 404 http status (Not Found)
	errorMessage <- try(getEntityFromSynapse(id))
	checkTrue(any(grep("404", errorMessage)) || typeof(errorMessage)=='try-error')
}

