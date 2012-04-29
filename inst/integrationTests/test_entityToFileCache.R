#  This tests entityToFileCache, the utility that downloads an Entity and its Annotations to a local cache

##require(RJSONIO)

.setUp <- 
		function()
{
	#synapseClient:::.setCache("debug", TRUE)

	sessionToken<-synapseClient:::sessionToken()
	if (is.null(synapseClient:::sessionToken)) stop("Failed to log in")
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
	synapseClient:::getEntityFromSynapse(entityId)

	checkTrue(!is.null(synapseClient:::getEntityFromFileCache(entityId)))
}

integrationTestGetAnnotations <- function()
{
	testProject <- synapseClient:::.getCache("testProject")
	entityId <- testProject$id
	if (is.null(entityId)) stop("Failed to get entity id from object")
	synapseClient:::getAnnotationsFromSynapse(entityId)

	checkTrue(!is.null(synapseClient:::getAnnotationsFromFileCache(entityId)))
}

integrationTestCreateUpdateDeleteEntity <- function() {
	# create a new Entity
	entityName<-paste("testName_", gsub(':', '_', date()))
	entityType<-"org.sagebionetworks.repo.model.Project"
	s <- SynapseEntity(list(name=entityName, entityType=entityType))
	s <- synapseClient:::doCreateEntity(s)
	id <- s@properties[["id"]]
	checkTrue(!is.null(id))
	# get the entity, to the file cache
	fileDir <- synapseClient:::.getAbsoluteFileCachePath(synapseClient:::.entityFileCachePath(id))
	filePath <- paste(fileDir, "entity.json", sep="/")
	checkTrue(!file.exists(filePath))
	synapseClient:::getEntityFromSynapse(id)
	# make sure it's there
	checkTrue(file.exists(filePath))
	# pull from the file cache
	entity<-synapseClient:::getEntityFromFileCache(id)
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
    synapseClient:::updateEntityInFileCache(entity)
	# make sure the file has been updated
	checkEquals(newName, synapseClient:::getEntityFromFileCache(id)@properties[["name"]])
	# push update to Synapse
    synapseClient:::updateEntityInSynapse(id)
	# let's hold on to the original file...
	origFilePath <- paste(fileDir, "origEntity.json", sep="/")
	file.rename(filePath, origFilePath)
	# get the entity and make sure it's updated
	synapseClient:::getEntityFromSynapse(id)
	entity<-synapseClient:::getEntityFromFileCache(id)
	checkTrue(!is.null(entity))
	# check the contents
	checkEquals(id, entity@properties[["id"]])
	checkEquals(entityType, entity@properties[["entityType"]])
	checkTrue(eTag != entity@properties[["etag"]])
	checkEquals(newName, synapseClient:::getEntityFromFileCache(id)@properties[["name"]])
	# delete the entity
    synapseClient:::deleteEntityFromSynapse(id)
	# make sure it's deleted.  GETting the id should now create a 404 http status (Not Found)
	errorMessage <- try(synapseClient:::getEntityFromSynapse(id))
	checkTrue(any(grep("404", errorMessage)) || typeof(errorMessage)=='try-error')
}

integrationTestCreateUpdateDeleteAnnotations <- function() {
	# create a new Entity
	entityName<-paste("annotTestName_", gsub(':', '_', date()))
	entityType<-"org.sagebionetworks.repo.model.Project"
	s <- SynapseEntity(list(name=entityName, entityType=entityType))
	s <- synapseClient:::doCreateEntity(s)
	id <- s@properties[["id"]]
	checkTrue(!is.null(id))

	# get the annotations, to the file cache
	fileDir <- synapseClient:::.getAbsoluteFileCachePath(synapseClient:::.entityFileCachePath(id))
	filePath <- paste(fileDir, "annotations.json", sep="/")
	checkTrue(!file.exists(filePath))
	synapseClient:::getAnnotationsFromSynapse(id)
	# make sure it's there
	checkTrue(file.exists(filePath))
	# pull from the file cache
	annots<-synapseClient:::getAnnotationsFromFileCache(id)
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

    synapseClient:::updateAnnotationsInFileCache(annots)
	# make sure the file has been updated

	fooList <- annotValue(synapseClient:::getAnnotationsFromFileCache(id), "foo")
	checkTrue(!is.null(fooList))
	checkEquals(2, length(fooList))
	checkEquals(newArray, fooList)
	# push update to Synapse
    synapseClient:::updateAnnotationsInSynapse(id)
	# let's hold on to the original file...
	origFilePath <- paste(fileDir, "origAnnotations.json", sep="/")
	file.rename(filePath, origFilePath)
	# get the annotations and make sure it's updated
	synapseClient:::getAnnotationsFromSynapse(id)
	annots<-synapseClient:::getAnnotationsFromFileCache(id)
	checkTrue(!is.null(annots))
	# check the contents
	checkEquals(id, propertyValue(annots, "id"))
	fooList <- annotValue(annots, "foo") # NOTE: This reflects a change in the deser behavior:  Annot lists are not deserialized!
	checkTrue(!is.null(fooList))
	checkEquals(2, length(fooList))
	checkEquals(newArray, fooList)
	checkTrue(eTag != propertyValue(annots, "etag"))

	# delete the entity (i.e. to delete the annotations we delete the parent entity
    synapseClient:::deleteEntityFromSynapse(id)
	# make sure it's deleted.  GETting the id should now create a 404 http status (Not Found)
	errorMessage <- try(synapseClient:::getEntityFromSynapse(id))
	checkTrue(any(grep("404", errorMessage)) || typeof(errorMessage)=='try-error')
}


integrationTestCRUDConvenienceMethods <- function() {
	# create a new SynapseEntity in memory
	entityName<-paste("convenienceTestName_", gsub(':', '_', date()))
	entityType<-"org.sagebionetworks.repo.model.Project"
	s <- SynapseEntity(list(name=entityName, entityType=entityType))
	fooList<-c("bar", "bas")
	annotValue(s, "foo")<-fooList
	
	s<-synapseClient:::createSynapseEntity(s)
	id<-propertyValue(s, "id")

	checkTrue(!is.null(id))

	fileDir <- synapseClient:::.getAbsoluteFileCachePath(synapseClient:::.entityFileCachePath(id))
	filePath <- paste(fileDir, "entity.json", sep="/")
	# make sure file is cached 
	checkTrue(file.exists(filePath))

	# check the contents
	eTag <- propertyValue(s, "etag")
	checkTrue(!is.null(eTag))
	checkEquals(entityName, propertyValue(s, "name"))
	checkEquals(entityType, propertyValue(s, "entityType"))
	checkEquals(fooList, annotValue(s, "foo"))
	
	# update the entity in the file cache
	newName <- paste(entityName, "modified")
	propertyValue(s, "name")<-newName
	newFooList<-c("incan", "monkey-god")
	annotValue(s, "foo")<-newFooList
	
	revisedEntity<-synapseClient:::updateSynapseEntity(s)
	
	# make sure the file has been updated
	checkEquals(newName, synapseClient:::getEntityFromFileCache(id)@properties[["name"]])

	# get the entity and make sure it's updated
	revisedEntity<-synapseClient:::getSynapseEntity(id)

	checkTrue(!is.null(revisedEntity))
	# check the contents
	checkEquals(id, propertyValue(revisedEntity, "id"))
	checkEquals(entityType, propertyValue(revisedEntity, "entityType"))
	checkTrue(eTag != propertyValue(revisedEntity, "etag"))
	checkEquals(newName, propertyValue(revisedEntity, "name"))
	checkEquals(newFooList, annotValue(s, "foo"))
	
	# delete the entity
    synapseClient:::deleteSynapseEntity(id)
	# make sure it's deleted.  GETting the id should now create a 404 http status (Not Found)
	errorMessage <- try(synapseClient:::getEntityFromSynapse(id))
	checkTrue(any(grep("404", errorMessage)) || typeof(errorMessage)=='try-error')
}

