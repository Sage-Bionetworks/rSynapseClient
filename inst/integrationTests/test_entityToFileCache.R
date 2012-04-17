#  This tests entityToFileCache, the utility that downloads an Entity and its Annotations to a local cache

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
	synapseClient:::.setCache("cacheRoot", testCacheRoot)
	
	
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