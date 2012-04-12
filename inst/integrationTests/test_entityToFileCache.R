#  This tests entityToFileCache, the utility that downloads an Entity and its Annotations to a local cache

.setUp <- 
		function()
{
	
	entityList <- list(
			name = paste("myProject", gsub(':', '_', date())),
			entityType = "org.sagebionetworks.repo.model.Project"
	)
	entityList <- synapseClient:::.synapsePostPut("/entity", entityList, "POST")
	
	synapseClient:::.setCache("testProject", entityList)
	
}

.tearDown <-
		function()
{
	entityList <- synapseClient:::.getCache("testProject")
	synapseClient:::.synapseGetDelete(paste("/entity", entityList$id, sep="/"), "DELETE")
	
	unlink(synapseCacheDir(), recursive=T)
}


integrationTestEntitytoFileCache <-
		function()
{
	
	sessionToken<-sessionToken()
	if (is.null(sessionToken)) stop("Failed to log in")
	# note, the repo' service endpoint is set up by the framework that calls the interation test suite
			
	testCacheRoot <- paste(tempdir(), ".synapseCache", sep="/")
	if (file.exists(testCacheRoot)) unlink(testCacheRoot, recursive=TRUE)
	synapseClient:::.setCache("cacheRoot", testCacheRoot)
	
	testProject <- synapseClient:::.getCache("testProject")
	entityId <- testProject$id
	if (is.null(entityId)) stop("Failed to get entity id from object")
	synapseEntityToFileCache(entityId)
	
	checkTrue(!is.null(getEntityFromFileCache(entityId)))
	checkTrue(!is.null(getEntityAnnotationsFromFileCache(entityId)))
}