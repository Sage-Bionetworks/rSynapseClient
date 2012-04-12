#  This tests entityToFileCache, the utility that downloads an Entity and its Annotations to a local cache

.setUp <- 
		function()
{
	## create a project
	project <- new(Class="Project")
	propertyValues(project) <- list(
			name = paste("myProject", gsub(':', '_', date()))
	)
	project <- createEntity(project)
	synapseClient:::.setCache("testProject", project)
	
}

.tearDown <-
		function()
{
	deleteEntity(synapseClient:::.getCache("testProject"))
	synapseClient:::.deleteCache("testProject")
	
	unlink(synapseCacheDir(), recursive=T)

}


integrationTestEntitytoFileCache <-
		function()
{
	# note, the repo' service endpoint is set up by the framework that calls the interation test suite
			
	testCacheRoot <- paste(tempdir(), ".synapseCache", sep="/")
	if (file.exists(testCacheRoot)) unlink(testCacheRoot, recursive=TRUE)
	.setCache("cacheRoot", testCacheRoot)
	
	testProject <- synapseClient:::.getCache("testProject")
	entityId <- getProperty(testProject, "id")
	synapseEntityToFileCache(entityId)
	
	checkTrue(!is.null(getEntityFromFileCache(entityId)))
	checkTrue(!is.null(getEntityAnnotationsFromFileCache(entityId)))
}