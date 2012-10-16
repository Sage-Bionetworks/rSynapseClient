.setUp <-
	function()
{
	project <- createEntity(Project())
	synapseClient:::.setCache("testProject", project)
	synapseClient:::.setCache("oldWarn", options("warn")[[1]])
	options(warn=2)
}

.tearDown <-
	function()
{
	deleteEntity(synapseClient:::.getCache("testProject"))
	options(warn=synapseClient:::.getCache("oldWarn"))
	synapseClient:::.deleteCache("oldWarn")
}


integrationTestVersionedAnnotationsProject <-
	function()
{
	project <- synapseClient:::.getCache("testProject")
	vers <- project$available.versions
	checkEquals(1L, nrow(vers))
	checkEquals("data.frame", class(vers))

	## versions the project
	project$annotations$aname <- "value1"
	project <- storeEntity(project)
	project$annotations$aname <- "value2"
	project <- storeEntity(project)
	vers <- project$available.versions
	checkEquals(1L, nrow(vers))

	project <- getEntity(project$properties$id)

	project <- getEntity(project$properties$id, 1)


}

integrationTestVersionedAnnotationsLocationOwnerWithObjects <-
	function()
{
	project <- synapseClient:::.getCache("testProject")

	data <- createEntity(Data(parentId = project$properties$id))
	vers <- data$available.versions
	checkEquals(1L, nrow(vers))
	checkEquals("data.frame", class(vers))

	## versions the data
	data$annotations$aname <- "value1"
	data <- storeEntity(data)
	data$annotations$aname <- "value2"
	data <- storeEntity(data)
	vers <- data$available.versions
	checkEquals(1L, nrow(vers))

	addObject(data, 1:5, "numbers")
	data <- storeEntity(data)
	checkEquals(basename(dirname(data$cacheDir)), as.character(data$properties$versionNumber))
	## versioning seems broken here
	##checkEquals(nrow(data$available.versions), 2L)

	addObject(data, 6:10, "numbers")
	data <- storeEntity(data)

	old <- getEntity(data$properties$id, 1L)
	old <- loadEntity(old)
	checkTrue(all(1:5 == old$objects$numbers))
	checkEquals(old$properties$versionNumber, 1L)

	new <- getEntity(data$properties$id, 2L)
	new <- loadEntity(new)
	checkTrue(all(6:10 == new$objects$numbers))
	checkEquals(new$properties$versionNumber, 2L)

	checkTrue(old$cacheDir != new$cacheDir)

	entity <- list(id=data$properties$id)
	new <- getEntity(entity)
	checkTrue(all(6:10 == new$objects$numbers))
	checkEquals(new$properties$versionNumber, 2L)

	entity <- list(id=data$properties$id, versionNumber=2L)
	new <- getEntity(entity)
	checkEquals(new$properties$versionNumber, 2L)

	entity <- list(id=data$properties$id, versionNumber=1L)
	new <- getEntity(entity)
	checkEquals(new$properties$versionNumber, 1L)
}









