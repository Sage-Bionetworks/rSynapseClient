.setUp <-
  function()
{
  ### create a project
  project <- createEntity(Project())
  synapseClient:::.setCache("testProject", project)
}

.tearDown <-
  function()
{
  deleteEntity(synapseClient:::.getCache("testProject"))
  synapseClient:::.deleteCache("testProject")
  synapseClient:::.deleteCache("testData")
}

integrationTestGetEntityByIdExistingFileCache <-
  function()
{
  file <- tempfile()
  cat("THIS IS A TEST", file=file)
  project <- synapseClient:::.getCache("testProject")

  ### create a data entity
  data <- Data(list(name="MyData", parentId=project$properties$id))
  data <- createEntity(data)
  data2 <- getEntity(data$properties$id)

  addFile(data, file)
  checkEquals(data$files, data2$files)
  checkEquals(length(data$files), 1L)
}

integrationTestCreateEntityAfterAddingFile <-
  function()
{
  file <- tempfile()
  cat("THIS IS A TEST", file=file)
  project <- synapseClient:::.getCache("testProject")

  ### create a data entity
  data <- Data(list(name="MyData", parentId=project$properties$id))
   addFile(data, file)
  data <- createEntity(data)

  checkEquals(length(data$files), 1L)
}

integrationTestGetEntityExistsInCache <-
  function()
{
  project <- synapseClient:::.getCache("testProject")
  data <- Data(parentId=project$properties$id)
  file <- tempfile()
  cat("THIS IS A TEST", file = file)
  addFile(data, file)
  data <- storeEntity(data)
  checkEquals(length(data$files), 1L)
  checkTrue(file.exists(file.path(data$cacheDir, data$files)))
  checkTrue(file.exists(file.path(dirname(data$cacheDir), "archive.zip")))
  synapseClient:::resetFactory(new("FileCacheFactory"))

  dd <- getEntity(data$properties$id)
  checkEquals(length(dd$files), 1L)
  checkTrue(file.exists(file.path(dd$cacheDir, dd$files)))
  checkTrue(file.exists(file.path(dirname(dd$cacheDir), "archive.zip")))
}
