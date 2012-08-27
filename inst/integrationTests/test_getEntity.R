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
