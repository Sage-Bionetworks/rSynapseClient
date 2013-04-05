### Integration tests for storing entities
###
### Author: Matthew D. Furia <matt.furia@sagebase.org>
#################################################################################


.setUp <-
  function()
{
  ## create a project
  project <- createEntity(Project())
  synapseClient:::.setCache("testProject", project)

}

.tearDown <-
  function()
{
  deleteEntity(synapseClient:::.getCache("testProject"))
  synapseClient:::.deleteCache("testProject")
}

integrationTestCheckFileCache <-
  function()
{
  project <- synapseClient:::.getCache('testProject')
  file <- tempfile()
  cat("THISISATEST", file=file)

  data <- Data(list(parentId = project$properties$id))
  data <- createEntity(data)

  addFile(data, file)
  fc <- data@archOwn@fileCache
  data <- storeEntity(data)

  checkEquals(length(fc$files()), 1L)
  checkEquals(fc$files(), data$files)

  fc2 <- synapseClient:::getFileCache(dirname(data$cacheDir))
  checkEquals(length(fc2$files()), 1L)
  checkEquals(fc2$files(), data$files)

  deleteFile(data, data$files)
  checkEquals(length(fc$files()), 0L)

}

integrationTestStoreEntity <-
  function()
{
  project <- synapseClient:::.getCache('testProject')
  a <- Folder(name="Test Star Wars Entity", parentId=project$properties$id)
  a$annotations$jedi <- c("Obi-wan", "Yoda", "Qui-Gon Jinn")
  a <- storeEntity(a)

  checkTrue("id" %in% names(properties(a)))
  b <- getEntity(a$properties$id)
  b$properties$parent


  deleteEntity(a)
}
