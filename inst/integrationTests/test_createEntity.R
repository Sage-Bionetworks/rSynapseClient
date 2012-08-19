.setUp <-
  function()
{
  synapseClient:::.setCache('oldWarn', options('warn')[[1]])
  options(warn=2)

  ## create a project
  project <- Project()
  propertyValues(project) <- list(
    name = paste("myProject", gsub(':', '_', date()))
  )
  project <- createEntity(project)
  synapseClient:::.setCache("testProject", project)

  synapseClient:::.setCache("oldCacheDir", synapseCacheDir())
  synapseCacheDir(tempfile(pattern="tempSynapseCache"))
}

.tearDown <-
  function()
{

  deleteEntity(synapseClient:::.getCache("testProject"))
  synapseClient:::.deleteCache("testProject")

  unlink(synapseCacheDir(), recursive=T)
  synapseCacheDir(synapseClient:::.getCache("oldCacheDir"))

  options(warn=synapseClient:::.getCache("oldWarn"))
  synapseClient:::.deleteCache("oldWarn")
}


integrationTestCreateCreateFileCacheFactory <-
  function()
{
  project <- synapseClient:::.getCache("testProject")
  file <- tempfile()
  cat("THISISATEST", file=file)
  data <- Data(list(parentId = project$properties$id))
  data <- createEntity(data)

  fc <- synapseClient:::getFileCache(dirname(data$cacheDir))
  addFile(data, file)

  checkEquals(length(fc$files()), 1L)
  checkEquals(length(data$files), 1L)
  checkEquals(data$files, fc$files())

  deleteFile(fc, fc$files())
  checkEquals(length(fc$files()), 0L)
  checkEquals(length(data$files), 0L)

}

integrationTestCreateEntityAddFileFirst <-
  function()
{
  project <- synapseClient:::.getCache("testProject")
  file <- tempfile()
  cat("THISISATEST", file=file)
  data <- Data(list(parentId = project$properties$id))
  fc <- synapseClient:::getFileCache(dirname(data$cacheDir))
  addFile(data, file)
  data <- createEntity(data)

  checkEquals(length(fc$files()), 1L)
  checkEquals(length(data$files), 1L)
  checkEquals(data$files, fc$files())

  deleteFile(fc, fc$files())
  checkEquals(length(fc$files()), 0L)
  checkEquals(length(data$files), 0L)
}

integrationTestCreateEntityAddFileFirstMultipleCopies <-
  function()
{
  project <- synapseClient:::.getCache("testProject")
  file <- tempfile()
  cat("THISISATEST", file=file)
  data <- Data(list(parentId = project$properties$id))
  copy <- data
  fc <- synapseClient:::getFileCache(dirname(data$cacheDir))
  addFile(data, file)
  data <- createEntity(data)

  checkEquals(length(fc$files()), 1L)
  checkEquals(length(data$files), 1L)
  checkEquals(length(copy$files), 1L)
  checkEquals(data$files, fc$files())

  deleteFile(fc, fc$files())
  checkEquals(length(fc$files()), 0L)
  checkEquals(length(data$files), 0L)
  checkEquals(length(copy$files), 0L)

  copy2 <- getEntity(data$properties$id)
  checkEquals(length(copy2$files), 0L)
  addFile(copy2, file)
  checkEquals(length(copy2$files), 1L)
  checkEquals(length(fc$files()), 1L)
  checkEquals(length(data$files), 1L)
  checkEquals(length(copy$files), 1L)
}


