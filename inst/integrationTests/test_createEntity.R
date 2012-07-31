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
