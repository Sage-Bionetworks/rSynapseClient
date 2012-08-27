.setUp <-
  function()
{
  ### create a project
  project <- createEntity(Project())
  synapseClient:::.setCache("testProject", project)

  synapseClient:::.setCache("oldWarn", options("warn")[[1]])
  options(warn=2)

}

.tearDown <-
  function()
{
  deleteEntity(synapseClient:::.getCache("testProject"))
  synapseClient:::.deleteCache("testProject")
  synapseClient:::.deleteCache("testData")

  options(warn=synapseClient:::.getCache("oldWarn"))
  synapseClient:::.deleteCache("oldWarn")
}

integrationTestNonDefalutArchiveName <-
  function()
{
  file <- tempfile()
  cat("THIS IS A TEST", file=file)
  project <- synapseClient:::.getCache("testProject")

  data <- Data(list(parentId = project$properties$id))
  addFile(data, file)

  data@archOwn@fileCache$setArchiveFileName("foobar.zip")

  checkEquals(data@archOwn@fileCache$archiveFile, "foobar.zip")
  checkTrue(file.exists(file.path(data$cacheDir, data$files)))

  data <- storeEntity(data)
  checkTrue(file.exists(file.path(dirname(data$cacheDir), "foobar.zip")))

  ## remove the filecache from the factory
  synapseClient:::resetFactory(new("FileCacheFactory"))

  data <- loadEntity(data$properties$id)
  checkEquals(data@archOwn@fileCache$archiveFile, "foobar.zip")
  checkTrue(grepl("foobar.zip_unpacked$", data$cacheDir))

}