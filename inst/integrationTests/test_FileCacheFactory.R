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

integrationTestVersions <- 
  function()
{
  p <- synapseClient:::.getCache("testProject")
  d <- Data(parentId=p$properties$id)
  x1 <- rnorm(5)
  x2 <- rnorm(100, 5, 2)
  addObject(d, x1, "random")
  d <- storeEntity(d)

  addObject(d, x2, "random")
  d <- storeEntity(d)

  synapseClient:::resetFactory(new("FileCacheFactory"))
  dd <- loadEntity(d$properties$id, 1)
  checkEquals(1L, dd$properties$versionNumber)
  checkTrue(all(x1 == dd$objects$random))

  synapseClient:::resetFactory(new("FileCacheFactory"))
  dd <- loadEntity(d$properties$id, 2)
  checkEquals(2L, dd$properties$versionNumber)
  checkTrue(all(x2 == dd$objects$random))

  d1 <- loadEntity(d$properties$id, 1)
  d2 <- loadEntity(d$properties$id, 2)

  checkEquals(1L, d1$properties$versionNumber)
  checkEquals(2L, d2$properties$versionNumber)
  checkTrue(all(x1 == d1$objects$random))
  checkTrue(all(x2 == d2$objects$random))

  x1 <- rnorm(10)
  addObject(d1, x1, "random")
  checkTrue(all(x1 == d1$objects$random))
  checkTrue(all(x2 == d2$objects$random))

  x2 <- rnorm(20)
  addObject(d2, x2, "random")
  checkTrue(all(x1 == d1$objects$random))
  checkTrue(all(x2 == d2$objects$random))

}





