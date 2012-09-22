# TODO: Add comment
#
# Author: furia
###############################################################################

.setUp <-
  function()
{
  ### create a project
  project <- createEntity(Project())
  synapseClient:::.setCache("testProject", project)

  ### create a study
  data <- Data(list(name="MyData", parentId=propertyValue(project, "id")))
  data <- createEntity(data)
  synapseClient:::.setCache("testData", data)
}

.tearDown <-
  function()
{
  deleteEntity(synapseClient:::.getCache("testProject"))
  synapseClient:::.deleteCache("testProject")
  synapseClient:::.deleteCache("testData")
}

integrationTestDeleteOnlyStoredFile <-
  function()
{
  d <- synapseClient:::.getCache("testData")

  file1 <- tempfile()
  cat("TEST FILE1", file=file1)

  addFile(d, file1)
  checkEquals(length(d$files), 1L)
  checkEquals(d$files, basename(file1))

  d <- storeEntity(d)

  deleteFile(d, basename(file1))
  checkEquals(length(d$files), 0L)
  d <- storeEntity(d)
  checkEquals(length(d$files), 0L)

  d2 <- loadEntity(d)
  checkEquals(length(d2$files), 0L)
}

integrationTestLoadUsingId <-
  function()
{
  d <- synapseClient:::.getCache("testData")
  file1 <- tempfile()
  cat("TEST FILE1", file=file1)

  addFile(d, file1)
  checkEquals(length(d$files), 1L)
  checkEquals(d$files, basename(file1))

  d <- storeEntity(d)

  deleteFile(d, basename(file1))
  checkEquals(length(d$files), 0L)
  d <- storeEntity(d)
  checkEquals(length(d$files), 0L)

  d2 <- loadEntity(d$properties$id)
  checkEquals(length(d2$files), 0L)
}

integrationTestLoadHoldTwoCopies <-
  function()
{
  d <- synapseClient:::.getCache("testData")
  file1 <- tempfile()
  cat("TEST FILE1", file=file1)

  addFile(d, file1)
  checkEquals(length(d$files), 1L)
  checkEquals(d$files, basename(file1))

  d <- storeEntity(d)
  checkEquals(length(d$files), 1L)
  checkEquals(d$files, basename(file1))

  d2 <- loadEntity(d$properties$id)
  checkEquals(length(d2$files), 1L)
  checkEquals(d2$files, basename(file1))

  deleteFile(d, basename(file1))
  checkEquals(length(d$files), 0L)
  checkEquals(length(d2$files), 0L)
}

integrationTestCode <-
  function()
{
  project <- synapseClient:::.getCache("testProject")
  code <- Code(parentId=project$properties$id)
  ff <- tempfile()
  cat("hello <- function(){print('hello')}", file=ff)
  checksum <- tools::md5sum(ff)
  addFile(code, ff, "code.R")

  code <- createEntity(code)
  checkEquals(length(code$files), 1L)
  checkEquals(code$files, "code.R")

  code.copy <- getEntity(code$properties$id)

}


