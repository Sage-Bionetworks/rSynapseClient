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



integrationTestDeleteOnlyStoredObject <-
  function()
{
  d <- synapseClient:::.getCache("testData")

  addObject(d, diag(10), "foo")
  checkEquals(length(d$objects), 1L)
  checkEquals(names(d$objects), "foo")

  d <- storeEntity(d)

  deleteObject(d, "foo")
  checkEquals(length(d$objects), 0L)
  d <- storeEntity(d)
  checkEquals(length(d$objects), 0L)

  d2 <- loadEntity(d)
  checkEquals(length(d2$objects), 0L)
}
