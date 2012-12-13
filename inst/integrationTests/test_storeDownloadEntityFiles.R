## Test functions for downloading entity files
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
################################################################################

.setUp <-
  function()
{
  ## create a project
  project <- createEntity(Project())
  synapseClient:::.setCache("testProject", project)
  
  ## create a project
  project <- Project()
  project <- createEntity(project)
  synapseClient:::.setCache("testProject2", project)
}

.tearDown <-
  function()
{
  try(deleteEntity(synapseClient:::.getCache("testProject")))
  synapseClient:::.deleteCache("testProject")
  try(deleteEntity(synapseClient:::.getCache("testProject2")))
  synapseClient:::.deleteCache("testProject2")
}

integrationTestStoreNoFiles <-
  function()
{
  project <- synapseClient:::.getCache("testProject")
  data <- Data(list(name='blah',type="C", parentId=propertyValue(project,'id')))
  data <- storeEntity(data)
  checkTrue(!is.null(data))
  checkTrue(!is.null(propertyValue(data,"id")))
}

integrationTestDownloadNoFiles <-
  function()
{
  project <- synapseClient:::.getCache("testProject")
  data <- Data(list(name='blah',type="C", parentId=propertyValue(project,'id')))
  data <- storeEntity(data)
  checkTrue(!is.null(data))
  checkTrue(!is.null(propertyValue(data,'id')))	
  data <- downloadEntity(propertyValue(data,"id"))        
  
  data <- loadEntity(propertyValue(data,"id"))
}

