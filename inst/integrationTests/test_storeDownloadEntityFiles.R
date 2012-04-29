## Test functiosn for downloading entity files
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
################################################################################

.setUp <-
  function()
{
  ## create a project
  project <- Project()
  propertyValues(project) <- list(
    name = paste("myProject", gsub(':', '_', date()))
  )
  project <- createEntity(project)
  synapseClient:::.setCache("testProject", project)
  
  ## create a project
  project <- Project(list(name="MyDataSet", parentId=propertyValue(project, "id")))
  project <- createEntity(project)
  synapseClient:::.setCache("testProject2", project)
}

.tearDown <-
  function()
{
  deleteEntity(synapseClient:::.getCache("testProject"))
  synapseClient:::.deleteCache("testProject")
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

