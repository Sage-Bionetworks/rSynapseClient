# Test functiosn for downloading entity files
# 
# Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

.setUp <-
  function()
{
  ## create a project
  project <- new(Class="Project")
  propertyValues(project) <- list(
    name = paste("myProject", gsub(':', '_', date()))
  )
  project <- createEntity(project)
  synapseClient:::.setCache("testProject", project)
  
  ## create a dataset
  dataset <- Dataset(list(name="MyDataSet", parentId=propertyValue(project, "id")))
  dataset <- createEntity(dataset)
  synapseClient:::.setCache("testDataset", dataset)
}

.tearDown <-
  function()
{
  deleteEntity(synapseClient:::.getCache("testProject"))
  synapseClient:::.deleteCache("testProject")
  synapseClient:::.deleteCache("testDataset")
}

integrationTestStoreNoFiles <-
  function()
{
  dataset <- synapseClient:::.getCache("testDataset")
  layer <- Layer(list(name='blah',type="C", parentId=propertyValue(dataset,'id')))
  layer <- storeEntity(layer)
  checkTrue(!is.null(layer))
  checkTrue(!is.null(propertyValue(layer,"id")))
}

integrationTestDownloadNoFiles <-
  function()
{
  dataset <- synapseClient:::.getCache("testDataset")
  layer <- Layer(list(name='blah',type="C", parentId=propertyValue(dataset,'id')))
  layer <- storeEntity(layer)
  checkTrue(!is.null(layer))
  checkTrue(!is.null(propertyValue(layer,'id')))	
  layer <- downloadEntity(propertyValue(layer,"id"))        
  
  layer <- loadEntity(propertyValue(layer,"id"))
}

