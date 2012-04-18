# Test functiosn for downloading entity files
# 
# Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

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
  
  ## create a study
  study <- Study(list(name="MyDataSet", parentId=propertyValue(project, "id")))
  study <- createEntity(study)
  synapseClient:::.setCache("testStudy", study)
}

.tearDown <-
  function()
{
  deleteEntity(synapseClient:::.getCache("testProject"))
  synapseClient:::.deleteCache("testProject")
  synapseClient:::.deleteCache("testStudy")
}

integrationTestStoreNoFiles <-
  function()
{
  study <- synapseClient:::.getCache("testStudy")
  data <- Data(list(name='blah',type="C", parentId=propertyValue(study,'id')))
  data <- storeEntity(data)
  checkTrue(!is.null(data))
  checkTrue(!is.null(propertyValue(data,"id")))
}

integrationTestDownloadNoFiles <-
  function()
{
  study <- synapseClient:::.getCache("testStudy")
  data <- Data(list(name='blah',type="C", parentId=propertyValue(study,'id')))
  data <- storeEntity(data)
  checkTrue(!is.null(data))
  checkTrue(!is.null(propertyValue(data,'id')))	
  data <- downloadEntity(propertyValue(data,"id"))        
  
  data <- loadEntity(propertyValue(data,"id"))
}

