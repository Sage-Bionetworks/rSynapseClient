### Test CRUD operations for FileEntity objects
### 
### Author: Bruce Hoff
################################################################################ 
function()
{
  ### create a project
  project <- createEntity(Project())
  synapseClient:::.setCache("testProject", project)
  

}

.tearDown <- 
  function()
{
  if(!is.null(synapseClient:::.getCache("testFile"))) {
    try(deleteEntity(synapseClient:::.getCache("testFile")))
    synapseClient:::.deleteCache("testFile")
  }
  if(!is.null(synapseClient:::.getCache("testProject"))) {
    try(deleteEntity(synapseClient:::.getCache("testProject")))
    synapseClient:::.deleteCache("testProject")
  }
}

integrationTestCRUDS4FileEntity <- 
  function()
{
  
  ## TODO create FileEntity
  
  ## TODO upload file to FileEntity
  
  ## TODO get FileEntity
  
  ## TODO get file from FileEntity
  
  
  ## TODO delete file
  
  ## TODO delete file entity
  
}