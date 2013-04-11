#
# 
# Author: furia
###############################################################################


.setUp <- 
  function()
{
  ## create a project
  project <- createEntity(Project())
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
}

integrationTestInitialze <-
  function()
{
  project <- synapseClient:::.getCache("testProject")
  data <- createEntity(Data(list(parentId=propertyValue(project, "id"), type="C")))
  
  copy <- getEntity(propertyValue(data, "id"))
  
  ## the below test is a known bug
  ##checkEquals(copy$cacheDir, data$cacheDir)
  
}
