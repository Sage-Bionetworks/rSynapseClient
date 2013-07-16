### Tests for downloading entity objects added using addObject methods
### 
### Author: Matthew D. Furia <matt.furia@sagebase.org>
#################################################################################

.setUp <- 
  function()
{
  ### create a project
  project <- createEntity(Project())
  synapseClient:::.setCache("testProject", project)
  
  ### create a study
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


integrationTestStore <-
  function()
{
  study <- synapseClient:::.getCache("testStudy")
  data <- Data(list(name="Test Data", parentId = propertyValue(study, "id"), type="C"))
  addObject(data, "bar", "foo")
  
  storedData <- synapseClient:::storeEntityObjects(data)
  checkEquals(length(storedData$objects), 1L)
  checkEquals(data$objects$foo, storedData$objects$foo)
  checkTrue(file.exists(file.path(data$cacheDir, synapseClient:::.getCache("rObjCacheDir"), "foo.rbin")))
}

integrationTestDownload <-
  function()
{
  study <- synapseClient:::.getCache("testStudy")
  data <- Data(list(name="Test Data", parentId = propertyValue(study, "id"), type="C"))
  addObject(data,"bar","foo")
  
  storedData <- synapseClient:::storeEntityObjects(data)
  
  downloadedFilePath<-file.path(data$cacheDir, synapseClient:::.getCache("rObjCacheDir"), "foo.rbin")
  unlink(downloadedFilePath)
  checkTrue(!file.exists(downloadedFilePath))
  downloadedData <- synGet(propertyValue(storedData, "id"))
  checkEquals(length(downloadedData$objects), 0L)
  checkTrue(file.exists(downloadedFilePath))
}

integrationTestLoad <-
  function()
{
  study <- synapseClient:::.getCache("testStudy")
  data <- Data(list(name="Test Data", parentId = propertyValue(study, "id"), type="C"))
  addObject(data, "bar", "foo")
  
  storedData <- synapseClient:::storeEntityObjects(data)
  downloadedData <- synGet(propertyValue(storedData, "id"))
  
  loadedData <- synGet(propertyValue(storedData, "id"), load=T)
  checkTrue(file.exists(file.path(data$cacheDir, synapseClient:::.getCache("rObjCacheDir"), "foo.rbin")))
  checkEquals(length(loadedData$objects), 1L)
  checkEquals(data$objects$foo, loadedData$objects$foo)
}

integrationTestDownloadFilesAndObjects <-
  function()
{
  study <- synapseClient:::.getCache("testStudy")
  data <- Data(list(name="Test Data", parentId = propertyValue(study, "id"), type="C"))
  addObject(data, "bar", "foo")
  
  addObject(data, diag(nrow=10, ncol=10), "diag")
  checkEquals(length(data$objects), 2L)
  storedData <- synapseClient:::storeEntityObjects(data)
  checkEquals(length(data$objects), 2L)
  
  downloadedData <- synGet(propertyValue(storedData, "id"))
  checkEquals(length(downloadedData$objects), 0L)
  
  loadedData <- synGet(propertyValue(downloadedData, "id"), load=T)
  checkEquals(length(loadedData$objects), 2L)

}



