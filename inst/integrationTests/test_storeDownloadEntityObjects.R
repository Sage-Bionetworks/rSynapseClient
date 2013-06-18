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
  ##checkEquals(length(data$files), 0L)
  
  storedData <- synapseClient:::storeEntityObjects(data)
  ##checkEquals(length(storedData$files), 0L)
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
  ##checkEquals(length(data$files), 0L)
  
  storedData <- synapseClient:::storeEntityObjects(data)
  
  downloadedData <- downloadEntity(propertyValue(storedData, "id"))
  ##checkEquals(length(downloadedData$files), 0L)
  checkEquals(length(downloadedData$objects), 0L)
  checkTrue(file.exists(file.path(data$cacheDir, synapseClient:::.getCache("rObjCacheDir"), "foo.rbin")))
}

integrationTestLoad <-
  function()
{
  study <- synapseClient:::.getCache("testStudy")
  data <- Data(list(name="Test Data", parentId = propertyValue(study, "id"), type="C"))
  addObject(data, "bar", "foo")
  ##checkEquals(length(data$files), 0L)
  
  storedData <- synapseClient:::storeEntityObjects(data)
  downloadedData <- downloadEntity(propertyValue(storedData, "id"))
  
  loadedData <- loadEntity(propertyValue(storedData, "id"))
  checkTrue(file.exists(file.path(data$cacheDir, synapseClient:::.getCache("rObjCacheDir"), "foo.rbin")))
  ##checkEquals(length(loadedData$files), 0L)
  checkEquals(length(loadedData$objects), 1L)
  checkEquals(data$objects$foo, loadedData$objects$foo)
}

integrationTestDownloadFilesAndObjects <-
  function()
{
  study <- synapseClient:::.getCache("testStudy")
  data <- Data(list(name="Test Data", parentId = propertyValue(study, "id"), type="C"))
  addObject(data, "bar", "foo")
##  checkEquals(length(data$files), 0L)
  
  addObject(data, diag(nrow=10, ncol=10), "diag")
  checkEquals(length(data$objects), 2L)
  storedData <- synapseClient:::storeEntityObjects(data)
##  checkEquals(length(data$files), 0L)
  checkEquals(length(data$objects), 2L)
  
  downloadedData <- downloadEntity(propertyValue(storedData, "id"))
##  checkEquals(length(downloadedData$files), 0L)
  checkEquals(length(downloadedData$objects), 0L)
  
  loadedData <- loadEntity(downloadedData)
##  checkEquals(length(loadedData$files), 0L)
  checkEquals(length(loadedData$objects), 2L)
  
  
  loadedData <- loadEntity(propertyValue(storedData,"id"))
 ## checkEquals(length(downloadedData$files), 0L)
  checkEquals(length(downloadedData$objects), 2L)
}



