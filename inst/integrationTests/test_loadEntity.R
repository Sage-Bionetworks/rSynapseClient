### Test loadEntity methods
### 
### Author: Matthew D. Furia <matt.furia@sagebase.org
#################################################################################
#
#.setUp <- 
#  function()
#{
#  ## create a project
#  project <- Project()
#  propertyValues(project) <- list(
#    name = paste("myProject", gsub(':', '_', date()))
#  )
#  project <- createEntity(project)
#  synapseClient:::.setCache("testProject", project)
#  
#}
#
#.tearDown <-
#  function()
#{
#  deleteEntity(synapseClient:::.getCache("testProject"))
#  synapseClient:::.deleteCache("testProject")
#  synapseClient:::.deleteCache("testDataset")
#}
#
#integrationTestLoadRbinData <- 
#  function()
#{
#  dataset <- synapseClient:::.getCache("testDataset")
#  
#  ## create a data entity
#  data <- Data(list(parentId=propertyValue(dataset,"id"), type="C", name="myData"))
#  annotValue(data, "format") <- "rbin"
#  
#  phenotypes <- diag(nrow=10, ncol=10)
#  tmpFile <- tempfile()
#  save(phenotypes, file=tempFile)
#  addFile(data, tmpFile, "/my_test_packet/phenotypes.rbin")
#  
#  
#  checksum <- as.character(tools::md5sum(file.path(data$cacheDir, data$files)))
#  
#  storedData <- storeEntity(data)
#  checkEquals(propertyValue(storedData, "name"), propertyValue(data, "name"))
#  checkEquals(propertyValue(storedData, "type"), propertyValue(data, "type"))
#  checkEquals(propertyValue(storedData, "parentId"), propertyValue(data, "parentId"))
#  
#  checkEquals(as.character(tools::md5sum(file.path(storedData$cacheDir, storedData$files))), checksum)
#  loadedData <- loadEntity(storedData)
#  checkEquals(propertyValue(loadedData, "name"), propertyValue(storedData, "name"))
#  checkEquals(propertyValue(loadedData, "id"), propertyValue(storedData, "id"))
#  checkEquals(propertyValue(loadedData, "parentId"), propertyValue(storedData, "parentId"))
#  checkEquals(propertyValue(loadedData, "type"), propertyValue(storedData, "type"))
#  checkEquals(loadedData$cacheDir, storedData$cacheDir)
#  checkTrue(all(loadedData$files %in% storedData$files))
#  checkTrue(all(storedData$files %in% loadedData$files))
#  
#  checkTrue(all(file.exists(file.path(storedData$cacheDir, storedData$files))))
#}

integrationTestWarnMe <-
  function(){
  warning("need to fix tests for test_loadEntity.R")
}