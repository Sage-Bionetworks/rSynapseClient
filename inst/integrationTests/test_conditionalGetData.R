## Test for creating and retrieving entities
## 
## Author: Nicole Deflaux <nicole.deflaux@sagebase.org>
################################################################################
#
#.setUp <- 
#  function() 
#{
#  ## Create a project
#  createdProject <- createEntity(Project(list(name=paste('R Conditional Get Integration Test Project', gsub(':', '_', date())))))
#  synapseClient:::.setCache("rIntegrationTestProject", createdProject)
#  
#  synapseClient:::.setCache("oldCacheDir", synapseCacheDir())
#  synapseCacheDir(file.path(tempdir(), ".conditionalGetCacheDir"))
#}
#
#.tearDown <- 
#  function() 
#{
#  deleteEntity(synapseClient:::.getCache("rIntegrationTestProject"))
#  synapseClient:::.deleteCache("rIntegrationTestProject")
#  
#  ## delete test cache dir
#  unlink(synapseCacheDir(), recursive=TRUE)
#  synapseCacheDir(synapseClient:::.getCache("oldCacheDir"))
#  synapseClient:::.deleteCache("oldCacheDir")
#}
#
#integrationTestConditionalGet <- 
#  function() 
#{
#  createdProject <- synapseClient:::.getCache("rIntegrationTestProject")
#  
#  ## Create a data and store a data R object
#  data <- Data(list(name='R Integration Test Data',
#      type='C',
#      parentId=propertyValue(createdProject, 'id')
#    )) 
#  
#  data <- data.frame(a=1:3, b=letters[10:12],
#    c=seq(as.Date("2004-01-01"), by = "week", len = 3),
#    stringsAsFactors = TRUE)
#  fileName <- file.path(tempdir(), "data.tab")
#  write.table(data, file=fileName, quote=F, sep="\t", row.names=F)
#  data <- addFile(data, fileName)
#  createdData <- createEntity(data)
#  checkEquals(propertyValue(data,"name"), propertyValue(createdData, "name"))
#  
#  ## Now download the data
#  loadedData1 <- loadEntity(data)
#  fileInfo1 <- file.info(normalizePath(file.path(loadedData1$cacheDir, loadedData1$files[1]))) 
#  
#  ## Now download the data again, but this time it should just read from the cache
#  loadedData2 <- loadEntity(data)
#  fileInfo2 <- file.info(normalizePath(file.path(loadedData2$cacheDir, loadedData2$files[1]))) 
#  
#  checkEquals(loadedData1$files[1], loadedData2$files[1])
#  ## If the modification time is the same, we did not download it the second time
#  checkEquals(fileInfo1$mtime, fileInfo2$mtime)
#}

integrationTestWarnMe <-
  function(){
  warning("need to fix tests for test_conditionalGetData.R")
}