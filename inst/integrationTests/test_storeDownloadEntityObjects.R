### Tests for downloading entity objects added using addObject methods
### 
### Author: Matthew D. Furia <matt.furia@sagebase.org>
#################################################################################

##  TODO: Confirm with Bruce that whole file was specific to Locationable

# .setUp <- 
#   function()
# {
#   ### create a project
#   project <- createEntity(Project())
#   synapseClient:::.setCache("testProject", project)
#   
#   ### create a folder
#   folder <- Folder(list(name="MyFolder", parentId=propertyValue(project, "id")))
#   folder <- createEntity(folder)
#   synapseClient:::.setCache("testFolder", folder)
# }
# 
# .tearDown <-
#   function()
# {
#   deleteEntity(synapseClient:::.getCache("testProject"))
#   synapseClient:::.deleteCache("testProject")
#   synapseClient:::.deleteCache("testFolder")
# }
# 
# createFileInMemory<-function(parentId) {
#   filePath<- tempfile()
#   connection<-file(filePath)
#   writeChar("this is a test", connection, eos=NULL)
#   close(connection)  
#   synapseStore<-TRUE
#   file<-File(filePath, synapseStore, parentId=parentId)
#   file
# }

##  storeEntityObjects() only applies to Locationable so remove
# integrationTestStore <-
#   function()
# {
#   study <- synapseClient:::.getCache("testStudy")
#   data <- Data(list(name="Test Data", parentId = propertyValue(study, "id"), type="C"))
#   addObject(data, "bar", "foo")
#   
#   storedData <- synapseClient:::storeEntityObjects(data)
#   checkEquals(length(storedData$objects), 1L)
#   checkEquals(data$objects$foo, storedData$objects$foo)
#   checkTrue(file.exists(file.path(data$cacheDir, synapseClient:::.getCache("rObjCacheDir"), "foo.rbin")))
# }
# 

##  Remove since Locationable-specific
# integrationTestNoSynStore <-
#   function()
# {
#   study <- synapseClient:::.getCache("testStudy")
#   data <- Data(list(name="Test Data", parentId = propertyValue(study, "id"), type="C"))
#   
#   # you can't call 'synStore' for Locationable objects
#   result<-try(synStore(data), silent=T)
#   
#   checkEquals("try-error", class(result))
# }
#

# integrationTestDownload <-
#   function()
# {
#   study <- synapseClient:::.getCache("testStudy")
#   data <- Data(list(name="Test Data", parentId = propertyValue(study, "id"), type="C"))
#   addObject(data,"bar","foo")
#   
#   storedData <- synapseClient:::storeEntityObjects(data)
#   
#   downloadedFilePath<-file.path(data$cacheDir, synapseClient:::.getCache("rObjCacheDir"), "foo.rbin")
#   unlink(downloadedFilePath)
#   checkTrue(!file.exists(downloadedFilePath))
#   downloadedData <- synGet(propertyValue(storedData, "id"))
#   checkEquals(length(downloadedData$objects), 0L)
#   checkTrue(file.exists(downloadedFilePath))
# }
# 
# integrationTestLoad <-
#   function()
# {
#   study <- synapseClient:::.getCache("testStudy")
#   data <- Data(list(name="Test Data", parentId = propertyValue(study, "id"), type="C"))
#   addObject(data, "bar", "foo")
#   
#   storedData <- synapseClient:::storeEntityObjects(data)
#   downloadedData <- synGet(propertyValue(storedData, "id"))
#   
#   loadedData <- synGet(propertyValue(storedData, "id"), load=T)
#   checkTrue(file.exists(file.path(data$cacheDir, synapseClient:::.getCache("rObjCacheDir"), "foo.rbin")))
#   checkEquals(length(loadedData$objects), 1L)
#   checkEquals(data$objects$foo, loadedData$objects$foo)
# }
# 
# integrationTestDownloadFilesAndObjects <-
#   function()
# {
#   study <- synapseClient:::.getCache("testStudy")
#   data <- Data(list(name="Test Data", parentId = propertyValue(study, "id"), type="C"))
#   addObject(data, "bar", "foo")
#   
#   addObject(data, diag(nrow=10, ncol=10), "diag")
#   checkEquals(length(data$objects), 2L)
#   storedData <- synapseClient:::storeEntityObjects(data)
#   checkEquals(length(data$objects), 2L)
#   
#   downloadedData <- synGet(propertyValue(storedData, "id"))
#   checkEquals(length(downloadedData$objects), 0L)
#   
#   loadedData <- synGet(propertyValue(downloadedData, "id"), load=T)
#   checkEquals(length(loadedData$objects), 2L)
# 
# }



