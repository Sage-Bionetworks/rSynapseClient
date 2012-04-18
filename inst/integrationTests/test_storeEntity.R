## Integration tests for storing entities
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
  
}

.tearDown <-
  function()
{
  deleteEntity(synapseClient:::.getCache("testProject"))
  synapseClient:::.deleteCache("testProject")
}

integrationTestStoreDataRbin <- 
  function()
{
  project <- synapseClient:::.getCache("testProject")
  
  ## create a data
  data <- Data(list(parentId=propertyValue(project,"id"), type="C", name="myData"))
  annotValue(data, "format") <- "rbin"
  data$files <- "/my_test_packet/phenotypes.rbin"
  
  phenotypes <- diag(nrow=10, ncol=10)
  if(file.exists(file.path(data$cacheDir, "my_test_packet")))
    unlink(file.path(data$cacheDir, "my_test_packet"), recursive=TRUE)
  dir.create(file.path(data$cacheDir, "my_test_packet"), recursive=TRUE)
  save(phenotypes, file=file.path(data$cacheDir, data$files))
  
  checksum <- as.character(tools::md5sum(file.path(data$cacheDir, data$files)))
  
  storedData <- storeEntityFiles(data)
  checkEquals(propertyValue(storedData, "name"), propertyValue(data, "name"))
  checkEquals(propertyValue(storedData, "type"), propertyValue(data, "type"))
  checkEquals(propertyValue(storedData, "parentId"), propertyValue(data, "parentId"))
  
  checkEquals(as.character(tools::md5sum(file.path(storedData$cacheDir, storedData$files))), checksum)
  downloadedData <- downloadEntity(storedData)
  checkEquals(propertyValue(downloadedData, "name"), propertyValue(storedData, "name"))
  checkEquals(propertyValue(downloadedData, "id"), propertyValue(storedData, "id"))
  checkEquals(propertyValue(downloadedData, "parentId"), propertyValue(storedData, "parentId"))
  checkEquals(propertyValue(downloadedData, "type"), propertyValue(storedData, "type"))
  checkEquals(downloadedData$cacheDir, storedData$cacheDir)
  checkTrue(all(downloadedData$files %in% storedData$files))
  checkTrue(all(storedData$files %in% downloadedData$files))
  
  checkTrue(all(file.exists(file.path(storedData$cacheDir, storedData$files))))
  
}

integrationTestStoreDataZip <-
  function()
{
  data <- data.frame(a=1:3, b=letters[10:12],
    c=c('hello', 'world', 'have a nice day'),
    stringsAsFactors = FALSE)
  
  dataFile <- file.path(tempdir(), "data.tab")
  write.table(data, file=dataFile, sep="\t", quote=F, row.names=F)
  zipFile <- file.path(tempdir(), "data.zip")
  zip(zipfile=zipFile, files=dataFile)
  
  project <- synapseClient:::.getCache("testProject")
  data <- Data(list(parentId=propertyValue(project,"id"), type="C", name="myZippedData"))
  data <- addFile(data, zipFile)
  
  storedData <- storeEntity(data)
  checkEquals(propertyValue(storedData, "name"), propertyValue(data, "name"))
  checkEquals(propertyValue(storedData, "type"), propertyValue(data, "type"))
  checkEquals(propertyValue(storedData, "parentId"), propertyValue(project, "id"))
  
  loadedData <- loadEntity(storedData)
  storedDataData <- read.delim(normalizePath(file.path(loadedData$cacheDir, loadedData$files[1])), sep='\t', stringsAsFactors = FALSE)
  checkEquals(storedDataData, data)	
}

integrationTestStoreDataCode <-
  function()
{
  project<- synapseClient:::.getCache("testProject") # check set up method
  code <- Code(list(name="a code data", parentId=propertyValue(project, "id")))
  codeFile <- file.path(tempdir(), "someCode.R")
  cat('run <- function(){return("executing test function")}',file=codeFile)
  code <- addFile(code, codeFile)
  checkTrue(file.exists(file.path(code$cacheDir, code$files)))
  code <- storeEntity(code)
  code <- loadEntity(propertyValue(code, "id"))
  checkEquals(code$objects$run(), "executing test function")
}

integrationTestStoreMediaData <- function() {
  
  project <- synapseClient:::.getCache("testProject")
  data <- Data(list(parentId=propertyValue(project,"id"), type="M", name="myZippedData"))
  
  ## Make a jpeg when PLFM-498 is fixed, for now, make a fake one
  filename <- "r_integration_test_plot.jpg"
  filepath <- file.path(tempdir(), filename)
  ##	attach(mtcars)
  ##	jpeg(filename)
  ##	plot(wt, mpg) 
  ##	abline(lm(mpg~wt))
  ##	title("Regression of MPG on Weight")
  ##	dev.off()
  
  data <- data.frame(a=1:3, b=letters[10:12],
    c=seq(as.Date("2004-01-01"), by = "week", len = 3),
    stringsAsFactors = FALSE)
  write.table(data, filepath)	
  
  data <- addFile(data, filepath)
  createdData <- storeEntity(data)
  loadedData <- loadEntity(createdData)
  checkEquals(1, length(loadedData$files))
  checkEquals(filename, loadedData$files[1])
}

integrationTestMultipleBinary <- 
  function()
{
  
  ## Make an R data object that we will store in a couple different ways
  data <- data.frame(a=1:3, b=letters[10:12],
    c=seq(as.Date("2004-01-01"), by = "week", len = 3),
    stringsAsFactors = FALSE)
  
  data2 <- diag(100)
  
  project <- synapseClient:::.getCache("testProject")
  data <- Data(entity = list(
      type = 'E',
      parentId = propertyValue(project, "id")
    )
  )
  
  data <- addObject(data, data)
  data <- addObject(data, data2)
  createdData <- storeEntity(data)
  loadedData <- loadEntity(createdData)
  
  checkTrue(all(c("data", "data2") %in% names(loadedData$objects)))
  
  checkEquals(data, loadedData$objects$data)
  checkEquals(data2, loadedData$objects$data2)
}