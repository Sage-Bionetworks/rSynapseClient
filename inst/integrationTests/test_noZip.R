#### Tests to check behavior when zip is not installed on client computer
#### 
#### Author: Matthew D. Furia <matt.furia@sagebase.org>
##################################################################################
#
#.setUp <- 
#  function()
#{
#  ## stub-out zip file to return 127
#  myZip <- function(zipfile, files){
#    return(127L)
#  }
#  attr(myZip, "origFcn") <- utils:::zip
#  ## detach packages so their functions can be overridden
#  suppressWarnings(unloadNamespace('synapseClient'))
#  suppressWarnings(detach('package:utils', force=TRUE))
#  utils:::assignInNamespace("zip", myZip, "utils")
#  ##reload detached packages
#  library(utils, quietly=TRUE)
#  attachNamespace("synapseClient")
#  
#  ## create project and add to cache
#  project <- list()
#  project$name <- paste('R noZip Integration Test Project', gsub(':', '_', date()))
#  project <- Project(project)
#  createdProject <- createEntity(entity=project)
#  synapseClient:::.setCache("rIntegrationTestProject", createdProject)
#}
#
#.tearDown <- 
#  function()
#{
#  testProject <- synapseClient:::.getCache("rIntegrationTestProject")
#  
#  ## put back method
#  suppressWarnings(unloadNamespace('synapseClient'))
#  suppressWarnings(detach('package:utils', force = TRUE))
#  utils:::assignInNamespace("zip", attr(utils:::zip, "origFcn"), "utils")
#  library(utils, quietly = TRUE)
#  attachNamespace("synapseClient")
#  
#  ## delete project 
#  deleteEntity(entity=testProject)
#  synapseClient:::.deleteCache("rIntegrationTestProject")
#}
#
#integrationTestNoZipFile <- 
#  function()
#{
#  dataset <- Study(entity = list(
#      name = 'R Integration Test Study',
#      parentId = propertyValue(synapseClient:::.getCache("rIntegrationTestProject"),"id")
#    )
#  )
#  
#  createdStudy <- createEntity(entity=dataset)
#  checkEquals(propertyValue(dataset, "name"), propertyValue(createdStudy, "name"))
#  
#  ## Make an R data object that we will store in a couple different ways
#  data <- data.frame(a=1:3, b=letters[10:12],
#    c=seq(as.Date("2004-01-01"), by = "week", len = 3),
#    stringsAsFactors = FALSE)
#  
#  dataFile <- file.path(tempdir(), "data.tab")
#  write.table(data, file=dataFile, sep="\t", quote=F, row.names=F)
#  
#  data <- Data(entity = list(
#      name = 'R Integration Test Data',
#      type = 'C',
#      parentId = propertyValue(createdStudy, "id")
#    )
#  )
#  data <- createEntity(data)
#  data <- addFile(data, dataFile)
#  data <- updateEntity(data)
#  
#  loadedData <- loadEntity(data)
#  checkTrue(grepl(sprintf("%s$", "data.tab"), loadedData$files[1]))
#}
#
#integrationTestNoZipMultipleFiles <- 
#  function()
#{
#  dataset <- Study(entity = list(
#      name = 'R Integration Test Study',
#      parentId = propertyValue(synapseClient:::.getCache("rIntegrationTestProject"), "id")
#    )
#  )
#  
#  createdStudy <- createEntity(entity=dataset)
#  checkEquals(propertyValue(dataset, "name"), propertyValue(createdStudy, "name"))
#  
#  ## Make an R data object that we will store in a couple different ways
#  data <- data.frame(a=1:3, b=letters[10:12],
#    c=seq(as.Date("2004-01-01"), by = "week", len = 3),
#    stringsAsFactors = FALSE)
#  
#  dataFile <- file.path(tempdir(), "data.tab")
#  dataFile2 <- file.path(tempdir(), "data2.tab")
#  write.table(data, file=dataFile, sep="\t", quote=F, row.names=F)
#  write.table(data, file=dataFile2, sep="\t", quote=F, row.names=F)
#  
#  data <- Data(entity = list(
#      name = 'R Integration Test Data',
#      type = 'C',
#      parentId = propertyValue(createdStudy, "id")
#    )
#  )
#  data <- createEntity(data)
#  data <- addFile(data, c(dataFile, dataFile2))
#  ## This should fail because we've added two files to the data but we don't have a zip utility
#  checkException(storeEntity(data))
#}
#
#integrationTestNoZipBinary <- 
#  function()
#{
#  dataset <- Study(entity = list(
#      name = 'R Integration Test Study',
#      parentId = propertyValue(synapseClient:::.getCache("rIntegrationTestProject"),"id")
#    )
#  )
#  
#  createdStudy <- createEntity(entity=dataset)
#  checkEquals(propertyValue(dataset, "name"), propertyValue(createdStudy, "name"))
#  
#  ## Make an R data object that we will store in a couple different ways
#  data <- data.frame(a=1:3, b=letters[10:12],
#    c=seq(as.Date("2004-01-01"), by = "week", len = 3),
#    stringsAsFactors = FALSE)
#  
#  data <- Data(entity = list(
#      name = 'R Integration Test Data',
#      type = 'C',
#      parentId = propertyValue(createdStudy, "id")
#    )
#  )
#  
#  data <- createEntity(data)
#  data <- addObject(data, data)
#  data <- storeEntity(data)
#  
#  loadedData <- loadEntity(data)
#  checkEquals(loadedData$files, "data.rbin")
#}
#
#
#integrationTestNoZipMultipleBinary <- 
#  function()
#{
#  dataset <- Study(entity = list(
#      name = 'R Integration Test Study',
#      parentId = propertyValue(synapseClient:::.getCache("rIntegrationTestProject"),"id")
#    )
#  )
#  
#  createdStudy <- createEntity(entity=dataset)
#  checkEquals(propertyValue(dataset, "name"), propertyValue(createdStudy, "name"))
#  
#  ## Make an R data object that we will store in a couple different ways
#  data <- data.frame(a=1:3, b=letters[10:12],
#    c=seq(as.Date("2004-01-01"), by = "week", len = 3),
#    stringsAsFactors = FALSE)
#  
#  data2 <- data
#  
#  data <- Data(entity = list(
#      name = 'R Integration Test Data',
#      type = 'C',
#      parentId = propertyValue(createdStudy, "id")
#    )
#  )
#  data <- createEntity(data)
#  data <- addObject(data, c(data, data2))
#  
#  ## This should fail because we've added two objectss to the data but we don't have a zip utility
#  checkException(storeEntity(data))
#}
