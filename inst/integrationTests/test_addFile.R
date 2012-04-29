### Test funtions for adding files to entities
### 
### Author: Matthew D. Furia <matt.furia@sagebase.org>
#################################################################################
#
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

integrationTestAddToNewDataEntity <-
  function()
{
  project <- synapseClient:::.getCache("testProject")
  data <- Data(list(parentId=propertyValue(project, "id"), type="C"))
  
  file <- "file1.rbin"
  path <- "/apath/"
  d <- diag(nrow=10, ncol=10)
  save(d, file=file.path(tempdir(), file))
  ## add a file in a subdirectory
  checkTrue(!file.exists(file.path(data$cacheDir, path, file)))
  checkTrue(!grepl(synapseClient:::synapseCacheDir(), data$cacheDir, fixed=TRUE))
  checkEquals(length(data$files), 0L)
  data <- addFile(data, file.path(tempdir(), file), path)
  checkEquals(length(data$files), 1L)
  
  checkTrue(file.exists(file.path(data$cacheDir, data$files)))
  checkEquals(data$files, gsub("^/+", "", gsub("/+", "/", file.path(path, file))))
  
  data <- storeEntity(data)
  checkTrue(grepl(gsub("[\\/]+", "/", normalizePath(synapseClient:::synapseCacheDir())), data$cacheDir, fixed=TRUE))
  checkEquals(length(data$files), 1L)
  checkTrue(file.exists(file.path(data$cacheDir, data$files)))
  
  ##update the data
  file <- "file2.rbin"
  path <- "/apath2/"
  d <- diag(x=2,nrow=10, ncol=10)
  save(d, file=file.path(tempdir(), file))
  data <- addFile(data, file.path(tempdir(), file), path)
  checkEquals(length(data$files), 2L)
  checkEquals(data$files[2], gsub("/+","/",gsub(sprintf("^%s", .Platform$file.sep), "", file.path(path, file))))
  checkTrue(grepl(gsub("[\\/]+", "/", normalizePath(synapseClient:::synapseCacheDir())), data$cacheDir, fixed=TRUE))
  
  checkTrue(all(file.exists(file.path(data$cacheDir, data$files))))
  
  data <- storeEntity(data)
  checkTrue(all(file.remove(file.path(data$cacheDir, data$files))))
  data2 <- downloadEntity(propertyValue(data,"id"))
  checkEquals(data$cacheDir, data2$cacheDir)
  checkEquals(propertyValue(data,"id"), propertyValue(data2, "id"))
  checkTrue(all(file.exists(file.path(data2$cacheDir, data2$files))))
  checkTrue(all(data$files == data2$files))
  
  ## add a file to the rood direcotory
  data <- data2
  file <- "file3.rbin"
  path <- "/"
  d <- diag(x=3,nrow=10, ncol=10)
  save(d, file=file.path(tempdir(), file))
  data <- addFile(data, file.path(tempdir(), file), path)
  checkEquals(length(data$files), 3L)
  checkEquals(data$files[3], gsub(sprintf("^%s+", .Platform$file.sep), "", file.path(path, file)))
  checkTrue(grepl(gsub("[\\/]+", "/", normalizePath(synapseClient:::synapseCacheDir())), data$cacheDir, fixed=TRUE))
  
  data <- storeEntity(data)
  checkEquals(length(data$files), 3L)
  checkTrue(all(file.exists(file.path(data$cacheDir, data$files))))
  checkTrue(all(file.remove(file.path(data$cacheDir, data$files))))
  
  data2 <- downloadEntity(propertyValue(data,"id"))
  checkEquals(data$cacheDir, data2$cacheDir)
  checkEquals(propertyValue(data,"id"), propertyValue(data2, "id"))
  checkTrue(all(file.exists(file.path(data2$cacheDir, data2$files))))
  checkTrue(all(data$files == data2$files))
  
  ## add a file to the rood direcotory
  data <- data2
  file <- "file4.rbin"
  path <- ""
  d <- diag(x=4,nrow=10, ncol=10)
  save(d, file=file.path(tempdir(), file))
  data <- addFile(data, file.path(tempdir(), file), path)
  checkEquals(length(data$files), 4L)
  checkEquals(data$files[4], gsub(sprintf("^%s+", .Platform$file.sep), "", file.path(path, file)))
  checkTrue(grepl(gsub("[\\/]+", "/", normalizePath(synapseClient:::synapseCacheDir())), data$cacheDir, fixed=TRUE))
  
  data <- storeEntity(data)
  checkEquals(length(data$files), 4L)
  checkTrue(all(file.exists(file.path(data$cacheDir, data$files))))
  checkTrue(all(file.remove(file.path(data$cacheDir, data$files))))
  
  data2 <- downloadEntity(propertyValue(data,"id"))
  checkEquals(data$cacheDir, data2$cacheDir)
  checkEquals(propertyValue(data,"id"), propertyValue(data2, "id"))
  checkTrue(all(file.exists(file.path(data2$cacheDir, data2$files))))
  checkTrue(all(data$files == data2$files))
  checkTrue(all(file.remove(file.path(data$cacheDir, data$files))))
}

