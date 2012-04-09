## Testing object cache
## 
## Author: Matthew D. Furia <matt.furia@sagebas.org>
###############################################################################
.setUp <- 
  function()
{
  env <- attach(NULL, name = "testEnv")
  setPackageName("testEnv", env)
  suppressWarnings(
    setRefClass(
      "wObjOwn",
      contains = "WritableObjectOwner"
    ) 
  )
}

.tearDown <-
  function()
{
  detach("testEnv")
}

  function()
{
  own <- getRefClass("wObjOwn")$new()
  cacheDir <- file.path(own$cacheDir, synapseClient:::synapseObjectCache())
  own$objects$object1 <- "foo"
  synapseClient:::.cacheObject(own, "object1")
  checkTrue(file.exists(file.path(cacheDir, "object1.rbin")))
  
  env <- new.env()
  load(file.path(cacheDir, "object1.rbin"), envir=env)
  checkEquals(own$objects$object1, env$object1)
}

unitTestTmpCacheObject <-
  function()
{
  own <- getRefClass("wObjOwn")$new()
  cacheDir <- file.path(own$cacheDir, synapseClient:::synapseObjectCache())
  own$objects$object1 <- "foo"
  
  checkException(synapseClient:::.tmpCacheObject("object1"))
  synapseClient:::.cacheObject(own, "object1")
  synapseClient:::.tmpCacheObject(own, "object1")
  checkTrue(file.exists(file.path(cacheDir, "object1.rbin.tmp")))
  checkTrue(!file.exists(file.path(cacheDir, "object1.rbin")))
}

unitTestRenameFromTmp <-
  function()
{
  own <- getRefClass("wObjOwn")$new()
  cacheDir <- file.path(own$cacheDir, synapseClient:::synapseObjectCache())
  
  object1 <- "foo"
  own$objects$object1 <- object1
  
  synapseClient:::.cacheObject(own, "object1")
  synapseClient:::.tmpCacheObject(own, "object1")
  
  own$objects$object2 <- object1
  synapseClient:::.renameCacheObjectFromTmp(own, "object1", "object2")
  checkTrue(!file.exists(file.path(cacheDir, "object1.rbin.tmp")))
  checkTrue(file.exists(file.path(cacheDir, "object2.rbin")))
  
  env <- new.env()
  load(file.path(cacheDir, "object2.rbin"), envir = env)
  checkEquals(object1, env$object1)
  
}

unitTestDeleteTmpFile <-
  function()
{
  own <- getRefClass("wObjOwn")$new()
  cacheDir <- file.path(own$cacheDir, synapseClient:::.getCache("rObjCacheDir"))
  own$objects$object1 <- "foo"
  
  synapseClient:::.cacheObject(own, "object1")
  synapseClient:::.tmpCacheObject(own, "object1")
  
  synapseClient:::.deleteTmpCacheFile(own, "object1")
  checkTrue(!(file.exists(file.path(cacheDir, "object1.rbin.tmp"))))
}

unitTestDeleteCacheFile <-
  function()
{
  own <- getRefClass("wObjOwn")$new()
  cacheDir <- file.path(own$cacheDir, synapseClient:::synapseObjectCache())
  own$objects$object1 <- "foo"
  
  synapseClient:::.cacheObject(own, "object1")
  synapseClient:::.deleteCacheFile(own, "object1")
  checkTrue(!(file.exists(file.path(cacheDir, "object1.rbin"))))
}

unitTestLoadCachedObjects <-
  function()
{
  own <- getRefClass("wObjOwn")$new()
  cacheDir <- file.path(own$cacheDir, synapseClient:::synapseObjectCache())
  object1 <- "foo"
  object2 <- diag(nrow=10,ncol=10)
  
  own$objects$object1 <- object1
  own$objects$object2 <- object2
  
  synapseClient:::.cacheObject(own, "object1")
  synapseClient:::.cacheObject(own, "object2")
  
  own$objects@env <- new.env()
  checkEquals(length(own$objects), 0L)
  synapseClient:::.loadCachedObjects(own)
  checkEquals(length(own$objects), 2L)
  
  checkTrue(all(object2 == own$objects$object2))
  checkEquals(object1, own$objects$object1)
}


