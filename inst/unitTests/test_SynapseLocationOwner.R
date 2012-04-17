# TODO: Add comment
# 
# Author: furia
###############################################################################
.setUp <- 
  function()
{
  env <- attach(NULL, name = "testEnv")
  tryCatch({
      setPackageName("testEnv", env)
      suppressWarnings(
        setClass(
          "LocOwn",
          contains = "SynapseLocationOwner",
          where = env
        ) 
      )
    },
    error = function(e){
      detach("testEnv")
      stop(e)
    }
  )
  
  synapseClient:::.setCache("oldWarn", options("warn")[[1]])
}

.tearDown <-
  function()
{
  detach("testEnv")
  options(warn = synapseClient:::.getCache("oldWarn"))
}


unitTestConstructors <-
  function()
{
  stop("not yet implemented")
}

unitTestAddFile <- 
  function()
{
  own <- new("LocOwn")
  
  checkTrue(grepl("_unpacked$", own$cacheDir))
  checkEquals(character(), own$files)
  file <- tempfile()
  cat(sprintf("THIS IS A TEST %s", Sys.time()), file = file)
  copy <- addFile(own, file)
  checkEquals(gsub("^.+/", "", file), own$files)
  checkEquals(gsub("^.+/", "", file), copy$files)
  
  ## make sure the cache re-initializes but running the exact same
  ## test again
  own <- new("LocOwn")
  checkTrue(grepl("_unpacked$", own$cacheDir))
  checkEquals(character(), own$files)
  file <- tempfile()
  cat(sprintf("THIS IS A TEST %s", Sys.time()), file = file)
  addFile(own, file)
  checkEquals(gsub("^.+/", "", file), own$files)
  
  addFile(own, file, "foo.bar")
  checkEquals()
  
  
}

unitTestMoveFile <-
  function()
{
  own <- new("LocOwn")
  
  checkTrue(grepl("_unpacked$", own$cacheDir))
  checkEquals(character(), own$files)
  file <- tempfile()
  cat(sprintf("THIS IS A TEST %s", Sys.time()), file = file)
  copy <- addFile(own, file)
  checkEquals(gsub("^.+/", "", file), own$files)
  checkEquals(gsub("^.+/", "", file), copy$files)
  
  
}

uniTestDeleteFile <-
  function()
{
  stop("not yet implemented")
}

unitTestLoadFromFiles <-
  function()
{
  
}
  






