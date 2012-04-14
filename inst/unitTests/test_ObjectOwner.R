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
        setRefClass(
          "ObjOwn",
          contains = "ObjectOwner",
          where = env
        )
      )
    }, 
    error = function(e){
      detach("testEnv")
      stop(e)
    }
  )
}

.tearDown <-
  function()
{
  detach("testEnv")
}

unitTestGet <-
  function()
{
  own <- new("ObjOwn")
  own$objects$bar <- "foo"
  checkEquals(own$objects$bar, "foo")
}

unitTestGetInvalidObject <-
  function()
{
  own <- new("ObjOwn")
  checkException(getObject(own, "bar"))
}

unitTestNames <-
  function()
{
  own <- new("ObjOwn")
  checkTrue(all(names(own) == c("cacheDir", "files", "objects")))
  own$objects$bar <- "foo"
  checkTrue(all(names(own) == c("cacheDir", "files", "objects")))
}

unitTestAddObject <-
  function()
{
  stop("Not Yet Implemented")
}

unitTestGetObject <-
  function()
{
  stop("Not Yet Implemented")
}

unitTestDeleteObject <-
  function()
{
  stop("Not Yet Implemented")
}

unitTestRenameObject <-
  function()
{
  stop("Not Yet Implemented")
}

unitTestGetEnv <-
  function()
{
  stop("Not Yet Implemented")
}

unitTestObjects <-
  function()
{
  stop("Not Yet Implemented")
}

unitTestAddListUnlist <-
  function()
{
  stop("Not Yet Implemented")
}

unitTestAddList <-
  function()
{
  stop("Not Yet Implemented")
}

unitTestAddDataFrame <-
  function()
{
  stop("Not Yet Implemented")
}


