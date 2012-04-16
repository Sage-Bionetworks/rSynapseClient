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
  checkTrue(is.null(getObject(own, "bar")))
}

unitTestNames <-
  function()
{
  own <- new("ObjOwn")
  checkTrue(all(names(own) == c("objects")))
  own$objects$bar <- "foo"
  checkTrue(all(names(own) == c("objects")))
}

unitTestAddObject <-
  function()
{
  own <- new("ObjOwn")
  own$objects$foo <- "bar"
  checkEquals(names(own$objects), "foo")
  checkEquals(own$objects$foo, "bar")
  
  copy <- addObject(own, "boo", "blah")
  checkTrue(all(names(own$objects) %in% c("foo", "blah")))
  checkTrue(all(c("foo", "blah") %in% names(own$objects)))
  checkEquals(own$objects$foo, "bar")
  checkEquals(own$objects$blah, "boo")
  
  checkTrue(all(names(copy$objects) %in% c("foo", "blah")))
  
}

unitTestGetObject <-
  function()
{
  own <- new("ObjOwn")
  own$objects$foo <- "bar"
  checkEquals("bar", getObject(own, "foo"))
}

unitTestDeleteObject <-
  function()
{
  own <- new("ObjOwn")
  own$objects$foo <- "bar"
  copy <- deleteObject(own, "foo")
  checkEquals(length(own$objects), 0L)
  checkEquals(length(copy$objects), 0L)
}

unitTestRenameObject <-
  function()
{
  own <- new("ObjOwn")
  own$objects$foo <- "bar"

  copy <- renameObject(own, "foo", "boo")
  checkEquals(length(own$objects), 1L)
  checkEquals(length(copy$objects), 1L)
  checkEquals(own$objects$boo, "bar")
  checkEquals(copy$objects$boo, "bar")
}

## not sure that this method should be exposed
## leaving getEnv unimplemented for now.
#unitTestGetEnv <-
#  function()
#{
#  stop("Not Yet Implemented")
#}

unitTestAddListUnlist <-
  function()
{
  own <- new("ObjOwn")
  aList <- list(foo = "bar", boo = 1L)
  addObject(own, aList)
  
  checkEquals(length(own$objects), 1L)
  checkEquals("aList", names(own$objects))
  checkEquals(own$objects$aList$foo, "bar")
  checkEquals(own$objects$aList$boo, 1L)
  checkEquals("list", as.character(class(own$objects$aList)))
  
  own <- new("ObjOwn")
  addObject(own, aList, unlist = TRUE)
  
  checkEquals(length(own$objects), 2L)
  checkTrue(all(c("foo", "boo") %in% names(own$objects)))
  checkEquals(own$objects$foo, "bar")
  checkEquals(own$objects$boo, 1L)
  
  own <- new("ObjOwn")
  addObject(own, aList, unlist = FALSE)
  
  checkEquals(length(own$objects), 1L)
  checkEquals("aList", names(own$objects))
  checkEquals(own$objects$aList$foo, "bar")
  checkEquals(own$objects$aList$boo, 1L)
  checkEquals("list", as.character(class(own$objects$aList)))
  
}


addDataFrameUnlist <-
    function()
{
  own <- new("ObjOwn")
  aList <- data.frame(foo = "bar", boo = 1L, stringsAsFactors=F)
  addObject(own, aList)
  
  checkEquals(length(own$objects), 1L)
  checkEquals("aList", names(own$objects))
  checkEquals(own$objects$aList$foo, "bar")
  checkEquals(own$objects$aList$boo, 1L)
  
  own <- new("ObjOwn")
  addObject(own, aList, unlist = TRUE)
  
  checkEquals(length(own$objects), 2L)
  checkTrue(all(c("foo", "boo") %in% names(own$objects)))
  checkEquals(own$objects$foo, "bar")
  checkEquals(own$objects$boo, 1L)
  
  own <- new("ObjOwn")
  addObject(own, aList, unlist = FALSE)
  
  checkEquals(length(own$objects), 1L)
  checkEquals("aList", names(own$objects))
  checkEquals(own$objects$aList$foo, "bar")
  checkEquals(own$objects$aList$boo, 1L)
}


