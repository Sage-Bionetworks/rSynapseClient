## TODO: Add comment
## 
## Author: furia
################################################################################
#
#.setUp <- 
#  function()
#{
#  env <- attach(NULL, name = "testEnv")
#  setPackageName("testEnv", env)
#  suppressWarnings(
#      setRefClass(
#          "wObjOwn",
#          contains = "WritableObjectOwner",
#          where = env
#      ) 
#  )
#}
#
#.tearDown <-
#  function()
#{
#  detach("testEnv")
#}
#
#unitTestAddObjectListUnlistTrue <-
#  function()
#{
#  own <- getRefClass("wObjOwn")$new()
#  obj <- list(foo="bar", boo="blah")
#  addObject(own, obj, unlist=TRUE)
#}
#
#
#unitTestDefaultName <-
#  function()
#{
#  own <- getRefClass("wObjOwn")$new()
#  checkEquals(length(own$objects), 0L)
#  
#  id <- 1234
#  addObject(own, id)
#  
#  checkEquals(length(own$objects), 1L)
#  checkEquals(objects(own$objects), "id")
#  checkEquals(own$objects$id, id)
#}
#
#unitTestDefaultNameStringConstant <-
#  function()
#{
#  own <- getRefClass("wObjOwn")$new()
#  checkEquals(length(own$objects), 0L)
#  
#  addObject(own, "foo")
#  
#  checkEquals(length(own$objects), 1L)
#  checkEquals(objects(own$objects), "foo")
#  checkEquals(own$objects$foo, "foo")
#}
#
#unitTestDefaultNameNumericExpression <-
#  function()
#{
#  own <- getRefClass("wObjOwn")$new()
#  checkEquals(length(own$objects), 0L)
#  
#  addObject(own, 1:5)
#  
#  checkEquals(length(own$objects), 1L)
#  
#  checkEquals(objects(own$objects), "1:5")
#  checkTrue(all(own$objects[["1:5"]] == 1:5))
#}
#
#unitTestSpecifyName <-
#  function()
#{
#  own <- getRefClass("wObjOwn")$new()
#  checkEquals(length(own$objects), 0L)
#  
#  name <- "aName"
#  
#  id <- 1234
#  addObject(own, id, name)
#  
#  checkEquals(length(own$objects), 1L)
#  checkEquals(objects(own$objects), name)
#  checkEquals(own$objects[[name]], id)
#}
#
#unitTestSpecifyNameStringConstant <-
#  function()
#{
#  own <- getRefClass("wObjOwn")$new()
#  checkEquals(length(own$objects), 0L)
#  
#  name <- "aName"
#  addObject(own, "foo", name)
#  
#  checkEquals(length(own$objects), 1L)
#  checkEquals(objects(own$objects), name)
#  checkEquals(own$objects[[name]], "foo")
#  
#}
#
#unitTestAddCatchReturn <-
#  function()
#{
#  own <- getRefClass("wObjOwn")$new()
#  
#  caughtOwn <- addObject(own, "foo")
#  checkEquals(names(caughtOwn$objects), "foo")
#}
#
#unitTestAddSingleAsList <- 
#  function()
#{
#  own <- getRefClass("wObjOwn")$new()
#  addObject(own, list(foo="bar"))
#  checkEquals(own$objects$foo, "bar")
#  checkEquals(length(own$objects), 1L)
#}
#
#unitTestAddMultipleAsList <-
#  function()
#{
#  own <- getRefClass("wObjOwn")$new()
#  addObject(own, list(foo="bar", boo="goo"))
#  checkEquals(own$objects$foo, "bar")
#  checkEquals(own$objects$boo, "goo")
#  checkEquals(length(own$objects), 2L)
#}
#
#unitTestAddSingleAsListNoName <-
#  function()
#{
#  own <- getRefClass("wObjOwn")$new()
#  checkException(addObject(own, list("bar")))
#}
#
#unitTestAddMultipleAsListNoName <-
#  function()
#{
#  own <- getRefClass("wObjOwn")$new()
#  checkException(addObject(own, list("bar", foo="goo")))
#}
#
#unitTestAddMatrixSpecifyName <-
#  function()
#{
#  own <- getRefClass("wObjOwn")$new()
#  addObject(own, diag(nrow=10, ncol=10), "diag")
#  checkTrue(all(own$objects$diag == diag(nrow=10, ncol=10)))
#}
#
#unitTestAddMatrixNoName <-
#  function()
#{
#  own <- getRefClass("wObjOwn")$new()
#  addObject(own, diag(nrow=10, ncol=10))
#  checkEquals(length(own$objects), 1L)
#  checkTrue(all(own$objects[[1]] == diag(nrow=10, ncol=10)))
#}
#
#unitTestAddDataFrameNoName <-
#  function()
#{
#  own <- getRefClass("wObjOwn")$new()
#  data <- data.frame(x=1:4, y=c("a", "b", "c", "d"))
#  addObject(own, data)
#  checkEquals(length(objects(own$objects)), 1L)
#  checkTrue(all(own$objects$data == data))
#}
#
#unitTestAddDataFrameWithName <-
#  function()
#{
#  own <- getRefClass("wObjOwn")$new()
#  data <- data.frame(x=1:4, y=c("a", "b", "c", "d"))
#  addObject(own, data, "newName")
#  checkEquals(length(objects(own$objects)), 1L)
#  checkTrue(all(own$objects$newName == data))
#}
#
#unitTestAddListOfDataFramesNoName <-
#  function()
#{
#  own <- getRefClass("wObjOwn")$new()
#  checkException(addObject(own, list(data.frame(x=1:4, y=c("a", "b", "c", "d")), data.frame(y=c("a", "b", "c", "d"), z=5:8))))
#}
#
#unitTestAddListUnlistFalse <-
#  function()
#{
#  own <- getRefClass("wObjOwn")$new()
#  foo <- list(one=1, two=2)
#  addObject(own, foo, unlist=F)
#  checkEquals(length(own$objects), 1L)
#}
#
#unitTestAddListUnlistTrue <-
#  function()
#{
#  own <- getRefClass("wObjOwn")$new()
#  foo <- list(one=1, two=2)
#  addObject(own, foo, unlist=T)
#  checkEquals(length(own$objects), 2L)
#}
#
#unitTestAddListUnlistFalseSpecifyName <-
#  function()
#{
#  own <- getRefClass("wObjOwn")$new()
#  foo <- list(one=1, two=2)
#  addObject(own, foo, name="aName")
#  checkEquals(length(own$objects), 1L)
#  checkEquals("aName", names(own$objects))
#}
#
#unitTestAddListUnlistTrueSpecifyName <-
#  function()
#{
#  own <- getRefClass("wObjOwn")$new()
#  foo <- list(one=1, two=2)
#  checkException(addObject(own, foo, name="aName", unlist=T))
#}
#
### TODO This test is broke. FIX ME!
###unitTestAddFromEnvironmentNoName <-
###    function()
###{
###  own <- new("Layer")
###  env <- new.env()
###  env$foo <- list(one=1, two=2)
###  addObject(own, env$foo, unlist=F)
###  checkEquals("foo", names(own$objects))
###}
#
#unitTestRename <-
#  function()
#{
#  own <- getRefClass("wObjOwn")$new()
#  addObject(own, "foo")
#  
#  renameObject(own, "foo", "bar")
#  checkEquals(length(own$objects), 1L)
#  checkEquals(names(own$objects), "bar")
#}
#
#unitTestRenamCatchReturn <-
#  function()
#{
#  own <- getRefClass("wObjOwn")$new()
#  addObject(own, "foo")
#  
#  caughtLayer <- renameObject(own, "foo", "bar")
#  checkEquals(length(caughtLayer$objects), 1L)
#  checkEquals(names(caughtLayer$objects), "bar")
#}
#
#unitTestRenameInvalidObject <-
#  function()
#{
#  own <- getRefClass("wObjOwn")$new()
#  checkException(renameObject(own, "foo", "bar"))
#  
#  addObject(own, "foo")
#  checkException(renameObject(own, "bar", "boo"))
#}
#
#unitTestRenameSameName <-
#  function()
#{
#  own <- getRefClass("wObjOwn")$new()
#  addObject(own, "foo")
#  renameObject(own, "foo", "foo")
#  checkEquals(length(own$objects), 1L)
#  checkEquals(names(own$objects), "foo")
#}
#
#unitTestMultipleRename <-
#  function()
#{
#  own <- getRefClass("wObjOwn")$new()
#  addObject(own, "foo", "foo")
#  addObject(own, "goo", "goo")
#  addObject(own, "keepMe", "ok")
#  
#  renameObject(own, c("foo", "goo"), c("goo", "foo"))
#  checkEquals(length(own$objects), 3L)
#  checkEquals(own$objects$foo, "goo")
#  checkEquals(own$objects$goo, "foo")
#  checkEquals(own$objects$ok, "keepMe")
#  
#}
#
#unitTestMoreNamesThanObjects <- 
#  function()
#{
#  own <- getRefClass("wObjOwn")$new()
#  addObject(own, "foo")
#  checkException(renameObject(own, "foo", c("bar", "boo")))
#}
#
#unitTestMoreObjectsThanNames <- 
#  function()
#{
#  own <- getRefClass("wObjOwn")$new()
#  addObject(own, "foo")
#  addObject(own, "boo")
#  checkException(renameObject(own, c("foo","boo"), "boo"))
#}
#
#unitTestInvalidWhich <- 
#  function()
#{
#  own <- getRefClass("wObjOwn")$new()
#  addObject(own, "foo")
#  checkException(renameObject(own, "boo", "goo"))
#}
#
#unitTestOverwrite <- 
#  function()
#{
#  own <- getRefClass("wObjOwn")$new()
#  addObject(own, "foo")
#  addObject(own, "goo")
#  renameObject(own, "foo", "goo")
#  checkEquals(length(own$objects), 1L)
#  checkEquals(own$objects$goo, "foo")
#}
#
#unitTestRenameToNew <- 
#  function()
#{
#  own <- getRefClass("wObjOwn")$new()
#  addObject(own, "foo")
#  renameObject(own, "foo", "goo")
#  checkEquals(length(own$objects), 1L)
#  checkEquals(own$objects$goo, "foo")
#}
#
#unitTestOverwriteOneRenameToNewAnother <- 
#  function()
#{
#  own <- getRefClass("wObjOwn")$new()
#  addObject(own, "foo")
#  addObject(own, "goo")
#  renameObject(own, c("foo", "goo"), c("goo", "bar"))
#  checkEquals(length(own$objects), 2L)
#  checkEquals(own$objects$goo, "foo")
#  checkEquals(own$objects$bar, "goo")
#}
#
#unitTestDelete <-
#  function()
#{
#  own <- getRefClass("wObjOwn")$new()
#  addObject(own,"foo", "bar")
#  
#  checkEquals(length(objects(own$objects)), 1L)
#  
#  deleteObject(own, "bar")
#  checkEquals(length(objects(own$objects)), 0L)
#}
#
#unitTestDeleteOneOfTwo <-
#  function()
#{
#  own <- getRefClass("wObjOwn")$new()
#  addObject(own,"foo")
#  checkEquals(length(objects(own$objects)), 1L)
#  
#  addObject(own,"goo")
#  checkEquals(length(objects(own$objects)), 2L)
#  
#  deleteObject(own, "goo")
#  checkEquals(length(objects(own$objects)), 1L)
#  checkEquals(names(own$objects), "foo")
#}
#
#unitTestDeleteAll <-
#  function()
#{
#  own <- getRefClass("wObjOwn")$new()
#  addObject(own,"foo")
#  addObject(own,"goo")
#  checkEquals(length(objects(own$objects)), 2L)
#  
#  deleteObject(own, c("goo", "foo"))
#  checkEquals(length(objects(own$objects)), 0L)
#}
#
#unitTestDeleteNonExisting <-
#  function()
#{
#  own <- getRefClass("wObjOwn")$new()
#  
#  ## this should warn, but not fail
#  deleteObject(own, "fakeObject")
#}
#
#unitTestDeleteCatchReturnValue <-
#  function()
#{
#  own <- getRefClass("wObjOwn")$new()
#  
#  addObject(own, "foo")
#  caughtLayer <- deleteObject(own, "foo")
#  checkEquals(length(own$objects), 0L)
#}
