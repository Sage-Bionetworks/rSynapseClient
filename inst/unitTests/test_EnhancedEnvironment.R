# TODO: Add comment
# 
# Author: furia
###############################################################################

unitTestAssignment <-
  function()
{
  ee <- new("EnhancedEnvironment")
  ee[['foo']] <- "bar"
  checkEquals(names(ee), "foo")
 
}

unitTestNames <-
  function()
{
  ee <- new("EnhancedEnvironment")
  ee$foo <- "bar"
  checkEquals(names(ee), "foo")
}

unitTestBracketAccessor <-
  function()
{
  ee <- new("EnhancedEnvironment")
  ee$foo <- "bar"
  
  checkEquals(ee["foo"], list(foo="bar"))
  checkEquals(class(ee["foo"]), "list")
  checkEquals(names(ee["foo"]), "foo")
}

unitTestDoubleBracketAccessor <-
  function()
{
  ee <- new("EnhancedEnvironment")
  ee$foo <- "bar"
  
  checkEquals(ee[["foo"]], "bar")
  ee$blah <- "boo"
  checkException(ee[[c("foo", "blah")]])
}

unitTestAccessor <-
  function()
{
  ee <- new("EnhancedEnvironment")
  ee$foo <- "bar"
  
  checkEquals(ee$foo, "bar")
}

unitTestMultipleObjects <-
  function()
{
  ee <- new("EnhancedEnvironment")
  ee$foo <- "bar"
  ee$boo <- "blah"

  checkTrue(all(names(ee) == sort(c("foo", "boo"))))
  checkTrue(all(ee[1:2] == c("blah", "bar")))
  checkTrue(all(names(ee[1:2]) == c("boo", "foo")))
  checkTrue(all(ee[c(2,1)] == c("bar", "blah")))
  checkTrue(all(names(ee[c(2,1)]) == c("foo", "boo")))
  
  checkTrue(is.null(names(ee[[1]])))
  checkEquals(ee[[1]], "blah")
  checkEquals(ee[[2]], "bar")
  
  checkTrue(all(names(ee[]) == sort(c("foo", "boo"))))
  checkTrue(all(ee[] == c("blah", "bar")))
}

unitTestShow <-
  function()
{
  ee <- new("EnhancedEnvironment")
  checkEquals(capture.output(show(ee)), "character(0)")
  ee$foo <- "bar"
  checkEquals(capture.output(show(ee)), "[1] foo (character)")
  ee$boo <- 1
  checkTrue(all(capture.output(show(ee)) == c("[1] boo (numeric)", "[2] foo (character)")))
}