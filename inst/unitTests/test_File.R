# These are the test_Locationable tests adapted for Files
#
###############################################################################

.setUp <-
    function()
{
  synapseClient:::.setCache("oldWarn", options("warn")[[1]])
  options(warn=2L)
}

.tearDown <-
    function()
{
  options(warn = synapseClient:::.getCache("oldWarn"))
  if(!is.null(name <- synapseClient:::.getCache("detachMe"))){
    detach(name)
    synapseClient:::.deleteCache('detachMe')
  }
}


unitTestAddObject <-
    function()
{
  own <- new("File")

  foo<-diag(10)
  copy <- addObject(own, foo)
  checkEquals(foo, getObject(copy, "foo"))
  
  copy<-renameObject(copy, "foo", "boo")
  checkEquals(foo, getObject(copy, "boo"))
  checkTrue(is.null(getObject(copy, "foo")))
  
  deleted<-deleteObject(copy, "boo")
  checkTrue(is.null(getObject(deleted, "boo")))
}


unitTestGetObject <-
    function()
{
  own <- new("File")
  foo <- "boo"
  own<-addObject(own, foo)
  checkEquals(getObject(own, "foo"), "boo")
}


