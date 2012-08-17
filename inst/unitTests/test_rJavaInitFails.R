.setUp <-
  function()
{
	myJinit <- function(classpath){ stop("failed to initialize") }
	attr(myJinit, "oldDef") <- rJava::.jinit
	assignInNamespace(".jinit", myJinit, "rJava")
}

.tearDown <-
  function()
{
	assignInNamespace(".jinit", attr(rJava::.jinit, "oldDef"), "rJava")
	unloadNamespace('synapseClient')
	library(synapseClient)
}

unitTestInitFails <-
  function()
{
	unloadNamespace('synapseClient')
	library(synapseClient, quiet=TRUE)
	checkEquals(synapseClient:::.getCache('useJava'), FALSE)
}