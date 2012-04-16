# TODO: Add comment
# 
# Author: mfuria
###############################################################################

.setUp <-
    function()
{
  synapseClient:::.setCache("oldWarn", options("warn"))
  
  env <- attach(NULL, name = "testEnv")
  tryCatch({
        setPackageName("testEnv", env)
        suppressWarnings(
            setRefClass(
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
  
}

.tearDown <-
    function()
{
  options(warn = synapseClient:::.getCache("oldWarn")[[1]])
  detach("testEnv")
}

unitTestAddFile <-
    function()
{
  own <- new("LocOwn")
  
}

