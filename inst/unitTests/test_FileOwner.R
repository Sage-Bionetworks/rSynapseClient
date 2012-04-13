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
          "wObjOwn",
          contains = "LocationOwner",
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



