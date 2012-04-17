# TODO: Add comment
# 
# Author: furia
###############################################################################


## package-local 'getter'
.getCache <-
  function(key)
{
  cache <- new("GlobalCache")
  cache@env[[key]]
}

## package-local 'setter'
.setCache <-
  function(key, value)
{
  cache <- new("GlobalCache")
  cache@env[[key]] <- value
}

.deleteCache <-
  function(keys)
{
  cache <- new("GlobalCache")
  indx <- which(keys %in% objects(cache@env))
  if(length(indx) > 0)
    rm(list=keys[indx], envir=cache@env)
}

as.environment.GlobalCache <-
  function(x)
{
  x@env
}

objects.GlobalCache <-
    function(name, all.names = FALSE, pattern=NULL)
{
  objects(envir = as.environment(name), all.names, pattern)
}