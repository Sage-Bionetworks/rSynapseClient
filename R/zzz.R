## Startup functions and global constants
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

# For user preferences and other configuration data
.cache <- new.env(parent=emptyenv())

## package-local 'getter'
.getCache <-
  function(key)
{
  .cache[[key]]
}

## package-local 'setter'
.setCache <-
  function(key, value)
{
  .cache[[key]] <- value
}

.deleteCache <-
  function(keys)
{
  indx <- which(keys %in% ls(.cache))
  if(length(indx) > 0)
    rm(list=keys[indx], envir=.cache)
}

.onLoad <-
  function(libname, pkgname)
{
  ##set the R_OBJECT cache directory. check for a funcitonal zip first
  message("Verifying zip installation")
  ff <- tempfile()
  file.create(ff)
  ans <- zip(tempfile(), ff)
  if(ans != 0){
    warning("zip was not found on your system and so the Synapse funcionality related to file and object storage will be limited. To fix this, make sure that 'zip' is executable from your system's command interpreter.")
    .setCache("rObjCacheDir", .Platform$file.sep)
    .setCache("hasZip", FALSE)
  }else{
    message("OK")
    .setCache("rObjCacheDir", ".R_OBJECTS")
    .setCache("hasZip", TRUE)
  }
}