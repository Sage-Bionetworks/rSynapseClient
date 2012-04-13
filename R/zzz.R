## Startup functions and global constants
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

.onLoad <-
  function(libname, pkgname)
{
  ##set the R_OBJECT cache directory. check for a funcitonal zip first
  message("Verifying zip installation")
  ff <- tempfile()
  file.create(ff)
  zipfile <- tempfile()
  suppressWarnings(
    ans <- zip(zipfile, ff)
  )
  unlink(ff)
  unlink(zipfile, recursive = TRUE, force = TRUE)
  if(ans != 0){
    warning("zip was not found on your system and so the Synapse funcionality related to file and object storage will be limited. To fix this, make sure that 'zip' is executable from your system's command interpreter.")
    .setCache("rObjCacheDir", .Platform$file.sep)
    .setCache("hasZip", FALSE)
  }else{
    message("OK")
    .setCache("rObjCacheDir", ".R_OBJECTS")
    .setCache("hasZip", TRUE)
  }
  
  .setCache("curlHeader", c('Content-Type'="application/json", Accept = "application/json", "Accept-Charset"="utf-8"))
  .setCache("sessionRefreshDurationMin", 1440)
}
