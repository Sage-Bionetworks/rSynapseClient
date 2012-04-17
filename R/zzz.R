## Startup functions and global constants
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

kCertBundle <- "certificateBundle/cacert.pem"
kSynapseRAnnotationTypeMap <- list(
  stringAnnotations = "character",
  longAnnotations = "integer",
  doubleAnnotations = "numeric",
  dateAnnotations = "POSIXt"
)
kSupportedDataLocationTypes <- c("external", "awss3")


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
  .setCache("curlOpts", list(followlocation=TRUE, ssl.verifypeer=TRUE, verbose = FALSE, cainfo=file.path(libname, pkgname, kCertBundle)))
  .setCache("curlHeader", c('Content-Type'="application/json", Accept = "application/json", "Accept-Charset"="utf-8"))
  .setCache("sessionRefreshDurationMin", 1440)
  .setCache("curlWriter", getNativeSymbolInfo("_writer_write", PACKAGE="synapseClient")$address)
  .setCache("curlReader", getNativeSymbolInfo("_reader_read", PACKAGE="synapseClient")$address)
  .setCache("synapseBannerPath", file.path(libname, pkgname, "images", "synapse_banner.gif"))
  .setCache("annotationTypeMap", kSynapseRAnnotationTypeMap)
  .setCache("anonymous", FALSE)
  .setCache("downloadSuffix", "unpacked")
  .setCache("debug", FALSE)
  
  synapseResetEndpoints()
  synapseCacheDir("~/.synapseCache")
  
  # used in entityToFileCache.R
  ENTITY_FILE_NAME<<-"entity.json"
  ANNOTATIONS_FILE_NAME<<-"annotations.json"
  
}

.onUnload <- function(libpath) .Last.lib()

.Last.lib <- function(...) {
  try(stoppedStep <- stopStep(), silent=TRUE)
}
