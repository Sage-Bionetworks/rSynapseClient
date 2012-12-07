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

.onAttach <-
  function(libname, pkgname)
{
  tou <- "\nTERMS OF USE NOTICE:
    When using Synapse, remember that the terms and conditions of use require that you:
    1) Attribute data contributors when discussing these data or results from these data.
    2) Not discriminate, identify, or recontact individuals or groups represented by the data.
    3) Use and contribute only data de-identified to HIPAA standards.
    4) Redistribute data only under these same terms of use.\n"

  ##set the R_OBJECT cache directory. check for a funcitonal zip first
  packageStartupMessage("Verifying zip installation...")

  ff <- tempfile()
  file.create(ff)
  zipfile <- tempfile()
  suppressWarnings(
    ans <- utils::zip(zipfile, ff)
  )
  unlink(ff)
  unlink(zipfile, recursive = TRUE)
  if(ans != 0){
    packageStartupMessage("zip was not found on your system and so the Synapse funcionality related to file and object storage will be limited. To fix this, make sure that 'zip' is executable from your system's command interpreter.")
    .setCache("rObjCacheDir", .Platform$file.sep)
    .setCache("hasZip", FALSE)
  }else{
    packageStartupMessage("OK")
    .setCache("rObjCacheDir", ".R_OBJECTS/")
    .setCache("hasZip", TRUE)
  }
  
  if(!.getCache("useJava"))
    packageStartupMessage("NOTE: rJava and/or the Synapse Java client are not installed on your system and so file \nupload/download performance will be reduced and file uploads will be limited to 5GB. \nConsider installing rJava by typing install.packages(rJava) at the command prompt")
  
  packageStartupMessage(tou)
}

.onLoad <-
  function(libname, pkgname)
{  

  classpath <- c(list.files(file.path(find.package("synapseClient"), "java"), full.names=TRUE, pattern='jar$', recursive=FALSE))
  packageStartupMessage("\nChecking rJava installation...")

  .setCache("useJava", FALSE)

  if(("rJava" %in% utils::installed.packages())){
    options(java.parameters="-Xrs")
    javaInitReturn <- tryCatch(
      rJava::.jinit(classpath), 
      error = function(e){
        print(e)
        .setCache("useJava", FALSE)
        -1L
      }
    )
    if(javaInitReturn >= 0L){
      tryCatch({
          # use the non-default text-based progress listener for uploads
          progress <- rJava::.jnew("org/sagebionetworks/client/TextProgressListener")
          uploader <- rJava::.jnew("org/sagebionetworks/client/DataUploaderMultipartImpl")
          uploader$setProgressListener(progress)

          .setCache("mpUploader", uploader)
          .setCache("useJava", TRUE)
          packageStartupMessage("OK")
        }, error = function(e) {
          print(e)
          .setCache("useJava", FALSE)
        }
      )
    }
  }

  .setCache("curlOpts", list(low.speed.time=60, low.speed.limit=1, connecttimeout=300, followlocation=TRUE, ssl.verifypeer=TRUE, verbose = FALSE, cainfo=file.path(libname, pkgname, kCertBundle)))
  .setCache("curlHeader", c('Content-Type'="application/json", Accept = "application/json", "Accept-Charset"="utf-8", "User-Agent" = .userAgent()))
  .setCache("sessionRefreshDurationMin", 1440)
  .setCache("curlWriter", getNativeSymbolInfo("_writer_write", PACKAGE="synapseClient")$address)
  .setCache("curlReader", getNativeSymbolInfo("_reader_read", PACKAGE="synapseClient")$address)
  .setCache("synapseBannerPath", file.path(libname, pkgname, "images", "synapse_banner.gif"))
  .setCache("annotationTypeMap", kSynapseRAnnotationTypeMap)
  .setCache("anonymous", FALSE)
  .setCache("downloadSuffix", "unpacked")
  .setCache("debug", FALSE)
  
  synapseResetEndpoints()
  
  synapseDataLocationPreferences(kSupportedDataLocationTypes)
  synapseCacheDir(gsub("[\\/]+", "/", path.expand("~/.synapseCache")))
  ## check RJSONIO version
  if(installed.packages()['RJSONIO', 'Version'] == "1.0-0")
    stop("An unsupported version of RJSONIO is installed on your system. For instructions on how to resolve the issue visit this web page: https://sagebionetworks.jira.com/wiki/display/SYNR/I%27m+unable+to+download+or+upload+entity+data")


  entities <- synapseClient:::entitiesToLoad()
  for(ee in entities){
    synapseClient:::defineEntityClass(ee, package="synapseClient", where=.Internal(getRegisteredNamespace(as.name("synapseClient"))))
    synapseClient:::defineEntityConstructors(ee, package="synapseClient", where=.Internal(getRegisteredNamespace(as.name("synapseClient"))))
  }
}


.userAgent<-function() {
  myOwnVersion<-packageDescription("synapseClient", fields="Version")
  paste("synapseRClient", myOwnVersion, sep="/")
}



