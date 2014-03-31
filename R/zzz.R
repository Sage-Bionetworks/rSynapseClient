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

  packageStartupMessage(tou)
}

.onLoad <-
  function(libname, pkgname)
{  
  ##set the R_OBJECT cache directory. check for a functional zip first
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
    packageStartupMessage("zip was not found on your system and so the Synapse functionality related to file and object storage will be limited. To fix this, make sure that 'zip' is executable from your system's command interpreter.")
    .setCache("rObjCacheDir", .Platform$file.sep)
    .setCache("hasZip", FALSE)
  }else{
    packageStartupMessage("OK")
    .setCache("rObjCacheDir", ".R_OBJECTS/")
    .setCache("hasZip", TRUE)
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
  # this is the maximum number of times a web request will be tried when there is a temporary outage.  Must be >0
  .setCache("webRequestMaxTries", 10)
  .setCache("webRequestMaxRedirects", 3)

  # Fetch endpoints from the config file
  synapseResetEndpoints()
  try({
      config <- ConfigParser()
      if (Config.hasOption(config, "endpoints", "repoEndpoint")) {
        synapseRepoServiceEndpoint(Config.getOption(config, "endpoints", "repoEndpoint"))
      }
      if (Config.hasOption(config, "endpoints", "authEndpoint")) {
        synapseAuthServiceEndpoint(Config.getOption(config, "endpoints", "authEndpoint"))
      }
      if (Config.hasOption(config, "endpoints", "fileHandleEndpoint")) {
        synapseFileServiceEndpoint(Config.getOption(config, "endpoints", "fileHandleEndpoint"))
      }
      if (Config.hasOption(config, "endpoints", "portalEndpoint")) {
        synapsePortalEndpoint(Config.getOption(config, "endpoints", "portalEndpoint"))
      }
      
      # Fetch the debug flag from the config file
      .setCache("debug", Config.hasSection(config, "debug"))
  }, silent = TRUE)
  
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
    
    nonEntities<-list(
      "org.sagebionetworks.repo.model.UserProfile",
      "org.sagebionetworks.evaluation.model.Evaluation",
      "org.sagebionetworks.evaluation.model.SubmissionStatus",
      "org.sagebionetworks.evaluation.model.SubmissionBundle",
      "org.sagebionetworks.evaluation.model.Participant",
      "org.sagebionetworks.repo.model.wiki.WikiHeader"
    )
    
    for(ee in nonEntities){ 
      synapseClient:::defineNONEntityClass(ee, package="synapseClient", where=.Internal(getRegisteredNamespace(as.name("synapseClient"))))
      synapseClient:::defineEntityConstructors(ee, package="synapseClient", where=.Internal(getRegisteredNamespace(as.name("synapseClient"))))
    }
    
    # we override FileEntity, Submission with our own class constructor.  See also entitiesToLoad() in AAAschema.R
addToEntityTypeMap(className="FileListConstructor", jsonSchemaName="org.sagebionetworks.repo.model.FileEntity")
addToEntityTypeMap(className="SubmissionListConstructor", jsonSchemaName="org.sagebionetworks.evaluation.model.Submission")
}


.userAgent<-function() {
  myOwnVersion<-packageDescription("synapseClient", fields="Version")
  paste("synapseRClient", myOwnVersion, sep="/")
}



