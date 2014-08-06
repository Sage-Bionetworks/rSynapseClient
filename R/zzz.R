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

  entities <- entitiesToLoad()
  for(ee in entities){ 
    defineEntityClass(ee, package="synapseClient", where=.Internal(getRegisteredNamespace(as.name("synapseClient"))))
    defineEntityConstructors(ee, package="synapseClient", where=.Internal(getRegisteredNamespace(as.name("synapseClient"))))
  }
  
  nonEntities<-list(
    # Note:  each class must come after its dependenciesboolean
    c("org.sagebionetworks.repo.model.attachment.AttachmentData", "AttachmentData"),
    c("org.sagebionetworks.repo.model.message.Settings", "Settings"),
    c("org.sagebionetworks.repo.model.UserPreference", "UserPreference"),
    c("org.sagebionetworks.repo.model.UserPreferenceBoolean", "UserPreferenceBoolean"),
    c("org.sagebionetworks.repo.model.UserProfile", "UserProfile"),
    c("org.sagebionetworks.evaluation.model.Evaluation", "Evaluation"),
    c("org.sagebionetworks.repo.model.annotation.AnnotationBase", "AnnotationBase"),
    c("org.sagebionetworks.repo.model.annotation.DoubleAnnotation", "DoubleAnnotation"),
    c("org.sagebionetworks.repo.model.annotation.LongAnnotation", "LongAnnotation"),
    c("org.sagebionetworks.repo.model.annotation.StringAnnotation", "StringAnnotation"),
    c("org.sagebionetworks.repo.model.annotation.Annotations", "Annotations"),
    c("org.sagebionetworks.evaluation.model.Submission", "SubmissionMetadata"),
    c("org.sagebionetworks.evaluation.model.SubmissionStatus", "SubmissionStatus"),
    c("org.sagebionetworks.evaluation.model.Participant", "Participant"),
    c("org.sagebionetworks.repo.model.wiki.WikiHeader", "WikiHeader"),
    c("org.sagebionetworks.repo.model.annotation.Annotations", "Annotations"),
    c("org.sagebionetworks.repo.model.annotation.DoubleAnnotation", "DoubleAnnotation"),
    c("org.sagebionetworks.repo.model.annotation.LongAnnotation", "LongAnnotation"),
    c("org.sagebionetworks.repo.model.annotation.StringAnnotation", "StringAnnotation"),
    c("org.sagebionetworks.repo.model.ResourceAccess", "ResourceAccess"),
    c("org.sagebionetworks.repo.model.AccessControlList", "AccessControlList"),
    
    # Table classes
    c("org.sagebionetworks.repo.model.table.Row", "Row"),
    c("org.sagebionetworks.repo.model.table.RowReference", "RowReference"),
    c("org.sagebionetworks.repo.model.table.RowReferenceSet", "RowReferenceSet"),
    c("org.sagebionetworks.repo.model.table.RowSet", "TableRowSet"),
    c("org.sagebionetworks.repo.model.table.ColumnModel", "TableColumn")
    #c("org.sagebionetworks.repo.model.table.", ""),
    )
    
  
  for(ee in nonEntities) { 
    # only define the class if it's not already defined
    if (!isClassDefined(ee)) {
      defineS4ClassForSchema(ee[1], ee[2])
    }
  }
}


.userAgent<-function() {
  myOwnVersion<-packageDescription("synapseClient", fields="Version")
  paste("synapseRClient", myOwnVersion, sep="/")
}



