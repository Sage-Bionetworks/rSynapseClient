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

	# low.speed.time increased to 600 to fix SYNR-949
  .setCache("curlOpts", list(low.speed.time=600, low.speed.limit=1, connecttimeout=300, followlocation=TRUE, ssl.verifypeer=TRUE, verbose = FALSE, cainfo=file.path(libname, pkgname, kCertBundle)))
  .setCache("curlHeader", c('Content-Type'="application/json; charset=utf-8", Accept = "application/json", "Accept-Charset"="utf-8", "User-Agent" = .userAgent()))
  .setCache("sessionRefreshDurationMin", 1440)
  .setCache("curlWriter", getNativeSymbolInfo("_writer_write", PACKAGE="synapseClient")$address)
	.setCache("curlReader", getNativeSymbolInfo("_reader_read", PACKAGE="synapseClient")$address)
	.setCache("curlStringReader", getNativeSymbolInfo("_string_reader_read", PACKAGE="synapseClient")$address)
	.setCache("synapseBannerPath", file.path(libname, pkgname, "images", "synapse_banner.gif"))
  .setCache("annotationTypeMap", kSynapseRAnnotationTypeMap)
  .setCache("anonymous", FALSE)
  .setCache("downloadSuffix", "unpacked")
  .setCache("debug", FALSE)
  # this is the maximum number of times a web request will be tried when there is a temporary outage.  Must be >0
  .setCache("webRequestMaxTries", 12)
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
  
  synapseCacheDir(gsub("[\\/]+", "/", path.expand("~/.synapseCache")))

  entities <- entitiesToLoad()
  where<-.Internal(getRegisteredNamespace(as.name("synapseClient")))
  for(ee in entities){ 
    defineEntityClass(ee, package="synapseClient", where=where)
    defineEntityConstructors(ee, package="synapseClient", where=where)
  }
  
  # we need a TypedList of UploadDestination, for which there is no schema
  defineTypedList("UploadDestination")
  
  # This is done during class generation but seems to be lost at package load time.  So we do it again.
  populateSchemaToClassMap()
}

.userAgent<-function() {
  myOwnVersion<-packageDescription("synapseClient", fields="Version")
  paste("synapseRClient", myOwnVersion, sprintf("Rv%s.%s", version$major, version$minor), sep="/")
}


