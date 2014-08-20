#!/usr/bin/env Rscript
#
# Retrieve dependencies necessary to build the package
# (Note:  This is distinct from the R package dependencies required to use 
# the package after it's built.)
# 
# Author: brucehoff
###############################################################################

# this is maven artifactory url where the Synapse JSON class schemas are found
# the version, e.g. "54.0" needs to be filled in
jsonSchemaURLTemplate<-"http://sagebionetworks.artifactoryonline.com/sagebionetworks/libs-releases-local/org/sagebionetworks/lib-auto-generated/%s/lib-auto-generated-%s.jar"

move<-function(filename, sourcefolder, destfolder) {
  src<-sprintf("%s/%s", sourcefolder, filename)
  dst<-sprintf("%s/%s", destfolder, filename)
  if (unlink(dst, recursive=T, force=T)!=0) stop(sprintf("Unable to delete %s", dst))
  file.rename(src, dst)
}

retrieveDependencies<-function(srcRootDir) {
  # download dependencies and 'unzip' into <srcRootDir>/inst/resources
  jsonSchemaVersion<-"54.0"
  jarFileURL<-sprintf(jsonSchemaURLTemplate, jsonSchemaVersion, jsonSchemaVersion)
  destFile<-tempfile()
  returnCode<-download.file(jarFileURL, destFile, quiet=T)
  if (returnCode!=0) stop(sprintf("Failed to download %s.  Return code is %s", jarFileURL, returnCode))
  unzipTarget<-tempdir()
  unzip(zipfile=destFile, overwrite=T, exdir=unzipTarget)
  packageTarget<-sprintf("%s/inst/resources", srcRootDir)
  move("Register.json", unzipTarget, packageTarget)
  move("schema", unzipTarget, packageTarget)
}

args <- commandArgs(TRUE)
srcRootDir<-args[1]
retrieveDependencies(srcRootDir)
