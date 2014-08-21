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
  # download dependencies and 'unzip' into <srcRootDir>/temp
  jsonSchemaVersion<-"54.0"
  jarFileURL<-sprintf(jsonSchemaURLTemplate, jsonSchemaVersion, jsonSchemaVersion)
  scratchDir<-sprintf("%s/%s", srcRootDir, "temp")
  dir.create(scratchDir)
  if (!file.exists(scratchDir)) stop(sprintf("Could not create %s", scratchDir))
  destFile<-sprintf("%s/%s", scratchDir, "temp.jar")
  returnCode<-download.file(jarFileURL, destFile, quiet=T)
  if (returnCode!=0) stop(sprintf("Failed to download %s.  Return code is %s", jarFileURL, returnCode))
  unzipTarget<-scratchDir
  unzip(zipfile=destFile, overwrite=T, exdir=unzipTarget)
  packageTarget<-sprintf("%s/inst/resources", srcRootDir)
  move("Register.json", unzipTarget, packageTarget)
  move("schema", unzipTarget, packageTarget)
 # if (unlink(scratchDir, recursive=T, force=T)!=0) stop(sprintf("Unable to delete %s", scratchDir))
}

args <- commandArgs(TRUE)
srcRootDir<-args[1]
retrieveDependencies(srcRootDir)
