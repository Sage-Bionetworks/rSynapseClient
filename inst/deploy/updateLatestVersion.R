###############################################################################
# updateLatestVersion.R
# Author: brucehoff
# reads the Version info (latest version and black list) from S3, updates the latestVersion, and writes it back
#
###############################################################################

library("RJSONIO")
library("RCurl") # includes base64()
library("digest") # includes hmac()

uploadToS3File<-function(content, bucket, targetFileName, awsAccessKeyId, secretAccessKey) {
  #UTC timestamp "yyyy-MM-dd'T'HH:mm:ss.SSS'Z'" a bit in the future (e.g. an hour from now)
  futureOffsetSeconds <- 100
  expirationTimestamp <- sprintf("%sZ", format(Sys.time()+futureOffsetSeconds,"%Y-%m-%dT%H:%M:%OS2", tz="UTC"))
  # this is a hack. drop leap seconds
  expirationTimestamp <- gsub(":6[01][\\.]", ":59.", expirationTimestamp)
  policyRaw <- sprintf("{\"expiration\": \"%s\",\"conditions\": [{\"bucket\": \"%s\" },{\"acl\": \"public-read\" },[\"eq\",\"$key\", \"%s\"],[\"content-length-range\", 0, \"10000\"],[\"starts-with\", \"$Content-Type\", \"application/json\"],]}",
    expirationTimestamp, bucket, targetFileName, bucket, targetFileName)
  policyBase64<-base64(policyRaw, encode=TRUE)[1]
  signatureRaw<-hmac(secretAccessKey, policyBase64, algo="sha1", raw=TRUE)
  signatureBase64 <- base64(signatureRaw, encode=TRUE)
  
  uri <-sprintf("http://s3.amazonaws.com/%s", bucket)
  postfields <- list(
    key=targetFileName, 
    acl="public-read", 
    AWSAccessKeyId=awsAccessKeyId, 
    Policy=policyBase64, 
    Signature=signatureBase64, 
    "Content-Type"="application/json",
    file=content)
  
  # if warnings are not suppressed prints "Found possible curl options in form parameters: file" 
  oldWarn<-options("warn")
  options(warn=-1)
  postForm(uri, .params=postfields)
  options(oldWarn)
}

# get the version of this package (assumed to be the latest) and put it in the public 'versions' resource
updateLatestVersion<-function(versionsEndpoint, awsAccessKeyId, secretAccessKey, releaseNotes=NULL) {
  targetFileName<-"synapseRClient"
  uri <- sprintf("%s/%s", versionsEndpoint, targetFileName)
  fileContent<-fromJSON(getURL(uri))
  fileContent$latestVersion<-packageDescription("synapseClient", fields="Version")
  if (!is.null(releaseNotes)) fileContent$releaseNotes<-releaseNotes
  # now upload to S3
  bucket <- versionsEndpoint
  uploadToS3File(toJSON(fileContent), bucket, targetFileName, awsAccessKeyId, secretAccessKey)
}

