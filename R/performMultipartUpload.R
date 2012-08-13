# TODO: Add comment
# 
# Author: furia
###############################################################################


.performMultipartUpload <- function(entity, filePath) {
  
  ## Get credentials needed to upload to S3
  s3Token <- list()
  s3Token$md5 <- as.character(tools::md5sum(filePath))
  s3Token$path <- filePath
  s3Token <- synapseClient:::synapsePost(propertyValue(entity, "s3Token"), s3Token)
  
  ## instantiate a java S3Token class and populate the fields
  javaS3Token <- rJava:::.jnew("org/sagebionetworks/repo/model/S3Token")
  
  rJava::.jcall(javaS3Token, "V", 'setMd5', s3Token$md5)
  rJava::.jcall(javaS3Token, "V", 'setContentType', s3Token$contentType)
  rJava::.jcall(javaS3Token, "V", 'setPath', s3Token$path)
  rJava::.jcall(javaS3Token, "V", 'setAccessKeyId', s3Token$accessKeyId)
  rJava::.jcall(javaS3Token, "V", 'setSecretAccessKey', s3Token$secretAccessKey)
  rJava::.jcall(javaS3Token, "V", "setBucket", s3Token$bucket)
  rJava::.jcall(javaS3Token, "V", "setSessionToken", sessionToken())
  
  jfile <- rJava::.jnew("java/io/File", filePath)
  uploader <- .getCache("mpUploader")
  rJava::.jcall(uploader, "V", "uploadDataMultiPart", javaS3Token, jfile)
  
  
  ## Store the new location in Synapse
  ## Note, to future-proof this we would put in logic to merge locationData, 
  ## but we don't have any entities with data in two locations yet, so let's 
  ## save that for the java implementation of this
  locationData <- list()
  locationData$path <- s3Token$path
  locationData$type <- "awss3"
  propertyValue(entity, "locations") <- list(locationData)
  propertyValue(entity, "md5") <- s3Token$md5
  propertyValue(entity, "contentType") <- s3Token$contentType
  entity <- updateEntity(entity)  
}
