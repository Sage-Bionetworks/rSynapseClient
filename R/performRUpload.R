# TODO: Add comment
# 
# Author: furia
###############################################################################



.performRUpload <- function(entity, filePath) {
  ## parse out the filename
  filename <- basename(filePath)
  
  ## Get credentials needed to upload to S3
  s3Token <- list()
  s3Token$md5 <- as.character(md5sum(filePath))
  s3Token$path <- filename
  s3Token <- synapsePost(propertyValue(entity, "s3Token"), s3Token)
  
  ## Upload the data file to S3
  tryCatch(
    synapseUploadFile(url = s3Token$presignedUrl,
      srcfile = filePath,
      checksum = s3Token$md5,
      contentType = s3Token$contentType
    ),
    error = function(e){
      warning(sprintf("failed to upload data file, please try again: %s", e))
      return(entity)
    }
  )
  
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

