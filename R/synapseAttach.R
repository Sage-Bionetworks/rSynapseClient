synapseAttach <- 
	function(entity, filepath) 
{

	## compute md5 of the file
	token <- list(fileName=basename(filepath), md5=as.character(tools::md5sum(filepath)))

	## get a fresh copy of the entity
	entity <- getEntity(entity)

	## as repo for a presigned S3 URL
	token <- synapseClient:::synapsePost(
		paste("/entity", entity$properties$id, "s3AttachmentToken", sep="/"), token)
	
	## upload attachment file to S3
	synapseClient:::synapseUploadFile(token$presignedUrl, filepath, token$md5, contentType=token$contentType)

	## append to the entity's list of attachment
	attachmentData <- list(name=token$fileName, tokenId=token$tokenId, md5=token$md5)
	if ( is.null(entity$properties$attachments) )
		entity$properties$attachments <- list()
	entity$properties$attachments[[length(entity$properties$attachments)+1]] <- attachmentData

	entity <- updateEntity(entity)
}
