#
# synapseLinkExternalFile
#
# Bruce Hoff
#
# links external URL via the Synapse File Services, returning a FileHandle
#

synapseLinkExternalFile<-function(externalURL, contentType, contentSize, contentMd5, storageLocationId) {
  fileName<-basename(externalURL)
  if (is.null(contentType)) {
    contentType<-getMimeTypeForFile(fileName)
  }
  uri<- "/externalFileHandle"
  body<-list(externalURL=externalURL, 
		  fileName=fileName, 
		  contentType=contentType, 
			contentSize=contentSize,
			contentMd5=contentMd5,
		  concreteType="org.sagebionetworks.repo.model.file.ExternalFileHandle",
		  storageLocationId=storageLocationId)
  synapsePost(uri, body, endpoint=synapseFileServiceEndpoint())
}