#
# synapseLinkExternalFile
#
# Bruce Hoff
#
# links external URL via the Synapse File Services, returning a FileHandle
#

synapseLinkExternalFile<-function(externalURL, contentType) {
  fileName<-basename(externalURL)
  if (is.null(contentType)) {
    contentType<-getMimeTypeForFile(fileName)
  }
  uri<- "/externalFileHandle"
  body<-list(externalURL=externalURL, fileName=fileName, contentType=contentType, concreteType="org.sagebionetworks.repo.model.file.ExternalFileHandle")
  synapsePost(uri, body, endpoint=synapseFileServiceEndpoint())
}