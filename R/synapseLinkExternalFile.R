#
# synapseLinkExternalFile
#
# Bruce Hoff
#
# links external URL via the Synapse File Services, returning a FileHandle
#

synapseLinkExternalFile<-function(externalURL, fileName, contentType) {
  uri<- "/externalFileHandle"
  body<-list(externalURL=externalURL, fileName=fileName, contentType=contentType, concreteType="org.sagebionetworks.repo.model.file.ExternalFileHandle")
  synapsePost(uri, body, service="FILE")
}