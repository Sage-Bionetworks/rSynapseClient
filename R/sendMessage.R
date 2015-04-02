## Send message to users
## 
## Author: Bruce Hoff <bruce.hoff@sagebase.org>
##############################################################################
sendMessage<-function(userIdList, subject, body) {
  fileHandle<-uploadStringToSynapseS3File(body)
  messageContent <- list(recipients=userIdList, fileHandleId=fileHandle$id, subject=subject)
  synRestPOST("/message", messageContent)
}