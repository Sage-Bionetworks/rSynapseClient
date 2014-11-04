# unit tests for functions in uplaodFile
# 
# Author: brucehoff
###############################################################################

unitTestGetDirectorySequence <- function() {
  checkEquals(synapseClient:::getDirectorySequence(""), "")
  checkEquals(synapseClient:::getDirectorySequence("/"), "/")
  checkEquals(synapseClient:::getDirectorySequence("foo"), "foo")
  checkEquals(synapseClient:::getDirectorySequence("/foo"), "/foo")
  checkEquals(synapseClient:::getDirectorySequence("foo/"), "foo")
  checkEquals(synapseClient:::getDirectorySequence("/foo/"), "/foo")
  checkEquals(synapseClient:::getDirectorySequence("foo/bar"), c("foo", "foo/bar"))
  checkEquals(synapseClient:::getDirectorySequence("foo/bar/"), c("foo", "foo/bar"))
  checkEquals(synapseClient:::getDirectorySequence("/foo/bar"), c("/foo", "/foo/bar"))
  checkEquals(synapseClient:::getDirectorySequence("/foo/bar/"), c("/foo", "/foo/bar"))
  checkEquals(synapseClient:::getDirectorySequence("/foo/bar/bas"), c("/foo", "/foo/bar", "/foo/bar/bas"))
  checkEquals(synapseClient:::getDirectorySequence("foo/bar/bas"), c("foo", "foo/bar", "foo/bar/bas"))
  checkEquals(synapseClient:::getDirectorySequence("foo/bar/bas/"), c("foo", "foo/bar", "foo/bar/bas"))
}