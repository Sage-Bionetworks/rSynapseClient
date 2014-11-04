# unit tests for functions in uplaodFile
# 
# Author: brucehoff
###############################################################################

unitTestGetDirectorySequence <- function() {
  checkEquals(getDirectorySequence(""), "")
  checkEquals(getDirectorySequence("/"), "/")
  checkEquals(getDirectorySequence("foo"), "foo")
  checkEquals(getDirectorySequence("/foo"), "/foo")
  checkEquals(getDirectorySequence("foo/"), "foo")
  checkEquals(getDirectorySequence("/foo/"), "/foo")
  checkEquals(getDirectorySequence("foo/bar"), c("foo", "foo/bar"))
  checkEquals(getDirectorySequence("foo/bar/"), c("foo", "foo/bar"))
  checkEquals(getDirectorySequence("/foo/bar"), c("/foo", "/foo/bar"))
  checkEquals(getDirectorySequence("/foo/bar/"), c("/foo", "/foo/bar"))
  checkEquals(getDirectorySequence("/foo/bar/bas"), c("/foo", "/foo/bar", "/foo/bar/bas"))
  checkEquals(getDirectorySequence("foo/bar/bas"), c("foo", "foo/bar", "foo/bar/bas"))
  checkEquals(getDirectorySequence("foo/bar/bas/"), c("foo", "foo/bar", "foo/bar/bas"))
}