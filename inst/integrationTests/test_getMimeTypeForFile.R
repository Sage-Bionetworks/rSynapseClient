integrationTestGetEntityByIdExistingFileCache <-
  function()
{
  checkEquals("image/jpeg", getMimeTypeForFile("foo.jpg"))
  checkEquals("image/jpeg", getMimeTypeForFile("foo.jpeg"))
  checkEquals("text/html", getMimeTypeForFile("this.is.a.file.html"))
  checkEquals("application/octet-stream", getMimeTypeForFile("FileWithoutExtension"))
}