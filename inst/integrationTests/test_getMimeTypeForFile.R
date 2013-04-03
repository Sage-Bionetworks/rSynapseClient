integrationTestGetEntityByIdExistingFileCache <-
  function()
{
  checkEquals("image/jpeg", synapseClient:::getMimeTypeForFile("foo.jpg"))
  checkEquals("image/jpeg", synapseClient:::getMimeTypeForFile("foo.jpeg"))
  checkEquals("text/html", synapseClient:::getMimeTypeForFile("this.is.a.file.html"))
  checkEquals("application/octet-stream", synapseClient:::getMimeTypeForFile("FileWithoutExtension"))
  checkEquals("application/octet-stream", synapseClient:::getMimeTypeForFile("foo.unrecognizedExtenion"))
}