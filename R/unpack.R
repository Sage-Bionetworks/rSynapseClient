## Unpack a file into the synapse cache after download
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

.unpack <- 
  function(filename, destdir)
{
  if(missing(destdir))
    destdir <- dirname(filename)
  filename <- path.expand(filename)
  splits <- strsplit(basename(filename), "\\.")
  extension <- splits[[1]][length(splits[[1]])]
  extension <- tolower(extension)
  
  switch(extension,
    zip = {unlink(destdir); unzip(filename, exdir = destdir)},
    gz = {unlink(destdir); untar(filename, exdir = destdir)},
    tar = {unlink(destdir); untar(filename, exdir = destdir)},
    { ## default
#      splits <- strsplit(filename, .Platform$file.sep)
#      destdir <- paste(splits[[1]][-length(splits[[1]])], collapse=.Platform$file.sep)
#      attr(filename, "rootDir") <- destdir
      if(file.info(filename)$isdir)
        stop("directories are not supported by unpack")
      if(file.exists(destdir))
        unlink(destdir, recursive=T)
      dir.create(destdir)
      file.copy(filename, file.path(destdir, basename(filename)))
      filename <- file.path(destdir,basename(filename))
      attr(filename, "rootDir") <- destdir
      return(filename)
    }
  ) 
  files <- list.files(destdir, full.names = TRUE, recursive=TRUE, all.files = TRUE)
  files <- setdiff(files, c(".",".."))
  attr(files, "rootDir") <- destdir
  files
}
