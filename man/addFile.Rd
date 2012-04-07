\name{addFile}
\alias{addFile}
\alias{deleteFile}
\alias{moveFile}
\title{
  Manipulate Entity Files
}
\description{
  Functions for manipulating the files belonging to an entity.
}
\usage{
  addFile(entity, file, path)
  deleteFile(entity, file)
  moveFile(entity, src, dest)
}
\arguments{
  \item{entity}{
    The entity whose files will be modified.
  }
  \item{file}{
    A path to the file on the local file system, when adding, or the relative path 
    to the file when deleting (see Details).
  }
  \item{path}{
    relative path in the entity's cache directory where the file should be placed.
  }
  \item{src}{
    full path to the source file relative to the entity's cache directory.
  }
  \item{dest}{
    destination path relative to the root of the entity's cache directory.
  }
}
\details{
  Synapse has the ability to store only a single file for entities that are "locationable", which
  are represented as the LocationOwner class in the Synapse R client. To get around the single file limitation,
  the Synapse R client provides methods for maintaining a "cache" of files that can be zipped into a single 
  archive and uploaded to Synapse by calling the \code{\link{storeEntity}} method.
  
  When adding a file to an entity, the \code{file} argument is the path to the file being added and
  the \code{path} argument is the relative path in the entity's cached directory where the file should be
  placed. When moving or deleting a file, the \code{file} and \code{src} arguments should be the path to 
  the file that should be moved/deleted relative to the root of the cache directory.
  
  The behavior of the \code{dest} argument is slightly more complex. If this arguments contains
  a trailing slash (i.e. "/"), they will be interpreted as a path to the directory in which the file
  specified by the \code{src} argument should be placed. Absent the trailing slash, \code{dest}
  will be interpreted as a path followed by a filename to which the source file \code{src} will be renamed.
}
\value{
  The modified entity.
}
\author{
  Matthew D. Furia <matt.furia@sagebase.org>
}
\note{
  If the return value is not captured, the entity will not be modified.
}
\seealso{
  \code{\link{addObject}}
  \code{\link{moveObject}}
  \code{\link{deleteObject}}
  \code{\link{storeEntity}}
  \code{\link{downloadEntity}}
}
\examples{
  ## create a Media entity (LocationOwner sub-class).
  entity <- Media()
  
  ## create a plot that will be added to the Media entity
  plotFile <- tempfile(fileext=".jpg")
  jpeg(filename = plotFile)
  plot(1:10, 1:10)
  dev.off()

  ## add the file to the root of the entity's cache
  ## BE SURE TO CAPTURE THE RETURN VALUE
  entity <- addFile(entity, plotFile)
  
  ## move the file to a subdirectory named foo
  entity <- moveFile(entity, entity$files[1], "/foo/")
  
  ## change the files name
  entity <- moveFile(entity, entity$files[1], "/foo/plot.jpg")
  
  ## delete the file
  entity <- deleteFile(entity, entity$files[1])

}

