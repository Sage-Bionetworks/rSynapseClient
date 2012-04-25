\name{addFile-methods}
\docType{methods}
\alias{addFile-methods}
\alias{addFile,CachedLocation,character,character-method}
\alias{addFile,LocationOwner,character,missing-method}
\alias{addFile,LocationOwner,character,character-method}
\title{Methods for Function \code{addFile}}
\description{
  Methods for function \code{addFile}
}
\section{Methods}{
  \describe{
    \item{\code{signature(entity = "CachedLocation", file = "character", path = "character")}}{
      Adds the file to the CachedLocation. This method is not intended to be called by the user. Instead
      user's should call the addFile methods for the LocationOwner directly which will call this method
      on their behalf.
    }
    \item{\code{signature(entity = "LocationOwner", file = "character", path = "character")}}{
      Add the file to the entity's cache directory under the subdirectory specified by the path argument.
    }
    \item{\code{signature(entity = "LocationOwner", file = "character", path = "missing")}}{
      Add the file to the root of the entity's cached directory.
    }
  }
}
\keyword{methods}
