\name{File}
\alias{File}
\title{
Synapse File Constructor
}
\description{
Constructor for File
}
\usage{
File(path, parentId, synapseStore=TRUE, ...)
}
\arguments{
 \item{path}{
Path to the file in local file system, or URL to file on the internet
}
\item{parentId}{
The Synapse ID of the parent Project or Folder in which to store the File (e.g. "syn101")
}
\item{synapseStore}{
If 'synapseStore' is TRUE then file is uploaded to S3, else only the file location is saved, along with other metadata
}
\item{...}{
Additional named parameters are interpreted as properties or annotations on the File
}
}

\value{
The created File is returned.  NOTE: This is an in-memory representation.  To persist to Synapse use \code{\link{synStore}()}.
}
\author{
Bruce Hoff
}

\seealso{
\code{\link{Folder}}
\code{\link{Project}}
\code{\link{synGet}}
\code{\link{synStore}}
}
