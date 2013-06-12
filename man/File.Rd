\name{File}
\alias{File}
\title{
File Constructor
}
\description{
Constructor for File
}
\usage{
File(path="/path/to/file", parentId="syn101", synapseStore=T, ...)
}
\arguments{
 \item{path}{
 path to the file in local file system, or URL to file on the internet.  If omitted, an 'empty' File object is created, which can be filled using the 'addObject' function.
}
\item{parentId}{
The Synapse ID of the parent project or folder.
}
\item{synapseStore}{
 If 'synapseStore' is TRUE then file is uploaded to S3, else only the file location is saved.
}
\item{...}{
Additional parameters are interpreted as properties or annotations on the File.
}
}

\value{
The created File is returned.  Note: This is an in-memory representation.  To persist to Synapse use 'synStore'.
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
