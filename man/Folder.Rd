\name{Folder}
\alias{Folder}
\title{
Synapse Folder Constructor
}
\description{
Constructor for Folder
}
\usage{
Folder(name, parentId, ...)
}
\arguments{
 \item{name}{
Name of the Folder to be created in Synapse
}
\item{parentId}{
The Synapse ID of the parent Project or Folder in which to create the Folder (e.g. "syn101")
}
\item{...}{
Additional named parameters are interpreted as properties or annotations on the Folder
}
}

\value{
The created Folder is returned.  NOTE: This is an in-memory representation.  To persist to Synapse use \code{\link{synStore}()}.
}
\author{
Bruce Hoff
}

\seealso{
\code{\link{File}}
\code{\link{Project}}
\code{\link{synGet}}
\code{\link{synStore}}
}
