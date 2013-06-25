\name{Project}
\alias{Project}
\title{
Synapse Project Constructor
}
\description{
Constructor for Project
}
\usage{
Project(name, ...)
}
\arguments{
 \item{name}{
Name of the Project to be created in Synapse
}
\item{...}{
Additional named parameters are interpreted as properties or annotations on the Project
}
}

\value{
The created Project is returned.  NOTE: This is an in-memory representation.  To persist to Synapse use \code{\link{synStore}()}.
}
\author{
Bruce Hoff
}

\seealso{
\code{\link{File}}
\code{\link{Folder}}
\code{\link{synGet}}
\code{\link{synStore}}
}
