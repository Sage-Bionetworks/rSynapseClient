\name{addObject-methods}
\docType{methods}
\alias{addObject-methods}
\alias{addObject,SynapseLocationOwner,ANY-method}
\alias{addObject,SynapseLocationOwner,data.frame-method}
\alias{addObject,SynapseLocationOwner,list-method}
\title{ Methods for Function \code{addObject}  }
\description{
 ~~ Methods for function \code{addObject}  ~~
}
\section{Methods}{
\describe{

\item{\code{signature(entity = "SynapseLocationOwner", object = "ANY", name="missing", unlist="missing")}}{
store the object with it's default name. don't unlist lists.
}

\item{\code{signature(entity = "SynapseLocationOwner", object = "data.frame", name="missing", "missing")}}{
data frames should never be "unlisted".
}
\item{\code{signature(entity = "SynapseLocationOwner", object = "data.frame", name="character", "missing")}}{
data frames should never be "unlisted". specify the name of the data frame object
}

\item{\code{signature(entity = "SynapseLocationOwner", object = "list")}}{
%%  ~~describe this method here~~
}

\item{\code{signature(entity = "SynapseLocationOwner", object = "list")}}{
%%  ~~describe this method here~~
}

}}
\keyword{methods}
