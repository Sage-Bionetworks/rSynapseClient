\name{deleteFile-methods}
\docType{methods}
\alias{deleteFile-methods}
\alias{deleteFile,CachedLocation,character-method}
\alias{deleteFile,Layer,character-method}
\alias{deleteFile,Layer,missing-method}
\title{ ~~ Methods for Function \code{deleteFile}  ~~}
\description{
 ~~ Methods for function \code{deleteFile}  ~~
}
\section{Methods}{
\describe{

\item{\code{signature(entity = "CachedLocation", file = "character")}}{
Delete the specified file from the location
}

\item{\code{signature(entity = "Layer", file = "character")}}{
Delete the specified file from the layer
}

\item{\code{signature(entity = "Layer", file = "missing")}}{
Open a file select dialog box and delete the selected file from the layer.
}
}}
\keyword{methods}
