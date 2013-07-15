\name{SynapseEntity}
\alias{Study}
\alias{Code}
\alias{Data}
\alias{Preview}
\alias{ExpressionData}
\alias{GenotypeData}
\alias{PhenotypeData}
\alias{RObject}
\title{
DEPRECATED Entity Constructors
}
\description{
Constructors for Synapse Entities. These Constructors are now deprecated. Please see \code{\link{Project}}, \code{\link{Folder}}, \code{\link{File}} for currently supported Synapse Objects.
}
\usage{
Study(entity)
Data(entity)
Preview(entity)
Link(entity)
ExpressionData(entity)
GenotypeData(entity)
PhenotypeData(entity)
RObject(entity)
}
\arguments{
  \item{entity}{
(optional) a named list whose elements will be set as the properties of the entity. The list of
valid properties is controlled by Synapse and currently no client-side verification is perfomed.
}
}

\value{
The entity
}
\author{
Matthew D. Furia
}
