\name{SynapseEntity}
\alias{Study}
\alias{Code}
\alias{Data}
\alias{SynapseEntity}
\alias{SynapseLocationOwner}
\alias{Project}
\title{
Entity Constructors
}
\description{
Constructors for Synapse Entities
}
\usage{
Study(entity)
Data(entity)
Project(entity)
Preview(entity)
Link(entity)
Folder(entity)
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
