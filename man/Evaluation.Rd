\name{Evaluation}
\alias{Evaluation}
\docType{data}
\title{
Evaluation Constructor
}
\description{
Constructor for Evaluation
}
\usage{Evaluation(name, description, contentSource, status)}
\arguments{
 \item{name}{
 name for this Evaluation.
 }
 \item{description}{
 description of this Evaluation
 }
 \item{contentSource}{
 Synapse ID of the entity which owns the Evaluation, usually a Project
 }
 \item{status}{
 One of "PLANNED", "OPEN", "CLOSED", or "COMPLETED".
 }
}
\seealso{
\code{\link{synGetEvaluation}}
\code{\link{synStore}}
}

