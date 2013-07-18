\name{Evaluation}
\alias{Evaluation}
\docType{methods}
\title{
Evaluation Constructor
}
\description{
Constructor for Evaluation
}
\usage{Evaluation(name, description, contentSource, status, submissionInstructionsMessage, submissionReceiptMessage)}
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
 \item{submissionInstructionsMessage}{
 Instructions to be show to participants on how to craft Synapse File objects for submission to the Evaluation.
 }
 \item{submissionReceiptMessage}{
 Message to display by client upon successful submission to the Evaluation.
 }
}
\seealso{
\code{\link{synGetEvaluation}}
\code{\link{synStore}}
\code{\link{submit}}
}

