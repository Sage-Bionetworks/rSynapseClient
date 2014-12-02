\name{synGetParticipants}
\alias{synGetParticipants}
\title{
synGetParticipants
}
\description{
Function to retrieve the Participants of an Evaluation.
}
\usage{
synGetParticipants(evaluationId,limit,offset)
}
\arguments{
\item{evaluationId}{
The ID of the evaluation queue of interest.
}
\item{limit}{
Optional pagination parameter: The number of results to return in the batch.  Default (imposed by Synapse) is 10.
}
\item{offset}{
Optional pagination parameter:  The first result to return in the batch.  Default is 0 (i.e. the first result).
}
}
\value{
A PaginatedResults object containing a list of Participant objects.
}
\examples{
\dontrun{
participants<-synGetParticipants("123456")
participants@totalNumberOfResults
participants@results
}
}
\seealso{
\code{\link{synGetEvaluation}}
}
