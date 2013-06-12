\name{synGetSubmissions}
\alias{synGetSubmissions}
\title{
synGetSubmissions
}
\description{
Get the Submissions for a given Evaluation.
}
\usage{
synGetSubmissions(evaluationId, status, limit, offset)
}
\value{
an object of type PaginatedResults
}
\examples{
# one of "OPEN", "CLOSED", "SCORED", "INVALID"
status<-"SCORED"
# get the first 100 submissions in the given evaluation queue having the given status
submissions<-synGetSubmissions(evaluationId, status, limit=100, offset=0) 
# get an arbitrary Submission from the list
submission<-submissions@results[[1]]
}
\seealso{
\code{\link{synGetSubmission}}
\code{\link{synGetOwnSubmissions}}
\code{\link{synGetSubmissionStatus}}
\code{\link{PaginatedResults}}
}
