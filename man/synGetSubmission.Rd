\name{synGetSubmission}
\alias{synGetSubmission}
\title{
synGetSubmission
}
\description{
Get a Submission object, given its ID.
}
\usage{
synGetSubmission("123456")
}
\examples{
# one of "OPEN", "CLOSED", "SCORED", "INVALID"
status<-"SCORED"
# get the first 100 submissions in the given evaluation queue having the given status
submissions<-synGetSubmissions(evaluationId, status, limit=100, offset=0) 
# get the ID of an arbitrary Submission
submissionId<-submissions@results[[1]]["id"]
submission<-synGetSubmission(submissionId)
}
\seealso{
\code{\link{synGetSubmissions}}
\code{\link{synGetOwnSubmissions}}
\code{\link{synGetSubmissionStatus}}
}
