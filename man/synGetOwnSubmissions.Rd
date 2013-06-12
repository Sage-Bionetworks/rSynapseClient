\name{synGetOwnSubmissions}
\alias{synGetOwnSubmissions}
\title{
synGetOwnSubmissions
}
\description{
Get the Submissions for a given Evaluation submitted by me.
}
\usage{
synGetOwnSubmissions(evaluationId, limit, offset)
}
\value{
an object of type PaginatedResults
}
\examples{
# get the first 10 submissions in the given evaluation queue that I submitted.
submissions<-synGetSubmissions(evaluationId, limit=10, offset=0) 
# get an arbitrary Submission from the list
submission<-submissions@results[[1]]
}
\seealso{
\code{\link{synGetSubmission}}
\code{\link{synGetSubmissions}}
\code{\link{synGetSubmissionStatus}}
\code{\link{PaginatedResults}}
}
