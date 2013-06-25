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
\arguments{
\item{evaluationId}{
The evaluation queue containing the submissions of interest.
}
\item{status}{
An optional filter on the submissions.  One of "OPEN", "CLOSED", "SCORED", or "INVALID".
}
\item{limit}{
The optional limit 'paging' parameter, default is 10.
}
\item{offset}{
The optional offset 'paging' parameter, default is 0.
}
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
