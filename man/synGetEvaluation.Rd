\name{synGetEvaluation}
\alias{synGetEvaluation}
\title{
synGetEvaluation
}
\description{
Function to retrieve an Evaluation object.
}
\usage{
synGetEvaluation(evaluationId)
}
\details{
Retrieves an Evaluation object, given its ID.
}
\examples{
# get an arbitrary Evaluation ID
anEvaluationId<-synRestGET("/evaluation")$results[[1]]["id"]
# retrieve the evaluation
evaluation<-synGetEvaluation(anEvaluationId)
# check its status
propertyValue(evaluation, "status")
# if I have administrative privileges I can update it
propertyValue(evaluation, "description")<-"some new description"
evaluation<-synStore(evaluation)
}
\seealso{
\code{\link{synStore}}
}
