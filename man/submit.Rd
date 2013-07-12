\name{submit}
\alias{submit}
\docType{methods}
\title{
submit
}
\description{
Submit an entity to an Evaluation
}
\usage{submit(evaluation, entity, submissionName, teamName)}
\arguments{
	\item{evaluation}{
	The evaluation queue to which the entity is submitted.  You must first 'join' the evaluation to be allowed to submit.
	This parameter may be the ID of the evaluation or the evaluation object itself.
	}
	\item{entity}{
	The entity to be submitted, e.g. a File.
	}
	\item{submissionName}{
	Optional:  A name for the submission.  If omitted, the entity's name is used.
	}
	\item{teamName}{
	Optional:  The name of the team on whose behalf the entity is submitted.
	}
}
\details{
The content of the given entity is added to the given evaluation queue.  Since the content is extracted, changing the entity
after submission does not change what is in the queue.
}
\seealso{
\code{\link{synGet}}
\code{\link{synGetEvaluation}}
\code{\link{synGetSubmission}}
\code{\link{synGetSubmissions}}
\code{\link{synGetOwnSubmissions}}
\code{\link{synGetSubmissionStatus}}
}
