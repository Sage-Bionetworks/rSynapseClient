# Function for submitting an entity to an Evaluation
# 
# Author: brucehoff
###############################################################################

submit<-function(evaluation, entity, submissionName, teamName, silent=F) {
  if (missing(entity)) stop("entity is required.")
  if (is(entity, "Entity")) {
    entityId<-propertyValue(entity, "id")
    if (is.null(entityId)) stop("The entity provided does not have an ID.")
    entityVersion<-propertyValue(entity, "versionNumber")
    if (is.null(entityVersion)) entityVersion<-"1" # takes care of non-versioned entities
    etag<-propertyValue(entity, "etag")
    if (is.null(etag)) stop("Entity is missing etag.")
    # if it's an old version we have to retrieve the latest to get the etag
    if (etag=="00000000-0000-0000-0000-000000000000") {
      latestEntity<-synGet(entityId, downloadFile=F)
      etag<-propertyValue(latestEntity, "etag")
    }
  } else {
    stop("You must provide an entity.")
  }
  if (missing(evaluation)) stop ("evaluation is required.")
  if (is(evaluation, "Evaluation")) {
    evaluationId<-propertyValue(evaluation, "id")
    if (is.null(evaluationId)) stop("The Evaluation provided does not have an ID.")
  } else if (is(evaluation, "character")) {
    evaluationId<-evaluation
    evaluation <- synGetEvaluation(evaluationId)
  } else {
    stop("You must provide an evaluation or and evaluation ID.")
  }
  if (missing(submissionName)) submissionName<-propertyValue(entity, "name")
  
  if (missing(teamName)) {
    submission<-createSubmissionFromProperties(list(evaluationId=evaluationId, 
        entityId=entityId, 
        versionNumber=entityVersion, 
        name=submissionName))
	createdSubmission<-synCreateSubmission(submission, entityEtag=etag)
  } else {
	# find the team ID for the given team name
	teamId<-findTeamIdForName(teamName)
	if (is.null(teamId)) stop(sprintf("There is no team named %s.", teamName))
	# get the eligible contributors
	tse<-getTeamSubmissionEligibility(evaluationId, teamId)
	if (!tse@teamEligibility@isEligible) {
		stop(sprintf("Team %s is ineligible to submit to the specified Evaluation at this time.", teamName))
	}
	contributors<-list()
	for (memberEligibility in tse@membersEligibility@content) {
		if (memberEligibility@isEligible) {
			contributors<-append(contributors, 
					list(principalId=as.character(memberEligibility@principalId)))
		}
	}
	
	# include the team id, contributors, and eligibility hash in the submission
    submission<-createSubmissionFromProperties(list(evaluationId=evaluationId, 
        entityId=entityId, 
        versionNumber=entityVersion, 
        name=submissionName,
        submitterAlias=teamName,
		teamId=teamId,
		contributors=contributors))
	createdSubmission<-synCreateSubmission(submission, entityEtag=etag, submissionEligibilityHash=tse@eligibilityStateHash)
  } 
  
  if (!silent) message(evaluation$submissionReceiptMessage)
  list(submission=createdSubmission, submissionReceiptMessage=evaluation$submissionReceiptMessage)
}

findTeamIdForName<-function(teamName) {
	limit<-10
	offset<-0
	teamId<-NULL
	repeat {
		page<-synRestGET(sprintf("/teams?fragment=%s&offset=%s&limit=%s", URLencode(teamName), offset, limit))
		teams<-page$results
		for (teamList in teams) {
			team<-createS4ObjectFromList(teamList, "Team")
			if (team@name==teamName) {
				teamId<-team@id
				break
			}
		}
		if (!is.null(teamId)) break
		offset<-offset+limit
		if (offset>=page$totalNumberOfResults) break
	}
	teamId
}

getTeamSubmissionEligibility<-function(evaluationId, teamId) {
	result<-synRestGET(sprintf("/evaluation/%s/team/%s/submissionEligibility", evaluationId, teamId))
	createS4ObjectFromList(result, "TeamSubmissionEligibility")
}

