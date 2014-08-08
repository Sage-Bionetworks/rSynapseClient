# Unit test for submitting to evaluation
###############################################################################

.setUp <-
  function() {
    
}

.tearDown <- function() {
    synapseClient:::.unmockAll()
}

unitTestSubmit_no_submissionReceiptMessage <- function() {
    # Intercept all the calls to other methods
    accessRequirementUnfulfilled_called <- FALSE
    synGetEvaluation_called <- FALSE
    createSubmissionFromProperties_called <- FALSE
    synCreateSubmission_called <- FALSE
    synapseClient:::.mock("synapseGet", function(uri, ...) {
        if (!grep("accessRequirementUnfulfilled", uri)) {
            stop("Mocked an unexpected call")
        }
        accessRequirementUnfulfilled_called <<- TRUE
        return(list(totalNumberOfResults=0))
    })
    synapseClient:::.mock("synGetEvaluation", function(id, ...) {
        synGetEvaluation_called <<- TRUE
        return(Evaluation(id=id))
    })
    synapseClient:::.mock("createSubmissionFromProperties", function(...) {createSubmissionFromProperties_called <<- TRUE})
    synapseClient:::.mock("synCreateSubmission", function(...) {synCreateSubmission_called <<- TRUE})
    
    # As per SYNR-626, pass in an ID, not an Evaluation object
    # The method should fetch the evaluation to show the proper confirmation message
    evaluation <- "evalId"
    entity <- File(id="fileId", parentId="parentId", etag="etag", name="name")
    submit(evaluation, entity)
    checkTrue(accessRequirementUnfulfilled_called)
    checkTrue(synGetEvaluation_called)
    checkTrue(createSubmissionFromProperties_called)
    checkTrue(synCreateSubmission_called)
}

