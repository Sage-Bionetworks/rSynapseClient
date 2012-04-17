## Test getEntity method for Datasets
##
## Author: Nicole Deflaux <nicole.deflaux@sagebase.org>
###############################################################################
#
#integrationTestGetDatasets <- function() {
#  datasets <- getStudies()
#  ## The fields returned by this service API may change over time, but
#  ## there are a few we should always expect to receive
#  checkTrue("id" %in% names(datasets))
#  checkTrue("name" %in% names(datasets))
#  checkTrue("versionLabel" %in% names(datasets))
#  checkTrue("species" %in% names(datasets))
#}
#
#integrationTestPaging <- function() {
#  firstPageDatasets <- getStudies(queryParams=list(limit=20, offset=1))
#  secondPageDatasets <- getStudies(queryParams=list(limit=20, offset=21))
#  ## We should get back 20 datasets
#  checkEquals(nrow(firstPageDatasets), 20)
#  checkTrue(nrow(secondPageDatasets) >= 3)
#  ## And they do not overlap
#  checkTrue(length(union(firstPageDatasets$id,
#        secondPageDatasets$id)) >= 23)
#}
#
#integrationTestQueryForDataset <- function() {
#  studies <- getStudies(queryParams=list(where='entity.name == "MSKCC Prostate Cancer"'))
#  ## We should get back 1 dataset
#  checkEquals(nrow(studies), 1)
#  ## And its name should match the one we searched for
#  checkEquals(studies$name, "MSKCC Prostate Cancer")
#}

## need to re-write this test
stop("need to re-write this test")