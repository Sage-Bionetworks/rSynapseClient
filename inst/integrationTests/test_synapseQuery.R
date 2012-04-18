### Integration tests for synapseQuery
###
### Author: Nicole Deflaux <nicole.deflaux@sagebase.org>
##################################################################################
#
#integrationTestQuery <-
#  function()
#{
#  studies <- synapseQuery(query='select * from entity where type = study limit 10')
#
#  ## We should get back 10 studies
#  checkEquals(nrow(studies), 10)
#
#  ## The fields returned by this service API may change over time, but
#  ## there are a few we should always expect to receive
#  checkTrue("entity.id" %in% names(studies))
#  checkTrue("entity.name" %in% names(studies))
#  checkTrue("entity.versionLabel" %in% names(studies))
#  checkTrue("entity.species" %in% names(studies))
#}
#
#integrationTestPaging <-
#  function()
#{
#  firstPageDatasets <- synapseQuery(query='select * from dataset limit 20 offset 1')
#  secondPageDatasets <- synapseQuery(query='select * from dataset limit 20 offset 21')
#
#  ## We should get back 20 studies
#  checkEquals(nrow(firstPageDatasets), 20)
#  checkTrue(nrow(secondPageDatasets) >= 3)
#
#  ## And they do not overlap
#  checkTrue(length(union(firstPageDatasets$entity.id,
#        secondPageDatasets$entity.id)) >= 23)
#}
#
#integrationTestQueryForDataset <-
#  function()
#{
#  studies <- synapseQuery(query='select * from dataset where entity.name == "MSKCC Prostate Cancer"')
#
#  ## We should get back 1 dataset
#  checkEquals(nrow(studies), 1)
#
#  ## And its name should match the one we searched for
#  checkEquals(studies$entity.name, "MSKCC Prostate Cancer")
#}
#
#integrationTestLotsOQueries <-
#  function()
#{
#  studiesOrderBy <- synapseQuery(query='select * from dataset order by Number_of_Samples DESC limit 3')
#  checkTrue(3 == nrow(studiesOrderBy))
#
#  studiesMultiWhere <- synapseQuery(query='select * from dataset where entity.species == "Homo sapiens" and entity.Number_of_Samples > 100 limit 3 offset 1')
#  checkTrue(3 == nrow(studiesMultiWhere))
#
#  studiesSingleWhere <- synapseQuery(query='select * from dataset where name == "MSKCC Prostate Cancer"')
#  checkTrue(1 == nrow(studiesSingleWhere))
#
#  mskccDatasetId <- studiesSingleWhere$entity.id[1]
#
#  layers <- synapseQuery(query=paste('select * from layer where layer.parentId =="', mskccDatasetId, '"', sep=' '))
#  checkTrue(6 <= nrow(layers))
#
#  layersOrderBy <- synapseQuery(query=paste('select * from layer where layer.parentId == "', mskccDatasetId, '" ORDER BY type', sep=' '))
#  checkTrue(6 <= nrow(layersOrderBy))
#}

integrationTestWarnMe <-
  function(){
  warning("need to fix tests for test_synapseQuery.R")
}