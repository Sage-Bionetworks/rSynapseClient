# TODO: Add comment
# 
# Author: furia
###############################################################################


synapseEntityTypeMap <-
  function()
{
  list(
    org.sagebionetworks.repo.model.Study = "Study",
    org.sagebionetworks.repo.model.Data = "Data",
    org.sagebionetworks.repo.model.Project = "Project",
    org.sagebionetworks.repo.model.Analysis = "Analysis",
    org.sagebionetworks.repo.model.Step = "Step",
    org.sagebionetworks.repo.model.Code = "Code",
    org.sagebionetworks.repo.model.Link = "Link",
    org.sagebionetworks.repo.model.Media = "Media",
    org.sagebionetworks.repo.model.ExpressionData = "ExpressionData"
  )
}

getSynapseTypeFromClass <-
  function(class)
{
  map <- synapseEntityTypeMap()
  indx <- which(map == class)
  if(length(indx) == 0L)
    return(NULL)
  names(map[indx])
}

getClassFromSynapseEntityType <-
  function(type)
{
  map <- synapseEntityTypeMap()
  map[[type]]
}