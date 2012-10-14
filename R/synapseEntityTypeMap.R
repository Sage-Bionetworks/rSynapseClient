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
    org.sagebionetworks.repo.model.Code = "Code",
    org.sagebionetworks.repo.model.Link = "Link",
    org.sagebionetworks.repo.model.ExpressionData = "ExpressionData",
    org.sagebionetworks.repo.model.GenotypeData = "GenotypeData",
    org.sagebionetworks.repo.model.PhenotypeData = "PhenotypeData",
    org.sagebionetworks.repo.model.RObject = "RObject",
    org.sagebionetworks.repo.model.Folder = "Folder",
    org.sagebionetworks.repo.model.Preview = "Preview"
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