## objects that users will interact with. can the generation of these class
## definitions be automated?
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
###############################################################################

##
## Project entity
##
setClass(
  "Project",
  contains = "SynapseEntity",
  prototype = prototype(
    synapseEntityKind = "org.sagebionetworks.repo.model.Project"
  )
)

##
## Link entity points to other Synapse entities
##
setClass(
  "Link",
  contains = "SynapseEntity",
  prototype = prototype(
    synapseEntityKind = "org.sagebionetworks.repo.model.Link"
  )
)

##
## Entity for storing code
##
setClass(
  "Code",
  contains="SynapseLocationOwner",
  prototype = prototype(
    synapseEntityKind = "org.sagebionetworks.repo.model.Code"
  )
)

##
## Generic Data entity
##
setClass(
  "Data",
  contains = "SynapseLocationOwnerWithObjects",
  prototype = prototype(
    synapseEntityKind = "org.sagebionetworks.repo.model.Data"
  
  )
)

##
## Generic Data entity
##
setClass(
  "GenotypeData",
  contains = "SynapseLocationOwnerWithObjects",
  prototype = prototype(
    synapseEntityKind = "org.sagebionetworks.repo.model.GenotypeData"
  
  )
)

##
## Study entity
##
setClass(
  "Study",
  contains = "SynapseLocationOwnerWithObjects",
  prototype = prototype(
    synapseEntityKind = "org.sagebionetworks.repo.model.Study"
  )
)

##
## Entity for storing Expression Data
##
setClass(
  Class = "ExpressionData",
  contains = "SynapseLocationOwnerWithObjects",
  prototype = prototype(
    synapseEntityKind = "org.sagebionetworks.repo.model.ExpressionData"
  )
)

setClass(
  Class = "RObject",
  contains = "SynapseLocationOwnerWithObjects",
  prototype = prototype(
    synapseEntityKind = "org.sagebionetworks.repo.model.RObject"
  )
)


setClass(
  Class = "PhenotypeData",
  contains = "SynapseLocationOwnerWithObjects",
  prototype = prototype(
    synapseEntityKind = "org.sagebionetworks.repo.model.PhenotypeData"
  )
)

setClass(
  Class = "Folder",
  contains = "SynapseEntity",
  prototype = prototype(
    synapseEntityKind = "org.sagebionetworks.repo.model.Folder"
  )
)

setClass(
  Class = "Preview",
  contains = "SynapseEntity",
  prototype = prototype(
    synapseEntityKind = "org.sagebionetworks.repo.model.Preview"
  )
)

setClass(
  Class = "GithubCode",
  contains = "Code"
)





