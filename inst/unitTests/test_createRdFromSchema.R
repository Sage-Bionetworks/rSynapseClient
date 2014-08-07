# Unit tests for Rd file generator
# 
# Author: brucehoff
###############################################################################


# We don't check the content, just that it's created without any error
unitTestCreateRdFromSchema<-function() {
  createRdFromSchema("Evaluation", "org.sagebionetworks.evaluation.model.Evaluation")
  createRdFromSchema("UserProfile", "org.sagebionetworks.repo.model.UserProfile")
  createRdFromSchema("Row", "org.sagebionetworks.repo.model.table.Row")
}