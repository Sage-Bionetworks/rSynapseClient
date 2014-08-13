# methods relating to the TableColumn class
# 
# Author: brucehoff
###############################################################################


setMethod(
  f = "synStore",
  signature = "TableColumn",
  definition = function(entity) {
    if (length(entity$id)==0) {
      # create
      createS4Object(entity, "/column")
    } else {
      stop("Cannot update an existing TableColumn.")  
    }
  }
)

synGetColumn<-function(id) {
  listResult<-synRestGET(sprintf("/column/%s", id))
  objectResult<-createS4ObjectFromList(listResult, "TableColumn")
}


