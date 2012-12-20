## Open the webpage of an entity
## 
## Author: Matthew D. Furia <matt.furia@sagebase.org>
##############################################################################


setMethod(
  f = "onWeb",
  signature = "numeric",
  definition = function(entity){
    onWeb(as.character(entity))
  }
)

setMethod(
  f = "onWeb",
  signature = "character",
  definition = function(entity){
    utils::browseURL(.buildSynapseUrl(entity))
  }
)

.doOnWeb <- function(entity) {
  if(entity@synapseWebUrl == "")
    stop("This Entity has not been saved to Synapse yet. Use createEntity() to save it and then try again.")
  tryCatch(
    utils::browseURL(entity@synapseWebUrl),
    error = function(e){
      warning("Unable to launch the web browser. Paste this url into your web browser: %s", entity@synapseWebUrl)
      warning(e)
    }
  )
  invisible(entity@synapseWebUrl)
}
