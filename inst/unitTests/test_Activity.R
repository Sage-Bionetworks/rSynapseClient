# These are testActivity tests
###############################################################################

.setUp <-
  function()
  {
    synapseClient:::.setCache("oldWarn", options("warn")[[1]])
    options(warn=2L)
  }

.tearDown <-
  function()
  {
    options(warn = synapseClient:::.getCache("oldWarn"))
    if(!is.null(name <- synapseClient:::.getCache("detachMe"))){
      detach(name)
      synapseClient:::.deleteCache('detachMe')
    }
  }


unitTestShowActivity <- function(){
  
  ## CREATE AN EMPTY ACTIVITY AND MAKE SURE THAT IT RENDERS
  act <- Activity()
  act
  
  ## CREATE AN ACTIVITY WITH A NAME BUT NOTHING USED
  act <- Activity(name="Sweet")
  act
  
  ## CREATE AN ACTIVITY THAT USES SOMETHING - SYNAPSE ID ONLY
  act <- Activity(name="Sweet", used=list(list(entity="syn1234", wasExecuted=F)))
  act
  
  ## CREATE AN ACTIVITY THAT USES SOMETHING executed and something non-executed - SYNAPSE ID ONLY
  act <- Activity(name="Sweet", used="syn1234", executed="syn1234")
  act
  
  ## CREATE AN ACTIVITY THAT USES SOMETHING - SYNAPSE ID AND A url
  act <- Activity(name="Sweet", used=list(list(entity="syn1234", wasExecuted=F),
                                          list(url="https://www.synapse.org", wasExecuted=T)))
  act

  ## CREATE AN ACTIVITY THAT USES SOMETHING  executed and something non-executed - SYNAPSE ID AND A url
  act <- Activity(name="Sweet", used=list(entity="syn1234"),
      executed=list(url="https://www.synapse.org"))
  act
  
}
