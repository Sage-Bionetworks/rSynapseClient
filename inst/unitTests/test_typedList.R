# tests for typed lists
# 
# Author: brucehoff
###############################################################################


unitTestEmptyTypedList<-function() {
  openIds<-new("CharacterList")
  p<-synapseClient:::UserProfile(openIds=openIds)
  synapseClient:::createListFromS4Object(p)
}

unitTestTypedList<-function() {
  t<-synapseClient:::CharacterList()
  t$foo<-"bar"
  checkEquals(t$foo, "bar")
  t[["foo"]]<-"bas"
  checkEquals(t$foo, "bas")
  
  t<-synapseClient:::CharacterList()
  t[[1]]<-"a"
  t[[2]]<-"b"
  checkEquals(2, length(t))
  checkEquals(t[[1]], "a")
  checkEquals(t[[2]], "b")
  checkEquals(list("a", "b"), synapseClient:::getList(t))
  
  # test 'add'
  t<-synapseClient:::CharacterList()
  t[[1]]<-"foo"
  t<-synapseClient:::add(t, "bar")
  checkEquals(list("foo", "bar"), synapseClient:::getList(t))
  t<-synapseClient:::set(t, list("a", "b"))
  checkEquals(list("a", "b"), synapseClient:::getList(t))
  
  t<-synapseClient:::CharacterList("a", "b")
  checkEquals(list("a", "b"), synapseClient:::getList(t))
}

unitTestCreateTypedList<-function() {
  created<-synapseClient:::createTypedList(c("1", "2", "3"))
  checkTrue(identical(CharacterList("1", "2", "3"), created))
}

