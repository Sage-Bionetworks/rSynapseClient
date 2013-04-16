# Test for jiraURLHelper
# 
# Author: brucehoff
###############################################################################


# TODO test URL encoding
unitTestJiraURLHelper<-function() {
  checkEquals("https://sagebionetworks.jira.com/secure/CreateIssueDetails!init.jspa?pid=10831&issuetype=11&summary=Request for ACT to add data restriction&reporter=synapse-jira-service&priority=3&description=By clicking 'Create' below, I request that the Synapse ACT contact me to assign the appropriate access restrictions for this dataset.&customfield_10741=Me Too&customfield_10840=me@sagebase.org&customfield_10742=syn1751313", 
  synapseClient:::createAccessRestrictionIssue("me@sagebase.org", "Me Too", "syn1751313"))
}
