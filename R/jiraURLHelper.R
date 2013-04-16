# Generates the JIRA URL for requesting an access restriction
# 
# duplicates the Java code found in the Synapse Web Client
# org.sagebionetworks.web.client.widget.entity.JiraURLHelperImpl
#
# Author: brucehoff
###############################################################################

# creates the URL that opens the brower to the creation page for an access restriction issue
createAccessRestrictionIssue<-function(userEmailAddress, userDisplayName, dataObjectId) {
  jiraProjectId <- "10831"
  access_restriction_type<-"11" 
  ACCESS_RESTRCTION_ISSUE_SUMMARY<-"Request for ACT to add data restriction"
  default_issue_reporter<-"synapse-jira-service"
  DEFAULT_RESTRICTION_DESCRIPTION<-"By clicking 'Create' below, I request that the Synapse ACT contact me to assign the appropriate access restrictions for this dataset."
  
  createIssueURL(jiraProjectId, 
    access_restriction_type, 
    ACCESS_RESTRCTION_ISSUE_SUMMARY, 
    default_issue_reporter, 
    DEFAULT_RESTRICTION_DESCRIPTION,
    userDisplayName, 
    userEmailAddress,
    dataObjectId)
}

# creates a URL to open the browser to the creation page for a Jira issue, in general
createIssueURL<-function(
    projectId,
    issueType,
    summary,
    reporter,
    description,
    userDisplayName,
    userEmailAddress,
    dataObjectId
) {
  confluenceEndpoint <- "https://sagebionetworks.jira.com"
  CREATE_JIRA_ISSUE_URI <- "/secure/CreateIssueDetails!init.jspa"
  major_priority<-"3"
  ISSUE_FIELD_DESCRIPTION<-"description"
  issue_field_user_display_name<-"customfield_10741"
  issue_field_user_email<-"customfield_10840"
  issue_field_data_object_id<-"customfield_10742"
  
  sprintf("%s%s?pid=%s&issuetype=%s&summary=%s&reporter=%s&priority=%s&%s=%s&%s=%s&%s=%s&%s=%s", 
    confluenceEndpoint, 
    CREATE_JIRA_ISSUE_URI, 
    projectId, 
    issueType, 
    summary,
    reporter,
    major_priority,
    ISSUE_FIELD_DESCRIPTION,
    description,
    issue_field_user_display_name,
    userDisplayName,
    issue_field_user_email,
    userEmailAddress,
    issue_field_data_object_id,
    dataObjectId
  )
}
