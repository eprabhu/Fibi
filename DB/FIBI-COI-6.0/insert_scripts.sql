
INSERT INTO MODULE_ACTION_TYPE(MODULE_ACTION_TYPE_ID,ACTION_TYPE,MODULE_CODE,SUB_MODULE_CODE,IS_ACTIVE,DESCRIPTION,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(1,'FCOI_SUBMIT',8,0,'Y','FCOI Disclosure Submit/Certify',now(),'quickstart');
INSERT INTO MODULE_ACTION_TYPE(MODULE_ACTION_TYPE_ID,ACTION_TYPE,MODULE_CODE,SUB_MODULE_CODE,IS_ACTIVE,DESCRIPTION,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(2,'PROJECT_SUBMIT',8,0,'Y','Project disclosure submit',now(),'quickstart');
INSERT INTO MODULE_ACTION_TYPE(MODULE_ACTION_TYPE_ID,ACTION_TYPE,MODULE_CODE,SUB_MODULE_CODE,IS_ACTIVE,DESCRIPTION,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(3,'FCOI_COMPLETE',8,0,'Y','FCOI Disclosure complete/approve',now(),'quickstart');
INSERT INTO MODULE_ACTION_TYPE(MODULE_ACTION_TYPE_ID,ACTION_TYPE,MODULE_CODE,SUB_MODULE_CODE,IS_ACTIVE,DESCRIPTION,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(4,'PROJECT_COMPLETE',8,0,'Y','Project Disclosure complete/approve',now(),'quickstart');
INSERT INTO MODULE_ACTION_TYPE(MODULE_ACTION_TYPE_ID,ACTION_TYPE,MODULE_CODE,SUB_MODULE_CODE,IS_ACTIVE,DESCRIPTION,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(5,'FCOI_RETURN',8,0,'Y','FCOI Disclosure Return',now(),'quickstart');
INSERT INTO MODULE_ACTION_TYPE(MODULE_ACTION_TYPE_ID,ACTION_TYPE,MODULE_CODE,SUB_MODULE_CODE,IS_ACTIVE,DESCRIPTION,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(6,'PROJECT_RETURN',8,0,'Y','Project Disclosure Return',now(),'quickstart');
INSERT INTO MODULE_ACTION_TYPE(MODULE_ACTION_TYPE_ID,ACTION_TYPE,MODULE_CODE,SUB_MODULE_CODE,IS_ACTIVE,DESCRIPTION,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(7,'FCOI_WITHDRAW',8,0,'Y','FCOI Disclosure Withdraw',now(),'quickstart');
INSERT INTO MODULE_ACTION_TYPE(MODULE_ACTION_TYPE_ID,ACTION_TYPE,MODULE_CODE,SUB_MODULE_CODE,IS_ACTIVE,DESCRIPTION,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(8,'PROJECT_WITHDRAW',8,0,'Y','Project Disclosure Withdraw',now(),'quickstart');
INSERT INTO MODULE_ACTION_TYPE(MODULE_ACTION_TYPE_ID,ACTION_TYPE,MODULE_CODE,SUB_MODULE_CODE,IS_ACTIVE,DESCRIPTION,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(9,'FCOI_REVIEWER_ASSIGN',8,0,'Y','FCOI Disclosure Reviewer added',now(),'quickstart');
INSERT INTO MODULE_ACTION_TYPE(MODULE_ACTION_TYPE_ID,ACTION_TYPE,MODULE_CODE,SUB_MODULE_CODE,IS_ACTIVE,DESCRIPTION,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(10,'PROJECT_REVIEWER_ASSIGN',8,0,'Y','Project Disclosure Reviewer updated',now(),'quickstart');
INSERT INTO MODULE_ACTION_TYPE(MODULE_ACTION_TYPE_ID,ACTION_TYPE,MODULE_CODE,SUB_MODULE_CODE,IS_ACTIVE,DESCRIPTION,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(11,'FCOI_REVIEWER_UPDATE',8,0,'Y','FCOI Disclosure  Reviewer added',now(),'quickstart');
INSERT INTO MODULE_ACTION_TYPE(MODULE_ACTION_TYPE_ID,ACTION_TYPE,MODULE_CODE,SUB_MODULE_CODE,IS_ACTIVE,DESCRIPTION,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(12,'PROJECT_REVIEWER_UPDATE',8,0,'Y','Project Disclosure Reviewer updated',now(),'quickstart');
INSERT INTO MODULE_ACTION_TYPE(MODULE_ACTION_TYPE_ID,ACTION_TYPE,MODULE_CODE,SUB_MODULE_CODE,IS_ACTIVE,DESCRIPTION,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(13,'FCOI_ASSIGN_ADMIN',8,0,'Y','FCOI Disclosure Assign Administrator',now(),'quickstart');
INSERT INTO MODULE_ACTION_TYPE(MODULE_ACTION_TYPE_ID,ACTION_TYPE,MODULE_CODE,SUB_MODULE_CODE,IS_ACTIVE,DESCRIPTION,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(14,'PROJECT_ASSIGN_ADMIN',8,0,'Y','Project Disclosure Assign Administrator',now(),'quickstart');
INSERT INTO MODULE_ACTION_TYPE(MODULE_ACTION_TYPE_ID,ACTION_TYPE,MODULE_CODE,SUB_MODULE_CODE,IS_ACTIVE,DESCRIPTION,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(15,'FCOI_RESUBMIT',8,0,'Y','FCOI Disclosure Resubmit',now(),'quickstart');
INSERT INTO MODULE_ACTION_TYPE(MODULE_ACTION_TYPE_ID,ACTION_TYPE,MODULE_CODE,SUB_MODULE_CODE,IS_ACTIVE,DESCRIPTION,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(16,'PROJECT_RESUBMIT',8,0,'Y','Project Disclosure Resubmit',now(),'quickstart');
INSERT INTO MODULE_ACTION_TYPE(MODULE_ACTION_TYPE_ID,ACTION_TYPE,MODULE_CODE,SUB_MODULE_CODE,IS_ACTIVE,DESCRIPTION,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(17,'FCOI_LOCATION_ASSIGN',8,0,'Y','FCOI Disclosure Reviewer Location added',now(),'quickstart');
INSERT INTO MODULE_ACTION_TYPE(MODULE_ACTION_TYPE_ID,ACTION_TYPE,MODULE_CODE,SUB_MODULE_CODE,IS_ACTIVE,DESCRIPTION,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(18,'PROJECT_LOCATION_ASSIGN',8,0,'Y','Project Disclosure Reviewer Location added',now(),'quickstart');
INSERT INTO MODULE_ACTION_TYPE(MODULE_ACTION_TYPE_ID,ACTION_TYPE,MODULE_CODE,SUB_MODULE_CODE,IS_ACTIVE,DESCRIPTION,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(19,'FCOI_LOCATION_UPDATE',8,0,'Y','FCOI Disclosure  Reviewer Location updated',now(),'quickstart');
INSERT INTO MODULE_ACTION_TYPE(MODULE_ACTION_TYPE_ID,ACTION_TYPE,MODULE_CODE,SUB_MODULE_CODE,IS_ACTIVE,DESCRIPTION,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(20,'PROJECT_LOCATION_UPDATE',8,0,'Y','Project Disclosure Reviewer Location updated',now(),'quickstart');
INSERT INTO MODULE_ACTION_TYPE(MODULE_ACTION_TYPE_ID,ACTION_TYPE,MODULE_CODE,SUB_MODULE_CODE,IS_ACTIVE,DESCRIPTION,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(21,'FCOI_REASSIGN_ADMIN',8,0,'Y','FCOI Disclosure Reassign Administrator',now(),'quickstart');
INSERT INTO MODULE_ACTION_TYPE(MODULE_ACTION_TYPE_ID,ACTION_TYPE,MODULE_CODE,SUB_MODULE_CODE,IS_ACTIVE,DESCRIPTION,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(22,'PROJECT_REASSIGN_ADMIN',8,0,'Y','Project Disclosure Reassign Administrator',now(),'quickstart');

INSERT INTO NOTIFY_PLACEHOLDER_HEADER(NOTIFY_PLACEHOLDER_HEADER_ID,MODULE_CODE,SUB_MODULE_CODE,QUERY_DEFINITION,QUERY_TYPE,ELEMENT_NAME,ELEMENT_TYPE,IS_ACTIVE,DESCRIPTION,UPDATE_TIMESTAMP,UPDATE_USER,UNIQUE_DISPLAY_NAME)
VALUES(1,8,0,'GET_COI_DISCL_GENERAL_DETAILS','P','','P','Y','',now(),'quickstart','COI_DISCLOSURE');
INSERT INTO NOTIFY_PLACEHOLDER_HEADER(NOTIFY_PLACEHOLDER_HEADER_ID,MODULE_CODE,SUB_MODULE_CODE,QUERY_DEFINITION,QUERY_TYPE,ELEMENT_NAME,ELEMENT_TYPE,IS_ACTIVE,DESCRIPTION,UPDATE_TIMESTAMP,UPDATE_USER,UNIQUE_DISPLAY_NAME)
VALUES(2,8,0,'','S','','S','Y','Hardcoded Placeholders',now(),'quickstart','COI_DISCL_OTHER');
INSERT INTO NOTIFY_PLACEHOLDER_HEADER(NOTIFY_PLACEHOLDER_HEADER_ID,MODULE_CODE,SUB_MODULE_CODE,QUERY_DEFINITION,QUERY_TYPE,ELEMENT_NAME,ELEMENT_TYPE,IS_ACTIVE,DESCRIPTION,UPDATE_TIMESTAMP,UPDATE_USER,UNIQUE_DISPLAY_NAME)
VALUES(3,8,0,'GET_COI_DISCL_REVIEW_DETAILS','P','','P','Y','Disclosure review details only',now(),'quickstart','COI_DISCL_REVIEW');

INSERT INTO NOTIFY_PLACEHOLDER_COLUMNS(NOTIFY_PLACEHOLDER_HEADER_ID,QUERY_COLUMN_NAME,LABEL_NAME,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(1,'DISCLOSURE_STATUS','Disclosure Status',now(),'quickstart');
INSERT INTO NOTIFY_PLACEHOLDER_COLUMNS(NOTIFY_PLACEHOLDER_HEADER_ID,QUERY_COLUMN_NAME,LABEL_NAME,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(1,'EXPIRATION_DATE','Disclosure Expiration Date',now(),'quickstart');
INSERT INTO NOTIFY_PLACEHOLDER_COLUMNS(NOTIFY_PLACEHOLDER_HEADER_ID,QUERY_COLUMN_NAME,LABEL_NAME,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(1,'CERTIFICATION_DATE','Disclosure Certification Date',now(),'quickstart');
INSERT INTO NOTIFY_PLACEHOLDER_COLUMNS(NOTIFY_PLACEHOLDER_HEADER_ID,QUERY_COLUMN_NAME,LABEL_NAME,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(1,'REVIEW_STATUS','Disclosure Review Status',now(),'quickstart');
INSERT INTO NOTIFY_PLACEHOLDER_COLUMNS(NOTIFY_PLACEHOLDER_HEADER_ID,QUERY_COLUMN_NAME,LABEL_NAME,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(1,'DEPARTMENT_NAME','Disclosure Department Name',now(),'quickstart');
INSERT INTO NOTIFY_PLACEHOLDER_COLUMNS(NOTIFY_PLACEHOLDER_HEADER_ID,QUERY_COLUMN_NAME,LABEL_NAME,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(1,'DEPARTMENT_NUMBER','Disclosure Department Number',now(),'quickstart');
INSERT INTO NOTIFY_PLACEHOLDER_COLUMNS(NOTIFY_PLACEHOLDER_HEADER_ID,QUERY_COLUMN_NAME,LABEL_NAME,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(1,'REPORTER_NAME','Reporter Name',now(),'quickstart');
INSERT INTO NOTIFY_PLACEHOLDER_COLUMNS(NOTIFY_PLACEHOLDER_HEADER_ID,QUERY_COLUMN_NAME,LABEL_NAME,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(2,'WITHDRAWAL_REASON','Disclosure Withdrawal Reason',now(),'quickstart');
INSERT INTO NOTIFY_PLACEHOLDER_COLUMNS(NOTIFY_PLACEHOLDER_HEADER_ID,QUERY_COLUMN_NAME,LABEL_NAME,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(2,'WITHDRAWAL_DATE','Disclosure Withdrawal Date',now(),'quickstart');
INSERT INTO NOTIFY_PLACEHOLDER_COLUMNS(NOTIFY_PLACEHOLDER_HEADER_ID,QUERY_COLUMN_NAME,LABEL_NAME,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(2,'RETURN_REASON','Disclosure Return Reason',now(),'quickstart');
INSERT INTO NOTIFY_PLACEHOLDER_COLUMNS(NOTIFY_PLACEHOLDER_HEADER_ID,QUERY_COLUMN_NAME,LABEL_NAME,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(1,'ADMINISTRATOR_NAME','Administrator Name',now(),'quickstart');
INSERT INTO NOTIFY_PLACEHOLDER_COLUMNS(NOTIFY_PLACEHOLDER_HEADER_ID,QUERY_COLUMN_NAME,LABEL_NAME,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(2,'ADMIN_ASSIGNED_BY','Administrator Assigned By',now(),'quickstart');
INSERT INTO NOTIFY_PLACEHOLDER_COLUMNS(NOTIFY_PLACEHOLDER_HEADER_ID,QUERY_COLUMN_NAME,LABEL_NAME,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(2,'ADMIN_ASSIGNED_TO','Administrator Assigned To',now(),'quickstart');
INSERT INTO NOTIFY_PLACEHOLDER_COLUMNS(NOTIFY_PLACEHOLDER_HEADER_ID,QUERY_COLUMN_NAME,LABEL_NAME,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(3,'REVIEW_LOCATION','Review Location',now(),'quickstart');
INSERT INTO NOTIFY_PLACEHOLDER_COLUMNS(NOTIFY_PLACEHOLDER_HEADER_ID,QUERY_COLUMN_NAME,LABEL_NAME,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(3,'REVIEWER_REVIEW_STATUS','Reviewer Review Status',now(),'quickstart');
INSERT INTO NOTIFY_PLACEHOLDER_COLUMNS(NOTIFY_PLACEHOLDER_HEADER_ID,QUERY_COLUMN_NAME,LABEL_NAME,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(1,'PROJECT_NUMBER','Disclosure Project ID/Number',now(),'quickstart');
INSERT INTO NOTIFY_PLACEHOLDER_COLUMNS(NOTIFY_PLACEHOLDER_HEADER_ID,QUERY_COLUMN_NAME,LABEL_NAME,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(1,'PROJECT_TITLE','Disclosure Project Title',now(),'quickstart');

INSERT INTO notification_type(NOTIFICATION_TYPE_ID,MODULE_CODE,SUB_MODULE_CODE,DESCRIPTION,SUBJECT,MESSAGE,PROMPT_USER,IS_ACTIVE,CREATE_USER,CREATE_TIMESTAMP,UPDATE_USER,UPDATE_TIMESTAMP,IS_SYSTEM_SPECIFIC)
VALUES(8000,8,0,'FCOI Disclosure Submitted','Action required: Approval for COI Annual Disclosure Submitted by {COI_DISCLOSURE#REPORTER_NAME}.','<p>A COI Annual Disclosure was submitted by {COI_DISCLOSURE#REPORTER_NAME} on {COI_DISCLOSURE#CERTIFICATION_DATE}.</p><p>Department : {COI_DISCLOSURE#DEPARTMENT_NUMBER} - {COI_DISCLOSURE#DEPARTMENT_NAME}</p><p>Disclosure Status: {COI_DISCLOSURE#DISCLOSURE_STATUS}</p><p>You can review this disclosure at {{DISCLOSURE_DETAIL_VIEW}}.</p><p>You can access the COI Admin Dashboard at {{APPLICATION_URL}}.</p><p>Note: This is a system-generated email. Please do not reply to this email.</p>','N','Y','quickstart',now(),'quickstart',now(),'N');
INSERT INTO notification_type(NOTIFICATION_TYPE_ID,MODULE_CODE,SUB_MODULE_CODE,DESCRIPTION,SUBJECT,MESSAGE,PROMPT_USER,IS_ACTIVE,CREATE_USER,CREATE_TIMESTAMP,UPDATE_USER,UPDATE_TIMESTAMP,IS_SYSTEM_SPECIFIC)
VALUES(8001,8,0,'FCOI Disclosure Withdrawn','Withdrawal of COI Annual Disclosure Submitted by {COI_DISCLOSURE#REPORTER_NAME}','<p>A COI Annual Disclosure was submitted by {COI_DISCLOSURE#REPORTER_NAME} on {COI_DISCLOSURE#CERTIFICATION_DATE} is Withdrawn for the following reason {COI_DISCL_OTHER#WITHDRAWAL_REASON}</p><p>Department : {COI_DISCLOSURE#DEPARTMENT_NUMBER} - {COI_DISCLOSURE#DEPARTMENT_NAME}</p><p>Disclosure Status: {COI_DISCLOSURE#DISCLOSURE_STATUS}</p><p>You can view this disclosure at {{APPLICATION_URL}}.</p><p>Note: This is a system generated email. Please do not reply to this email.</p>','N','Y','quickstart',now(),'quickstart',now(),'N');
INSERT INTO notification_type(NOTIFICATION_TYPE_ID,MODULE_CODE,SUB_MODULE_CODE,DESCRIPTION,SUBJECT,MESSAGE,PROMPT_USER,IS_ACTIVE,CREATE_USER,CREATE_TIMESTAMP,UPDATE_USER,UPDATE_TIMESTAMP,IS_SYSTEM_SPECIFIC)
VALUES(8002,8,0,'FCOI Disclosure Returned','Action Required: COI Annual Disclosure Submitted by  {COI_DISCLOSURE#REPORTER_NAME} has been returned by the admin {COI_DISCLOSURE#ADMINISTRATOR_NAME}','<p>Your COI annual disclosure submitted on {COI_DISCLOSURE#CERTIFICATION_DATE} was returned for the following reason {COI_DISCL_OTHER#RETURN_REASON}.</p><p>Department : {COI_DISCLOSURE#DEPARTMENT_NUMBER} - {COI_DISCLOSURE#DEPARTMENT_NAME}</p><p>Disclosure Status: {COI_DISCLOSURE#DISCLOSURE_STATUS}</p><p>Please login to COI and access your disclosure at {{APPLICATION_URL}} to update and submit your disclosure.</p><p>Note: This is a system-generated email. Please do not reply to this email.</p>','N','Y','quickstart',now(),'quickstart',now(),'N');
INSERT INTO notification_type(NOTIFICATION_TYPE_ID,MODULE_CODE,SUB_MODULE_CODE,DESCRIPTION,SUBJECT,MESSAGE,PROMPT_USER,IS_ACTIVE,CREATE_USER,CREATE_TIMESTAMP,UPDATE_USER,UPDATE_TIMESTAMP,IS_SYSTEM_SPECIFIC)
VALUES(8003,8,0,'FCOI Disclosure Approved','Annual COI Disclosure of  {COI_DISCLOSURE#REPORTER_NAME} submitted on {COI_DISCLOSURE#CERTIFICATION_DATE} is Approved','<p>Your COI annual disclosure submitted on {COI_DISCLOSURE#CERTIFICATION_DATE} was approved.</p><p>Department: {COI_DISCLOSURE#DEPARTMENT_NUMBER} - {COI_DISCLOSURE#DEPARTMENT_NAME}</p><p>Disclosure Status: {COI_DISCLOSURE#DISCLOSURE_STATUS}</p><p>You can view the disclosure at {{APPLICATION_URL}}</p><p>Note: This is a system-generated email. Please do not reply to this email.</p>','N','Y','quickstart',now(),'quickstart',now(),'N');
INSERT INTO notification_type(NOTIFICATION_TYPE_ID,MODULE_CODE,SUB_MODULE_CODE,DESCRIPTION,SUBJECT,MESSAGE,PROMPT_USER,IS_ACTIVE,CREATE_USER,CREATE_TIMESTAMP,UPDATE_USER,UPDATE_TIMESTAMP,IS_SYSTEM_SPECIFIC)
VALUES(8004,8,0,'FCOI Disclosure Administrator Assigned','Action required: You are assigned as an Administrator for the COI Annual disclosure Submitted by {COI_DISCLOSURE#REPORTER_NAME}','<p>Hello {COI_DISCL_OTHER#ADMIN_ASSIGNED_TO},</p><p>The COI Administrator {COI_DISCL_OTHER#ADMIN_ASSIGNED_BY} assigned you as the Administrator of the COI Annual Disclosure submitted on {COI_DISCLOSURE#CERTIFICATION_DATE} by {COI_DISCLOSURE#REPORTER_NAME}.</p><p>Department: {COI_DISCLOSURE#DEPARTMENT_NUMBER} - {COI_DISCLOSURE#DEPARTMENT_NAME}</p><p>Disclosure Status: {COI_DISCLOSURE#DISCLOSURE_STATUS}</p><p>Please review this disclosure at {{APPLICATION_URL}}. Thanks for your helping in reviewing this disclosure.</p><p>Note: This is a system-generated email. Please do not reply to this email.</p>','N','Y','quickstart',now(),'quickstart',now(),'N');
INSERT INTO notification_type(NOTIFICATION_TYPE_ID,MODULE_CODE,SUB_MODULE_CODE,DESCRIPTION,SUBJECT,MESSAGE,PROMPT_USER,IS_ACTIVE,CREATE_USER,CREATE_TIMESTAMP,UPDATE_USER,UPDATE_TIMESTAMP,IS_SYSTEM_SPECIFIC)
VALUES(8005,8,0,'FCOI Disclosure Administrator Reassigned','Review reassigned for COI Annual Disclosure Submitted by {COI_DISCLOSURE#REPORTER_NAME}','<p>Hello {COI_DISCLOSURE#ADMINISTRATOR_NAME},</p><p>COI Administrator removed you as a Administrator for the COI Annual Disclosure submitted on {COI_DISCLOSURE#CERTIFICATION_DATE} by {COI_DISCLOSURE#REPORTER_NAME}.</p><p>Department: {COI_DISCLOSURE#DEPARTMENT_NUMBER} - {COI_DISCLOSURE#DEPARTMENT_NAME}</p><p>You are no longer responsible for reviewing this disclosure.</p><p>You can access the COI Dashboard at {{APPLICATION_URL}}.</p><p>Note: This is a system-generated email. Please do not reply to this email.</p>','N','Y','quickstart',now(),'quickstart',now(),'N');
INSERT INTO notification_type(NOTIFICATION_TYPE_ID,MODULE_CODE,SUB_MODULE_CODE,DESCRIPTION,SUBJECT,MESSAGE,PROMPT_USER,IS_ACTIVE,CREATE_USER,CREATE_TIMESTAMP,UPDATE_USER,UPDATE_TIMESTAMP,IS_SYSTEM_SPECIFIC)
VALUES(8006,8,0,'FCOI Disclosure Review Assigned – Only Location','Action required: COI Annual disclosure Submitted by {COI_DISCLOSURE#REPORTER_NAME} waiting for review at {COI_DISCL_REVIEW#REVIEW_LOCATION} with {COI_DISCL_REVIEW#REVIEWER_REVIEW_STATUS} status ','<p>COI Administrator {COI_DISCLOSURE#ADMINISTRATOR_NAME} assigned you to review the COI Annual Disclosure submitted on {COI_DISCLOSURE#CERTIFICATION_DATE} by {COI_DISCLOSURE#REPORTER_NAME}.</p><p>Department: {COI_DISCLOSURE#DEPARTMENT_NUMBER} - {COI_DISCLOSURE#DEPARTMENT _NAME} </p><p>Disclosure Status: {COI_DISCLOSURE#DISCLOSURE_STATUS} </p><p>Review Location:{COI_DISCL_REVIEW#REVIEW_LOCATION}</p><p>Review Status:{COI_DISCL_REVIEW#REVIEWER_REVIEW_STATUS}</p><p>Please review this disclosure at {{APPLICATION_URL}}. Thanks for your helping in reviewing this disclosure.</p><p><span style=background-color:rgb(255,255,255);color:rgb(23,43,77);>Note: This is a system-generated email. Please do not reply to this email. </span></p>','N','Y','quickstart',now(),'quickstart',now(),'N');
INSERT INTO notification_type(NOTIFICATION_TYPE_ID,MODULE_CODE,SUB_MODULE_CODE,DESCRIPTION,SUBJECT,MESSAGE,PROMPT_USER,IS_ACTIVE,CREATE_USER,CREATE_TIMESTAMP,UPDATE_USER,UPDATE_TIMESTAMP,IS_SYSTEM_SPECIFIC)
VALUES(8007,8,0,'Project Disclosure Submission','Action required: Approval for Project Disclosure Submitted by {COI_DISCLOSURE#REPORTER_NAME}','<p>A Project Disclosure was submitted by {COI_DISCLOSURE#REPORTER_NAME} on {COI_DISCLOSURE#CERTIFICATION_DATE}.</p><p>Project: {COI_DISCLOSURE#PROJECT_TYPE}: {COI_DISCLOSURE#PROJECT_NUMBER}  -  {COI_DISCLOSURE#PROJECT_TITLE}</p><p>Department : {COI_DISCLOSURE#DEPARTMENT_NUMBER} - {COI_DISCLOSURE#DEPARTMENT_NAME} </p><p>Disclosure Status:{COI_DISCLOSURE#DISCLOSURE_STATUS}</p><p>You can access the COI Admin Dashboard at {{APPLICATION_URL}}.</p><p>Note: This is a system-generated email. Please do not reply to this email. </p>','N','Y','quickstart',now(),'quickstart',now(),'N');
INSERT INTO notification_type(NOTIFICATION_TYPE_ID,MODULE_CODE,SUB_MODULE_CODE,DESCRIPTION,SUBJECT,MESSAGE,PROMPT_USER,IS_ACTIVE,CREATE_USER,CREATE_TIMESTAMP,UPDATE_USER,UPDATE_TIMESTAMP,IS_SYSTEM_SPECIFIC)
VALUES(8008,8,0,'Project Disclosure Withdrawn ','Withdrawal of Project Disclosure Submitted by {COI_DISCLOSURE#REPORTER_NAME}','<p>A Project Disclosure was submitted by {COI_DISCLOSURE#REPORTER_NAME} on {COI_DISCLOSURE#CERTIFICATION_DATE} is Withdrawn for the following reason {COI_DISCL_OTHER#WITHDRAWAL_REASON}</p><p>Department : {COI_DISCLOSURE#DEPARTMENT_NUMBER} - {COI_DISCLOSURE#DEPARTMENT_NAME}</p><p>Project: {COI_DISCLOSURE#PROJECT_TYPE}: {COI_DISCLOSURE#PROJECT_NUMBER} {COI_DISCLOSURE#PROJECT_TITLE}</p><p>Disclosure Status:{COI_DISCLOSURE#DISCLOSURE_STATUS}</p><p>You can access the COI Admin Dashboard at {{APPLICATION_URL}}.</p><p>Note: This is a system-generated email. Please do not reply to this email.</p>','N','Y','quickstart',now(),'quickstart',now(),'N');
INSERT INTO notification_type(NOTIFICATION_TYPE_ID,MODULE_CODE,SUB_MODULE_CODE,DESCRIPTION,SUBJECT,MESSAGE,PROMPT_USER,IS_ACTIVE,CREATE_USER,CREATE_TIMESTAMP,UPDATE_USER,UPDATE_TIMESTAMP,IS_SYSTEM_SPECIFIC)
VALUES(8009,8,0,'Project Disclosure Returned','Action Required: Project Disclosure Submitted by {COI_DISCLOSURE#REPORTER_NAME} has been returned by the admin {COI_DISCLOSURE#ADMINISTRATOR_NAME} ','<p>Dear {COI_DISCLOSURE#REPORTER_NAME},</p><p>Your Project disclosure submitted on {COI_DISCLOSURE#CERTIFICATION_DATE} was returned for the following reason {RETURN_REASON}.</p><p>Department        : {COI_DISCLOSURE#DEPARTMENT_NUMBER} - {COI_DISCLOSURE#DEPARTMENT_NAME} </p><p>Project: {COI_DISCLOSURE#PROJECT_TYPE}:{COI_DISCLOSURE#PROJECT_NUMBER} - {COI_DISCLOSURE#PROJECT_TITLE}</p><p>Disclosure Status: {COI_DISCLOSURE#DISCLOSURE_STATUS}</p><p>Please login to COI and access your disclosure at {APPLICATION_URL} to update and submit your disclosure.</p><p>Note: This is a system-generated email. Please do not reply to this email. </p>','N','Y','quickstart',now(),'quickstart',now(),'N');
INSERT INTO notification_type(NOTIFICATION_TYPE_ID,MODULE_CODE,SUB_MODULE_CODE,DESCRIPTION,SUBJECT,MESSAGE,PROMPT_USER,IS_ACTIVE,CREATE_USER,CREATE_TIMESTAMP,UPDATE_USER,UPDATE_TIMESTAMP,IS_SYSTEM_SPECIFIC)
VALUES(8010,8,0,'Project Disclosure Administrator Assigned','Action required: You are assigned as an Administrator for the Project disclosure Submitted by {COI_DISCLOSURE#REPORTER_NAME}','<p>Hello {COI_DISCL_OTHER#ADMIN_ASSIGNED_TO},</p><p>COI Administrator {COI_DISCL_OTHER#ADMIN_ASSIGNED_BY} assigned you as the Administrator of the Project Disclosure submitted on {COI_DISCLOSURE#CERTIFICATION_DATE} by {COI_DISCLOSURE#REPORTER_NAME}.</p><p>Department: {COI_DISCLOSURE#DEPARTMENT_NUMBER} - {COI_DISCLOSURE#DEPARTMENT_NAME} </p><p>Project: {COI_DISCLOSURE#PROJECT_TYPE}: {COI_DISCLOSURE#PROJECT_NUMBER} - {COI_DISCLOSURE#PROJECT_TITLE}</p><p>Disclosure Status: {DISCLOSURE_STATUS} </p><p>Please review this disclosure at<s> </s>{{APPLICATION_URL}}. Thanks for your helping in reviewing this disclosure.</p><p>Note: This is a system-generated email. Please do not reply to this email. </p>','N','Y','quickstart',now(),'quickstart',now(),'N');
INSERT INTO notification_type(NOTIFICATION_TYPE_ID,MODULE_CODE,SUB_MODULE_CODE,DESCRIPTION,SUBJECT,MESSAGE,PROMPT_USER,IS_ACTIVE,CREATE_USER,CREATE_TIMESTAMP,UPDATE_USER,UPDATE_TIMESTAMP,IS_SYSTEM_SPECIFIC)
VALUES(8011,8,0,'Project Disclosure Administrator Reassigned','Review reassigned for Project Disclosure Submitted by {COI_DISCLOSURE#REPORTER_NAME}','<p>Hello {COI_DISCLOSURE#ADMINISTRATOR_NAME},</p><p>COI Administrator removed you as a Administrator for the Project submitted on {COI_DISCLOSURE#CERTIFICATION_DATE} by {COI_DISCLOSURE#REPORTER_NAME}.</p><p>Department: {COI_DISCLOSURE#DEPARTMENT_NUMBER} - {COI_DISCLOSURE#DEPARTMENT_NAME} </p><p>Project: {COI_DISCLOSURE#PROJECT_TYPE}: {COI_DISCLOSURE#PROJECT_NUMBER} - {COI_DISCLOSURE#PROJECT_TITLE}</p><p>You are no longer responsible for reviewing this disclosure.</p><p>You can access the COI Dashboard at {APPLICATION_URL}.</p><p>Note: This is a system-generated email. Please do not reply to this email. </p>','N','Y','quickstart',now(),'quickstart',now(),'N');
INSERT INTO notification_type(NOTIFICATION_TYPE_ID,MODULE_CODE,SUB_MODULE_CODE,DESCRIPTION,SUBJECT,MESSAGE,PROMPT_USER,IS_ACTIVE,CREATE_USER,CREATE_TIMESTAMP,UPDATE_USER,UPDATE_TIMESTAMP,IS_SYSTEM_SPECIFIC)
VALUES(8012,8,0,'FCOI Disclosure Notify Reassigned Administrator ','Action required: You are assigned as an Administrator for the COI Annual disclosure Submitted by {COI_DISCLOSURE#REPORTER_NAME}','<p>Hello {COI_DISCL_OTHER#ADMIN_ASSIGNED_TO},</p><p>The COI Administrator {COI_DISCL_OTHER#ADMIN_ASSIGNED_BY} assigned you as the Administrator of the COI Annual Disclosure submitted on {COI_DISCLOSURE#CERTIFICATION_DATE} by {COI_DISCLOSURE#REPORTER_NAME}.</p><p>Department: {COI_DISCLOSURE#DEPARTMENT_NUMBER} - {COI_DISCLOSURE#DEPARTMENT_NAME}</p><p>Disclosure Status: {COI_DISCLOSURE#DISCLOSURE_STATUS}</p><p>Please review this disclosure at {{APPLICATION_URL}}. Thanks for your helping in reviewing this disclosure.</p><p>Note: This is a system-generated email. Please do not reply to this email.</p>','N','Y','quickstart',now(),'quickstart',now(),'N');
INSERT INTO notification_type(NOTIFICATION_TYPE_ID,MODULE_CODE,SUB_MODULE_CODE,DESCRIPTION,SUBJECT,MESSAGE,PROMPT_USER,IS_ACTIVE,CREATE_USER,CREATE_TIMESTAMP,UPDATE_USER,UPDATE_TIMESTAMP,IS_SYSTEM_SPECIFIC)
VALUES(8013,8,0,'Project Disclosure Notify Reassigned Administrator ','Action required: You are assigned as an Administrator for the Project disclosure Submitted by {COI_DISCLOSURE#REPORTER_NAME}','<p>Hello {COI_DISCL_OTHER#ADMIN_ASSIGNED_TO},</p><p>COI Administrator {COI_DISCL_OTHER#ADMIN_ASSIGNED_BY} assigned you as the Administrator of the Project Disclosure submitted on {COI_DISCLOSURE#CERTIFICATION_DATE} by {COI_DISCLOSURE#REPORTER_NAME}.</p><p>Department: {COI_DISCLOSURE#DEPARTMENT_NUMBER} - {COI_DISCLOSURE#DEPARTMENT_NAME} </p><p>Project: {COI_DISCLOSURE#PROJECT_TYPE}: {COI_DISCLOSURE#PROJECT_NUMBER} - {COI_DISCLOSURE#PROJECT_TITLE}</p><p>Disclosure Status: {DISCLOSURE_STATUS} </p><p>Please review this disclosure at<s> </s>{{APPLICATION_URL}}. Thanks for your helping in reviewing this disclosure.</p><p>Note: This is a system-generated email. Please do not reply to this email. </p>','N','Y','quickstart',now(),'quickstart',now(),'N');

INSERT INTO notify_action_type_map(NOTIFICATION_TYPE_ID,MODULE_ACTION_TYPE_ID,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(8000,1,now(),'quickstart');
INSERT INTO notify_action_type_map(NOTIFICATION_TYPE_ID,MODULE_ACTION_TYPE_ID,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(8001,7,now(),'quickstart');
INSERT INTO notify_action_type_map(NOTIFICATION_TYPE_ID,MODULE_ACTION_TYPE_ID,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(8002,5,now(),'quickstart');
INSERT INTO notify_action_type_map(NOTIFICATION_TYPE_ID,MODULE_ACTION_TYPE_ID,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(8003,3,now(),'quickstart');
INSERT INTO notify_action_type_map(NOTIFICATION_TYPE_ID,MODULE_ACTION_TYPE_ID,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(8004,13,now(),'quickstart');
INSERT INTO notify_action_type_map(NOTIFICATION_TYPE_ID,MODULE_ACTION_TYPE_ID,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(8005,21,now(),'quickstart');
INSERT INTO notify_action_type_map(NOTIFICATION_TYPE_ID,MODULE_ACTION_TYPE_ID,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(8006,17,now(),'quickstart');
INSERT INTO notify_action_type_map(NOTIFICATION_TYPE_ID,MODULE_ACTION_TYPE_ID,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(8007,2,now(),'quickstart');
INSERT INTO notify_action_type_map(NOTIFICATION_TYPE_ID,MODULE_ACTION_TYPE_ID,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(8008,8,now(),'quickstart');
INSERT INTO notify_action_type_map(NOTIFICATION_TYPE_ID,MODULE_ACTION_TYPE_ID,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(8009,6,now(),'quickstart');
INSERT INTO notify_action_type_map(NOTIFICATION_TYPE_ID,MODULE_ACTION_TYPE_ID,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(8010,14,now(),'quickstart');
INSERT INTO notify_action_type_map(NOTIFICATION_TYPE_ID,MODULE_ACTION_TYPE_ID,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(8011,22,now(),'quickstart');
INSERT INTO notify_action_type_map(NOTIFICATION_TYPE_ID,MODULE_ACTION_TYPE_ID,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(8012,21,now(),'quickstart');
INSERT INTO notify_action_type_map(NOTIFICATION_TYPE_ID,MODULE_ACTION_TYPE_ID,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES(8013,22,now(),'quickstart');

INSERT INTO PERSON_ROLE_TYPE (ROLE_TYPE_CODE, DESCRIPTION, UPDATE_TIMESTAMP, UPDATE_USER, IS_ACTIVE)
VALUES ('52', 'COI Administrators', now(), 'quickstart', 'Y');
INSERT INTO PERSON_ROLE_TYPE (ROLE_TYPE_CODE, DESCRIPTION, UPDATE_TIMESTAMP, UPDATE_USER, IS_ACTIVE)
VALUES ('53', 'COI Reporter', now(), 'quickstart', 'Y');
INSERT INTO PERSON_ROLE_TYPE (ROLE_TYPE_CODE, DESCRIPTION, UPDATE_TIMESTAMP, UPDATE_USER, IS_ACTIVE)
VALUES ('54', 'COI Admin Group', now(), 'quickstart', 'Y');
INSERT INTO PERSON_ROLE_TYPE (ROLE_TYPE_CODE, DESCRIPTION, UPDATE_TIMESTAMP, UPDATE_USER, IS_ACTIVE)
VALUES ('55', 'COI Reviewers', now(), 'quickstart', 'Y');
INSERT INTO PERSON_ROLE_TYPE (ROLE_TYPE_CODE, DESCRIPTION, UPDATE_TIMESTAMP, UPDATE_USER, IS_ACTIVE)
VALUES ('56', 'COI Primary Administrator', now(), 'quickstart', 'Y');
INSERT INTO PERSON_ROLE_TYPE (ROLE_TYPE_CODE, DESCRIPTION, UPDATE_TIMESTAMP, UPDATE_USER, IS_ACTIVE)
VALUES ('56', 'COI Reviewer', now(), 'quickstart', 'Y');

INSERT INTO mq_router_action_configuration(ACTION_TYPE,IS_ACTIVE,MODULE_CODE,QUEUE_NAME,SUB_MODULE_CODE,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES('FCOI_COMPLETE','Y',8,'Q_NOTIFICATION',0,now(),'quickstart');
INSERT INTO mq_router_action_configuration(ACTION_TYPE,IS_ACTIVE,MODULE_CODE,QUEUE_NAME,SUB_MODULE_CODE,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES('FCOI_SUBMIT','Y',8,'Q_NOTIFICATION',0,now(),'quickstart');
INSERT INTO mq_router_action_configuration(ACTION_TYPE,IS_ACTIVE,MODULE_CODE,QUEUE_NAME,SUB_MODULE_CODE,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES('FCOI_WITHDRAW','Y',8,'Q_NOTIFICATION',0,now(),'quickstart');
INSERT INTO mq_router_action_configuration(ACTION_TYPE,IS_ACTIVE,MODULE_CODE,QUEUE_NAME,SUB_MODULE_CODE,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES('FCOI_RETURN','Y',8,'Q_NOTIFICATION',0,now(),'quickstart');
INSERT INTO mq_router_action_configuration(ACTION_TYPE,IS_ACTIVE,MODULE_CODE,QUEUE_NAME,SUB_MODULE_CODE,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES('FCOI_ASSIGN_ADMIN','Y',8,'Q_NOTIFICATION',0,now(),'quickstart');
INSERT INTO mq_router_action_configuration(ACTION_TYPE,IS_ACTIVE,MODULE_CODE,QUEUE_NAME,SUB_MODULE_CODE,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES('FCOI_REASSIGN_ADMIN','Y',8,'Q_NOTIFICATION',0,now(),'quickstart');
INSERT INTO mq_router_action_configuration(ACTION_TYPE,IS_ACTIVE,MODULE_CODE,QUEUE_NAME,SUB_MODULE_CODE,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES('PROJECT_SUBMIT','Y',8,'Q_NOTIFICATION',0,now(),'quickstart');
INSERT INTO mq_router_action_configuration(ACTION_TYPE,IS_ACTIVE,MODULE_CODE,QUEUE_NAME,SUB_MODULE_CODE,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES('PROJECT_WITHDRAW','Y',8,'Q_NOTIFICATION',0,now(),'quickstart');
INSERT INTO mq_router_action_configuration(ACTION_TYPE,IS_ACTIVE,MODULE_CODE,QUEUE_NAME,SUB_MODULE_CODE,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES('PROJECT_ASSIGN_ADMIN','Y',8,'Q_NOTIFICATION',0,now(),'quickstart');
INSERT INTO mq_router_action_configuration(ACTION_TYPE,IS_ACTIVE,MODULE_CODE,QUEUE_NAME,SUB_MODULE_CODE,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES('PROJECT_REASSIGN_ADMIN','Y',8,'Q_NOTIFICATION',0,now(),'quickstart');
INSERT INTO mq_router_action_configuration(ACTION_TYPE,IS_ACTIVE,MODULE_CODE,QUEUE_NAME,SUB_MODULE_CODE,UPDATE_TIMESTAMP,UPDATE_USER)
VALUES('PROJECT_RETURN','Y',8,'Q_NOTIFICATION',0,now(),'quickstart');

INSERT INTO notification_recipient(NOTIFICATION_TYPE_ID,ROLE_TYPE_CODE,CREATE_USER,CREATE_TIMESTAMP,UPDATE_USER,UPDATE_TIMESTAMP,RECIPIENT_TYPE,RECIPIENT_NAME)
VALUES(8003,53,'quickstart',now(),'quickstart',now(),'TO','COI Reporter');
INSERT INTO notification_recipient(NOTIFICATION_TYPE_ID,ROLE_TYPE_CODE,CREATE_USER,CREATE_TIMESTAMP,UPDATE_USER,UPDATE_TIMESTAMP,RECIPIENT_TYPE,RECIPIENT_NAME)
VALUES(8003,52,'quickstart',now(),'quickstart',now(),'CC','COI Administrators');
INSERT INTO notification_recipient(NOTIFICATION_TYPE_ID,ROLE_TYPE_CODE,CREATE_USER,CREATE_TIMESTAMP,UPDATE_USER,UPDATE_TIMESTAMP,RECIPIENT_TYPE,RECIPIENT_NAME)
VALUES(8003,54,'quickstart',now(),'quickstart',now(),'CC','COI Admin Group');
INSERT INTO notification_recipient(NOTIFICATION_TYPE_ID,ROLE_TYPE_CODE,CREATE_USER,CREATE_TIMESTAMP,UPDATE_USER,UPDATE_TIMESTAMP,RECIPIENT_TYPE,RECIPIENT_NAME)
VALUES(8000,52,'quickstart',now(),'quickstart',now(),'TO','COI Administrators');
INSERT INTO notification_recipient(NOTIFICATION_TYPE_ID,ROLE_TYPE_CODE,CREATE_USER,CREATE_TIMESTAMP,UPDATE_USER,UPDATE_TIMESTAMP,RECIPIENT_TYPE,RECIPIENT_NAME)
VALUES(8001,52,'quickstart',now(),'quickstart',now(),'TO','COI Administrators');
INSERT INTO notification_recipient(NOTIFICATION_TYPE_ID,ROLE_TYPE_CODE,CREATE_USER,CREATE_TIMESTAMP,UPDATE_USER,UPDATE_TIMESTAMP,RECIPIENT_TYPE,RECIPIENT_NAME)
VALUES(8002,53,'quickstart',now(),'quickstart',now(),'TO','COI Reporter');
INSERT INTO notification_recipient(NOTIFICATION_TYPE_ID,ROLE_TYPE_CODE,CREATE_USER,CREATE_TIMESTAMP,UPDATE_USER,UPDATE_TIMESTAMP,RECIPIENT_TYPE,RECIPIENT_NAME)
VALUES(8002,54,'quickstart',now(),'quickstart',now(),'CC','COI Admin Group');
INSERT INTO notification_recipient(NOTIFICATION_TYPE_ID,ROLE_TYPE_CODE,CREATE_USER,CREATE_TIMESTAMP,UPDATE_USER,UPDATE_TIMESTAMP,RECIPIENT_TYPE,RECIPIENT_NAME)
VALUES(8002,52,'quickstart',now(),'quickstart',now(),'CC','COI Administrators');
INSERT INTO notification_recipient(NOTIFICATION_TYPE_ID,ROLE_TYPE_CODE,CREATE_USER,CREATE_TIMESTAMP,UPDATE_USER,UPDATE_TIMESTAMP,RECIPIENT_TYPE,RECIPIENT_NAME)
VALUES(8002,55,'quickstart',now(),'quickstart',now(),'CC','COI Reviewers');
INSERT INTO notification_recipient(NOTIFICATION_TYPE_ID,ROLE_TYPE_CODE,CREATE_USER,CREATE_TIMESTAMP,UPDATE_USER,UPDATE_TIMESTAMP,RECIPIENT_TYPE,RECIPIENT_NAME)
VALUES(8004,56,'quickstart',now(),'quickstart',now(),'TO','COI Primary Administrator');
INSERT INTO notification_recipient(NOTIFICATION_TYPE_ID,ROLE_TYPE_CODE,CREATE_USER,CREATE_TIMESTAMP,UPDATE_USER,UPDATE_TIMESTAMP,RECIPIENT_TYPE,RECIPIENT_NAME)
VALUES(8004,54,'quickstart',now(),'quickstart',now(),'CC','COI Admin Group');
INSERT INTO notification_recipient(NOTIFICATION_TYPE_ID,ROLE_TYPE_CODE,CREATE_USER,CREATE_TIMESTAMP,UPDATE_USER,UPDATE_TIMESTAMP,RECIPIENT_TYPE,RECIPIENT_NAME)
VALUES(8005,54,'quickstart',now(),'quickstart',now(),'CC','COI Admin Group');
INSERT INTO notification_recipient(NOTIFICATION_TYPE_ID,ROLE_TYPE_CODE,CREATE_USER,CREATE_TIMESTAMP,UPDATE_USER,UPDATE_TIMESTAMP,RECIPIENT_TYPE,RECIPIENT_NAME)
VALUES(8012,56,'quickstart',now(),'quickstart',now(),'TO','COI Primary Administrator');
INSERT INTO notification_recipient(NOTIFICATION_TYPE_ID,ROLE_TYPE_CODE,CREATE_USER,CREATE_TIMESTAMP,UPDATE_USER,UPDATE_TIMESTAMP,RECIPIENT_TYPE,RECIPIENT_NAME)
VALUES(8012,54,'quickstart',now(),'quickstart',now(),'CC','COI Admin Group');
INSERT INTO notification_recipient(NOTIFICATION_TYPE_ID,ROLE_TYPE_CODE,CREATE_USER,CREATE_TIMESTAMP,UPDATE_USER,UPDATE_TIMESTAMP,RECIPIENT_TYPE,RECIPIENT_NAME)
VALUES(8007,52,'quickstart',now(),'quickstart',now(),'TO','COI Administrators');
INSERT INTO notification_recipient(NOTIFICATION_TYPE_ID,ROLE_TYPE_CODE,CREATE_USER,CREATE_TIMESTAMP,UPDATE_USER,UPDATE_TIMESTAMP,RECIPIENT_TYPE,RECIPIENT_NAME)
VALUES(8008,52,'quickstart',now(),'quickstart',now(),'TO','COI Administrators');
INSERT INTO notification_recipient(NOTIFICATION_TYPE_ID,ROLE_TYPE_CODE,CREATE_USER,CREATE_TIMESTAMP,UPDATE_USER,UPDATE_TIMESTAMP,RECIPIENT_TYPE,RECIPIENT_NAME)
VALUES(8010,52,'quickstart',now(),'quickstart',now(),'TO','COI Administrators');
INSERT INTO notification_recipient(NOTIFICATION_TYPE_ID,ROLE_TYPE_CODE,CREATE_USER,CREATE_TIMESTAMP,UPDATE_USER,UPDATE_TIMESTAMP,RECIPIENT_TYPE,RECIPIENT_NAME)
VALUES(8010,54,'quickstart',now(),'quickstart',now(),'CC','COI Admin Group');
INSERT INTO notification_recipient(NOTIFICATION_TYPE_ID,ROLE_TYPE_CODE,CREATE_USER,CREATE_TIMESTAMP,UPDATE_USER,UPDATE_TIMESTAMP,RECIPIENT_TYPE,RECIPIENT_NAME)
VALUES(8011,54,'quickstart',now(),'quickstart',now(),'TO','COI Admin Group');
INSERT INTO notification_recipient(NOTIFICATION_TYPE_ID,ROLE_TYPE_CODE,CREATE_USER,CREATE_TIMESTAMP,UPDATE_USER,UPDATE_TIMESTAMP,RECIPIENT_TYPE,RECIPIENT_NAME)
VALUES(8013,54,'quickstart',now(),'quickstart',now(),'TO','COI Admin Group');
INSERT INTO notification_recipient(NOTIFICATION_TYPE_ID,ROLE_TYPE_CODE,CREATE_USER,CREATE_TIMESTAMP,UPDATE_USER,UPDATE_TIMESTAMP,RECIPIENT_TYPE,RECIPIENT_NAME)
VALUES(8009,53,'quickstart',now(),'quickstart',now(),'TO','COI Reporter');


