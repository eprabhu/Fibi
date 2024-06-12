INSERT INTO `form_builder_counter` (`COUNTER_NAME`, `COUNTER_VALUE`) VALUES ('FORM_BUILDER_NUMBER', '1');

INSERT INTO `coeus_module` (`MODULE_CODE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `IS_ACTIVE`) 
VALUES ('27', 'Consulting Disclosure', now(), 'quickstart', 'Y');

INSERT INTO `rights` (`RIGHT_ID`, `RIGHT_NAME`, `DESCRIPTION`, `UPDATE_USER`, `UPDATE_TIMESTAMP`, `RIGHTS_TYPE_CODE`) 
VALUES ((SELECT T.ID FROM (SELECT MAX(RIGHT_ID) + 1 AS ID FROM RIGHTS) AS T), 'MANAGE_CONSULTING_DISCLOSURE', 'To manage consulting disclosure', 'quickstart', now(), '2');
INSERT INTO `rights` (`RIGHT_ID`, `RIGHT_NAME`, `DESCRIPTION`, `UPDATE_USER`, `UPDATE_TIMESTAMP`, `RIGHTS_TYPE_CODE`) 
VALUES ((SELECT T.ID FROM (SELECT MAX(RIGHT_ID) + 1 AS ID FROM RIGHTS) AS T), 'VIEW_CONSULTING_DISCLOSURE', 'To view consulting disclosure', 'quickstart', now(), '2');

INSERT INTO `consulting_discl_action_log_type` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('1', 'Consulting disclosure <b>created</b> by <b>{REPORTER}</b>', 'Created', now(), 'quickstart');
INSERT INTO `consulting_discl_action_log_type` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('2', 'Consulting disclosure <b>submitted</b> by <b>{REPORTER}</b>', 'Submitted', now(), 'quickstart');
INSERT INTO `consulting_discl_action_log_type` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('3', 'Consulting disclosure <b>recalled</b> by <b>{REPORTER}</b>', 'Withdrawn', now(), 'quickstart');
INSERT INTO `consulting_discl_action_log_type` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('4', 'Consulting disclosure <b>returned</b> by <b>{ADMIN_NAME}</b>', 'Returned', now(), 'quickstart');
INSERT INTO `consulting_discl_action_log_type` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('5', 'Consulting disclosure <b>approved</b>', 'Approved', now(), 'quickstart');
INSERT INTO `consulting_discl_action_log_type` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('6', 'Primary Administrator <b>{ASSIGNED_ADMIN}</b> <b>assigned</b> by <b>{ADMIN_NAME}</b>', 'Assigned to Admin /Admin Group', now(), 'quickstart');
INSERT INTO `consulting_discl_action_log_type` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('7', 'Primary Administrator <b>{ASSIGNED_ADMIN}</b> <b>reassigned</b> to <b>{REASSIGNED_ADMIN}</b> by <b>{ADMIN_NAME}</b>', 'Re Assigned to Admin /Admin Group', now(), 'quickstart');

INSERT INTO `consulting_discl_review_status_type` (`REVIEW_STATUS_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `SORT_ORDER`) VALUES ('1', 'Pending', 'Y', now(), 'quickstart', '1');
INSERT INTO `consulting_discl_review_status_type` (`REVIEW_STATUS_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `SORT_ORDER`) VALUES ('2', 'Submitted', 'Y', now(), 'quickstart', '2');
INSERT INTO `consulting_discl_review_status_type` (`REVIEW_STATUS_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `SORT_ORDER`) VALUES ('3', 'Review in progress', 'Y', now(), 'quickstart', '3');
INSERT INTO `consulting_discl_review_status_type` (`REVIEW_STATUS_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `SORT_ORDER`) VALUES ('4', 'Review Assigned', 'Y', now(), 'quickstart', '4');
INSERT INTO `consulting_discl_review_status_type` (`REVIEW_STATUS_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `SORT_ORDER`) VALUES ('5', 'Assigned Review Completed', 'Y', now(), 'quickstart', '5');
INSERT INTO `consulting_discl_review_status_type` (`REVIEW_STATUS_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `SORT_ORDER`) VALUES ('6', 'Completed', 'Y', now(), 'quickstart', '6');
INSERT INTO `consulting_discl_review_status_type` (`REVIEW_STATUS_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `SORT_ORDER`) VALUES ('7', 'Withdrawn', 'Y', now(), 'quickstart', '7');
INSERT INTO `consulting_discl_review_status_type` (`REVIEW_STATUS_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `SORT_ORDER`) VALUES ('8', 'Returned', 'Y', now(), 'quickstart', '8');

INSERT INTO `consulting_discl_disposition_status_type` (`DISPOSITION_STATUS_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `SORT_ORDER`) VALUES ('1', 'Pending', 'Y', now(), 'quickstart', '1');
INSERT INTO `consulting_discl_disposition_status_type` (`DISPOSITION_STATUS_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `SORT_ORDER`) VALUES ('2', 'Approved', 'Y', now(), 'quickstart', '2');
INSERT INTO `consulting_discl_disposition_status_type` (`DISPOSITION_STATUS_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `SORT_ORDER`) VALUES ('3', 'Void', 'Y', now(), 'quickstart', '3');
