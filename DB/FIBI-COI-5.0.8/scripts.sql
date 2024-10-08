

DELETE FROM `notify_placeholder_columns` WHERE (`NOTIFY_PLACEHOLDER_HEADER_ID` = '4');
DELETE FROM `notify_placeholder_header` WHERE (`NOTIFY_PLACEHOLDER_HEADER_ID` = '4');

UPDATE `notification_type` SET `MESSAGE` = '<p>Dear {COI_DISCLOSURE#REPORTER_NAME},</p> <p>We kindly request you to complete and submit your disclosure for the following:</p> <p><strong>Project:</strong> {COI_DISCLOSURE#PROJECT_NUMBER} - {COI_DISCLOSURE#PROJECT_TITLE}</p> <p><strong>Disclosure Status:</strong> {COI_DISCLOSURE#REVIEW_STATUS}</p> <p><br /><span >Please login to COI and access your disclosure at {APPLICATION_URL} to complete and submit your disclosure.</span></p> <p>&nbsp;</p>', `SHOW_TEMPLATE_IN_MODULE` = 'Y' WHERE (`NOTIFICATION_TYPE_ID` = '8014');

UPDATE DISCLOSURE_ACTION_TYPE SET MESSAGE = 'Revised Disclosure has been <b>amended</b> by <b>{REPORTER}</b>' WHERE (ACTION_TYPE_CODE = '28');

