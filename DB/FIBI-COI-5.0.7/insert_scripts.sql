
INSERT INTO entity_document_status_type (DOCUMENT_STATUS_TYPE_CODE, DESCRIPTION, IS_ACTIVE, UPDATE_TIMESTAMP, UPDATED_BY) VALUES ('1', 'Active', 'Y', now(), '10000000001');
INSERT INTO entity_document_status_type (DOCUMENT_STATUS_TYPE_CODE, DESCRIPTION, IS_ACTIVE, UPDATE_TIMESTAMP, UPDATED_BY) VALUES ('2', 'Inactive', 'Y', now(), '10000000001');
INSERT INTO entity_document_status_type (DOCUMENT_STATUS_TYPE_CODE, DESCRIPTION, IS_ACTIVE, UPDATE_TIMESTAMP, UPDATED_BY) VALUES ('3', 'Duplicate', 'Y', now(), '10000000001');

INSERT INTO entity_action_type (ACTION_TYPE_CODE, MESSAGE, DESCRIPTION, UPDATE_TIMESTAMP, UPDATE_USER) VALUES ('1', '{ENTITY_NAME} <b>created</b> by <b>{ADMIN_NAME}</b>', 'Creation', now(), '10000000001');
INSERT INTO entity_action_type (ACTION_TYPE_CODE, MESSAGE, DESCRIPTION, UPDATE_TIMESTAMP, UPDATE_USER) VALUES ('2', '{ENTITY_NAME} <b>matched</b> with DUNS <b>#{DUNS_NUMBER}</b> by <b>{ADMIN_NAME}</b>', 'DUNS Match', now(), '10000000001');
INSERT INTO entity_action_type (ACTION_TYPE_CODE, MESSAGE, DESCRIPTION, UPDATE_TIMESTAMP, UPDATE_USER) VALUES ('3', '{ENTITY_NAME} <b>resynced</b> with DUNS', 'Resync', now(), '10000000001');
INSERT INTO entity_action_type (ACTION_TYPE_CODE, MESSAGE, DESCRIPTION, UPDATE_TIMESTAMP, UPDATE_USER) VALUES ('4', '{ENTITY_NAME} <b>verified</b> by <b>{ADMIN_NAME}</b>', 'Verification', now(), '10000000001');
INSERT INTO entity_action_type (ACTION_TYPE_CODE, MESSAGE, DESCRIPTION, UPDATE_TIMESTAMP, UPDATE_USER) VALUES ('5', '{ENTITY_NAME} marked as <b>active</b> by <b>{ADMIN_NAME}</b>', 'Activation', now(), '10000000001');
INSERT INTO entity_action_type (ACTION_TYPE_CODE, MESSAGE, DESCRIPTION, UPDATE_TIMESTAMP, UPDATE_USER) VALUES ('6', '{ENTITY_NAME} marked as <b>inactive</b> by <b>{ADMIN_NAME}</b>', 'Inactivation', now(), '10000000001');
INSERT INTO entity_action_type (ACTION_TYPE_CODE, MESSAGE, DESCRIPTION, UPDATE_TIMESTAMP, UPDATE_USER) VALUES ('7', '{ENTITY_NAME} marked as <b>duplicate</b> by <b>{ADMIN_NAME}</b>', 'Duplicate', now(), '10000000001');
INSERT INTO entity_action_type (ACTION_TYPE_CODE, MESSAGE, DESCRIPTION, UPDATE_TIMESTAMP, UPDATE_USER) VALUES ('8', '{TAB_NAME} section of {ENTITY_NAME} <b>Modified</b> by <b>{ADMIN_NAME}</b>', 'Modification', now(), '10000000001');
INSERT INTO entity_action_type (ACTION_TYPE_CODE, MESSAGE, DESCRIPTION, UPDATE_TIMESTAMP, UPDATE_USER) VALUES ('9', '{ENTITY_NAME} <b>updated</b> based on DUNS', 'Auto Update', now(), '10000000001');
INSERT INTO entity_action_type (ACTION_TYPE_CODE, MESSAGE, DESCRIPTION, UPDATE_TIMESTAMP, UPDATE_USER) VALUES ('10', 'Sponsor feed status of {ENTITY_NAME} <b>updated</b> from <b>{OLD_STATUS}</b> to <b>{NEW_STATUS}</b>', 'Sponsor Feed', now(), '10000000001');
INSERT INTO entity_action_type (ACTION_TYPE_CODE, MESSAGE, DESCRIPTION, UPDATE_TIMESTAMP, UPDATE_USER) VALUES ('11', 'Organization feed status of {ENTITY_NAME} <b>updated</b> from <b>{OLD_STATUS}</b> to <b>{NEW_STATUS}</b>', 'Organization Feed', now(), '10000000001');