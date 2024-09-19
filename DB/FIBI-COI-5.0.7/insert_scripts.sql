
INSERT INTO entity_document_status_type (DOCUMENT_STATUS_TYPE_CODE, DESCRIPTION, IS_ACTIVE, UPDATE_TIMESTAMP, UPDATED_BY) VALUES ('1', 'Active', 'Y', now(), '10000000001');
INSERT INTO entity_document_status_type (DOCUMENT_STATUS_TYPE_CODE, DESCRIPTION, IS_ACTIVE, UPDATE_TIMESTAMP, UPDATED_BY) VALUES ('2', 'Inactive', 'Y', now(), '10000000001');
INSERT INTO entity_document_status_type (DOCUMENT_STATUS_TYPE_CODE, DESCRIPTION, IS_ACTIVE, UPDATE_TIMESTAMP, UPDATED_BY) VALUES ('3', 'Duplicate', 'Y', now(), '10000000001');

INSERT INTO entity_action_type (ACTION_TYPE_CODE, MESSAGE, DESCRIPTION, UPDATE_TIMESTAMP, UPDATE_USER) VALUES ('1', 'Entity <b>created</b> by <b>{ADMIN_NAME}</b>', 'Creation', now(), '10000000001');
INSERT INTO entity_action_type (ACTION_TYPE_CODE, MESSAGE, DESCRIPTION, UPDATE_TIMESTAMP, UPDATE_USER) VALUES ('2', 'Entity <b>matched</b> with DUNS <b>#{DUNS_NUMBER}</b> by <b>{ADMIN_NAME}</b>', 'DUNS Match', now(), '10000000001');
INSERT INTO entity_action_type (ACTION_TYPE_CODE, MESSAGE, DESCRIPTION, UPDATE_TIMESTAMP, UPDATE_USER) VALUES ('3', 'Entity <b>resynced</b> with DUNS', 'Resync', now(), '10000000001');
INSERT INTO entity_action_type (ACTION_TYPE_CODE, MESSAGE, DESCRIPTION, UPDATE_TIMESTAMP, UPDATE_USER) VALUES ('4', 'Entity <b>verified</b> by <b>{ADMIN_NAME}</b>', 'Verification', now(), '10000000001');
INSERT INTO entity_action_type (ACTION_TYPE_CODE, MESSAGE, DESCRIPTION, UPDATE_TIMESTAMP, UPDATE_USER) VALUES ('5', 'Entity marked as <b>active</b> by <b>{ADMIN_NAME}</b>', 'Activation', now(), '10000000001');
INSERT INTO entity_action_type (ACTION_TYPE_CODE, MESSAGE, DESCRIPTION, UPDATE_TIMESTAMP, UPDATE_USER) VALUES ('6', 'Entity marked as <b>inactive</b> by <b>{ADMIN_NAME}</b>', 'Inactivation', now(), '10000000001');
INSERT INTO entity_action_type (ACTION_TYPE_CODE, MESSAGE, DESCRIPTION, UPDATE_TIMESTAMP, UPDATE_USER) VALUES ('7', 'Entity marked as <b>duplicate</b> by <b>{ADMIN_NAME}</b>', 'Duplicate', now(), '10000000001');
INSERT INTO entity_action_type (ACTION_TYPE_CODE, MESSAGE, DESCRIPTION, UPDATE_TIMESTAMP, UPDATE_USER) VALUES ('8', '{TAB_NAME} section of Entity <b>Modified</b> by <b>{ADMIN_NAME}</b>', 'Modification', now(), '10000000001');
INSERT INTO entity_action_type (ACTION_TYPE_CODE, MESSAGE, DESCRIPTION, UPDATE_TIMESTAMP, UPDATE_USER) VALUES ('9', 'Entity <b>updated</b> based on DUNS', 'Auto Update', now(), '10000000001');
INSERT INTO entity_action_type (ACTION_TYPE_CODE, MESSAGE, DESCRIPTION, UPDATE_TIMESTAMP, UPDATE_USER) VALUES ('10', 'Sponsor feed status of Entity <b>updated</b> from <b>{OLD_STATUS}</b> to <b>{NEW_STATUS}</b>', 'Sponsor Feed', now(), '10000000001');
INSERT INTO entity_action_type (ACTION_TYPE_CODE, MESSAGE, DESCRIPTION, UPDATE_TIMESTAMP, UPDATE_USER) VALUES ('11', 'Organization feed status of Entity <b>updated</b> from <b>{OLD_STATUS}</b> to <b>{NEW_STATUS}</b>', 'Organization Feed', now(), '10000000001');

INSERT INTO ATTACHMENT_NUMBER_COUNTER (COUNTER_NAME, COUNTER_VALUE) VALUES("COI_COMMON_ATTACHMENT_COUNTER",1);

INSERT INTO dyn_subsection_config (SUB_SECTION_CODE, SECTION_CODE, DESCRIPTION, IS_ACTIVE, UPDATE_TIMESTAMP, UPDATE_USER) VALUES ('2623', 'GE2601', 'Entity Attachments', 'Y', now(), 'admin');
INSERT INTO dyn_element_config (UI_REFERENCE_ID, DESCRIPTION, SUB_SECTION_CODE, SECTION_CODE, UPDATE_USER, UPDATE_TIMESTAMP) VALUES ('coi-sub-head-2623', 'Entity Name', '2623', 'GE2601', 'admin', now());
INSERT INTO dyn_element_config (UI_REFERENCE_ID, DESCRIPTION, SUB_SECTION_CODE, SECTION_CODE, UPDATE_USER, UPDATE_TIMESTAMP) VALUES ('coi-sub-modal-head-2623', 'Entity Name', '2623', 'GE2601', 'admin', now());
INSERT INTO dyn_element_config (UI_REFERENCE_ID, DESCRIPTION, SUB_SECTION_CODE, SECTION_CODE, UPDATE_USER, UPDATE_TIMESTAMP) VALUES ('coi-attach-type-2623', 'Entity Name', '2623', 'GE2601', 'admin', now());
INSERT INTO dyn_element_config (UI_REFERENCE_ID, DESCRIPTION, SUB_SECTION_CODE, SECTION_CODE, UPDATE_USER, UPDATE_TIMESTAMP) VALUES ('coi-attach-filename-2623', 'Entity Name', '2623', 'GE2601', 'admin', now());
INSERT INTO dyn_element_config (UI_REFERENCE_ID, DESCRIPTION, SUB_SECTION_CODE, SECTION_CODE, UPDATE_USER, UPDATE_TIMESTAMP) VALUES ('coi-attach-desc-2623', 'Entity Name', '2623', 'GE2601', 'admin', now());
