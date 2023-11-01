INSERT INTO `discl_component_type` (`COMPONENT_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('1', 'Disclosure detail comment', 'Y', now(),'quickstart');
INSERT INTO `discl_comment_type` (`COMMENT_TYPE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('1', 'Disclosure detail comment', 'Y', now(),'quickstart');
ALTER TABLE `discl_comment` 
CHANGE COLUMN `COMMENT` `COMMENT` VARCHAR(2000) NULL DEFAULT NULL;

ALTER TABLE `coi_disclosure` ADD COLUMN `HOME_UNIT` VARCHAR(8) DEFAULT NULL;
ALTER TABLE `coi_disclosure` ADD CONSTRAINT `COI_DIS_HOME_UNIT_FK7` FOREIGN KEY (`HOME_UNIT`) REFERENCES `unit` (`UNIT_NUMBER`);

INSERT INTO `rights` (`RIGHT_ID`, `RIGHT_NAME`, `DESCRIPTION`, `UPDATE_USER`, `UPDATE_TIMESTAMP`, `RIGHTS_TYPE_CODE`) VALUES ((SELECT A.ID FROM (SELECT MAX(RIGHT_ID) + 1 AS ID FROM RIGHTS ) AS A), 'MANAGE_FCOI_DISCLOSURE', 'To manage all actions against an FCOI disclosure', 'quickstart', now(), '2');
INSERT INTO `rights` (`RIGHT_ID`, `RIGHT_NAME`, `DESCRIPTION`, `UPDATE_USER`, `UPDATE_TIMESTAMP`, `RIGHTS_TYPE_CODE`) VALUES ((SELECT A.ID FROM (SELECT MAX(RIGHT_ID) + 1 AS ID FROM RIGHTS ) AS A), 'VIEW_FCOI_DISCLOSURE', 'To view any FCOI disclosure in a unit', 'quickstart', now(), '2');
INSERT INTO `rights` (`RIGHT_ID`, `RIGHT_NAME`, `DESCRIPTION`, `UPDATE_USER`, `UPDATE_TIMESTAMP`, `RIGHTS_TYPE_CODE`) VALUES ((SELECT A.ID FROM (SELECT MAX(RIGHT_ID) + 1 AS ID FROM RIGHTS ) AS A), 'VIEW_FCOI_DISCLOSURE_PRIVATE_COMMENTS', 'To view private comments tagged in the disclosure', 'quickstart', now(), '2');
INSERT INTO `rights` (`RIGHT_ID`, `RIGHT_NAME`, `DESCRIPTION`, `UPDATE_USER`, `UPDATE_TIMESTAMP`, `RIGHTS_TYPE_CODE`) VALUES ((SELECT A.ID FROM (SELECT MAX(RIGHT_ID) + 1 AS ID FROM RIGHTS ) AS A), 'VIEW_FCOI_DISCLOSURE_PRIVATE_ATTACHMENTS', 'To view private attachments tagged in the disclosure', 'quickstart', now(), '2');
INSERT INTO `rights` (`RIGHT_ID`, `RIGHT_NAME`, `DESCRIPTION`, `UPDATE_USER`, `UPDATE_TIMESTAMP`, `RIGHTS_TYPE_CODE`) VALUES ((SELECT A.ID FROM (SELECT MAX(RIGHT_ID) + 1 AS ID FROM RIGHTS ) AS A), 'MANAGE_DISCLOSURE_REVIEW', 'To manage review in disclosure', 'quickstart', now(), '2');
INSERT INTO `rights` (`RIGHT_ID`, `RIGHT_NAME`, `DESCRIPTION`, `UPDATE_USER`, `UPDATE_TIMESTAMP`, `RIGHTS_TYPE_CODE`) VALUES ((SELECT A.ID FROM (SELECT MAX(RIGHT_ID) + 1 AS ID FROM RIGHTS ) AS A), 'VIEW_DISCLOSURE_REVIEW', 'To view review members and comments tagged in the disclosure', 'quickstart', now(), '2');
INSERT INTO `rights` (`RIGHT_ID`, `RIGHT_NAME`, `DESCRIPTION`, `UPDATE_USER`, `UPDATE_TIMESTAMP`, `RIGHTS_TYPE_CODE`) VALUES ((SELECT A.ID FROM (SELECT MAX(RIGHT_ID) + 1 AS ID FROM RIGHTS ) AS A), 'MANAGE_PROJECT_DISCLOSURE', 'To manage all actions against a project disclosure', 'quickstart', now(), '2');
INSERT INTO `rights` (`RIGHT_ID`, `RIGHT_NAME`, `DESCRIPTION`, `UPDATE_USER`, `UPDATE_TIMESTAMP`, `RIGHTS_TYPE_CODE`) VALUES ((SELECT A.ID FROM (SELECT MAX(RIGHT_ID) + 1 AS ID FROM RIGHTS ) AS A), 'VIEW_PROJECT_DISCLOSURE', 'To view any project disclosure in a unit', 'quickstart', now(), '2');
INSERT INTO `rights` (`RIGHT_ID`, `RIGHT_NAME`, `DESCRIPTION`, `UPDATE_USER`, `UPDATE_TIMESTAMP`, `RIGHTS_TYPE_CODE`) VALUES ((SELECT A.ID FROM (SELECT MAX(RIGHT_ID) + 1 AS ID FROM RIGHTS ) AS A), 'MANAGE_TRAVEL_DISCLOSURE', 'To manage all actions against a travel disclosure', 'quickstart', now(), '2');
INSERT INTO `rights` (`RIGHT_ID`, `RIGHT_NAME`, `DESCRIPTION`, `UPDATE_USER`, `UPDATE_TIMESTAMP`, `RIGHTS_TYPE_CODE`) VALUES ((SELECT A.ID FROM (SELECT MAX(RIGHT_ID) + 1 AS ID FROM RIGHTS ) AS A), 'VIEW_TRAVEL_DISCLOSURE', 'To view any travel disclosure in a unit', 'quickstart', now(), '2');
INSERT INTO `rights` (`RIGHT_ID`, `RIGHT_NAME`, `DESCRIPTION`, `UPDATE_USER`, `UPDATE_TIMESTAMP`, `RIGHTS_TYPE_CODE`) VALUES ((SELECT A.ID FROM (SELECT MAX(RIGHT_ID) + 1 AS ID FROM RIGHTS ) AS A), 'MANAGE_ENTITY', 'To manage all actions against a Entity', 'quickstart', now(), '2');
INSERT INTO `rights` (`RIGHT_ID`, `RIGHT_NAME`, `DESCRIPTION`, `UPDATE_USER`, `UPDATE_TIMESTAMP`, `RIGHTS_TYPE_CODE`) VALUES ((SELECT A.ID FROM (SELECT MAX(RIGHT_ID) + 1 AS ID FROM RIGHTS ) AS A), 'VIEW_ENTITY', 'To view any  Entities', 'quickstart', now(), '2');



ALTER TABLE `coi_disclosure` 
ADD COLUMN `ADMIN_GROUP_ID` INT(3) NULL,
ADD COLUMN `ADMIN_PERSON_ID` VARCHAR(45) NULL;

INSERT INTO `rights` (`RIGHT_ID`, `RIGHT_NAME`, `DESCRIPTION`, `UPDATE_USER`, `UPDATE_TIMESTAMP`, `RIGHTS_TYPE_CODE`) 
VALUES ('1356', 'VIEW_ADMIN_GROUP_COI', 'Allow to view coi based on admin group', 'quickstart', now(), '1');

INSERT INTO `role_rights` (`ROLE_RIGHTS_ID`, `RIGHT_ID`, `ROLE_ID`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) 
VALUES ('1911', '1356', '1335', now(), 'quickstart');

INSERT INTO `person_roles` (`PERSON_ROLES_ID`, `PERSON_ID`, `ROLE_ID`, `UNIT_NUMBER`, `DESCEND_FLAG`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) 
VALUES ('904', '10000000001', '1335', '000001', 'N', now(), 'quikstart');

ALTER TABLE `coi_travel_disclosure` ADD COLUMN `SUBMISSION_DATE` DATE;
ALTER TABLE `coi_travel_disclosure` ADD COLUMN `DESCRIPTION` varchar(2000);
ALTER TABLE `coi_travel_disclosure` ADD COLUMN `HOME_UNIT` varchar(2000);

ALTER TABLE `coi_travel_disclosure` CHANGE COLUMN `ACKNOWLEDGE_AT` `ACKNOWLEDGE_AT` DATE NULL DEFAULT NULL;
ALTER TABLE `coi_travel_disclosure` ADD COLUMN `REVIEW_STATUS` varchar(2000);
ALTER TABLE `coi_travel_disclosure` ADD COLUMN `EXPIRATION_DATE` DATE;
ALTER TABLE `coi_travel_disclosure` ADD COLUMN `TRAVEL_DISCLOSURE_STATUS` varchar(2000);
ALTER TABLE `coi_travel_disclosure` ADD COLUMN `DISPOSITION_STATUS` varchar(2000);

CREATE TABLE `coi_travel_disclosure_status_type` (
		TRAVEL_DISCLOSURE_STATUS_CODE varchar(3) primary key not null,
		DESCRIPTION varchar(200),
		IS_ACTIVE varchar(1),
		UPDATE_TIMESTAMP datetime,
		UPDATE_USER varchar(50)
);

CREATE TABLE `coi_travel_review_status_type`(
		REVIEW_STATUS_CODE VARCHAR(3) not null,
		DESCRIPTION VARCHAR(200),
		IS_ACTIVE VARCHAR(1),
		UPDATE_TIMESTAMP DATETIME,
		UPDATE_USER VARCHAR(60)
);

CREATE TABLE `coi_travel_document_status_type`(
		DOCUMENT_STATUS_CODE VARCHAR(3) not null,
		DESCRIPTION VARCHAR(200),
		IS_ACTIVE VARCHAR(1),
		UPDATE_TIMESTAMP DATETIME,
		UPDATE_USER VARCHAR(60)
);

INSERT INTO `coi_travel_disclosure_status_type` VALUES('1', 'Draft', 'Y', now(), 'admin');
INSERT INTO `coi_travel_disclosure_status_type` VALUES('2', 'Acknowledged', 'Y', now(), 'admin');
INSERT INTO `coi_travel_disclosure_status_type` VALUES('3', 'Risk', 'Y', now(), 'admin');

ALTER TABLE `coi_travel_disclosure` RENAME COLUMN VERSION_STATUS TO VERSION_STATUS_CODE;
ALTER TABLE `coi_travel_disclosure` RENAME COLUMN REVIEW_STATUS TO REVIEW_STATUS_CODE;
ALTER TABLE `coi_travel_disclosure` RENAME COLUMN DISPOSITION_STATUS TO DISPOSITION_STATUS_CODE;
ALTER TABLE `coi_travel_disclosure` RENAME COLUMN TRAVEL_DISCLOSURE_STATUS TO TRAVEL_DISCLOSURE_STATUS_CODE;
ALTER TABLE `coi_travel_disclosure` RENAME COLUMN VERSION_STATUS_CODE TO VERSION_STATUS;

ALTER TABLE `coi_travel_disclosure` ADD COLUMN `ADMIN_GROUP_ID` INT(3) NULL;
ALTER TABLE `coi_travel_disclosure` ADD COLUMN `ADMIN_PERSON_ID` VARCHAR(45) NULL;

ALTER TABLE `coi_travel_disclosure` MODIFY COLUMN `PURPOSE_OF_THE_TRIP` varchar(500);
ALTER TABLE `coi_travel_disclosure` MODIFY COLUMN `RELATIONSHIP_TO_YOUR_RESEARCH` varchar(500);
ALTER TABLE `coi_travel_disclosure` ADD COLUMN `CERTIFIED_BY` VARCHAR(45) NULL;
ALTER TABLE `coi_travel_disclosure` ADD COLUMN `CERTIFIED_AT` datetime NULL;
ALTER TABLE `coi_travel_disclosure` ADD COLUMN `DOCUMENT_STATUS_CODE` varchar(3);
ALTER TABLE `coi_travel_disclosure` MODIFY COLUMN `REVIEW_STATUS_CODE` varchar(3);
ALTER TABLE `coi_travel_disclosure` MODIFY COLUMN `TRAVEL_DISCLOSURE_STATUS_CODE` varchar(3);
ALTER TABLE `coi_travel_disclosure` MODIFY COLUMN ACKNOWLEDGE_AT TIMESTAMP;

INSERT INTO `coi_travel_disclosure_status_type` VALUES ('4', 'Conflict Managed', 'Y', now(), 'admin');
INSERT INTO `coi_travel_review_status_type` VALUES ('1', 'Pending', 'Y', now(), 'admin');
INSERT INTO `coi_travel_review_status_type` VALUES ('2', 'Submitted', 'Y', now(), 'admin');
INSERT INTO `coi_travel_review_status_type` VALUES ('3', 'Review In progress', 'Y', now(), 'admin');
INSERT INTO `coi_travel_review_status_type` VALUES ('4', 'Returned to PI', 'Y', now(), 'admin');
INSERT INTO `coi_travel_review_status_type` VALUES ('5', 'Withdrawn', 'Y', now(), 'admin');
INSERT INTO `coi_travel_review_status_type` VALUES ('6', 'Completed', 'Y', now(), 'admin');
INSERT INTO `coi_travel_review_status_type` VALUES ('7', 'Approved/Acknowledge', 'Y', now(), 'admin');
INSERT INTO `coi_travel_document_status_type` VALUES ('1', 'Draft', 'Y', now(), 'admin');
INSERT INTO `coi_travel_document_status_type` VALUES ('2', 'Approved [Acknowkledged]', 'Y', now(), 'admin');
	
UPDATE coi_travel_disclosure SET VERSION_STATUS = 'PENDING' WHERE VERSION_STATUS = 'DRAFT';
UPDATE `coi_travel_disclosure_status_type` SET DESCRIPTION = 'No Conflict' WHERE TRAVEL_DISCLOSURE_STATUS_CODE = '1';
UPDATE `coi_travel_disclosure_status_type` SET DESCRIPTION = 'Potential Conflict' WHERE TRAVEL_DISCLOSURE_STATUS_CODE = '2';
UPDATE `coi_travel_disclosure_status_type` SET DESCRIPTION = 'Conflict Identified' WHERE TRAVEL_DISCLOSURE_STATUS_CODE = '3';

RENAME TABLE `COI_TRAVEL_DISCLOSURE_STATUS_TYPE` TO `COI_TRAVEL_DISCLOSURE_STATUS`;
RENAME TABLE `COI_TRAVEL_STATUS_TYPE` TO `COI_TRAVEL_STATUS`;
RENAME TABLE `COI_TRAVELER_TYPE` TO `COI_TRAVELER`;
RENAME TABLE `COI_TRAVEL_REVIEW_STATUS_TYPE` TO `COI_TRAVEL_REVIEW_STATUS`;
RENAME TABLE `COI_TRAVEL_DOCUMENT_STATUS_TYPE` TO `COI_TRAVEL_DOCUMENT_STATUS`;

UPDATE `COI_TRAVEL_REVIEW_STATUS` SET DESCRIPTION='Approved' WHERE REVIEW_STATUS_CODE = '7';
UPDATE `COI_TRAVEL_DOCUMENT_STATUS` SET DESCRIPTION='Approved' WHERE DOCUMENT_STATUS_CODE = '2';

DROP TABLE IF EXISTS `coi_conflict_history`;
CREATE TABLE `coi_conflict_history` (
  `COI_CONFLICT_HISTORY_ID` int NOT NULL AUTO_INCREMENT,
  `DISCLOSURE_DETAILS_ID` int NOT NULL,
  `DISCLOSURE_ID` int NOT NULL,
  `COMMENT` varchar(4000) DEFAULT NULL,
  `CONFLICT_STATUS_CODE` varchar(3) DEFAULT NULL,
  `UPDATE_TIMESTAMP` datetime DEFAULT NULL,
  `UPDATE_USER` varchar(60) DEFAULT NULL,
  PRIMARY KEY (`COI_CONFLICT_HISTORY_ID`)
);

ALTER TABLE `discl_comment` 
CHANGE COLUMN `COMMENT` `COMMENT` VARCHAR(4000) NULL DEFAULT NULL ;

ALTER TABLE `entity` 
ADD COLUMN `REVISION_REASON` VARCHAR(100) NULL;


ALTER TABLE `person_entity` 
ADD COLUMN `PERSON_ENTITY_NUMBER` INT NULL;


ALTER TABLE `person_entity` 
ADD COLUMN `REVISION_REASON` VARCHAR(100) NULL;


CREATE TABLE `attachment_number_counter` (
  `counter_name` varchar(255) NOT NULL,
  `counter_value` int DEFAULT NULL,
  PRIMARY KEY (`counter_name`)
);

ALTER TABLE `discl_attachment` 
DROP FOREIGN KEY `DISCL_ATTACHMENT_FK5`;
ALTER TABLE `discl_attachment` 
DROP INDEX `DISCL_ATTACHMENT_FK5_idx` ;

DELETE FROM `entity_status` WHERE (`ENTITY_STATUS_CODE` = '3');
UPDATE `entity_status` SET `DESCRIPTION` = 'Verified' WHERE (`ENTITY_STATUS_CODE` = '1');
UPDATE `entity_status` SET `DESCRIPTION` = 'Unverified' WHERE (`ENTITY_STATUS_CODE` = '2');
UPDATE `COI_TRAVEL_REVIEW_STATUS` SET DESCRIPTION = 'Returned' WHERE REVIEW_STATUS_CODE='4';

INSERT INTO `entity_relationship_type` (`ENTITY_REL_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('1', 'New', 'Y', now(), 'quickstart');
INSERT INTO `entity_relationship_type` (`ENTITY_REL_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('2', 'Duplicate', 'Y', now(), 'quickstart');
INSERT INTO `entity_relationship_type` (`ENTITY_REL_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('3', 'Parent', 'Y', now(), 'quickstart');
INSERT INTO `entity_relationship_type` (`ENTITY_REL_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('4', 'Subsidiary', 'Y', now(), 'quickstart');

SET FOREIGN_KEY_CHECKS=0;

CREATE TABLE `coi_quest_answer` (
  `QUESTIONNAIRE_ANSWER_ID` int NOT NULL AUTO_INCREMENT,
  `QUESTIONNAIRE_ANS_HEADER_ID` int DEFAULT NULL,
  `QUESTION_ID` int DEFAULT NULL,
  `OPTION_NUMBER` int DEFAULT NULL,
  `ANSWER_NUMBER` int DEFAULT NULL,
  `ANSWER` varchar(4000) DEFAULT NULL,
  `ANSWER_LOOKUP_CODE` varchar(100) DEFAULT NULL,
  `EXPLANATION` varchar(4000) DEFAULT NULL,
  `UPDATE_TIMESTAMP` datetime DEFAULT NULL,
  `UPDATE_USER` varchar(60) DEFAULT NULL,
  PRIMARY KEY (`QUESTIONNAIRE_ANSWER_ID`),
  KEY `COI_QUEST_ANSWER_FK1` (`QUESTIONNAIRE_ANS_HEADER_ID`),
  KEY `COI_QUEST_ANSWER_FK2` (`QUESTION_ID`),
  CONSTRAINT `COI_QUEST_ANSWER_FK1` FOREIGN KEY (`QUESTIONNAIRE_ANS_HEADER_ID`) REFERENCES `quest_answer_header` (`QUESTIONNAIRE_ANS_HEADER_ID`),
  CONSTRAINT `COI_QUEST_ANSWER_FK2` FOREIGN KEY (`QUESTION_ID`) REFERENCES `quest_question` (`QUESTION_ID`)
);

CREATE TABLE `coi_quest_answer_attachment` (
  `QUESTIONNAIRE_ANSWER_ATT_ID` int NOT NULL AUTO_INCREMENT,
  `QUESTIONNAIRE_ANSWER_ID` int DEFAULT NULL,
  `ATTACHMENT` longblob,
  `FILE_NAME` varchar(300) DEFAULT NULL,
  `CONTENT_TYPE` varchar(100) DEFAULT NULL,
  `UPDATE_TIMESTAMP` datetime DEFAULT NULL,
  `UPDATE_USER` varchar(60) DEFAULT NULL,
  PRIMARY KEY (`QUESTIONNAIRE_ANSWER_ATT_ID`),
  KEY `COI_QUEST_ANSWER_ATTACHMENT_FK1` (`QUESTIONNAIRE_ANSWER_ID`),
  CONSTRAINT `COI_QUEST_ANSWER_ATTACHMENT_FK1` FOREIGN KEY (`QUESTIONNAIRE_ANSWER_ID`) REFERENCES `coi_quest_answer` (`QUESTIONNAIRE_ANSWER_ID`)
);

CREATE TABLE `coi_quest_table_answer` (
  `QUEST_TABLE_ANSWER_ID` int NOT NULL AUTO_INCREMENT,
  `QUESTIONNAIRE_ANS_HEADER_ID` int DEFAULT NULL,
  `QUESTION_ID` int DEFAULT NULL,
  `ORDER_NUMBER` int DEFAULT NULL,
  `COLUMN_1` varchar(250) DEFAULT NULL,
  `COLUMN_2` varchar(250) DEFAULT NULL,
  `COLUMN_3` varchar(250) DEFAULT NULL,
  `COLUMN_4` varchar(250) DEFAULT NULL,
  `COLUMN_5` varchar(250) DEFAULT NULL,
  `COLUMN_6` varchar(250) DEFAULT NULL,
  `COLUMN_7` varchar(250) DEFAULT NULL,
  `COLUMN_8` varchar(250) DEFAULT NULL,
  `COLUMN_9` varchar(250) DEFAULT NULL,
  `COLUMN_10` varchar(250) DEFAULT NULL,
  `UPDATE_TIMESTAMP` datetime DEFAULT NULL,
  `UPDATE_USER` varchar(60) DEFAULT NULL,
  PRIMARY KEY (`QUEST_TABLE_ANSWER_ID`),
  KEY `COI_QUESTIONNAIRE_ANS_HEADER_ID_FK1` (`QUESTIONNAIRE_ANS_HEADER_ID`),
  CONSTRAINT `COI_QUESTIONNAIRE_ANS_HEADER_ID_FK1` FOREIGN KEY (`QUESTIONNAIRE_ANS_HEADER_ID`) REFERENCES `quest_answer_header` (`QUESTIONNAIRE_ANS_HEADER_ID`)
);

DROP TABLE IF EXISTS disclosure_action_type;
CREATE TABLE `disclosure_action_type` (
  `ACTION_TYPE_CODE` varchar(3) NOT NULL,
  `MESSAGE` varchar(200) DEFAULT NULL,
  `DESCRIPTION` varchar(200) DEFAULT NULL,
  `UPDATE_TIMESTAMP` datetime DEFAULT NULL,
  `UPDATE_USER` varchar(60) DEFAULT NULL,
  PRIMARY KEY (`ACTION_TYPE_CODE`)
);

DROP TABLE IF EXISTS entity_action_type;
CREATE TABLE `entity_action_type` (
  `ACTION_TYPE_CODE` varchar(3) NOT NULL,
  `MESSAGE` varchar(200) DEFAULT NULL,
  `DESCRIPTION` varchar(200) DEFAULT NULL,
  `UPDATE_TIMESTAMP` datetime DEFAULT NULL,
  `UPDATE_USER` varchar(60) DEFAULT NULL,
  PRIMARY KEY (`ACTION_TYPE_CODE`)
);

DROP TABLE IF EXISTS disclosure_action_log;
CREATE TABLE `disclosure_action_log` (
  `ACTION_LOG_ID` int NOT NULL AUTO_INCREMENT,
  `DISCLOSURE_ID` int DEFAULT NULL,
  `DISCLOSURE_NUMBER` int DEFAULT NULL,
  `ACTION_TYPE_CODE` varchar(3) DEFAULT NULL,
  `DESCRIPTION` varchar(200) DEFAULT NULL,
  `COMMENT` varchar(4000) DEFAULT NULL,
  `UPDATE_TIMESTAMP` datetime DEFAULT NULL,
  `UPDATE_USER` varchar(60) DEFAULT NULL,
  PRIMARY KEY (`ACTION_LOG_ID`),
  KEY `DISCLOSURE_ACTION_LOG_FK1` (`ACTION_TYPE_CODE`),
  KEY `DISCLOSURE_ACTION_LOG_FK2` (`DISCLOSURE_ID`),
  CONSTRAINT `DISCLOSURE_ACTION_LOG_FK1` FOREIGN KEY (`ACTION_TYPE_CODE`) REFERENCES `disclosure_action_type` (`ACTION_TYPE_CODE`),
  CONSTRAINT `DISCLOSURE_ACTION_LOG_FK2` FOREIGN KEY (`DISCLOSURE_ID`) REFERENCES `coi_disclosure` (`DISCLOSURE_ID`)
);

DROP TABLE IF EXISTS entity_action_log;
CREATE TABLE `entity_action_log` (
  `ACTION_LOG_ID` int NOT NULL AUTO_INCREMENT,
  `ENTITY_ID` int DEFAULT NULL,
  `ENTITY_NUMBER` int DEFAULT NULL,
  `ACTION_TYPE_CODE` varchar(3) DEFAULT NULL,
  `DESCRIPTION` varchar(200) DEFAULT NULL,
  `COMMENT` varchar(4000) DEFAULT NULL,
  `UPDATE_TIMESTAMP` datetime DEFAULT NULL,
  `UPDATE_USER` varchar(60) DEFAULT NULL,
  PRIMARY KEY (`ACTION_LOG_ID`),
  KEY `ENTITY_ACTION_LOG_FK1` (`ACTION_TYPE_CODE`),
  KEY `ENTITY_ACTION_LOG_FK2` (`ENTITY_ID`),
  CONSTRAINT `ENTITY_ACTION_LOG_FK1` FOREIGN KEY (`ACTION_TYPE_CODE`) REFERENCES `entity_action_type` (`ACTION_TYPE_CODE`),
  CONSTRAINT `ENTITY_ACTION_LOG_FK2` FOREIGN KEY (`ENTITY_ID`) REFERENCES `entity` (`ENTITY_ID`)
);

DROP TABLE IF EXISTS travel_disclosure_action_log;
CREATE TABLE `travel_disclosure_action_log` (
  `ACTION_LOG_ID` int NOT NULL AUTO_INCREMENT,
  `TRAVEL_DISCLOSURE_ID` int DEFAULT NULL,
  `TRAVEL_NUMBER` int DEFAULT NULL,
  `ACTION_TYPE_CODE` varchar(3) DEFAULT NULL,
  `DESCRIPTION` varchar(200) DEFAULT NULL,
  `COMMENT` varchar(4000) DEFAULT NULL,
  `UPDATE_TIMESTAMP` datetime DEFAULT NULL,
  `UPDATE_USER` varchar(60) DEFAULT NULL,
  PRIMARY KEY (`ACTION_LOG_ID`),
  KEY `TRAVEL_DISCLOSURE_ACTION_LOG_FK1` (`ACTION_TYPE_CODE`),
  KEY `TRAVEL_DISCLOSURE_ACTION_LOG_FK2` (`TRAVEL_DISCLOSURE_ID`),
  CONSTRAINT `TRAVEL_DISCLOSURE_ACTION_LOG_FK1` FOREIGN KEY (`ACTION_TYPE_CODE`) REFERENCES `disclosure_action_type` (`ACTION_TYPE_CODE`),
  CONSTRAINT `TRAVEL_DISCLOSURE_ACTION_LOG_FK2` FOREIGN KEY (`TRAVEL_DISCLOSURE_ID`) REFERENCES `coi_travel_disclosure` (`TRAVEL_DISCLOSURE_ID`)
);

CREATE TABLE `coi_travel_conflict_history` (
  `COI_TRAVEL_CONFLICT_HISTORY_ID` int NOT NULL AUTO_INCREMENT,
  `TRAVEL_DISCLOSURE_ID` int NOT NULL,
  `COMMENT` varchar(4000) DEFAULT NULL,
  `CONFLICT_STATUS_CODE` varchar(3) DEFAULT NULL,
  `UPDATE_TIMESTAMP` datetime DEFAULT NULL,
  `UPDATE_USER` varchar(60) DEFAULT NULL,
  PRIMARY KEY (`COI_TRAVEL_CONFLICT_HISTORY_ID`),
  KEY `COI_TRAVEL_CONFLICT_HISTORY_FK1_idx` (`CONFLICT_STATUS_CODE`)
);

DROP TABLE IF EXISTS `coi_reviewer_status_type`;
CREATE TABLE `coi_reviewer_status_type` (
  `REVIEW_STATUS_CODE` varchar(3) NOT NULL,
  `DESCRIPTION` varchar(200) DEFAULT NULL,
  `IS_ACTIVE` varchar(1) DEFAULT 'Y',
  `UPDATE_TIMESTAMP` datetime DEFAULT NULL,
  `UPDATE_USER` varchar(60) DEFAULT NULL,
  PRIMARY KEY (`REVIEW_STATUS_CODE`)
);

DROP TABLE IF EXISTS `COI_REVIEW_LOCATION_TYPE`;
CREATE TABLE `COI_REVIEW_LOCATION_TYPE` (
  `LOCATION_TYPE_CODE` varchar(3) NOT NULL,
  `DESCRIPTION` varchar(200) DEFAULT NULL,
  `IS_ACTIVE` varchar(1) DEFAULT 'Y',
  `UPDATE_TIMESTAMP` datetime DEFAULT NULL,
  `UPDATE_USER` varchar(60) DEFAULT NULL,
  PRIMARY KEY (`LOCATION_TYPE_CODE`)
);

DROP TABLE IF EXISTS `OPA_DISCLOSURE_STATUS_TYPE`;
CREATE TABLE `OPA_DISCLOSURE_STATUS_TYPE` (
  `OPA_DISCLOSURE_STATUS_CODE` varchar(3) NOT NULL,
  `DESCRIPTION` varchar(200) DEFAULT NULL,
  `IS_ACTIVE` varchar(1) DEFAULT 'Y',
  `UPDATE_TIMESTAMP` datetime DEFAULT NULL,
  `UPDATE_USER` varchar(60) DEFAULT NULL,
  PRIMARY KEY (`OPA_DISCLOSURE_STATUS_CODE`)
);

DROP TABLE IF EXISTS `opa_disposition_status_type`;
CREATE TABLE `opa_disposition_status_type` (
  `DISPOSITION_STATUS_CODE` varchar(3) NOT NULL,
  `DESCRIPTION` varchar(200) DEFAULT NULL,
  `IS_ACTIVE` varchar(1) DEFAULT 'Y',
  `UPDATE_TIMESTAMP` datetime DEFAULT NULL,
  `UPDATE_USER` varchar(60) DEFAULT NULL,
  PRIMARY KEY (`DISPOSITION_STATUS_CODE`)
);


DROP TABLE IF EXISTS OPA_ACTION_LOG_TYPE;
CREATE TABLE `OPA_ACTION_LOG_TYPE` (
  `ACTION_TYPE_CODE` varchar(3) NOT NULL,
  `MESSAGE` varchar(200) DEFAULT NULL,
  `DESCRIPTION` varchar(200) DEFAULT NULL,
  `UPDATE_TIMESTAMP` datetime DEFAULT NULL,
  `UPDATE_USER` varchar(60) DEFAULT NULL,
  PRIMARY KEY (`ACTION_TYPE_CODE`)
);

DROP TABLE IF EXISTS OPA_ACTION_LOG;
CREATE TABLE `OPA_ACTION_LOG` (
  `ACTION_LOG_ID` int NOT NULL AUTO_INCREMENT,
  `OPA_DISCLOSURE_ID` int DEFAULT NULL,
  `OPA_DISCLOSURE_NUMBER` varchar(20) DEFAULT NULL,
  `ACTION_TYPE_CODE` varchar(3) DEFAULT NULL,
  `DESCRIPTION` varchar(200) DEFAULT NULL,
  `COMMENT` varchar(4000) DEFAULT NULL,
  `UPDATE_TIMESTAMP` datetime DEFAULT NULL,
  `UPDATE_USER` varchar(60) DEFAULT NULL,
  PRIMARY KEY (`ACTION_LOG_ID`),
  KEY `OPA_ACTION_LOG_FK2` (`ACTION_TYPE_CODE`),
  KEY `OPA_ACTION_LOG_FK1` (`OPA_DISCLOSURE_ID`),
  CONSTRAINT `OPA_ACTION_LOG_FK2` FOREIGN KEY (`ACTION_TYPE_CODE`) REFERENCES `OPA_ACTION_LOG_TYPE` (`ACTION_TYPE_CODE`),
  CONSTRAINT `OPA_ACTION_LOG_FK1` FOREIGN KEY (`OPA_DISCLOSURE_ID`) REFERENCES `OPA_DISCLOSURE` (`OPA_DISCLOSURE_ID`)
);

DROP TABLE IF EXISTS person_entity_action_type;
CREATE TABLE `person_entity_action_type` (
  `ACTION_TYPE_CODE` varchar(3) NOT NULL,
  `MESSAGE` varchar(200) DEFAULT NULL,
  `DESCRIPTION` varchar(200) DEFAULT NULL,
  `UPDATE_TIMESTAMP` datetime DEFAULT NULL,
  `UPDATE_USER` varchar(60) DEFAULT NULL,
  PRIMARY KEY (`ACTION_TYPE_CODE`)
);

DROP TABLE IF EXISTS person_entity_action_log;
CREATE TABLE `person_entity_action_log` (
  `ACTION_LOG_ID` int NOT NULL AUTO_INCREMENT,
  `PERSON_ENTITY_ID` int DEFAULT NULL,
  `PERSON_ENTITY_NUMBER` int DEFAULT NULL,
  `ACTION_TYPE_CODE` varchar(3) DEFAULT NULL,
  `DESCRIPTION` varchar(200) DEFAULT NULL,
  `COMMENT` varchar(4000) DEFAULT NULL,
  `UPDATE_TIMESTAMP` datetime DEFAULT NULL,
  `UPDATE_USER` varchar(60) DEFAULT NULL,
  PRIMARY KEY (`ACTION_LOG_ID`),
  KEY `PERSON_ENTITY_AL_FK1` (`ACTION_TYPE_CODE`),
  KEY `PERSON_ENTITY_AL_FK2` (`PERSON_ENTITY_ID`),
  CONSTRAINT `PERSON_ENTITY_AL_FK1` FOREIGN KEY (`ACTION_TYPE_CODE`) REFERENCES `person_entity_action_type` (`ACTION_TYPE_CODE`),
  CONSTRAINT `PERSON_ENTITY_AL_FK2` FOREIGN KEY (`PERSON_ENTITY_ID`) REFERENCES `PERSON_ENTITY` (`PERSON_ENTITY_ID`)
);


INSERT INTO `discl_component_type` (`COMPONENT_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('2', 'Travel disclosure conflict comment', 'Y', now(), 'quickstart');
INSERT INTO `discl_comment_type` (`COMMENT_TYPE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('2', 'Travel disclosure conflict comment', 'Y', now(), 'quickstart');

ALTER TABLE `coi_project_award` 
ADD COLUMN `PROJECT_ROLE` VARCHAR(60) NULL AFTER `SPONSOR_AWARD_NUMBER`;

INSERT INTO `disclosure_action_type` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('1', 'New {FCOI /Project /Travel} application has been created', 'Created', now(), 'quickstart');
INSERT INTO `disclosure_action_type` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('2', '{FCOI /Project /Travel} application has been submitted', 'Submitted', now(), 'quickstart');
INSERT INTO `disclosure_action_type` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('3', '{FCOI /Project /Travel} application has been recalled', 'Withdraw', now(), 'quickstart');
INSERT INTO `disclosure_action_type` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('4', 'New Admin <b>{ADMIN_ONE}</b> has been assigned', 'Assigned to admin /Admin Group', now(), 'quickstart');
INSERT INTO `disclosure_action_type` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('5', 'Admin <b>{ADMIN_ONE}</b> reassigned to <b>{ADMIN_TWO}</b>', 'Re Assigned to admin /Admin Group', now(), 'quickstart');
INSERT INTO `disclosure_action_type` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('6', '{FCOI /Project /Travel} application has been returned', 'Returned', now(), 'quickstart');
INSERT INTO `disclosure_action_type` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('7', '<b>{Reviewer Name}</b> assigned to review {FCOI /Project /Travel} application', 'Assigned For Review', now(), 'quickstart');
INSERT INTO `disclosure_action_type` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('8', '{FCOI /Project /Travel} Disclosure review has been completed', 'Assigned Review Completed', now(), 'quickstart');
INSERT INTO `disclosure_action_type` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('9', 'Risk Level <b>{LOW}</b> has been added', 'Added Risk', now(), 'quickstart');
INSERT INTO `disclosure_action_type` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('10', 'Risk for  {FCOI /Project /Travel} Disclosure changed from <b>{LOW}</b> to <b>{HIGH}</b>', 'Manage Risk', now(), 'quickstart');
INSERT INTO `disclosure_action_type` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('11', '{FCOI /Project /Travel}  Disclosure admin-review has been completed', 'Admin  Review Completed', now(), 'quickstart');
INSERT INTO `disclosure_action_type` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('12', '{FCOI /Project /Travel} Disclosure has been expired', 'Expired', now(), 'quickstart');
INSERT INTO `disclosure_action_type` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('13', '{FCOI /Project /Travel} application has been approved', 'Approved', now(), 'quickstart');

INSERT INTO `ENTITY_ACTION_TYPE` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) 
VALUES ('1', 'New Entity <b>{ENTITY_NAME}</b>  has been <b>created</b>', 'Create', now(), 'quickstart');

INSERT INTO `ENTITY_ACTION_TYPE` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) 
VALUES ('2', '<b>{ENTITY_NAME}</b> has been <b>activated</b>', 'Activate', now(), 'quickstart');

INSERT INTO `ENTITY_ACTION_TYPE` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) 
VALUES ('3', '<b>{ENTITY_NAME}</b> has been <b>inactivated</b>', 'Inactivate', now(), 'quickstart');

INSERT INTO `ENTITY_ACTION_TYPE` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) 
VALUES ('4', '<b>{ENTITY_NAME}</b> has been <b>verified</b>', 'Inactivate', now(), 'quickstart');

INSERT INTO `ENTITY_ACTION_TYPE` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) 
VALUES ('5', 'Admin <b>{ADMIN_NAME}</b> changed the risk status from <b>{RISK}</b> to <b>{NEW_RISK}</b>', 'Modify Risk', now(), 'quickstart');

INSERT INTO `ENTITY_ACTION_TYPE` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) 
VALUES ('6', '<b>{ENTITY_NAME}</b> has been <b>modified</b>', 'Modify Entity', now(), 'quickstart');

ALTER TABLE `quest_question` ADD COLUMN `FOOTER_DESCRIPTION` TEXT NULL;

SET FOREIGN_KEY_CHECKS=1;

ALTER TABLE `coi_project_proposal` 
ADD COLUMN `KEY_PERSON_ID` VARCHAR(45) DEFAULT NULL,
ADD COLUMN `KEY_PERSON_NAME` VARCHAR(60) DEFAULT NULL,
ADD COLUMN `KEY_PERSON_ROLE_CODE` INT DEFAULT NULL;


ALTER TABLE `coi_project_award`
ADD COLUMN `KEY_PERSON_ID` VARCHAR(45) DEFAULT NULL,
ADD COLUMN `KEY_PERSON_NAME` VARCHAR(60) DEFAULT NULL,
ADD COLUMN `KEY_PERSON_ROLE_CODE` INT DEFAULT NULL,
ADD COLUMN `ACCOUNT_NUMBER` VARCHAR(100) DEFAULT NULL,
ADD COLUMN `SPONSOR_AWARD_NUMBER` VARCHAR(70) DEFAULT NULL;

INSERT INTO `discl_comment_type` (`COMMENT_TYPE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('3', 'General', 'Y', now(), 'quickstart');
INSERT INTO `discl_comment_type` (`COMMENT_TYPE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('4', 'Questionnaire', 'Y', now(), 'quickstart');
INSERT INTO `discl_comment_type` (`COMMENT_TYPE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('5', 'SFIs', 'Y', now(), 'quickstart');
INSERT INTO `discl_comment_type` (`COMMENT_TYPE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('6', 'Project relationships', 'Y', now(), 'quickstart');
INSERT INTO `discl_comment_type` (`COMMENT_TYPE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('7', 'Certification', 'Y', now(), 'quickstart');
SET SQL_SAFE_UPDATES = 0;
SET FOREIGN_KEY_CHECKS = 0;
UPDATE `coi_sections_type` SET `COI_SECTIONS_TYPE_CODE` = '7' WHERE (`DESCRIPTION` = 'Certification');
UPDATE `coi_sections_type` SET `COI_SECTIONS_TYPE_CODE` = '6' WHERE (`DESCRIPTION` = 'Project relationships');
UPDATE `coi_sections_type` SET `COI_SECTIONS_TYPE_CODE` = '5' WHERE (`DESCRIPTION` = 'SFIs');
UPDATE `coi_sections_type` SET `COI_SECTIONS_TYPE_CODE` = '4' WHERE (`DESCRIPTION` = 'Questionnaire');
INSERT INTO `coi_sections_type` (`COI_SECTIONS_TYPE_CODE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `IS_ACTIVE`) VALUES ('3', 'General', now(), 'quickstart', 'Y');
SET SQL_SAFE_UPDATES = 1;
SET FOREIGN_KEY_CHECKS = 1;
ALTER TABLE `coi_review_comment_tags` 
DROP FOREIGN KEY `COI_REVIEW_CMT_TAGS_FK2`;
ALTER TABLE `coi_review_comment_tags`;
ALTER TABLE `coi_review_comment_tags` RENAME INDEX `COI_REVIEW_CMT_TAGS_FK2` TO `COI_REVIEW_CMT_TAGS_FK2_idx`;
ALTER TABLE `coi_review_comment_tags` ALTER INDEX `COI_REVIEW_CMT_TAGS_FK2_idx` VISIBLE;
ALTER TABLE `coi_review_comment_tags` 
ADD CONSTRAINT `COI_REVIEW_CMT_TAGS_FK2`
FOREIGN KEY (`COI_REVIEW_COMMENT_ID`)
REFERENCES `discl_comment` (`COMMENT_ID`);
ALTER TABLE `coi_travel_disclosure` 
ADD COLUMN `RISK_CATEGORY_CODE` VARCHAR(3) NULL AFTER `DOCUMENT_STATUS_CODE`,
ADD INDEX `COI_TRAVEL_DISCLOSURE_FK5_idx` (`RISK_CATEGORY_CODE` ASC) VISIBLE;
ALTER TABLE `coi_travel_disclosure` 
ADD CONSTRAINT `COI_TRAVEL_DISCLOSURE_FK5`
  FOREIGN KEY (`RISK_CATEGORY_CODE`)
  REFERENCES `coi_risk_category` (`RISK_CATEGORY_CODE`)
  ON DELETE NO ACTION
  ON UPDATE NO ACTION;
ALTER TABLE `discl_comment` 
ADD COLUMN `COMPONENT_SUB_REFERENCE_ID` INT DEFAULT NULL;

INSERT INTO `entity_action_type` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('7', 'Risk Level <b>{RISK}</b> has been added', 'Add Risk', now(), 'quickstart');

UPDATE `disclosure_action_type` SET MESSAGE = 'New {FCOI /Project /Travel} disclosure has been created' WHERE ACTION_TYPE_CODE = '1';
UPDATE `disclosure_action_type` SET MESSAGE = '{FCOI /Project /Travel} disclosure has been submitted' WHERE ACTION_TYPE_CODE = '2';
UPDATE `disclosure_action_type` SET MESSAGE = '{FCOI /Project /Travel} disclosure has been recalled' WHERE ACTION_TYPE_CODE = '3';
UPDATE `disclosure_action_type` SET MESSAGE = 'New admin <b>{ADMIN_ONE}</b> has been assigned' WHERE ACTION_TYPE_CODE = '4';
UPDATE `disclosure_action_type` SET MESSAGE = '{FCOI /Project /Travel} disclosure has been returned' WHERE ACTION_TYPE_CODE = '6';
UPDATE `disclosure_action_type` SET MESSAGE = '<b>{Reviewer Name}</b> assigned to review {FCOI /Project /Travel} disclosure' WHERE ACTION_TYPE_CODE = '7';
UPDATE `disclosure_action_type` SET MESSAGE = '{FCOI /Project /Travel} disclosure review has been completed' WHERE ACTION_TYPE_CODE = '8';
UPDATE `disclosure_action_type` SET MESSAGE = 'Risk <b>{LOW}</b> has been added' WHERE ACTION_TYPE_CODE = '9';
UPDATE `disclosure_action_type` SET MESSAGE = 'Risk for  {FCOI /Project /Travel} disclosure changed from <b>{LOW}</b> to <b>{HIGH}</b>' WHERE ACTION_TYPE_CODE = '10';
UPDATE `disclosure_action_type` SET MESSAGE = '{FCOI /Project /Travel} disclosure admin-review has been completed' WHERE ACTION_TYPE_CODE = '11';
UPDATE `disclosure_action_type` SET MESSAGE = '{FCOI /Project /Travel} disclosure has been expired' WHERE ACTION_TYPE_CODE = '12';
UPDATE `disclosure_action_type` SET MESSAGE = '{FCOI /Project /Travel} disclosure has been approved' WHERE ACTION_TYPE_CODE = '13';
UPDATE `disclosure_action_type` SET MESSAGE = '{FCOI /Project /Travel} disclosure status has been added as <b>No Conflict</b>' WHERE ACTION_TYPE_CODE = '14';
UPDATE `disclosure_action_type` SET MESSAGE = '{FCOI /Project /Travel} disclosure status has been changed from <b>{STATUS_ONE}</b> to <b>{STATUS_TWO}</b>' WHERE ACTION_TYPE_CODE = '15';

INSERT INTO `disclosure_action_type` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('14', '{FCOI /Project /Travel} disclosure status has been added as <b>No Conflict</b>', 'Created', now(), 'quickstart');
INSERT INTO `disclosure_action_type` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('15', '{FCOI /Project /Travel} disclosure status has been changed from <b>{STATUS_ONE}</b> to <b>{STATUS_TWO}</b>', 'Manage Status', now(), 'quickstart');

INSERT INTO `disclosure_action_type` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('16', 'New reviewer <b>{REVIEWER_ONE}</b> has been added by the Administartor <b>{ADMIN_NAME}</b>', 'Reviewer has been added', now(), 'quickstart');
INSERT INTO `disclosure_action_type` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('17', '<b>{REVIEWER_NAME}</b> has started the review', 'Reviewer has started the review', now(), 'quickstart');
INSERT INTO `disclosure_action_type` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('18', 'Review of <b>{REVIEWER_NAME}</b> has been started by the Administartor <b>{ADMIN_NAME}</b>', 'Admin has started the review', now(), 'quickstart');
INSERT INTO `disclosure_action_type` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('19', '<b>{REVIEWER_NAME}</b> has Completed the review', 'Reviewer has completed the review', now(), 'quickstart');
INSERT INTO `disclosure_action_type` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('20', 'Review of <b>{REVIEWER_NAME}</b> has been Completed by the Administartor <b>{ADMIN_NAME}</b>', 'Admin has completed the review', now(), 'quickstart');
INSERT INTO `disclosure_action_type` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('21', '<b>{REVIEWER_NAME}</b> has been removed by the Administartor <b>{ADMIN_NAME}</b>', 'Reviewer Removed', now(), 'quickstart');
INSERT INTO `disclosure_action_type` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('22', 'Reviewer <b>{REVIEWER_ONE}</b> reassigned to <b>{REVIEWER_TWO}</b>', 'Reviewer ressigned', now(), 'quickstart');
UPDATE `disclosure_action_type` SET MESSAGE = '<b>{REVIEWER_NAME}</b> assigned to review {FCOI /Project /Travel} disclosure' WHERE ACTION_TYPE_CODE = '7';

SET FOREIGN_KEY_CHECKS=0;
ALTER TABLE `valid_person_entity_rel_type` 
CHANGE COLUMN `VALID_PERSON_ENTITY_REL_TYPE_CODE` `VALID_PERS_ENTITY_REL_TYP_CODE` INT NOT NULL AUTO_INCREMENT ;

ALTER TABLE `person_entity_relationship` 
DROP FOREIGN KEY `PERSON_ENTITY_RELATIONSHIP_FK2`;
ALTER TABLE `person_entity_relationship` 
CHANGE COLUMN `VALID_PERSON_ENTITY_REL_TYPE_CODE` `VALID_PERS_ENTITY_REL_TYP_CODE` INT NULL DEFAULT NULL ;

ALTER TABLE `person_entity_relationship` 
ADD CONSTRAINT `PERSON_ENTITY_RELATIONSHIP_FK2`
  FOREIGN KEY (`VALID_PERS_ENTITY_REL_TYP_CODE`)
  REFERENCES `valid_person_entity_rel_type` (`VALID_PERS_ENTITY_REL_TYP_CODE`);
  
INSERT INTO `lookup_window` (`LOOKUP_WINDOW_NAME`, `DESCRIPTION`, `TABLE_NAME`, `COLUMN_NAME`, `OTHERS_DISPLAY_COLUMNS`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `DATA_TYPE_CODE`) VALUES ('PERSON_ENTITY_REL_TYPE_LOOKUP', 'Relationship dropdown', 'person_entity_rel_type', 'RELATIONSHIP_TYPE_CODE', 'DESCRIPTION', now(), 'quickstart', '8');
INSERT INTO `business_rule_variable` (`VARIABLE_NAME`, `MODULE_CODE`, `SUB_MODULE_CODE`, `DESCRIPTION`, `TABLE_NAME`, `COLUMN_NAME`, `SHOW_LOOKUP`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `LOOKUP_WINDOW_NAME`) VALUES ('Relationship Type', '8', '801', 'Person Entity Relationship Type', 'PERSON_ENTITY_RELATIONSHIP', 'VALID_PERS_ENTITY_REL_TYP_CODE', 'Y', now(), 'quickstart', 'PERSON_ENTITY_REL_TYPE_LOOKUP');

INSERT INTO business_rules (`RULE_ID`, `DESCRIPTION`, `RULE_TYPE`, `UNIT_NUMBER`, `MODULE_CODE`, `SUB_MODULE_CODE`, `IS_ACTIVE`, `RULE_EXPRESSION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `NOTIFICATION_ID`, `USER_MESSAGE`, `RULE_EVALUATION_ORDER`, `RULE_CATEGORY`) 
VALUES 
((SELECT T.ID FROM (SELECT MAX(RULE_ID) + 1 AS ID FROM business_rules) AS T), 'Dependant relationship rule', 'Q', '000001', '8', '801', 'Y', ' ( E1 )', now(), 'quickstart', '0', '', '2', 'R');

INSERT INTO business_rules_experssion(RULES_EXPERSSION_ID, RULE_ID, EXPRESSION_NUMBER, EXPRESSION_TYPE_CODE, LVALUE, CONDITION_OPERATOR, RVALUE, UPDATE_TIMESTAMP, UPDATE_USER, PARENT_EXPRESSION_NUMBER) 
VALUES
((SELECT T.ID FROM (SELECT MAX(RULES_EXPERSSION_ID) + 1 AS ID FROM business_rules_experssion) AS T), (SELECT MAX(RULE_ID)  AS ID FROM business_rules), '1', 'V', 'Relationship Type', 'Equal to', '3', now(), 'quickstart', '0');

INSERT INTO `business_rules` (`RULE_ID`, `DESCRIPTION`, `RULE_TYPE`, `UNIT_NUMBER`, `MODULE_CODE`, `SUB_MODULE_CODE`, `IS_ACTIVE`, `RULE_EXPRESSION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `NOTIFICATION_ID`, `USER_MESSAGE`, `RULE_EVALUATION_ORDER`, `RULE_CATEGORY`) 
VALUES 
((SELECT T.ID FROM (SELECT MAX(RULE_ID) + 1 AS ID FROM business_rules) AS T), 'Spouse relationship rule', 'Q', '000001', '8', '801', 'Y', ' ( E1 )', now(), 'quickstart', '0', '', '3', 'R');

INSERT INTO business_rules_experssion(RULES_EXPERSSION_ID, RULE_ID, EXPRESSION_NUMBER, EXPRESSION_TYPE_CODE, LVALUE, CONDITION_OPERATOR, RVALUE, UPDATE_TIMESTAMP, UPDATE_USER, PARENT_EXPRESSION_NUMBER) 
VALUES
((SELECT T.ID FROM (SELECT MAX(RULES_EXPERSSION_ID) + 1 AS ID FROM business_rules_experssion) AS T), (SELECT MAX(RULE_ID)  AS ID FROM business_rules), '1', 'V', 'Relationship Type', 'Equal to', '2', now(), 'quickstart', '0');

INSERT INTO `business_rules` (`RULE_ID`, `DESCRIPTION`, `RULE_TYPE`, `UNIT_NUMBER`, `MODULE_CODE`, `SUB_MODULE_CODE`, `IS_ACTIVE`, `RULE_EXPRESSION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `NOTIFICATION_ID`, `USER_MESSAGE`, `RULE_EVALUATION_ORDER`, `RULE_CATEGORY`) 
VALUES 
((SELECT T.ID FROM (SELECT MAX(RULE_ID) + 1 AS ID FROM business_rules) AS T), 'Self relationship rule', 'Q', '000001', '8', '801', 'Y', ' ( E1 )', now(), 'quickstart', '0', '', '4', 'R');

INSERT INTO business_rules_experssion(RULES_EXPERSSION_ID, RULE_ID, EXPRESSION_NUMBER, EXPRESSION_TYPE_CODE, LVALUE, CONDITION_OPERATOR, RVALUE, UPDATE_TIMESTAMP, UPDATE_USER, PARENT_EXPRESSION_NUMBER) 
VALUES
((SELECT T.ID FROM (SELECT MAX(RULES_EXPERSSION_ID) + 1 AS ID FROM business_rules_experssion) AS T), (SELECT MAX(RULE_ID)  AS ID FROM business_rules), '1', 'V', 'Relationship Type', 'Equal to', '1', now(), 'quickstart', '0');

INSERT INTO `business_rules` (`RULE_ID`, `DESCRIPTION`, `RULE_TYPE`, `UNIT_NUMBER`, `MODULE_CODE`, `SUB_MODULE_CODE`, `IS_ACTIVE`, `RULE_EXPRESSION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `NOTIFICATION_ID`, `USER_MESSAGE`, `RULE_EVALUATION_ORDER`, `RULE_CATEGORY`) 
VALUES 
((SELECT T.ID FROM (SELECT MAX(RULE_ID) + 1 AS ID FROM business_rules) AS T), 'Travel relationship rule', 'Q', '000001', '8', '801', 'Y', ' ( E1 )', now(), 'quickstart', '0', '', '5', 'R');

INSERT INTO business_rules_experssion(RULES_EXPERSSION_ID, RULE_ID, EXPRESSION_NUMBER, EXPRESSION_TYPE_CODE, LVALUE, CONDITION_OPERATOR, RVALUE, UPDATE_TIMESTAMP, UPDATE_USER, PARENT_EXPRESSION_NUMBER) 
VALUES
((SELECT T.ID FROM (SELECT MAX(RULES_EXPERSSION_ID) + 1 AS ID FROM business_rules_experssion) AS T), (SELECT MAX(RULE_ID)  AS ID FROM business_rules), '1', 'V', 'Relationship Type', 'Equal to', '4', now(), 'quickstart', '0');

ALTER TABLE `coi_review` 
CHANGE COLUMN `DESCRIPTION` `DESCRIPTION` VARCHAR(2000) NULL DEFAULT NULL ;

SET FOREIGN_KEY_CHECKS=1;

SET SQL_SAFE_UPDATES = 0;

UPDATE `disclosure_action_type` SET `MESSAGE` = '{FCOI /Project /Travel} disclosure <b>created</b> by <b>{REPORTER}</b> ' WHERE (`ACTION_TYPE_CODE` = '1');
UPDATE `disclosure_action_type` SET `MESSAGE` = '{FCOI /Project /Travel} Disclosure Risk <b>changed</b> from <b>{LOW}</b> to <b>{HIGH}</b> by <b>{ADMIN_NAME}</b> ' WHERE (`ACTION_TYPE_CODE` = '10');
UPDATE `disclosure_action_type` SET `MESSAGE` = '{FCOI /Project /Travel} disclosure review <b>completed</b> by <b>{ADMIN_NAME}</b> ' WHERE (`ACTION_TYPE_CODE` = '11');
UPDATE `disclosure_action_type` SET `MESSAGE` = '{FCOI /Project /Travel} disclosure has <b>expired</b> ' WHERE (`ACTION_TYPE_CODE` = '12');
UPDATE `disclosure_action_type` SET `MESSAGE` = '{FCOI /Project /Travel} disclosure <b>approved</b> ' WHERE (`ACTION_TYPE_CODE` = '13');
UPDATE `disclosure_action_type` SET `MESSAGE` = '{FCOI /Project /Travel} disclosure status <b>changed</b> from <b>{STATUS_ONE}</b> to <b>{STATUS_TWO}</b> ' WHERE (`ACTION_TYPE_CODE` = '15');
UPDATE `disclosure_action_type` SET `MESSAGE` = '{FCOI /Project /Travel} disclosure <b>submitted</b> by <b>{REPORTER}</b> ' WHERE (`ACTION_TYPE_CODE` = '2');
UPDATE `disclosure_action_type` SET `MESSAGE` = '{FCOI /Project /Travel} disclosure <b>recalled</b> by <b>{REPORTER}</b> ' WHERE (`ACTION_TYPE_CODE` = '3');
UPDATE `disclosure_action_type` SET `MESSAGE` = 'Primary Administrator <b>{ADMIN_ONE}</b> <b>assigned</b> by <b>{COI_ADMIN}</b> ' WHERE (`ACTION_TYPE_CODE` = '4');
UPDATE `disclosure_action_type` SET `MESSAGE` = 'Primary Administrator <b>{ADMIN_ONE}</b> <b>reassigned</b> to <b>{ADMIN_TWO}</b> by <b>{COI_ADMIN}</b> ' WHERE (`ACTION_TYPE_CODE` = '5');
UPDATE `disclosure_action_type` SET `MESSAGE` = '{FCOI /Project /Travel} disclosure <b>returned</b> by <b>{ADMIN_NAME}</b> ' WHERE (`ACTION_TYPE_CODE` = '6');
UPDATE `disclosure_action_type` SET `MESSAGE` = '{FCOI /Project /Travel} disclosure <b>assigned</b> to Review by <b>{ADMIN_NAME}</b> ' WHERE (`ACTION_TYPE_CODE` = '7');
UPDATE `disclosure_action_type` SET `MESSAGE` = 'Assigned Review <b>completed</b> for {FCOI /Project /Travel} disclosure ' WHERE (`ACTION_TYPE_CODE` = '8');
UPDATE `disclosure_action_type` SET `MESSAGE` = '{FCOI /Project /Travel} disclosure Risk <b>added</b> as <b>{LOW}</b>' WHERE (`ACTION_TYPE_CODE` = '9');

SET SQL_SAFE_UPDATES = 1;

INSERT INTO `COI_PROJECT_TYPE` (`COI_PROJECT_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('4', 'IRB Protocol', 'Y', now(), 'quickstart');
INSERT INTO `COI_PROJECT_TYPE` (`COI_PROJECT_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('5', 'IACUC Protocol', 'Y', now(), 'quickstart');

INSERT INTO `COI_PROJ_CONFLICT_STATUS_TYPE` (`PROJECT_CONFLICT_STATUS_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('200', 'Potential Conflict', 'Y', now(), 'quickstart');
INSERT INTO `COI_PROJ_CONFLICT_STATUS_TYPE` (`PROJECT_CONFLICT_STATUS_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('300', 'Conflict Identified', 'Y', now(), 'quickstart');
INSERT INTO `COI_PROJ_CONFLICT_STATUS_TYPE` (`PROJECT_CONFLICT_STATUS_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('100', 'No Conflict', 'Y', now(), 'quickstart');

SET SQL_SAFE_UPDATES = 0;

UPDATE coi_discl_ent_proj_details
SET PROJECT_CONFLICT_STATUS_CODE = 300
WHERE PROJECT_CONFLICT_STATUS_CODE = 3;

UPDATE coi_discl_ent_proj_details
SET PROJECT_CONFLICT_STATUS_CODE = 200
WHERE PROJECT_CONFLICT_STATUS_CODE = 2;

UPDATE coi_discl_ent_proj_details
SET PROJECT_CONFLICT_STATUS_CODE = 100
WHERE PROJECT_CONFLICT_STATUS_CODE = 1;

SET SQL_SAFE_UPDATES = 1;

DELETE FROM `COI_PROJ_CONFLICT_STATUS_TYPE` WHERE (`PROJECT_CONFLICT_STATUS_CODE` = '1');
DELETE FROM `COI_PROJ_CONFLICT_STATUS_TYPE` WHERE (`PROJECT_CONFLICT_STATUS_CODE` = '2');
DELETE FROM `COI_PROJ_CONFLICT_STATUS_TYPE` WHERE (`PROJECT_CONFLICT_STATUS_CODE` = '3');

ALTER TABLE `coi_review`
DROP FOREIGN KEY `COI_REVIEW_FK4`;
ALTER TABLE `coi_review`
ADD INDEX `COI_REVIEW_FK4_idx` (`REVIEW_STATUS_TYPE_CODE` ASC) VISIBLE,
DROP INDEX `COI_REVIEW_FK4_idx` ;
;
ALTER TABLE `coi_review`
ADD CONSTRAINT `COI_REVIEW_FK4`
  FOREIGN KEY (`REVIEW_STATUS_TYPE_CODE`)
  REFERENCES `coi_reviewer_status_type` (`REVIEW_STATUS_CODE`);


ALTER TABLE `coi_review`
ADD COLUMN `LOCATION_TYPE_CODE` VARCHAR(3) NULL AFTER `DESCRIPTION`,
ADD INDEX `COI_REVIEW_FK5_idx` (`LOCATION_TYPE_CODE` ASC) VISIBLE;
;
ALTER TABLE `coi_review`
ADD CONSTRAINT `COI_REVIEW_FK5`
  FOREIGN KEY (`LOCATION_TYPE_CODE`)
  REFERENCES `coi_review_location_type` (`LOCATION_TYPE_CODE`)
  ON DELETE NO ACTION
  ON UPDATE NO ACTION;

ALTER TABLE `coi_review` 
ADD COLUMN `START_DATE` DATE NULL AFTER `LOCATION_TYPE_CODE`,
ADD COLUMN `END_DATE` DATE NULL AFTER `START_DATE`;


INSERT INTO `COI_REVIEW_LOCATION_TYPE` (`LOCATION_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('1', '3rd Party Location', 'Y', now(), 'quickstart');
INSERT INTO `COI_REVIEW_LOCATION_TYPE` (`LOCATION_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('2', 'Department', 'Y', now(), 'quickstart');
INSERT INTO `COI_REVIEW_LOCATION_TYPE` (`LOCATION_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('3', 'Legal', 'Y', now(), 'quickstart');
INSERT INTO `COI_REVIEW_LOCATION_TYPE` (`LOCATION_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('4', 'HOD', 'Y', now(), 'quickstart');

INSERT INTO `COI_REVIEWER_STATUS_TYPE` (`REVIEW_STATUS_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('1', 'Assigned', 'Y', now(), 'quickstart');
INSERT INTO `COI_REVIEWER_STATUS_TYPE` (`REVIEW_STATUS_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('2', 'Completed', 'Y', now(), 'quickstart');
INSERT INTO `COI_REVIEWER_STATUS_TYPE` (`REVIEW_STATUS_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('3', 'In Progress', 'Y', now(), 'quickstart');

INSERT INTO `coi_review_status_type` (`REVIEW_STATUS_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) 
VALUES ('7', 'Review assigned', 'Y', now(), 'quickstart');

INSERT INTO `coi_review_status_type` (`REVIEW_STATUS_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) 
VALUES ('8', 'Assigned Review Completed', 'Y', now(), 'quickstart');

UPDATE `entity_action_type` SET `MESSAGE` = '{ENTITY_NAME} <b>created</b>' WHERE (`ACTION_TYPE_CODE` = '1');
UPDATE `entity_action_type` SET `MESSAGE` = '{ENTITY_NAME} <b>activated</b>' WHERE (`ACTION_TYPE_CODE` = '2');
UPDATE `entity_action_type` SET `MESSAGE` = '{ENTITY_NAME} <b>inactivated</b>' WHERE (`ACTION_TYPE_CODE` = '3');
UPDATE `entity_action_type` SET `MESSAGE` = '{ENTITY_NAME} <b>verified</b>' WHERE (`ACTION_TYPE_CODE` = '4');
UPDATE `entity_action_type` SET `MESSAGE` = 'Entity Risk changed from <b>{RISK}</b> to <b>{NEW_RISK}</b>' WHERE (`ACTION_TYPE_CODE` = '5');
UPDATE `entity_action_type` SET `MESSAGE` = '{ENTITY_NAME} <b>modified</b> by' WHERE (`ACTION_TYPE_CODE` = '6');
UPDATE `entity_action_type` SET `MESSAGE` = 'Entity Risk <b>added</b> as <b>{RISK}</b>' WHERE (`ACTION_TYPE_CODE` = '7');

ALTER TABLE `coi_travel_disclosure` 
ADD INDEX `COI_TRAVEL_DISCLOSURE_FK6_idx` (`TRAVEL_DISCLOSURE_STATUS_CODE` ASC) VISIBLE;
;
ALTER TABLE `coi_travel_disclosure` 
ADD CONSTRAINT `COI_TRAVEL_DISCLOSURE_FK6`
  FOREIGN KEY (`TRAVEL_DISCLOSURE_STATUS_CODE`)
  REFERENCES `coi_travel_disclosure_status` (`TRAVEL_DISCLOSURE_STATUS_CODE`)
  ON DELETE NO ACTION
  ON UPDATE NO ACTION;

INSERT INTO `entity_type` (`ENTITY_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('5', 'Federal', 'Y', now(), 'quickstart');

INSERT INTO `coeus_module` (MODULE_CODE, DESCRIPTION, UPDATE_TIMESTAMP, UPDATE_USER, IS_ACTIVE) VALUES (24, 'COI Travel Disclosure', now(), 'admin', 'Y');
INSERT INTO `message` (MESSAGE_TYPE_CODE, DESCRIPTION, UPDATE_TIMESTAMP, UPDATE_USER) VALUES ((SELECT A.ID FROM (SELECT MAX(INBOX_ID) + 1 AS ID FROM INBOX ) AS A), 'FCOI Disclosure Expiration', now(), 'quickstart');
INSERT INTO `message` (MESSAGE_TYPE_CODE, DESCRIPTION, UPDATE_TIMESTAMP, UPDATE_USER) VALUES ((SELECT A.ID FROM (SELECT MAX(INBOX_ID) + 1 AS ID FROM INBOX ) AS A), 'Travel Disclosure Expiration', now(), 'quickstart');

ALTER TABLE `INBOX` ADD COLUMN `ALERT_TYPE` VARCHAR(45) NULL;
ALTER TABLE `INBOX` ADD COLUMN `EXPIRATION_DATE` DATETIME NULL;
ALTER TABLE `REMINDER_NOTIFICATION` ADD COLUMN `REMINDER_TYPE_FLAG` VARCHAR(3) NULL AFTER `PLACEHOLDER_VALUES`;

INSERT INTO `REMINDER_NOTIFICATION` (REMINDER_NOTIFICATION_ID, REMINDER_NAME, NOTIFICATION_TYPE_ID, DAYS_TO_DUE_DATE, PROCEDURE_NAME, IS_ACTVE, UPDATE_USER, UPDATE_TIMESTAMP, PLACEHOLDER_VALUES,REMINDER_TYPE_FLAG)
VALUES((SELECT A.ID FROM (SELECT MAX(REMINDER_NOTIFICATION_ID) + 1 AS ID FROM REMINDER_NOTIFICATION ) AS A), 'Reminder Banner for Coi Disclosures', 804, 360, 'GET_COI_DISCLOSURES_FOR_BANNER', 'Y', 'quickstart', now(), 'DESCRIPTION', 'B');

/*
OPA related changes 
*/
CREATE TABLE `opa_person` (
  `PERSON_ID` varchar(40) NOT NULL,
  `PERSON_NAME` varchar(90) DEFAULT NULL,
  `FORM_OF_ADDRESS_SHORT` varchar(40) DEFAULT NULL,
  `FIRST_NAME` varchar(30) DEFAULT NULL,
  `MIDDLE_NAME` varchar(30) DEFAULT NULL,
  `LAST_NAME` varchar(30) DEFAULT NULL,
  `KRB_NAME_UPPERCASE` varchar(60) DEFAULT NULL,
  `EMAIL_ADDRESS` varchar(60) DEFAULT NULL,
  `JOB_ID` varchar(20) DEFAULT NULL,
  `JOB_TITLE` varchar(50) DEFAULT NULL,
  `ADMIN_EMPLOYEE_TYPE` varchar(40) DEFAULT NULL,
  `HR_DEPARTMENT_CODE_OLD` varchar(10) DEFAULT NULL,
  `HR_DEPARTMENT_NAME` varchar(50) DEFAULT NULL,
  `ADMIN_ORG_UNIT_ID` varchar(10) DEFAULT NULL,
  `ADMIN_ORG_UNIT_TITLE` varchar(50) DEFAULT NULL,
  `ADMIN_POSITION_TITLE` varchar(50) DEFAULT NULL,
  `PAYROLL_RANK` varchar(40) DEFAULT NULL,
  `IS_FACULTY` varchar(1) DEFAULT NULL,
  `EMPLOYMENT_PERCENT` decimal(5,2) DEFAULT NULL,
  `IS_CONSULT_PRIV` varchar(1) DEFAULT NULL,
  `IS_PAID_APPT` varchar(1) DEFAULT NULL,
  `IS_SUMMER_SESSION_APPT` varchar(1) DEFAULT NULL,
  `SUMMER_SESSION_MONTHS` int DEFAULT NULL,
  `IS_SABBATICAL` varchar(1) DEFAULT NULL,
  `SABBATICAL_BEGIN_DATE` date DEFAULT NULL,
  `SABBATICAL_END_DATE` date DEFAULT NULL,
  `WAREHOUSE_LOAD_DATE` date DEFAULT NULL,
  PRIMARY KEY (`PERSON_ID`)
);

CREATE TABLE `opa_cycles` (
  `OPA_CYCLE_NUMBER` int NOT NULL AUTO_INCREMENT,
  `PERIOD_START_DATE` date NOT NULL,
  `PERIOD_END_DATE` date NOT NULL,
  `OPA_CYCLE_STATUS` varchar(1) NOT NULL,
  `OPEN_DATE` date DEFAULT NULL,
  `CLOSE_DATE` date DEFAULT NULL,
  `UPDATE_TIMESTAMP` datetime NOT NULL,
  `UPDATE_USER` varchar(60) NOT NULL,
  PRIMARY KEY (`OPA_CYCLE_NUMBER`)
);

CREATE TABLE `opa_disclosure` (
  `OPA_DISCLOSURE_ID` int NOT NULL AUTO_INCREMENT,
  `OPA_DISCLOSURE_NUMBER` varchar(20) DEFAULT NULL,
  `OPA_CYCLE_NUMBER` int NOT NULL,
  `PERSON_ID` varchar(40) NOT NULL,
  `PERSON_NAME` varchar(90) NOT NULL,
  `HOME_UNIT` varchar(8) NOT NULL,
  `STATUS_FLAG` varchar(1) NOT NULL,
  `IS_FACULTY` varchar(1) NOT NULL,
  `IS_FALL_SABATICAL` varchar(1) DEFAULT NULL,
  `IS_SPRING_SABATICAL` varchar(1) DEFAULT NULL,
  `RECEIVED_SUMMER_COMP` varchar(1) DEFAULT NULL,
  `SUMMER_COMP_MONTHS` decimal(5,2) DEFAULT NULL,
  `IS_FULL_TIME` varchar(1) DEFAULT NULL,
  `IS_PART_TIME` varchar(1) DEFAULT NULL,
  `APPOINTMENT_PERCENT` decimal(5,2) DEFAULT NULL,
  `IS_COMPENSATED` varchar(1) DEFAULT NULL,
  `HAS_POTENTIAL_CONFLICT` varchar(1) DEFAULT NULL,
  `CONFLICT_DESCRIPTION` varchar(4000) DEFAULT NULL,
  `CREATE_TIMESTAMP` datetime NOT NULL,
  `CREATE_USER` varchar(60) NOT NULL,
  `SUBMISSION_TIMESTAMP` datetime DEFAULT NULL,
  `UPDATE_TIMESTAMP` datetime NOT NULL,
  `UPDATE_USER` varchar(60) NOT NULL,
  PRIMARY KEY (`OPA_DISCLOSURE_ID`),
  KEY `OPA_DISCLOSURE_FK1_idx` (`OPA_CYCLE_NUMBER`),
  CONSTRAINT `OPA_DISCLOSURE_FK1` FOREIGN KEY (`OPA_CYCLE_NUMBER`) REFERENCES `opa_cycles` (`OPA_CYCLE_NUMBER`)
) ;

CREATE TABLE `opa_disclosure_number_counter` (
  `counter_name` varchar(255) NOT NULL,
  `counter_value` int DEFAULT NULL,
  PRIMARY KEY (`counter_name`)
);

INSERT INTO `opa_disclosure_number_counter` (`counter_name`, `counter_value`) VALUES ('OPA_DISCLOSURE_NUMBER_COUNTER', '1');

ALTER TABLE `opa_disclosure` 
CHANGE COLUMN `STATUS_FLAG` `STATUS_CODE` VARCHAR(1) NOT NULL ;


CREATE TABLE `opa_status_type` (
  `STATUS_TYPE_CODE` varchar(3) NOT NULL,
  `DESCRIPTION` varchar(200) DEFAULT NULL,
  `IS_ACTIVE` varchar(1) DEFAULT NULL,
  `UPDATE_TIMESTAMP` datetime DEFAULT NULL,
  `UPDATE_USER` varchar(60) DEFAULT NULL,
  PRIMARY KEY (`STATUS_TYPE_CODE`)
) ;

DROP TABLE IF EXISTS `opa_review_status_type`;
CREATE TABLE `opa_review_status_type` (
  `REVIEW_STATUS_CODE` varchar(3) NOT NULL,
  `DESCRIPTION` varchar(200) DEFAULT NULL,
  `IS_ACTIVE` varchar(1) DEFAULT 'Y',
  `UPDATE_TIMESTAMP` datetime DEFAULT NULL,
  `UPDATE_USER` varchar(60) DEFAULT NULL,
  PRIMARY KEY (`REVIEW_STATUS_CODE`)
);

DROP TABLE IF EXISTS `OPA_REVIEW_LOCATION_TYPE`;
CREATE TABLE `OPA_REVIEW_LOCATION_TYPE` (
  `LOCATION_TYPE_CODE` varchar(3) NOT NULL,
  `DESCRIPTION` varchar(200) DEFAULT NULL,
  `IS_ACTIVE` varchar(1) DEFAULT 'Y',
  `UPDATE_TIMESTAMP` datetime DEFAULT NULL,
  `UPDATE_USER` varchar(60) DEFAULT NULL,
  PRIMARY KEY (`LOCATION_TYPE_CODE`)
);

DROP TABLE IF EXISTS `opa_review`;
CREATE TABLE `opa_review` (
  `OPA_REVIEW_ID` int NOT NULL AUTO_INCREMENT,
  `ASSIGNEE_PERSON_ID` varchar(40) DEFAULT NULL,
  `OPA_DISCLOSURE_ID` int DEFAULT NULL,
  `ADMIN_GROUP_ID` int DEFAULT NULL,
  `REVIEW_STATUS_TYPE_CODE` varchar(3) DEFAULT NULL,
  `UPDATE_TIMESTAMP` datetime DEFAULT NULL,
  `UPDATE_USER` varchar(60) DEFAULT NULL,
  `CREATE_TIMESTAMP` datetime DEFAULT NULL,
  `CREATE_USER` varchar(60) DEFAULT NULL,
  `DESCRIPTION` varchar(2000) DEFAULT NULL,
  `LOCATION_TYPE_CODE` varchar(3) DEFAULT NULL,
  `START_DATE` date DEFAULT NULL,
  `END_DATE` date DEFAULT NULL,
  PRIMARY KEY (`OPA_REVIEW_ID`),
  KEY `OPA_REVIEW_FK5` (`ASSIGNEE_PERSON_ID`),
  KEY `OPA_REVIEW_FK1` (`OPA_DISCLOSURE_ID`),
  KEY `OPA_REVIEW_FK2` (`ADMIN_GROUP_ID`),
  KEY `OPA_REVIEW_FK3_idx` (`REVIEW_STATUS_TYPE_CODE`),
  KEY `OPA_REVIEW_FK4_idx` (`LOCATION_TYPE_CODE`),
  CONSTRAINT `OPA_REVIEW_FK5` FOREIGN KEY (`ASSIGNEE_PERSON_ID`) REFERENCES `person` (`PERSON_ID`),
  CONSTRAINT `OPA_REVIEW_FK1` FOREIGN KEY (`OPA_DISCLOSURE_ID`) REFERENCES `OPA_DISCLOSURE` (`OPA_DISCLOSURE_ID`),
  CONSTRAINT `OPA_REVIEW_FK2` FOREIGN KEY (`ADMIN_GROUP_ID`) REFERENCES `admin_group` (`ADMIN_GROUP_ID`),
  CONSTRAINT `OPA_REVIEW_FK3` FOREIGN KEY (`REVIEW_STATUS_TYPE_CODE`) REFERENCES `OPA_REVIEW_STATUS_TYPE` (`REVIEW_STATUS_CODE`),
  CONSTRAINT `OPA_REVIEW_FK4` FOREIGN KEY (`LOCATION_TYPE_CODE`) REFERENCES `OPA_REVIEW_LOCATION_TYPE` (`LOCATION_TYPE_CODE`)
) ;

INSERT INTO `opa_status_type` (`STATUS_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('I', 'In progress', 'Y', now(), 'quickstart');
INSERT INTO `opa_status_type` (`STATUS_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('S', 'Submitted', 'Y', now(), 'quickstart');


/* Notes related changes */
CREATE TABLE `NOTES` (
		NOTE_ID INTEGER PRIMARY KEY NOT NULL AUTO_INCREMENT,
		PERSON_ID VARCHAR(45),
		CONTENT TEXT,
		UPDATE_USER VARCHAR(50),
		UPDATE_TIMESTAMP DATETIME
);

/* Attachments related changes */
CREATE TABLE `ATTACHMENTS`(
	ATTACHMENT_ID INT NOT NULL,
	PERSON_ID VARCHAR(45),
	DESCRIPTION VARCHAR(2000),
	ATTA_TYPE_CODE VARCHAR(3),
	FILE_NAME VARCHAR(100),
	MIME_TYPE VARCHAR(255),
	FILE_DATA_ID VARCHAR(100),
	CREATE_USER VARCHAR(50),
	CREATE_TIMESTAMP DATETIME
	UPDATE_USER VARCHAR(50),
	UPDATE_TIMESTAMP DATETIME
);

ALTER TABLE `opa_disclosure`
ADD COLUMN `ADMIN_PERSON_ID` VARCHAR(45) NULL ;

ALTER TABLE `opa_disclosure` 
ADD COLUMN `ADMIN_GROUP_ID` INT(3) NULL ;

ALTER TABLE `opa_disclosure` 
ADD COLUMN `OPA_DISCLOSURE_STATUS_CODE` VARCHAR(3) NULL ,
ADD INDEX `OPA_DISCLOSURE_FK2_idx` (`OPA_DISCLOSURE_STATUS_CODE` ASC) VISIBLE;
;
ALTER TABLE `opa_disclosure` 
ADD CONSTRAINT `OPA_DISCLOSURE_FK2`
  FOREIGN KEY (`OPA_DISCLOSURE_STATUS_CODE`)
  REFERENCES `OPA_DISCLOSURE_STATUS_TYPE` (`OPA_DISCLOSURE_STATUS_CODE`)
  ON DELETE NO ACTION
  ON UPDATE NO ACTION;
  
 ALTER TABLE `opa_disclosure` 
ADD COLUMN `DISPOSITION_STATUS_CODE` VARCHAR(3) NULL ,
ADD INDEX `OPA_DISCLOSURE_FK3_idx` (`DISPOSITION_STATUS_CODE` ASC) VISIBLE;
;
ALTER TABLE `opa_disclosure` 
ADD CONSTRAINT `OPA_DISCLOSURE_FK3`
  FOREIGN KEY (`DISPOSITION_STATUS_CODE`)
  REFERENCES `OPA_DISPOSITION_STATUS_TYPE` (`DISPOSITION_STATUS_CODE`)
  ON DELETE NO ACTION
  ON UPDATE NO ACTION;

INSERT INTO `OPA_DISCLOSURE_STATUS_TYPE` (`OPA_DISCLOSURE_STATUS_CODE`,`DESCRIPTION`,`IS_ACTIVE`,`UPDATE_TIMESTAMP`,`UPDATE_USER`) VALUES ('1','Pending','Y',now(),'quickstart');
INSERT INTO `OPA_DISCLOSURE_STATUS_TYPE` (`OPA_DISCLOSURE_STATUS_CODE`,`DESCRIPTION`,`IS_ACTIVE`,`UPDATE_TIMESTAMP`,`UPDATE_USER`) VALUES ('2','Submitted','Y',now(),'quickstart');
INSERT INTO `OPA_DISCLOSURE_STATUS_TYPE` (`OPA_DISCLOSURE_STATUS_CODE`,`DESCRIPTION`,`IS_ACTIVE`,`UPDATE_TIMESTAMP`,`UPDATE_USER`) VALUES ('3','Review in progress','Y',now(),'quickstart');
INSERT INTO `OPA_DISCLOSURE_STATUS_TYPE` (`OPA_DISCLOSURE_STATUS_CODE`,`DESCRIPTION`,`IS_ACTIVE`,`UPDATE_TIMESTAMP`,`UPDATE_USER`) VALUES ('4','Completed','Y',now(),'quickstart');
INSERT INTO `OPA_DISCLOSURE_STATUS_TYPE` (`OPA_DISCLOSURE_STATUS_CODE`,`DESCRIPTION`,`IS_ACTIVE`,`UPDATE_TIMESTAMP`,`UPDATE_USER`) VALUES ('5','Returned','Y',now(),'quickstart');
INSERT INTO `OPA_DISCLOSURE_STATUS_TYPE` (`OPA_DISCLOSURE_STATUS_CODE`,`DESCRIPTION`,`IS_ACTIVE`,`UPDATE_TIMESTAMP`,`UPDATE_USER`) VALUES ('6','Withdrawn','Y',now(),'quickstart');

INSERT INTO `OPA_ACTION_LOG_TYPE` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('1', 'OPA disclosure <b>created</b> by <b>{REPORTER}</b>', 'Created', now(), 'quickstart');
INSERT INTO `OPA_ACTION_LOG_TYPE` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('2', 'OPA disclosure <b>submitted</b> by <b>{REPORTER}</b>', 'Submitted', now(), 'quickstart');
INSERT INTO `OPA_ACTION_LOG_TYPE` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('3', 'OPA disclosure <b>recalled</b> by <b>{REPORTER}</b>', 'Withdrawn', now(), 'quickstart');
INSERT INTO `OPA_ACTION_LOG_TYPE` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('4', 'OPA disclosure <b>returned</b> by <b>{ADMIN_NAME}</b>', 'Returned', now(), 'quickstart');
INSERT INTO `OPA_ACTION_LOG_TYPE` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('5', 'OPA disclosure <b>approved</b> by <b>{ADMIN_NAME}</b>', 'Approved', now(), 'quickstart');
INSERT INTO `OPA_ACTION_LOG_TYPE` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('6', 'Primary Administrator <b>{ASSIGNED_ADMIN}</b> <b>assigned</b> by <b>{ADMIN_NAME}</b>', 'Assigned to admin /Admin Group', now(), 'quickstart');
INSERT INTO `OPA_ACTION_LOG_TYPE` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('7', 'Primary Administrator <b>{ASSIGNED_ADMIN}</b> <b>reassigned</b> to <b>{REASSIGNED_ADMIN}</b> by <b>{ADMIN_NAME}</b>', 'Re Assigned to admin /Admin Group', now(), 'quickstart');

ALTER TABLE `opa_disclosure` 
ADD COLUMN `OPA_DISCLOSURE_STATUS_CODE` VARCHAR(3) NULL ,
ADD INDEX `OPA_DISCLOSURE_FK2_idx` (`OPA_DISCLOSURE_STATUS_CODE` ASC) VISIBLE;
;
ALTER TABLE `opa_disclosure` 
ADD CONSTRAINT `OPA_DISCLOSURE_FK2`
  FOREIGN KEY (`OPA_DISCLOSURE_STATUS_CODE`)
  REFERENCES `OPA_DISCLOSURE_STATUS_TYPE` (`OPA_DISCLOSURE_STATUS_CODE`)
  ON DELETE NO ACTION
  ON UPDATE NO ACTION;
  
 ALTER TABLE `opa_disclosure` 
ADD COLUMN `DISPOSITION_STATUS_CODE` VARCHAR(3) NULL ,
ADD INDEX `OPA_DISCLOSURE_FK3_idx` (`DISPOSITION_STATUS_CODE` ASC) VISIBLE;
;
ALTER TABLE `opa_disclosure` 
ADD CONSTRAINT `OPA_DISCLOSURE_FK3`
  FOREIGN KEY (`DISPOSITION_STATUS_CODE`)
  REFERENCES `OPA_DISPOSITION_STATUS_TYPE` (`DISPOSITION_STATUS_CODE`)
  ON DELETE NO ACTION
  ON UPDATE NO ACTION;

UPDATE `DISCLOSURE_ACTION_TYPE` SET `MESSAGE` = 'Review assigned to <b>{REVIEWER_ONE}</b> at  <b>{LOCATION}</b> with status <b>{REVIEW_STATUS}</b> by Administrator <b>{ADMIN_NAME}</b>',
`DESCRIPTION` = 'Review Added with Reviewer' WHERE (`ACTION_TYPE_CODE` = '16');
	 
UPDATE `DISCLOSURE_ACTION_TYPE` SET `MESSAGE` = 'Review modified and assigned to <b>{REVIEWER_ONE}</b> at <b>{LOCATION}</b> with status <b>{REVIEW_STATUS}</b> by <b>{ADMIN_NAME}</b>',
`DESCRIPTION` = 'Review Modified with Reviewer'  WHERE (`ACTION_TYPE_CODE` = '22');


UPDATE ``DISCLOSURE_ACTION_TYPE` SET `MESSAGE` = 'Assigned Review <b>started</b> by <b>{REVIEWER_NAME}</b>   at <b>{LOCATION}</b>', 
`DESCRIPTION` = 'Review Started by Reviewer' WHERE (`ACTION_TYPE_CODE` = '17');

UPDATE ``DISCLOSURE_ACTION_TYPE` SET `MESSAGE` = 'Assigned Review <b>started</b> by <b>{ADMIN_NAME}</b>   at <b>{LOCATION}</b>', 
`DESCRIPTION` = 'Review Started by Admin with Reviewer' WHERE (`ACTION_TYPE_CODE` = '18');

UPDATE ``DISCLOSURE_ACTION_TYPE` SET `MESSAGE` = 'Assigned review <b>completed</b> by <b>{REVIEWER_NAME}</b>  at <b>{LOCATION}</b>', 
`DESCRIPTION` = 'Review Completed by Reviewer' WHERE (`ACTION_TYPE_CODE` = '19');

UPDATE ``DISCLOSURE_ACTION_TYPE` SET `MESSAGE` = 'Assigned Review by <b>{REVIEWER_NAME}</b> at <b>{LOCATION}</b> <b>completed</b> by <b>{ADMIN_NAME}</b>', 
`DESCRIPTION` = 'Review Completed by Admin with Reviewer' WHERE (`ACTION_TYPE_CODE` = '20');

UPDATE ``DISCLOSURE_ACTION_TYPE` SET `MESSAGE` = 'Assigned Review by <b>{REVIEWER_NAME}</b> at <b>{LOCATION}</b> <b>deleted</b> by <b>{ADMIN_NAME}</b>', 
`DESCRIPTION` = 'Review Deleted with Reviewer' WHERE (`ACTION_TYPE_CODE` = '21');


	 

INSERT INTO ``DISCLOSURE_ACTION_TYPE` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) 
VALUES ('23', 'Review assigned at <b>{LOCATION}</b> with status <b>{REVIEW_STATUS}</b> by <b>{ADMIN_NAME}</b>', 'Review Added without Reviewer', now(), 'quickstart');

INSERT INTO ``DISCLOSURE_ACTION_TYPE` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) 
VALUES ('24', 'Review modified and Assigned at <b>{LOCATION}</b> with status <b>{REVIEW_STATUS}</b> by <b>{ADMIN_NAME}</b>', 'Review Modified without Reviewer', now(), 'quickstart');

INSERT INTO ``DISCLOSURE_ACTION_TYPE` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) 
VALUES ('25', 'Assigned Review started  at <b>{LOCATION}</b> by <b>{ADMIN_NAME}</b>', 'Review Started by Admin without Reviewer', now(), 'quickstart');

INSERT INTO ``DISCLOSURE_ACTION_TYPE` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) 
VALUES ('26', 'Assigned review <b>completed</b>  at <b>{LOCATION}</b> by <b>{ADMIN_NAME}</b>', 'Review Completed by Admin without Reviewer', now(), 'quickstart');

INSERT INTO ``DISCLOSURE_ACTION_TYPE` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) 
VALUES ('27', 'Assigned Review  at <b>{LOCATION}</b>  deleted by <b>{ADMIN_NAME}</b>', 'Review Deleted without Reviewer', now(), 'quickstart');

CREATE TABLE `opa_form_builder_details` (
  `OPA_FORM_BUILDER_DETAILS_ID` INT NOT NULL AUTO_INCREMENT,
  `OPA_DISCLOSURE_ID` INT NOT NULL,
  `OPA_DISCLOSURE_NUMBER` VARCHAR(20) NOT NULL,
  `PERSON_ID` VARCHAR(40) NOT NULL,
  `FORM_BUILDER_ID` INT NOT NULL,
  `IS_PRIMARY_FORM` VARCHAR(1) NOT NULL,
  `UPDATE_TIMESTAMP` DATETIME NOT NULL,
  `UPDATE_USER` VARCHAR(90) NOT NULL,
  PRIMARY KEY (`OPA_FORM_BUILDER_DETAILS_ID`));

INSERT INTO `rights` (`RIGHT_ID`, `RIGHT_NAME`, `DESCRIPTION`, `UPDATE_USER`, `UPDATE_TIMESTAMP`, `RIGHTS_TYPE_CODE`) 
VALUES ((SELECT A.ID FROM (SELECT MAX(RIGHT_ID) + 1 AS ID FROM RIGHTS ) AS A), 'MANAGE_OPA_DISCLOSURE', 'To manage all actions against an OPA disclosure', 'quickstart', now(), '2');

INSERT INTO `rights` (`RIGHT_ID`, `RIGHT_NAME`, `DESCRIPTION`, `UPDATE_USER`, `UPDATE_TIMESTAMP`, `RIGHTS_TYPE_CODE`) 
VALUES ((SELECT A.ID FROM (SELECT MAX(RIGHT_ID) + 1 AS ID FROM RIGHTS ) AS A), 'VIEW_OPA_DISCLOSURE', 'To view any OPA disclosure', 'quickstart', now(), '2');

INSERT INTO `rights` (`RIGHT_ID`, `RIGHT_NAME`, `DESCRIPTION`, `UPDATE_USER`, `UPDATE_TIMESTAMP`, `RIGHTS_TYPE_CODE`) 
VALUES ((SELECT A.ID FROM (SELECT MAX(RIGHT_ID) + 1 AS ID FROM RIGHTS ) AS A), 'VIEW_OPA_DISCLOSURE_PRIVATE_COMMENTS', 'To view private comments tagged in the opa disclosure', 'quickstart', now(), '2');

INSERT INTO `rights` (`RIGHT_ID`, `RIGHT_NAME`, `DESCRIPTION`, `UPDATE_USER`, `UPDATE_TIMESTAMP`, `RIGHTS_TYPE_CODE`) 
VALUES ((SELECT A.ID FROM (SELECT MAX(RIGHT_ID) + 1 AS ID FROM RIGHTS ) AS A), 'VIEW_OPA_DISCLOSURE_PRIVATE_ATTACHMENTS', 'To view private attachments tagged in the opa disclosure', 'quickstart', now(), '2');

INSERT INTO `rights` (`RIGHT_ID`, `RIGHT_NAME`, `DESCRIPTION`, `UPDATE_USER`, `UPDATE_TIMESTAMP`, `RIGHTS_TYPE_CODE`) 
VALUES ((SELECT A.ID FROM (SELECT MAX(RIGHT_ID) + 1 AS ID FROM RIGHTS ) AS A), 'VIEW_ADMIN_GROUP_OPA', 'Allow to view opa based on admin group', 'quickstart', now(), '1');

INSERT INTO `rights` (`RIGHT_ID`, `RIGHT_NAME`, `DESCRIPTION`, `UPDATE_USER`, `UPDATE_TIMESTAMP`, `RIGHTS_TYPE_CODE`) 
VALUES ((SELECT A.ID FROM (SELECT MAX(RIGHT_ID) + 1 AS ID FROM RIGHTS ) AS A), 'OPA_ADMINISTRATOR', 'OPA Administrator', 'quickstart', now(), '1');

INSERT INTO `opa_review_status_type` (`REVIEW_STATUS_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('1', 'Assigned', 'Y', now(), 'quickstart');
INSERT INTO `opa_review_status_type` (`REVIEW_STATUS_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('2', 'In Progress', 'Y', now(), 'quickstart');
INSERT INTO `opa_review_status_type` (`REVIEW_STATUS_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('3', 'Completed', 'Y', now(), 'quickstart');


INSERT INTO `OPA_REVIEW_LOCATION_TYPE` (`LOCATION_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('1', '3rd Party Location', 'Y', now(), 'quickstart');
INSERT INTO `OPA_REVIEW_LOCATION_TYPE` (`LOCATION_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('2', 'Department', 'Y', now(), 'quickstart');
INSERT INTO `OPA_REVIEW_LOCATION_TYPE` (`LOCATION_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('3', 'Legal', 'Y', now(), 'quickstart');
INSERT INTO `OPA_REVIEW_LOCATION_TYPE` (`LOCATION_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('4', 'HOD', 'Y', now(), 'quickstart');

ALTER TABLE `opa_disclosure`
ADD COLUMN `CERTIFICATION_TEXT` varchar(1000) DEFAULT NULL;

ALTER TABLE `opa_disclosure`
ADD COLUMN `CERTIFIED_BY` varchar(60) DEFAULT NULL;

INSERT INTO `opa_disposition_status_type`(`DISPOSITION_STATUS_CODE`,`DESCRIPTION`,`IS_ACTIVE`,`UPDATE_TIMESTAMP`,`UPDATE_USER`)
VALUES('1', 'Pending', 'Y', now(), 'quickstart');
INSERT INTO `opa_disposition_status_type`(`DISPOSITION_STATUS_CODE`,`DESCRIPTION`,`IS_ACTIVE`,`UPDATE_TIMESTAMP`,`UPDATE_USER`)
VALUES('2', 'Void', 'Y', now(), 'quickstart');
INSERT INTO `opa_disposition_status_type`(`DISPOSITION_STATUS_CODE`,`DESCRIPTION`,`IS_ACTIVE`,`UPDATE_TIMESTAMP`,`UPDATE_USER`)
VALUES('3', 'Approved', 'Y', now(), 'quickstart');

INSERT INTO `person_entity_action_type` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`)
		VALUES ('1', 'SFI <b>created</b>', 'Created', now(), 'quickstart');
INSERT INTO `person_entity_action_type` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`)
		VALUES ('2', 'SFI <b>activated</b>', 'Activated', now(), 'quickstart');
INSERT INTO `person_entity_action_type` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`)
		VALUES ('3', 'SFI <b>inactivated</b>', 'Inactivated', now(), 'quickstart');

INSERT INTO `person_entity_action_type` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`)
		VALUES ('4', 'SFI <b>modified</b>', 'Modified', now(), 'quickstart');
INSERT INTO `person_entity_action_type` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`)
		VALUES ('5', '<b>{RELATIONSHIP_NAME}</b> relationship <b>added</b>', 'Relationship Added', now(), 'quickstart');
INSERT INTO `person_entity_action_type` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`)
		VALUES ('6', '<b>{RELATIONSHIP_NAME}</b> relationship <b>deleted</b>', 'Relationship Removed', now(), 'quickstart');


UPDATE `coi_disclosure_type` SET `DESCRIPTION` = 'Financial' WHERE (`DISCLOSURE_TYPE_CODE` = '1');
DELETE FROM `coi_disclosure_type` WHERE (`DISCLOSURE_TYPE_CODE` = '3');
UPDATE `coi_disclosure_type` SET `DESCRIPTION` = 'OPA', `IS_ACTIVE` = 'Y' WHERE (`DISCLOSURE_TYPE_CODE` = '2');
UPDATE `coi_disclosure_type` SET `DESCRIPTION` = 'Travel', `IS_ACTIVE` = 'Y' WHERE (`DISCLOSURE_TYPE_CODE` = '4');

INSERT INTO `coeus_module` (`MODULE_CODE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `IS_ACTIVE`) VALUES ('23', 'OPA', now(), 'admin', 'Y');

INSERT INTO opa_person (`PERSON_ID`,`PERSON_NAME`,`FORM_OF_ADDRESS_SHORT`,`FIRST_NAME`,`MIDDLE_NAME`,`LAST_NAME`,`KRB_NAME_UPPERCASE`,`EMAIL_ADDRESS`,`JOB_ID`,`JOB_TITLE`,`ADMIN_EMPLOYEE_TYPE`,`HR_DEPARTMENT_CODE_OLD`,`HR_DEPARTMENT_NAME`,`ADMIN_ORG_UNIT_ID`,`ADMIN_ORG_UNIT_TITLE`,`ADMIN_POSITION_TITLE`,`PAYROLL_RANK`,`IS_FACULTY`,`EMPLOYMENT_PERCENT`,`IS_CONSULT_PRIV`,`IS_PAID_APPT`,`IS_SUMMER_SESSION_APPT`,`SUMMER_SESSION_MONTHS`,`IS_SABBATICAL`,`SABBATICAL_BEGIN_DATE`,`SABBATICAL_END_DATE`,`WAREHOUSE_LOAD_DATE`) VALUES ('10000000001','Smith, Will',NULL,'Will',NULL,'Smith',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,'Y',NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL,NULL);

INSERT INTO `opa_cycles` (`OPA_CYCLE_NUMBER`, `PERIOD_START_DATE`, `PERIOD_END_DATE`, `OPA_CYCLE_STATUS`, `OPEN_DATE`, `CLOSE_DATE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('1', '2023-04-04', '2023-10-04', 'Y', '2023-04-04', '2023-10-04', now(), 'willsmith');

INSERT INTO person_entity_rel_type (`RELATIONSHIP_TYPE_CODE`,`DESCRIPTION`,`IS_ACTIVE`,`UPDATE_TIMESTAMP`,`UPDATE_USER`) VALUES ('4','Travel','Y',now(),'quickstart');
INSERT INTO person_entity_rel_type (`RELATIONSHIP_TYPE_CODE`,`DESCRIPTION`,`IS_ACTIVE`,`UPDATE_TIMESTAMP`,`UPDATE_USER`) VALUES ('5','Commitment','Y',now(),'quickstart');

INSERT INTO `valid_person_entity_rel_type` (`VALID_PERS_ENTITY_REL_TYP_CODE`, `DISCLOSURE_TYPE_CODE`, `RELATIONSHIP_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('4', '4', '4', 'Travel', 'Y', now(), 'quickstart');
INSERT INTO `valid_person_entity_rel_type` (`VALID_PERS_ENTITY_REL_TYP_CODE`, `DISCLOSURE_TYPE_CODE`, `RELATIONSHIP_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('5', '2', '5', 'Commitment', 'Y', now(), 'quickstart');

INSERT INTO `OPA_ACTION_LOG_TYPE` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) 
VALUES ('8', 'Review assigned to <b>{REVIEWER_NAME}</b> at <b>{LOCATION}</b> with status <b>{REVIEW_STATUS}</b> by Administrator <b>{ADMIN_NAME}</b>', 'Review Added with Reviewer', now(), 'quickstart');

INSERT INTO `OPA_ACTION_LOG_TYPE` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) 
VALUES ('9', 'Review assigned at <b>{LOCATION}</b> with status <b>{REVIEW_STATUS}</b> by <b>{ADMIN_NAME}</b>', 'Review Added without Reviewer', now(), 'quickstart');

INSERT INTO `OPA_ACTION_LOG_TYPE` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) 
VALUES ('10', 'Review modified and assigned to <b>{REVIEWER_NAME}</b> at <b>{LOCATION}</b> with status <b>{REVIEW_STATUS}</b> by <b>{ADMIN_NAME}</b>', 'Review Modified with Reviewer', now(), 'quickstart');

INSERT INTO `OPA_ACTION_LOG_TYPE` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) 
VALUES ('11', 'Review modified and Assigned at <b>{LOCATION}</b> with status <b>{REVIEW_STATUS}</b> by <b>{ADMIN_NAME}</b>', 'Review Modified without Reviewer', now(), 'quickstart');

INSERT INTO `OPA_ACTION_LOG_TYPE` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) 
VALUES ('12', 'Assigned Review <b>started</b> by <b>{ADMIN_NAME}</b> at <b>{LOCATION}</b>', 'Review Started by Admin with Reviewer', now(), 'quickstart');

INSERT INTO `OPA_ACTION_LOG_TYPE` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) 
VALUES ('13', 'Assigned Review started at <b>{LOCATION}</b> by <b>{ADMIN_NAME}</b>', 'Review Started by Admin without Reviewer', now(), 'quickstart');

INSERT INTO `OPA_ACTION_LOG_TYPE` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) 
VALUES ('14', 'Assigned Review <b>started</b> by <b>{REVIEWER_NAME}</b> at <b>{LOCATION}</b>', 'Review Started by Reviewer', now(), 'quickstart');

INSERT INTO `OPA_ACTION_LOG_TYPE` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) 
VALUES ('15', 'Assigned review <b>completed</b> at <b>{LOCATION}</b> by <b>{ADMIN_NAME}</b>', 'Review Completed by Admin without Reviewer', now(), 'quickstart');

INSERT INTO `OPA_ACTION_LOG_TYPE` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) 
VALUES ('16', 'Assigned Review by <b>{REVIEWER_NAME}</b> at <b>{LOCATION}</b> <b>completed</b> by <b>{ADMIN_NAME}</b>', 'Review Completed by Admin with Reviewer', now(), 'quickstart');

INSERT INTO `OPA_ACTION_LOG_TYPE` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) 
VALUES ('17', 'Assigned review <b>completed</b> by <b>{REVIEWER_NAME}</b> at <b>{LOCATION}</b>', 'Review Completed by Reviewer', now(), 'quickstart');

INSERT INTO `OPA_ACTION_LOG_TYPE` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) 
VALUES ('18', 'Assigned Review at <b>{LOCATION}</b> deleted by <b>{ADMIN_NAME}</b>', 'Review Deleted without Reviewer', now(), 'quickstart');

INSERT INTO `OPA_ACTION_LOG_TYPE` (`ACTION_TYPE_CODE`, `MESSAGE`, `DESCRIPTION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) 
VALUES ('19', 'Assigned Review by <b>{REVIEWER_NAME}</b> at <b>{LOCATION}</b> <b>deleted</b> by <b>{ADMIN_NAME}</b>', 'Review Deleted with Reviewer', now(), 'quickstart');

UPDATE `LOOKUP_WINDOW` SET `TABLE_NAME` = 'VALID_PERSON_ENTITY_REL_TYPE', `COLUMN_NAME` = 'VALID_PERS_ENTITY_REL_TYP_CODE' 
WHERE (`LOOKUP_WINDOW_NAME` = 'PERSON_ENTITY_REL_TYPE_LOOKUP');


INSERT INTO `person_entity_rel_type`(`RELATIONSHIP_TYPE_CODE`,`DESCRIPTION`,`IS_ACTIVE`,`UPDATE_TIMESTAMP`,`UPDATE_USER`)
VALUES('1','Self','Y',now(),'quickstart');
INSERT INTO `person_entity_rel_type`(`RELATIONSHIP_TYPE_CODE`,`DESCRIPTION`,`IS_ACTIVE`,`UPDATE_TIMESTAMP`,`UPDATE_USER`)
VALUES('2','Spouse','Y',now(),'quickstart');
INSERT INTO `person_entity_rel_type`(`RELATIONSHIP_TYPE_CODE`,`DESCRIPTION`,`IS_ACTIVE`,`UPDATE_TIMESTAMP`,`UPDATE_USER`)
VALUES('3','Dependant','Y',now(),'quickstart');

INSERT INTO `coi_disclosure_type` (`DISCLOSURE_TYPE_CODE`,`DESCRIPTION`,`IS_ACTIVE`,`UPDATE_TIMESTAMP`,`UPDATE_USER`) 
VALUES ('1','Financial Conflict of Interest','Y',now(),'quickstart');
INSERT INTO `coi_disclosure_type` (`DISCLOSURE_TYPE_CODE`,`DESCRIPTION`,`IS_ACTIVE`,`UPDATE_TIMESTAMP`,`UPDATE_USER`) 
VALUES ('2','Other Professional Activities','Y',now(),'quickstart');
INSERT INTO `coi_disclosure_type` (`DISCLOSURE_TYPE_CODE`,`DESCRIPTION`,`IS_ACTIVE`,`UPDATE_TIMESTAMP`,`UPDATE_USER`) 
VALUES ('3','Travel Disclosures','Y',now(),'quickstart');

INSERT INTO `VALID_PERSON_ENTITY_REL_TYPE` (`VALID_PERS_ENTITY_REL_TYP_CODE`, `DISCLOSURE_TYPE_CODE`, `RELATIONSHIP_TYPE_CODE`, `DESCRIPTION`, 
`IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('1', '1', '1', 'FCOI : Self', 'Y', now(), 'quickstart');
INSERT INTO `VALID_PERSON_ENTITY_REL_TYPE` (`VALID_PERS_ENTITY_REL_TYP_CODE`, `DISCLOSURE_TYPE_CODE`, `RELATIONSHIP_TYPE_CODE`, `DESCRIPTION`, 
`IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('2', '1', '2', 'FCOI : Spouse', 'Y', now(), 'quickstart');
INSERT INTO `VALID_PERSON_ENTITY_REL_TYPE` (`VALID_PERS_ENTITY_REL_TYP_CODE`, `DISCLOSURE_TYPE_CODE`, `RELATIONSHIP_TYPE_CODE`, `DESCRIPTION`, 
`IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('3', '1', '3', 'FCOI : Dependant', 'Y', now(), 'quickstart');
INSERT INTO `VALID_PERSON_ENTITY_REL_TYPE` (`VALID_PERS_ENTITY_REL_TYP_CODE`, `DISCLOSURE_TYPE_CODE`, `RELATIONSHIP_TYPE_CODE`, `DESCRIPTION`, 
`IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('4', '2', '1', 'OPA : Self', 'Y', now(), 'quickstart');
INSERT INTO `VALID_PERSON_ENTITY_REL_TYPE` (`VALID_PERS_ENTITY_REL_TYP_CODE`, `DISCLOSURE_TYPE_CODE`, `RELATIONSHIP_TYPE_CODE`, `DESCRIPTION`, 
`IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES ('5', '3', '1', 'Travel : Self', 'Y', now(), 'quickstart');

DROP PROCEDURE IF EXISTS GET_COI_DISCLOSURE_DASHBOARD;
DROP PROCEDURE IF EXISTS GET_COI_DISCLOSURE_DASHBOARD_COUNT;
DROP PROCEDURE IF EXISTS GET_COI_DISCLOSURE_ADMIN_DASHBOARD;
DROP PROCEDURE IF EXISTS GET_COI_DISCLOSURE_ADMIN_DASHBOARD_COUNT;
DROP PROCEDURE IF EXISTS GET_ALL_SYSTEM_ENTITY_LIST_COUNT;
DROP PROCEDURE IF EXISTS SYNC_PROJECTS_DISCLOSURE;
DROP FUNCTION IF EXISTS FN_SYNC_SFI_WITH_FCOI_DISC;
DROP PROCEDURE IF EXISTS GET_COI_DISCLOSURE_DASHBOARD;
DROP PROCEDURE IF EXISTS GET_COI_DISCLOSURE_REVIEWER_DASHBOARD;
DROP PROCEDURE IF EXISTS GET_COI_DISCLOSURE_DASHBOARD;
DROP PROCEDURE IF EXISTS GET_ALL_RIGHTS_FOR_A_MODULE;
DROP PROCEDURE IF EXISTS SYNC_SFIS_AND_DISCLOSURE;
DROP PROCEDURE IF EXISTS VALIDATE_PROJECT_DISCLOSURE;
DROP PROCEDURE IF EXISTS GET_PERSON_ENTITIES;
DROP FUNCTION IF EXISTS FN_PERSON_HAS_AUTHORIZATION;
DROP PROCEDURE IF EXISTS GET_COI_DISCLOSURE_REVIEWER_DASHBOARD;
DROP PROCEDURE IF EXISTS GET_COI_DISCLOSURE_REVIEWER_DASHBOARD_COUNT;
DROP PROCEDURE IF EXISTS GET_PERSON_ENTITY_DASHBOARD;
DROP PROCEDURE IF EXISTS GET_PERSON_ENTITY_DASHBOARD_COUNT;
DROP PROCEDURE IF EXISTS COI_VALIDATE_DISCLOSURE_CONFLICTS;
DROP PROCEDURE IF EXISTS COI_EVALUATE_VALIDATION;
DROP PROCEDURE IF EXISTS GENERATE_ATTACHMENT_NUMBER;
DROP PROCEDURE IF EXISTS RULE_EVALUATE_QUESTION;
DROP PROCEDURE IF EXISTS COI_UPD_ANS_TO_NEW_QUEST_VERSN;
DROP PROCEDURE IF EXISTS INSERT_QUESTIONNAIRE_ANSWERS;
DROP FUNCTION IF EXISTS FN_EVAL_DISCLOSURE_QUESTIONNAIRE;
DROP PROCEDURE IF EXISTS GET_DISCLOSURE_RELATIONS;
DROP PROCEDURE IF EXISTS GET_COI_DISCLOSURE_HISTORY;
DROP VIEW IF EXISTS `coi_project_award_v`;
DROP VIEW IF EXISTS `coi_project_proposal_v`;
DROP PROCEDURE IF EXISTS GET_COI_TRAVEL_DISCLOSURE_ADMIN_DASHBOARD;
DROP PROCEDURE IF EXISTS GET_ALL_SYSTEM_ENTITY_LIST;
DROP PROCEDURE IF EXISTS RULE_EVALUATE_VARIABLE;
DROP PROCEDURE IF EXISTS GET_SFI_DASHBOARD;
DROP PROCEDURE IF EXISTS GET_COI_DISCLOSURES_FOR_BANNER;
DROP FUNCTION IF EXISTS FN_CAN_CREATE_OPA_DISCLOSURE;
DROP PROCEDURE IF EXISTS GENERATE_OPA_DISCLOSURE_NUMBER;
DROP PROCEDURE IF EXISTS INSERT_OPA_DISCLOSURE_DETAILS;
DROP PROCEDURE IF EXISTS GET_COI_OPA_DASHBOARD;
DROP PROCEDURE IF EXISTS RULE_EVALUATE_FUNCTION;
DROP PROCEDURE IF EXISTS RULE_EVALUATE_EXPRESSION;
DROP PROCEDURE IF EXISTS GET_RULE_EVALUATE_VALIDATION;
DROP PROCEDURE IF EXISTS GET_RULE_EVALUATE_NOTIFICATION;
DROP PROCEDURE IF EXISTS GET_COI_ENTITY_PERSON_ENTITY_DETAILS;
DROP PROCEDURE IF EXISTS GET_COI_PERSON_ENTITY_REL_DETAILS;
DROP PROCEDURE IF EXISTS PROC_SYNC_OPA_PER_ENTITY;


\. ./Procedures/GET_COI_DISCLOSURE_DASHBOARD.sql
\. ./Procedures/GET_COI_DISCLOSURE_DASHBOARD_COUNT.sql
\. ./Procedures/GET_COI_DISCLOSURE_ADMIN_DASHBOARD.sql
\. ./Procedures/GET_COI_DISCLOSURE_ADMIN_DASHBOARD_COUNT.sql
\. ./Procedures/GET_ALL_SYSTEM_ENTITY_LIST_COUNT.sql
\. ./Procedures/SYNC_PROJECTS_DISCLOSURE.sql
\. ./Procedures/GET_COI_DISCLOSURE_DASHBOARD.sql
\. ./Procedures/GET_COI_DISCLOSURE_REVIEWER_DASHBOARD.sql
\. ./Procedures/GET_COI_DISCLOSURE_DASHBOARD.sql
\. ./Procedures/GET_ALL_RIGHTS_FOR_A_MODULE.sql
\. ./Procedures/SYNC_SFIS_AND_DISCLOSURE.sql;
\. ./Procedures/VALIDATE_PROJECT_DISCLOSURE.sql;
\. ./Procedures/GET_PERSON_ENTITIES.sql;
\. ./Functions/FN_PERSON_HAS_AUTHORIZATION.sql;
\. ./Procedures/GET_COI_DISCLOSURE_REVIEWER_DASHBOARD.sql
\. ./Procedures/GET_COI_DISCLOSURE_REVIEWER_DASHBOARD_COUNT.sql
\. ./Procedures/GET_PERSON_ENTITY_DASHBOARD.sql
\. ./Procedures/GET_PERSON_ENTITY_DASHBOARD_COUNT.sql
\. ./Procedures/COI_VALIDATE_DISCLOSURE_CONFLICTS.sql;
\. ./Procedures/COI_EVALUATE_VALIDATION.sql;
\. ./Procedures/GENERATE_ATTACHMENT_NUMBER.sql;
\. ./Procedures/RULE_EVALUATE_QUESTION.sql
\. ./Procedures/COI_UPD_ANS_TO_NEW_QUEST_VERSN.sql
\. ./Procedures/INSERT_QUESTIONNAIRE_ANSWERS.sql
\. ./Functions/FN_EVAL_DISCLOSURE_QUESTIONNAIRE.sql;
\. ./Procedures/GET_DISCLOSURE_RELATIONS.sql
\. ./Procedures/GET_COI_DISCLOSURE_HISTORY.sql
\. ./views/coi_project_award_v.sql;
\. ./views/coi_project_proposal_v.sql;
\. ./Procedures/GET_COI_TRAVEL_DISCLOSURE_ADMIN_DASHBOARD.sql;
\. ./Procedures/GET_ALL_SYSTEM_ENTITY_LIST.sql
\. ./Procedures/RULE_EVALUATE_VARIABLE.sql;
\. ./Procedures/GET_SFI_DASHBOARD.sql
\. ./Procedures/GET_COI_DISCLOSURES_FOR_BANNER.sql
\. ./Functions/FN_CAN_CREATE_OPA_DISCLOSURE.sql
\. ./Procedures/GENERATE_OPA_DISCLOSURE_NUMBER.sql
\. ./Procedures/INSERT_OPA_DISCLOSURE_DETAILS.sql
\. ./Procedures/GET_COI_OPA_DASHBOARD.sql;
\. ./Procedures/RULE_EVALUATE_FUNCTION.sql;
\. ./Procedures/RULE_EVALUATE_EXPRESSION.sql;
\. ./Procedures/GET_RULE_EVALUATE_VALIDATION.sql;
\. ./Procedures/GET_RULE_EVALUATE_NOTIFICATION.sql;
\. ./Procedures/GET_COI_ENTITY_PERSON_ENTITY_DETAILS.sql;
\. ./Procedures/GET_COI_PERSON_ENTITY_REL_DETAILS.sql;
\. ./Procedures/PROC_SYNC_OPA_PER_ENTITY.sql;

