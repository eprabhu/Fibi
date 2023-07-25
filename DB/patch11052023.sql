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

\. ./Procedures/GET_COI_DISCLOSURE_DASHBOARD.sql
\. ./Procedures/GET_COI_DISCLOSURE_DASHBOARD_COUNT.sql
\. ./Procedures/GET_COI_DISCLOSURE_ADMIN_DASHBOARD.sql
\. ./Procedures/GET_COI_DISCLOSURE_ADMIN_DASHBOARD_COUNT.sql
\. ./Procedures/GET_ALL_SYSTEM_ENTITY_LIST_COUNT.sql
\. ./Procedures/SYNC_PROJECTS_DISCLOSURE.sql
\. ./Functions/FN_SYNC_SFI_WITH_FCOI_DISC.sql
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

DROP VIEW IF EXISTS `coi_project_award_v`;
CREATE VIEW `coi_project_award_v` AS
    SELECT 
        NULL AS `ID`,
        `t1`.`AWARD_ID` AS `EXTERNAL_SYSTEM_REF_ID`,
        '1' AS `COI_PROJECT_TYPE_CODE`,
        `t1`.`AWARD_NUMBER` AS `AWARD_NUMBER`,
        `t1`.`ACCOUNT_NUMBER` AS `ACCOUNT_NUMBER`,
        `t1`.`SPONSOR_AWARD_NUMBER` AS `SPONSOR_AWARD_NUMBER`,
        `t1`.`TITLE` AS `TITLE`,
        (SELECT 
                `award_persons`.`PERSON_ID`
            FROM
                `award_persons`
            WHERE
                ((`award_persons`.`PI_FLAG` = 'Y')
                    AND (`award_persons`.`AWARD_ID` = `t1`.`AWARD_ID`)
                    AND ((`t1`.`AWARD_SEQUENCE_STATUS` = 'ACTIVE')
                    OR ((`t1`.`AWARD_SEQUENCE_STATUS` = 'PENDING')
                    AND (`t1`.`AWARD_DOCUMENT_TYPE_CODE` = 1))))
            LIMIT 1) AS `PI_PERSON_ID`,
        (SELECT 
                `award_persons`.`FULL_NAME`
            FROM
                `award_persons`
            WHERE
                (`award_persons`.`PERSON_ID` = `PI_PERSON_ID`)
            LIMIT 1) AS `PI_NAME`,
        `t5`.`PERSON_ID` AS `KEY_PERSON_ID`,
        `t7`.`FULL_NAME` AS `KEY_PERSON_NAME`,
        `t5`.`PERSON_ROLE_ID` AS `KEY_PERSON_ROLE_CODE`,
        `t6`.`DESCRIPTION` AS `AWARD_STATUS`,
        `t4`.`SPONSOR_NAME` AS `SPONSOR_NAME`,
        `t40`.`SPONSOR_NAME` AS `PRIME_SPONSOR_NAME`,
        `t1`.`BEGIN_DATE` AS `AWARD_START_DATE`,
        `t1`.`FINAL_EXPIRATION_DATE` AS `AWARD_END_DATE`,
        `t1`.`LEAD_UNIT_NUMBER` AS `LEAD_UNIT_NUMBER`,
        `t3`.`unit_name` AS `LEAD_UNIT_NAME`,
        `t5`.`PROJECT_ROLE` AS `PROJECT_ROLE`
    FROM
        ((((((`award` `t1`
        JOIN `unit` `t3` ON ((`t3`.`UNIT_NUMBER` = `t1`.`LEAD_UNIT_NUMBER`)))
        JOIN `sponsor` `t4` ON ((`t4`.`SPONSOR_CODE` = `t1`.`SPONSOR_CODE`)))
        LEFT JOIN `sponsor` `t40` ON ((`t40`.`SPONSOR_CODE` = `t1`.`PRIME_SPONSOR_CODE`)))
        JOIN `award_persons` `t5` ON ((`t5`.`AWARD_ID` = `t1`.`AWARD_ID`)))
        JOIN `award_status` `t6` ON ((`t6`.`STATUS_CODE` = `t1`.`STATUS_CODE`)))
        JOIN `person` `t7` ON ((`t5`.`PERSON_ID` = `t7`.`PERSON_ID`)))
    WHERE
        ((`t1`.`AWARD_SEQUENCE_STATUS` = 'ACTIVE')
            OR ((`t1`.`AWARD_SEQUENCE_STATUS` = 'PENDING')
            AND (`t1`.`AWARD_DOCUMENT_TYPE_CODE` = 1))) 
    UNION ALL SELECT 
        `coi_project_award`.`ID` AS `ID`,
        `coi_project_award`.`EXTERNAL_SYSTEM_REF_ID` AS `EXTERNAL_SYSTEM_REF_ID`,
        `coi_project_award`.`COI_PROJECT_TYPE_CODE` AS `COI_PROJECT_TYPE_CODE`,
        `coi_project_award`.`AWARD_NUMBER` AS `AWARD_NUMBER`,
        `coi_project_award`.`ACCOUNT_NUMBER` AS `ACCOUNT_NUMBER`,
        `coi_project_award`.`SPONSOR_AWARD_NUMBER` AS `SPONSOR_AWARD_NUMBER`,
        `coi_project_award`.`TITLE` AS `TITLE`,
        `coi_project_award`.`PI_PERSON_ID` AS `PI_PERSON_ID`,
        `coi_project_award`.`PI_NAME` AS `PI_NAME`,
        `coi_project_award`.`KEY_PERSON_ID` AS `KEY_PERSON_ID`,
        `coi_project_award`.`KEY_PERSON_NAME` AS `KEY_PERSON_NAME`,
        `coi_project_award`.`KEY_PERSON_ROLE_CODE` AS `KEY_PERSON_ROLE_CODE`,
        NULL AS `AWARD_STATUS`,
        `coi_project_award`.`SPONSOR_NAME` AS `SPONSOR_NAME`,
        `coi_project_award`.`PRIME_SPONSOR_NAME` AS `PRIME_SPONSOR_NAME`,
        `coi_project_award`.`AWARD_START_DATE` AS `AWARD_START_DATE`,
        `coi_project_award`.`AWARD_END_DATE` AS `AWARD_END_DATE`,
        NULL AS `LEAD_UNIT_NUMBER`,
        `coi_project_award`.`LEAD_UNIT_NAME` AS `LEAD_UNIT_NAME`,
        `coi_project_award`.`PROJECT_ROLE` AS `PROJECT_ROLE`
    FROM
        `coi_project_award`
    WHERE
        (`coi_project_award`.`COI_PROJECT_TYPE_CODE` = 2);

DROP VIEW IF EXISTS `coi_project_proposal_v`;
CREATE VIEW `coi_project_proposal_v` AS
    SELECT 
        NULL AS `ID`,
        `t1`.`PROPOSAL_ID` AS `EXTERNAL_SYSTEM_REF_ID`,
        '3' AS `COI_PROJECT_TYPE_CODE`,
        `t1`.`PROPOSAL_ID` AS `PROPOSAL_NUMBER`,
        `t1`.`TITLE` AS `TITLE`,
        (SELECT 
                `eps_proposal_persons`.`PERSON_ID`
            FROM
                `eps_proposal_persons`
            WHERE
                ((`eps_proposal_persons`.`PROP_PERSON_ROLE_ID` = 3)
                    AND (`eps_proposal_persons`.`PI_FLAG` = 'Y')
                    AND (`eps_proposal_persons`.`PROPOSAL_ID` = `t1`.`PROPOSAL_ID`)
                    AND (`t1`.`DOCUMENT_STATUS_CODE` <> 3)
                    AND (`t1`.`STATUS_CODE` NOT IN (1 , 29, 35, 9, 22, 20, 24, 30, 3, 12, 11))
                    AND (`t1`.`DOCUMENT_STATUS_CODE` <> 3))
            LIMIT 1) AS `PI_PERSON_ID`,
        (SELECT 
                `eps_proposal_persons`.`FULL_NAME`
            FROM
                `eps_proposal_persons`
            WHERE
                (`eps_proposal_persons`.`PERSON_ID` = `PI_PERSON_ID`)
            LIMIT 1) AS `PI_NAME`,
        `t6`.`PERSON_ID` AS `KEY_PERSON_ID`,
        `t7`.`FULL_NAME` AS `KEY_PERSON_NAME`,
        `t6`.`PROP_PERSON_ROLE_ID` AS `KEY_PERSON_ROLE_CODE`,
        `t3`.`DESCRIPTION` AS `PROPOSAL_STATUS`,
        `t4`.`SPONSOR_NAME` AS `SPONSOR_NAME`,
        `t5`.`SPONSOR_NAME` AS `PRIME_SPONSOR_NAME`,
        `t1`.`START_DATE` AS `PROPOSAL_START_DATE`,
        `t1`.`END_DATE` AS `PROPOSAL_END_DATE`,
        `t1`.`HOME_UNIT_NUMBER` AS `LEAD_UNIT_NUMBER`,
        `t1`.`HOME_UNIT_NAME` AS `LEAD_UNIT_NAME`
    FROM
        (((((`eps_proposal` `t1`
        JOIN `eps_proposal_status` `t3` ON ((`t3`.`STATUS_CODE` = `t1`.`STATUS_CODE`)))
        JOIN `sponsor` `t4` ON ((`t4`.`SPONSOR_CODE` = `t1`.`SPONSOR_CODE`)))
        LEFT JOIN `sponsor` `t5` ON ((`t5`.`SPONSOR_CODE` = `t1`.`PRIME_SPONSOR_CODE`)))
        JOIN `eps_proposal_persons` `t6` ON ((`t6`.`PROPOSAL_ID` = `t1`.`PROPOSAL_ID`)))
        JOIN `person` `t7` ON ((`t6`.`PERSON_ID` = `t7`.`PERSON_ID`)))
    WHERE
        ((`t1`.`DOCUMENT_STATUS_CODE` <> 3)
            AND (`t1`.`STATUS_CODE` NOT IN (1 , 29, 35, 9, 22, 20, 24, 30, 3, 12, 11))
            AND (`t1`.`DOCUMENT_STATUS_CODE` <> 3)) 
    UNION ALL SELECT 
        `coi_project_proposal`.`ID` AS `ID`,
        `coi_project_proposal`.`EXTERNAL_SYSTEM_REF_ID` AS `EXTERNAL_SYSTEM_REF_ID`,
        `coi_project_proposal`.`COI_PROJECT_TYPE_CODE` AS `COI_PROJECT_TYPE_CODE`,
        `coi_project_proposal`.`PROPOSAL_NUMBER` AS `PROPOSAL_NUMBER`,
        `coi_project_proposal`.`TITLE` AS `TITLE`,
        `coi_project_proposal`.`PI_PERSON_ID` AS `PI_PERSON_ID`,
        `coi_project_proposal`.`PI_NAME` AS `PI_NAME`,
        `coi_project_proposal`.`KEY_PERSON_ID` AS `KEY_PERSON_ID`,
        `coi_project_proposal`.`KEY_PERSON_NAME` AS `KEY_PERSON_NAME`,
        `coi_project_proposal`.`KEY_PERSON_ROLE_CODE` AS `KEY_PERSON_ROLE_CODE`,
        NULL AS `PROPOSAL_STATUS`,
        `coi_project_proposal`.`SPONSOR_NAME` AS `SPONSOR_NAME`,
        `coi_project_proposal`.`PRIME_SPONSOR_NAME` AS `PRIME_SPONSOR_NAME`,
        `coi_project_proposal`.`PROPOSAL_START_DATE` AS `PROPOSAL_START_DATE`,
        `coi_project_proposal`.`PROPOSAL_END_DATE` AS `PROPOSAL_END_DATE`,
        NULL AS `LEAD_UNIT_NUMBER`,
        `coi_project_proposal`.`LEAD_UNIT_NAME` AS `LEAD_UNIT_NAME`
    FROM
        `coi_project_proposal`
    WHERE
        (`coi_project_proposal`.`COI_PROJECT_TYPE_CODE` = 4);

