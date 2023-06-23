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

