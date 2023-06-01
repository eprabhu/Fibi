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
DROP PROCEDURE IF EXISTS FN_PERSON_HAS_AUTHORIZATION;
DROP PROCEDURE IF EXISTS GET_COI_DISCLOSURE_REVIEWER_DASHBOARD;
DROP PROCEDURE IF EXISTS GET_COI_DISCLOSURE_REVIEWER_DASHBOARD_COUNT;
DROP PROCEDURE IF EXISTS GET_PERSON_ENTITY_DASHBOARD;
DROP PROCEDURE IF EXISTS GET_PERSON_ENTITY_DASHBOARD_COUNT;

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

