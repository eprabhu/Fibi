SET FOREIGN_KEY_CHECKS=0;

ALTER TABLE `form_section_component_type` 
ADD COLUMN `SORT_ORDER` INT NULL AFTER `UPDATE_USER`;

Truncate form_section_component_type;

INSERT INTO `form_section_component_type` (`COMPONENT_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `SORT_ORDER`) VALUES ('AS', 'Autosuggest', 'N', '2020-03-18 09:17:16', 'admin', '7');
INSERT INTO `form_section_component_type` (`COMPONENT_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `SORT_ORDER`) VALUES ('BR', 'Context Break', 'Y', '2023-08-31 09:36:02', 'quickstart', '2');
INSERT INTO `form_section_component_type` (`COMPONENT_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `SORT_ORDER`) VALUES ('CB', 'Check Box', 'Y', '2019-08-09 09:42:36', 'admin', '8');
INSERT INTO `form_section_component_type` (`COMPONENT_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `SORT_ORDER`) VALUES ('CE', 'Custom Element', 'N', '2023-08-31 09:36:02', 'quickstart', '5');
INSERT INTO `form_section_component_type` (`COMPONENT_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `SORT_ORDER`) VALUES ('DE', 'Date', 'Y', '2019-08-09 09:42:36', 'admin', '9');
INSERT INTO `form_section_component_type` (`COMPONENT_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `SORT_ORDER`) VALUES ('ES', 'Elastic Search', 'N', '2020-03-18 09:17:15', 'admin', '10');
INSERT INTO `form_section_component_type` (`COMPONENT_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `SORT_ORDER`) VALUES ('HL', 'Horizontal Line', 'Y', '2023-08-31 09:36:02', 'quickstart', '1');
INSERT INTO `form_section_component_type` (`COMPONENT_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `SORT_ORDER`) VALUES ('NE', 'Number', 'Y', '2019-08-09 09:42:36', 'admin', '11');
INSERT INTO `form_section_component_type` (`COMPONENT_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `SORT_ORDER`) VALUES ('PE', 'Programmed Element', 'Y', '2023-08-31 09:36:02', 'quickstart', '6');
INSERT INTO `form_section_component_type` (`COMPONENT_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `SORT_ORDER`) VALUES ('QN', 'Questionnaire', 'Y', '2023-08-31 09:36:02', 'quickstart', '4');
INSERT INTO `form_section_component_type` (`COMPONENT_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `SORT_ORDER`) VALUES ('RB', 'Radio Button', 'Y', '2019-08-09 09:42:36', 'admin', '12');
INSERT INTO `form_section_component_type` (`COMPONENT_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `SORT_ORDER`) VALUES ('RT', 'Rich Textbox', 'Y', '2023-08-31 09:36:02', 'quickstart', '3');
INSERT INTO `form_section_component_type` (`COMPONENT_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `SORT_ORDER`) VALUES ('SD', 'System Dropdown', 'N', '2020-03-18 09:17:16', 'admin', '13');
INSERT INTO `form_section_component_type` (`COMPONENT_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `SORT_ORDER`) VALUES ('SE', 'String', 'Y', '2019-08-09 09:42:36', 'admin', '14');
INSERT INTO `form_section_component_type` (`COMPONENT_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `SORT_ORDER`) VALUES ('TE', 'Text', 'Y', '2022-05-05 09:26:40', 'admin', '15');
INSERT INTO `form_section_component_type` (`COMPONENT_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `SORT_ORDER`) VALUES ('UD', 'User Dropdown', 'N', '2020-03-18 09:17:16', 'admin', '16');

SET FOREIGN_KEY_CHECKS=1;
