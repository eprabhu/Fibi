SET FOREIGN_KEY_CHECKS=0;

ALTER TABLE `form_section_component_type` 
ADD COLUMN `SORT_ORDER` INT NULL AFTER `UPDATE_USER`;

Truncate form_section_component_type;

INSERT INTO `form_section_component_type` (`COMPONENT_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `SORT_ORDER`) VALUES ('AS', 'Autosuggest', 'N', now(), 'quickstart', '7');
INSERT INTO `form_section_component_type` (`COMPONENT_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `SORT_ORDER`) VALUES ('BR', 'Context Break', 'Y', now(), 'quickstart', '2');
INSERT INTO `form_section_component_type` (`COMPONENT_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `SORT_ORDER`) VALUES ('CB', 'Check Box', 'Y', now(), 'quickstart', '8');
INSERT INTO `form_section_component_type` (`COMPONENT_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `SORT_ORDER`) VALUES ('CE', 'Custom Element', 'N', now(), 'quickstart', '5');
INSERT INTO `form_section_component_type` (`COMPONENT_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `SORT_ORDER`) VALUES ('DE', 'Date', 'Y', now(), 'quickstart', '9');
INSERT INTO `form_section_component_type` (`COMPONENT_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `SORT_ORDER`) VALUES ('ES', 'Elastic Search', 'N', now(), 'quickstart', '10');
INSERT INTO `form_section_component_type` (`COMPONENT_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `SORT_ORDER`) VALUES ('HL', 'Horizontal Line', 'Y', now(), 'quickstart', '1');
INSERT INTO `form_section_component_type` (`COMPONENT_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `SORT_ORDER`) VALUES ('NE', 'Number', 'Y', now(), 'quickstart', '11');
INSERT INTO `form_section_component_type` (`COMPONENT_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `SORT_ORDER`) VALUES ('PE', 'Programmed Element', 'Y', now(), 'quickstart', '6');
INSERT INTO `form_section_component_type` (`COMPONENT_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `SORT_ORDER`) VALUES ('QN', 'Questionnaire', 'Y', now(), 'quickstart', '4');
INSERT INTO `form_section_component_type` (`COMPONENT_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `SORT_ORDER`) VALUES ('RB', 'Radio Button', 'Y', now(), 'quickstart', '12');
INSERT INTO `form_section_component_type` (`COMPONENT_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `SORT_ORDER`) VALUES ('RT', 'Rich Textbox', 'Y', now(), 'quickstart', '3');
INSERT INTO `form_section_component_type` (`COMPONENT_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `SORT_ORDER`) VALUES ('SD', 'System Dropdown', 'N', now(), 'quickstart', '13');
INSERT INTO `form_section_component_type` (`COMPONENT_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `SORT_ORDER`) VALUES ('SE', 'String', 'Y', now(), 'quickstart', '14');
INSERT INTO `form_section_component_type` (`COMPONENT_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `SORT_ORDER`) VALUES ('TE', 'Text', 'Y', now(), 'quickstart', '15');
INSERT INTO `form_section_component_type` (`COMPONENT_TYPE_CODE`, `DESCRIPTION`, `IS_ACTIVE`, `UPDATE_TIMESTAMP`, `UPDATE_USER`, `SORT_ORDER`) VALUES ('UD', 'User Dropdown', 'N', now(), 'quickstart', '16');

SET FOREIGN_KEY_CHECKS=1;
