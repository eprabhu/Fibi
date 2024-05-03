ALTER TABLE `form_builder_section_component` 
ADD COLUMN `IS_MANDATORY` VARCHAR(1) NULL AFTER `UPDATE_USER`,
ADD COLUMN `VALIDATION_MESSAGE` VARCHAR(2000) NULL AFTER `IS_MANDATORY`,
ADD COLUMN `LABEL` VARCHAR(300) NULL AFTER `VALIDATION_MESSAGE`;

ALTER TABLE `entity` 
CHANGE COLUMN `ENTITY_NAME` `ENTITY_NAME` VARCHAR(500) NULL DEFAULT NULL ;

ALTER TABLE `entity_action_log` 
CHANGE COLUMN `DESCRIPTION` `DESCRIPTION` VARCHAR(600) NULL DEFAULT NULL ;