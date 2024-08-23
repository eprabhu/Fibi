CREATE TABLE `coi_discl_projects` (
  `COI_DISCL_PROJECTS_ID` INT NOT NULL AUTO_INCREMENT,
  `DISCLOSURE_ID` INT NULL,
  `DISCLOSURE_NUMBER` INT NULL,
  `MODULE_CODE` VARCHAR(10) NULL,
  `MODULE_ITEM_KEY` VARCHAR(45) NULL,
  `UPDATED_BY` VARCHAR(40) NULL,
  `UPDATE_TIMESTAMP` DATETIME NULL,
  PRIMARY KEY (`COI_DISCL_PROJECTS_ID`),
  INDEX `COI_DISCL_PROJECTS_FK1_idx` (`DISCLOSURE_ID` ASC) VISIBLE,
  CONSTRAINT `COI_DISCL_PROJECTS_FK1`
    FOREIGN KEY (`DISCLOSURE_ID`)
    REFERENCES `coi_disclosure` (`DISCLOSURE_ID`)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION);
	
	
	
CREATE TABLE `coi_discl_project_entity_rel` (
  `COI_DISCL_PROJECT_ENTITY_REL_ID` int NOT NULL AUTO_INCREMENT,
  `COI_DISCL_PROJECTS_ID` int DEFAULT NULL,
  `PERSON_ENTITY_ID` int DEFAULT NULL,
  `PERSON_ENTITY_NUMBER` int DEFAULT NULL,
  `PREVIOUS_PERSON_ENTITY_ID` int DEFAULT NULL,
  `ENTITY_ID` int DEFAULT NULL,
  `PROJECT_CONFLICT_STATUS_CODE` varchar(10) DEFAULT NULL,
  `UPDATED_BY` varchar(40) DEFAULT NULL,
  `UPDATE_TIMESTAMP` datetime DEFAULT NULL,
  PRIMARY KEY (`COI_DISCL_PROJECT_ENTITY_REL_ID`),
  KEY `COI_DISCL_PROJECT_ENTITY_REL_FK1_idx` (`COI_DISCL_PROJECTS_ID`),
  KEY `COI_DISCL_PROJECT_ENTITY_REL_FK2_idx` (`PERSON_ENTITY_ID`),
  KEY `COI_DISCL_PROJECT_ENTITY_REL_FK3_idx` (`ENTITY_ID`),
  KEY `COI_DISCL_PROJECT_ENTITY_REL_FK4_idx` (`PROJECT_CONFLICT_STATUS_CODE`),
  CONSTRAINT `COI_DISCL_PROJECT_ENTITY_REL_FK1` FOREIGN KEY (`COI_DISCL_PROJECTS_ID`) REFERENCES `coi_discl_projects` (`COI_DISCL_PROJECTS_ID`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `COI_DISCL_PROJECT_ENTITY_REL_FK2` FOREIGN KEY (`PERSON_ENTITY_ID`) REFERENCES `person_entity` (`PERSON_ENTITY_ID`),
  CONSTRAINT `COI_DISCL_PROJECT_ENTITY_REL_FK3` FOREIGN KEY (`ENTITY_ID`) REFERENCES `entity` (`ENTITY_ID`),
  CONSTRAINT `COI_DISCL_PROJECT_ENTITY_REL_FK4` FOREIGN KEY (`PROJECT_CONFLICT_STATUS_CODE`) REFERENCES `coi_proj_conflict_status_type` (`PROJECT_CONFLICT_STATUS_CODE`)
);