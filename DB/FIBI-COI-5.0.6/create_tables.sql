
set foreign_key_checks = 0;



DROP TABLE IF EXISTS ENTITY_ATTACHMENT_STATUS_TYPE;
CREATE TABLE ENTITY_ATTACHMENT_STATUS_TYPE (
    ATTACHMENT_STATUS_CODE VARCHAR(10) PRIMARY KEY,
    DESCRIPTION varchar(500),
    IS_ACTIVE VARCHAR(1),
	UPDATE_TIMESTAMP TIMESTAMP,
    UPDATED_BY VARCHAR(60)
);

DROP TABLE IF EXISTS ENTITY_ATTACHMENT_TYPE;
CREATE TABLE ENTITY_ATTACHMENT_TYPE (
    ATTACHMENT_TYPE_CODE VARCHAR(10) PRIMARY KEY,
    DESCRIPTION varchar(500),
    IS_PRIVATE VARCHAR(1),
    IS_ACTIVE VARCHAR(1),
	UPDATE_TIMESTAMP TIMESTAMP,
    UPDATED_BY VARCHAR(60)
);

DROP TABLE IF EXISTS ENTITY_FAMILY_ROLE_TYPE;
CREATE TABLE ENTITY_FAMILY_ROLE_TYPE (
    FAMILY_ROLE_TYPE_CODE VARCHAR(10) PRIMARY KEY,
    DESCRIPTION VARCHAR(500),
    IS_ACTIVE VARCHAR(1),
    UPDATE_TIMESTAMP TIMESTAMP,
    UPDATED_BY VARCHAR(60)
);

DROP TABLE IF EXISTS ENTITY_OPERATING_STATUS_TYPE;
CREATE TABLE ENTITY_OPERATING_STATUS_TYPE (
    OPERATING_STATUS_TYPE_CODE VARCHAR(10) PRIMARY KEY,
    DESCRIPTION varchar(500),
    IS_ACTIVE VARCHAR(1),
	UPDATE_TIMESTAMP TIMESTAMP,
    UPDATED_BY VARCHAR(60)
);

DROP TABLE IF EXISTS ENTITY_REGISTRATION_TYPE;
CREATE TABLE ENTITY_REGISTRATION_TYPE (
    REG_TYPE_CODE VARCHAR(10) PRIMARY KEY,
    DESCRIPTION VARCHAR(500),
    IS_ACTIVE VARCHAR(1),
    UPDATE_TIMESTAMP TIMESTAMP,
    UPDATED_BY VARCHAR(60)
);

DROP TABLE IF EXISTS ENTITY_RISK_LEVEL;
CREATE TABLE ENTITY_RISK_LEVEL (
    RISK_LEVEL_CODE VARCHAR(3) PRIMARY KEY,
    DESCRIPTION varchar(500),
    IS_ACTIVE VARCHAR(1),
	UPDATE_TIMESTAMP TIMESTAMP,
    UPDATED_BY VARCHAR(60)
);

DROP TABLE IF EXISTS ENTITY_RISK_TYPE;
CREATE TABLE ENTITY_RISK_TYPE (
    RISK_TYPE_CODE VARCHAR(10) PRIMARY KEY,
    RISK_CATEGORY_CODE VARCHAR(3),
    DESCRIPTION varchar(500),
    IS_ACTIVE VARCHAR(1),
	UPDATE_TIMESTAMP TIMESTAMP,
    UPDATED_BY VARCHAR(60)
);

DROP TABLE IF EXISTS ENTITY_SECTION;
CREATE TABLE ENTITY_SECTION (
    ENTITY_SECTION_CODE varchar(3) PRIMARY KEY,
    DESCRIPTION varchar(500),
    IS_ACTIVE VARCHAR(1),
	UPDATE_TIMESTAMP TIMESTAMP,
    UPDATED_BY VARCHAR(60)
);

DROP TABLE IF EXISTS ENTITY_SOURCE_TYPE;
CREATE TABLE ENTITY_SOURCE_TYPE (
    ENTITY_SOURCE_TYPE_CODE VARCHAR(10) PRIMARY KEY,
    DESCRIPTION varchar(500),
    IS_ACTIVE VARCHAR(1),
	UPDATE_TIMESTAMP TIMESTAMP,
    UPDATED_BY VARCHAR(60)
);

DROP TABLE IF EXISTS entity_status_type;
CREATE TABLE entity_status_type (
  ENTITY_STATUS_TYPE_CODE varchar(10) NOT NULL,
  DESCRIPTION varchar(500) DEFAULT NULL,
  IS_ACTIVE varchar(1) DEFAULT NULL,
  UPDATE_TIMESTAMP timestamp NULL DEFAULT NULL,
  UPDATED_BY varchar(60) DEFAULT NULL,
  PRIMARY KEY (ENTITY_STATUS_TYPE_CODE)
);

DROP TABLE IF EXISTS INDUSTRY_CATEGORY_TYPE;
CREATE TABLE INDUSTRY_CATEGORY_TYPE (
    INDUSTRY_CATEGORY_TYPE_CODE VARCHAR(10) PRIMARY KEY,
    IS_PRIMARY VARCHAR(1),
    DESCRIPTION VARCHAR(500),
    IS_ACTIVE VARCHAR(1),
	UPDATE_TIMESTAMP TIMESTAMP,
    UPDATED_BY VARCHAR(60)
);

DROP TABLE IF EXISTS industry_category_code;
CREATE TABLE industry_category_code (
  INDUSTRY_CATEGORY_ID int NOT NULL AUTO_INCREMENT,
  INDUSTRY_CATEGORY_CODE varchar(10) DEFAULT NULL,
  INDUSTRY_CATEGORY_TYPE_CODE varchar(10) DEFAULT NULL,
  DESCRIPTION varchar(500) DEFAULT NULL,
  IS_ACTIVE varchar(1) DEFAULT NULL,
  UPDATE_TIMESTAMP timestamp NULL DEFAULT NULL,
  UPDATED_BY varchar(60) DEFAULT NULL,
  PRIMARY KEY (INDUSTRY_CATEGORY_ID),
  KEY INDUSTRY_CATEGORY_CODE_FK1 (INDUSTRY_CATEGORY_TYPE_CODE),
  CONSTRAINT INDUSTRY_CATEGORY_CODE_FK1 FOREIGN KEY (INDUSTRY_CATEGORY_TYPE_CODE) REFERENCES industry_category_type (INDUSTRY_CATEGORY_TYPE_CODE)
);

DROP TABLE IF EXISTS entity_section_access_right;
CREATE TABLE entity_section_access_right (
  SEC_ACCESS_RIGHT_ID int NOT NULL,
  SECTION_CODE varchar(3) DEFAULT NULL,
  RIGHT_NAME varchar(100) DEFAULT NULL,
  UPDATE_TIMESTAMP timestamp NULL DEFAULT NULL,
  UPDATED_BY varchar(60) DEFAULT NULL,
  PRIMARY KEY (SEC_ACCESS_RIGHT_ID),
  KEY ENTITY_SECTION_ACCESS_RIGHT_FK1 (SECTION_CODE),
  CONSTRAINT ENTITY_SECTION_ACCESS_RIGHT_FK1 FOREIGN KEY (SECTION_CODE) REFERENCES entity_section (ENTITY_SECTION_CODE)
);

DROP TABLE IF EXISTS entity_comment_type;
CREATE TABLE entity_comment_type (
  COMMENT_TYPE_CODE varchar(10) NOT NULL,
  SECTION_CODE varchar(3) DEFAULT NULL,
  DESCRIPTION varchar(500) DEFAULT NULL,
  IS_SYSTEM_COMMENT_TYPE varchar(1) DEFAULT NULL,
  IS_PRIVATE varchar(1) DEFAULT NULL,
  IS_ACTIVE varchar(1) DEFAULT NULL,
  UPDATE_TIMESTAMP timestamp NULL DEFAULT NULL,
  UPDATED_BY varchar(60) DEFAULT NULL,
  PRIMARY KEY (COMMENT_TYPE_CODE),
  KEY ENTITY_COMMENT_TYPE_FK1 (SECTION_CODE),
  CONSTRAINT ENTITY_COMMENT_TYPE_FK1 FOREIGN KEY (SECTION_CODE) REFERENCES entity_section (ENTITY_SECTION_CODE)
);

DROP TABLE IF EXISTS entity;
CREATE TABLE entity (
  ENTITY_ID int NOT NULL AUTO_INCREMENT,
  PRIMARY_NAME varchar(500) DEFAULT NULL,
  FOREIGN_NAME varchar(500) DEFAULT NULL,
  PRIOR_NAME varchar(200) DEFAULT NULL,
  SHORT_NAME varchar(200) DEFAULT NULL,
  DUNS_NUMBER varchar(20) DEFAULT NULL,
  UEI_NUMBER varchar(20) DEFAULT NULL,
  CAGE_NUMBER varchar(20) DEFAULT NULL,
  ENTITY_OWNERSHIP_TYPE_CODE varchar(10) DEFAULT NULL,
  ENTITY_STATUS_TYPE_CODE varchar(10) DEFAULT NULL,
  OPERATING_STATUS_TYPE_CODE varchar(10) DEFAULT NULL,
  BUSINESS_TYPE_CODE varchar(10) DEFAULT NULL,
  WEBSITE_ADDRESS varchar(500) DEFAULT NULL,
  START_DATE varchar(10) DEFAULT NULL,
  INCORPORATION_DATE varchar(10) DEFAULT NULL,
  INCORPORATED_IN varchar(30) DEFAULT NULL,
  CONGRESSIONAL_DISTRICT varchar(45) DEFAULT NULL,
  NUMBER_OF_EMPLOYEES int DEFAULT NULL,
  FEDERAL_EMPLOYER_ID varchar(50) DEFAULT NULL,
  CERTIFIED_EMAIL varchar(200) DEFAULT NULL,
  ACTIVITY_TEXT varchar(1000) DEFAULT NULL,
  CURRENCY_CODE varchar(3) DEFAULT NULL,
  ENTITY_SOURCE_TYPE_CODE varchar(10) DEFAULT NULL,
  PHONE_NUMBER varchar(15) DEFAULT NULL,
  PRIMARY_ADDRESS_LINE_1 varchar(500) DEFAULT NULL,
  PRIMARY_ADDRESS_LINE_2 varchar(500) DEFAULT NULL,
  CITY varchar(30) DEFAULT NULL,
  STATE varchar(30) DEFAULT NULL,
  POST_CODE varchar(15) DEFAULT NULL,
  COUNTRY_CODE varchar(3) DEFAULT NULL,
  HUMAN_SUB_ASSURANCE varchar(50) DEFAULT NULL,
  ANIMAL_WELFARE_ASSURANCE varchar(50) DEFAULT NULL,
  ANIMAL_ACCREDITATION varchar(50) DEFAULT NULL,
  APPROVED_BY varchar(40) DEFAULT NULL,
  APPROVED_TIMESTAMP datetime DEFAULT NULL,
  CREATED_BY varchar(40) DEFAULT NULL,
  CREATE_TIMESTAMP datetime DEFAULT NULL,
  UPDATED_BY varchar(40) DEFAULT NULL,
  UPDATE_TIMESTAMP datetime DEFAULT NULL,
  ENTITY_NUMBER int DEFAULT NULL,
  IS_ACTIVE varchar(1) DEFAULT NULL,
  VERSION_NUMBER int DEFAULT NULL,
  VERSION_STATUS varchar(45) DEFAULT NULL,
  IS_DUNS_MATCHED varchar(1) DEFAULT NULL,
  PRIMARY KEY (ENTITY_ID),
  KEY ENTITY_FK1_idx (ENTITY_STATUS_TYPE_CODE),
  KEY ENTITY_FK2_idx (OPERATING_STATUS_TYPE_CODE),
  KEY ENTITY_FK3_idx (ENTITY_SOURCE_TYPE_CODE),
  KEY ENTITY_FK4_idx (COUNTRY_CODE),
  KEY ENTITY_FK5_idx (ENTITY_OWNERSHIP_TYPE_CODE),
  KEY ENTITY_FK6_idx (BUSINESS_TYPE_CODE),
  CONSTRAINT ENTITY_FK1 FOREIGN KEY (ENTITY_STATUS_TYPE_CODE) REFERENCES entity_status_type (ENTITY_STATUS_TYPE_CODE),
  CONSTRAINT ENTITY_FK2 FOREIGN KEY (OPERATING_STATUS_TYPE_CODE) REFERENCES entity_operating_status_type (OPERATING_STATUS_TYPE_CODE),
  CONSTRAINT ENTITY_FK3 FOREIGN KEY (ENTITY_SOURCE_TYPE_CODE) REFERENCES entity_source_type (ENTITY_SOURCE_TYPE_CODE),
  CONSTRAINT ENTITY_FK4 FOREIGN KEY (COUNTRY_CODE) REFERENCES country (COUNTRY_CODE),
  CONSTRAINT ENTITY_FK5 FOREIGN KEY (ENTITY_OWNERSHIP_TYPE_CODE) REFERENCES entity_ownership_type (OWNERSHIP_TYPE_CODE),
  CONSTRAINT ENTITY_FK6 FOREIGN KEY (BUSINESS_TYPE_CODE) REFERENCES entity_business_type (BUSINESS_TYPE_CODE)
);

DROP TABLE IF EXISTS entity_family_tree;
CREATE TABLE entity_family_tree (
  ENTITY_FAMILY_TREE_ID INT NOT NULL AUTO_INCREMENT,
  ENTITY_ID INT NULL,
  PARENT_ENTITY_ID INT NULL,
  GLOBAL_ULTIMATE_ENTITY_NUMBER VARCHAR(45) NULL,
  HIERARCHY_LEVEL VARCHAR(20) NULL,
  UPDATED_BY VARCHAR(40) NULL DEFAULT NULL,
  UPDATE_TIMESTAMP DATETIME NULL DEFAULT NULL,
  PRIMARY KEY (ENTITY_FAMILY_TREE_ID),
  INDEX ENTITY_FAMILY_TREE_FK1_idx (ENTITY_ID ASC) VISIBLE,
  INDEX ENTITY_FAMILY_TREE_FK2_idx (PARENT_ENTITY_ID ASC) VISIBLE,
  CONSTRAINT ENTITY_FAMILY_TREE_FK1
    FOREIGN KEY (ENTITY_ID)
    REFERENCES entity (ENTITY_ID),
  CONSTRAINT ENTITY_FAMILY_TREE_FK2
    FOREIGN KEY (PARENT_ENTITY_ID)
    REFERENCES entity (ENTITY_ID)
);

DROP TABLE IF EXISTS entity_family_tree_role;
CREATE TABLE entity_family_tree_role (
  ENTITY_FAMILY_TREE_ROLE_ID INT NOT NULL AUTO_INCREMENT,
  ENTITY_ID INT NULL,
  FAMILY_ROLE_TYPE_CODE VARCHAR(10) NULL,
  UPDATED_BY VARCHAR(40) NULL DEFAULT NULL,
  UPDATE_TIMESTAMP DATETIME NULL DEFAULT NULL,
  PRIMARY KEY (ENTITY_FAMILY_TREE_ROLE_ID),
  INDEX ENTITY_FAMILY_TREE_ROLE_FK1_idx (ENTITY_ID ASC) VISIBLE,
  INDEX ENTITY_FAMILY_TREE_ROLE_FK2_idx (FAMILY_ROLE_TYPE_CODE ASC) VISIBLE,
  CONSTRAINT ENTITY_FAMILY_TREE_ROLE_FK1
    FOREIGN KEY (ENTITY_ID)
    REFERENCES entity (ENTITY_ID),
  CONSTRAINT ENTITY_FAMILY_TREE_ROLE_FK2
    FOREIGN KEY (FAMILY_ROLE_TYPE_CODE)
    REFERENCES entity_family_role_type (FAMILY_ROLE_TYPE_CODE)
);

DROP TABLE IF EXISTS entity_trade_style_name;
CREATE TABLE entity_trade_style_name (
  ENTITY_TRADE_NAME_ID INT NOT NULL AUTO_INCREMENT,
  ENTITY_ID INT NULL,
  TRADE_STYLE_NAME VARCHAR(200) NULL,
  PRIORITY VARCHAR(10) NULL,
  UPDATED_BY VARCHAR(40) NULL DEFAULT NULL,
  UPDATE_TIMESTAMP DATETIME NULL DEFAULT NULL,
  PRIMARY KEY (ENTITY_TRADE_NAME_ID),
  INDEX ENTITY_TRADE_STYLE_NAME_FK1_idx (ENTITY_ID ASC) VISIBLE,
  CONSTRAINT ENTITY_TRADE_STYLE_NAME_FK1
    FOREIGN KEY (ENTITY_ID)
    REFERENCES entity (ENTITY_ID)
);

DROP TABLE IF EXISTS entity_prior_name;
CREATE TABLE entity_prior_name (
  ID INT NOT NULL AUTO_INCREMENT,
  ENTITY_ID INT NULL,
  PRIOR_NAME VARCHAR(500) NULL,
  EFFECTIVE_YEAR DATE NULL,
  UPDATED_BY VARCHAR(40) NULL DEFAULT NULL,
  UPDATE_TIMESTAMP DATETIME NULL DEFAULT NULL,
  PRIMARY KEY (ID),
  INDEX ENTITY_PRIOR_NAME_FK1_idx (ENTITY_ID ASC) VISIBLE,
  CONSTRAINT ENTITY_PRIOR_NAME_FK1
    FOREIGN KEY (ENTITY_ID)
    REFERENCES entity (ENTITY_ID)
);

DROP TABLE IF EXISTS entity_external_id_mapping;
CREATE TABLE entity_external_id_mapping (
  ENTITY_EXTERNAL_MAPPING_ID int NOT NULL AUTO_INCREMENT,
  ENTITY_ID int DEFAULT NULL,
  EXTERNAL_ID_TYPE_CODE varchar(3) DEFAULT NULL,
  EXTERNAL_ID varchar(50) DEFAULT NULL,
  DESCRIPTION varchar(500) DEFAULT NULL,
  SPONSOR_CODE varchar(10) DEFAULT NULL,
  ORGANIZATION_ID int DEFAULT NULL,
  UPDATED_BY varchar(40) DEFAULT NULL,
  UPDATE_TIMESTAMP datetime DEFAULT NULL,
  PRIMARY KEY (ENTITY_EXTERNAL_MAPPING_ID),
  KEY ENTITY_EXTERNAL_MAPPING_FK1_idx (ENTITY_ID),
  KEY ENTITY_EXTERNAL_MAPPING_FK2_idx (EXTERNAL_ID_TYPE_CODE),
  CONSTRAINT ENTITY_EXTERNAL_MAPPING_FK1 FOREIGN KEY (ENTITY_ID) REFERENCES entity (ENTITY_ID),
  CONSTRAINT ENTITY_EXTERNAL_MAPPING_FK2 FOREIGN KEY (EXTERNAL_ID_TYPE_CODE) REFERENCES entity_external_id_type (EXTERNAL_ID_TYPE_CODE)
);

DROP TABLE IF EXISTS entity_industry_classification;
CREATE TABLE entity_industry_classification (
  ENTITY_INDUSTRY_CLASS_ID int NOT NULL AUTO_INCREMENT,
  ENTITY_ID int DEFAULT NULL,
  INDUSTRY_CATEGORY_ID int DEFAULT NULL,
  IS_PRIMARY varchar(1) DEFAULT NULL,
  UPDATED_BY varchar(40) DEFAULT NULL,
  UPDATE_TIMESTAMP datetime DEFAULT NULL,
  PRIMARY KEY (ENTITY_INDUSTRY_CLASS_ID),
  KEY ENTITY_INDUSTRY_CLASS_FK1_idx (ENTITY_ID),
  KEY ENTITY_INDUSTRY_CLASS_FK2_idx (INDUSTRY_CATEGORY_ID),
  CONSTRAINT ENTITY_INDUSTRY_CLASS_FK1 FOREIGN KEY (ENTITY_ID) REFERENCES entity (ENTITY_ID),
  CONSTRAINT ENTITY_INDUSTRY_CLASS_FK2 FOREIGN KEY (INDUSTRY_CATEGORY_ID) REFERENCES industry_category_code (INDUSTRY_CATEGORY_ID)
);

DROP TABLE IF EXISTS entity_ext;
CREATE TABLE entity_ext (
  ID INT NOT NULL AUTO_INCREMENT,
  ENTITY_ID INT NOT NULL,
  COL_1_STRING_LABEL VARCHAR(90) NULL,
  COL_1_STRING_VALUE VARCHAR(90) NULL,
  COL_2_STRING_LABEL VARCHAR(90) NULL,
  COL_2_STRING_VALUE VARCHAR(90) NULL,
  COL_3_STRING_LABEL VARCHAR(90) NULL,
  COL_3_STRING_VALUE VARCHAR(90) NULL,
  COL_4_STRING_LABEL VARCHAR(90) NULL,
  COL_4_STRING_VALUE VARCHAR(90) NULL,
  COL_5_DATE_LABEL VARCHAR(90) NULL,
  COL_5_DATE_VALUE DATE NULL,
  COL_6_DATE_LABEL VARCHAR(90) NULL,
  COL_6_DATE_VALUE DATE NULL,
  COL_7_DATE_LABEL VARCHAR(90) NULL,
  COL_7_DATE_VALUE DATE NULL,
  PRIMARY KEY (ID),
  INDEX ENTITY_EXT_FK1_idx (ENTITY_ID ASC) VISIBLE,
  CONSTRAINT ENTITY_EXT_FK1
    FOREIGN KEY (ENTITY_ID)
    REFERENCES entity (ENTITY_ID)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION
);

DROP TABLE IF EXISTS entity_comment;
CREATE TABLE entity_comment (
  ENTITY_COMMENT_ID INT NOT NULL AUTO_INCREMENT,
  ENTITY_ID INT NULL,
  COMMENT_TYPE_CODE VARCHAR(10) NULL,
  IS_PRIVATE VARCHAR(1) NULL,
  COMMENT TEXT NULL,
  PARENT_COMMENT_ID INT NULL,
  PRIMARY KEY (ENTITY_COMMENT_ID),
  INDEX ENTITY_COMMENT_FK1_idx (ENTITY_ID ASC) VISIBLE,
  INDEX ENTITY_COMMENT_FK2_idx (COMMENT_TYPE_CODE ASC) VISIBLE,
  CONSTRAINT ENTITY_COMMENT_FK1
    FOREIGN KEY (ENTITY_ID)
    REFERENCES entity (ENTITY_ID)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT ENTITY_COMMENT_FK2
    FOREIGN KEY (COMMENT_TYPE_CODE)
    REFERENCES entity_comment_type (COMMENT_TYPE_CODE)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION
);

DROP TABLE IF EXISTS entity_attachment;
CREATE TABLE entity_attachment (
  ENTITY_ATTACHMENT_ID INT NOT NULL AUTO_INCREMENT,
  ATTACHMENT_NUMBER INT NULL,
  VERSION_NUMBER INT NULL,
  VERSION_STATUS VARCHAR(45) NULL,
  ENTITY_ID INT NULL,
  COMMENT VARCHAR(2000) NULL,
  ATTACHMENT_TYPE_CODE VARCHAR(10) NULL,
  ATTACHMENT_STATUS_CODE VARCHAR(3) NULL,
  FILE_NAME VARCHAR(200) NULL,
  MIME_TYPE VARCHAR(255) NULL,
  FILE_DATA_ID VARCHAR(100) NULL,
  UPDATE_TIMESTAMP TIMESTAMP NULL DEFAULT NULL,
  UPDATED_BY VARCHAR(60) NULL DEFAULT NULL,
  PRIMARY KEY (ENTITY_ATTACHMENT_ID),
  INDEX ENTITY_ATTACH_FK1_idx (ENTITY_ID ASC) VISIBLE,
  INDEX ENTITY_ATTACH_FK2_idx (ATTACHMENT_TYPE_CODE ASC) VISIBLE,
  INDEX ENTITY_ATTACH_FK3_idx (ATTACHMENT_STATUS_CODE ASC) VISIBLE,
  CONSTRAINT ENTITY_ATTACH_FK1
    FOREIGN KEY (ENTITY_ID)
    REFERENCES entity (ENTITY_ID)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT ENTITY_ATTACH_FK2
    FOREIGN KEY (ATTACHMENT_TYPE_CODE)
    REFERENCES entity_attachment_type (ATTACHMENT_TYPE_CODE)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT ENTITY_ATTACH_FK3
    FOREIGN KEY (ATTACHMENT_STATUS_CODE)
    REFERENCES entity_attachment_status_type (ATTACHMENT_STATUS_CODE)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION
);

DROP TABLE IF EXISTS entity_section_comment_ref;
CREATE TABLE entity_section_comment_ref (
  ENTITY_SEC_COM_REF_ID INT NOT NULL AUTO_INCREMENT,
  ENTITY_ID INT NULL,
  ENTITY_SECTION_CODE VARCHAR(3) NULL,
  ENTITY_COMMENT_ID INT NULL,
  UPDATE_TIMESTAMP TIMESTAMP NULL DEFAULT NULL,
  UPDATED_BY VARCHAR(60) NULL DEFAULT NULL,
  PRIMARY KEY (ENTITY_SEC_COM_REF_ID),
  INDEX ENTITY_SEC_COM_REF_FK1_idx (ENTITY_ID ASC) VISIBLE,
  INDEX ENTITY_SEC_COM_REF_FK2_idx (ENTITY_SECTION_CODE ASC) VISIBLE,
  INDEX ENTITY_SEC_COM_REF_FK3_idx (ENTITY_COMMENT_ID ASC) VISIBLE,
  CONSTRAINT ENTITY_SEC_COM_REF_FK1
    FOREIGN KEY (ENTITY_ID)
    REFERENCES entity (ENTITY_ID)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT ENTITY_SEC_COM_REF_FK2
    FOREIGN KEY (ENTITY_SECTION_CODE)
    REFERENCES entity_section (ENTITY_SECTION_CODE)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT ENTITY_SEC_COM_REF_FK3
    FOREIGN KEY (ENTITY_COMMENT_ID)
    REFERENCES entity_comment (ENTITY_COMMENT_ID)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION
);

DROP TABLE IF EXISTS entity_section_attach_ref;
CREATE TABLE entity_section_attach_ref (
  ENTITY_SEC_ATT_REF_ID INT NOT NULL AUTO_INCREMENT,
  ENTITY_ID INT NULL,
  ENTITY_SECTION_CODE VARCHAR(3) NULL,
  ENTITY_ATTACHMENT_ID INT NULL,
  UPDATE_TIMESTAMP TIMESTAMP NULL DEFAULT NULL,
  UPDATED_BY VARCHAR(60) NULL DEFAULT NULL,
  PRIMARY KEY (ENTITY_SEC_ATT_REF_ID),
  INDEX ENTITY_SEC_ATT_REF_FK1_idx (ENTITY_ID ASC) VISIBLE,
  INDEX ENTITY_SEC_ATT_REF_FK2_idx (ENTITY_SECTION_CODE ASC) VISIBLE,
  INDEX ENTITY_SEC_ATT_REF_FK3_idx (ENTITY_ATTACHMENT_ID ASC) VISIBLE,
  CONSTRAINT ENTITY_SEC_ATT_REF_FK1
    FOREIGN KEY (ENTITY_ID)
    REFERENCES entity (ENTITY_ID)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT ENTITY_SEC_ATT_REF_FK2
    FOREIGN KEY (ENTITY_SECTION_CODE)
    REFERENCES entity_section (ENTITY_SECTION_CODE)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT ENTITY_SEC_ATT_REF_FK3
    FOREIGN KEY (ENTITY_ATTACHMENT_ID)
    REFERENCES entity_attachment (ENTITY_ATTACHMENT_ID)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION
);

DROP TABLE IF EXISTS entity_risk;
CREATE TABLE entity_risk (
  ENTITY_RSIK_ID INT NOT NULL AUTO_INCREMENT,
  ENTITY_ID INT NULL,
  RISK_TYPE_CODE VARCHAR(10) NULL,
  RISK_LEVEL_CODE VARCHAR(3) NULL,
  DESCRIPTION VARCHAR(500) NULL DEFAULT NULL,
  UPDATE_TIMESTAMP TIMESTAMP NULL DEFAULT NULL,
  UPDATED_BY VARCHAR(60) NULL DEFAULT NULL,
  PRIMARY KEY (ENTITY_RSIK_ID),
  INDEX ENTITY_RISK_FK1_idx (ENTITY_ID ASC) VISIBLE,
  INDEX ENTITY_RISK_FK2_idx (RISK_TYPE_CODE ASC) VISIBLE,
  INDEX ENTITY_RISK_FK3_idx (RISK_LEVEL_CODE ASC) VISIBLE,
  CONSTRAINT ENTITY_RISK_FK1
    FOREIGN KEY (ENTITY_ID)
    REFERENCES entity (ENTITY_ID)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT ENTITY_RISK_FK2
    FOREIGN KEY (RISK_TYPE_CODE)
    REFERENCES entity_risk_type (RISK_TYPE_CODE)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT ENTITY_RISK_FK3
    FOREIGN KEY (RISK_LEVEL_CODE)
    REFERENCES entity_risk_level (RISK_LEVEL_CODE)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION
);

DROP TABLE IF EXISTS entity_sub_org_info;
CREATE TABLE entity_sub_org_info (
  ID int NOT NULL AUTO_INCREMENT,
  ENTITY_ID int DEFAULT NULL,
  ORGANIZATION_ID int DEFAULT NULL,
  ORGANIZATION_TYPE_CODE varchar(10) DEFAULT NULL,
  FEED_STATUS_CODE varchar(3) DEFAULT NULL,
  IRS_TAX_EXEMPTION varchar(50) DEFAULT NULL,
  MASS_TAX_EXEMPT_NUM varchar(50) DEFAULT NULL,
  AGENCY_SYMBOL varchar(50) DEFAULT NULL,
  VENDOR_CODE varchar(50) DEFAULT NULL,
  COM_GOV_ENTITY_CODE varchar(50) DEFAULT NULL,
  MASS_EMPLOYEE_CLAIM varchar(50) DEFAULT NULL,
  SCIENCE_MISCONDUCT_COMPL_DATE date DEFAULT NULL,
  PHS_ACOUNT varchar(50) DEFAULT NULL,
  NSF_INSTITUTIONAL_CODE varchar(50) DEFAULT NULL,
  INDIRECT_COST_RATE_AGREEMENT varchar(50) DEFAULT NULL,
  COGNIZANT_AUDITOR varchar(50) DEFAULT NULL,
  ONR_RESIDENT_REP varchar(50) DEFAULT NULL,
  LOBBYING_REGISTRANT varchar(50) DEFAULT NULL,
  LOBBYING_INDIVIDUAL varchar(50) DEFAULT NULL,
  SAM_EXPIRATION_DATE date DEFAULT NULL,
  SUB_AWD_RISK_ASSMT_DATE date DEFAULT NULL,
  UPDATE_TIMESTAMP timestamp NULL DEFAULT NULL,
  UPDATED_BY varchar(60) DEFAULT NULL,
  PRIMARY KEY (ID),
  KEY ENTITY_SUB_ORG_INFO_FK1_idx (ENTITY_ID),
  KEY ENTITY_SUB_ORG_INFO_FK2_idx (ORGANIZATION_TYPE_CODE),
  KEY ENTITY_SUB_ORG_INFO_FK3_idx (FEED_STATUS_CODE),
  CONSTRAINT ENTITY_SUB_ORG_INFO_FK1 FOREIGN KEY (ENTITY_ID) REFERENCES entity (ENTITY_ID),
  CONSTRAINT ENTITY_SUB_ORG_INFO_FK2 FOREIGN KEY (ORGANIZATION_TYPE_CODE) REFERENCES entity_organization_type (ORGANIZATION_TYPE_CODE),
  CONSTRAINT ENTITY_SUB_ORG_INFO_FK3 FOREIGN KEY (FEED_STATUS_CODE) REFERENCES entity_feed_status_type (FEED_STATUS_CODE)
);

DROP TABLE IF EXISTS entity_sponsor_info;
CREATE TABLE entity_sponsor_info (
  ID int NOT NULL AUTO_INCREMENT,
  ENTITY_ID int DEFAULT NULL,
  SPONSOR_CODE varchar(10) DEFAULT NULL,
  FEED_STATUS_CODE varchar(3) DEFAULT NULL,
  SPONSOR_TYPE_CODE varchar(10) DEFAULT NULL,
  ACRONYM varchar(200) DEFAULT NULL,
  DODAC_NUMBER varchar(50) DEFAULT NULL,
  AUDIT_REPORT_SENT_FOR_FY varchar(50) DEFAULT NULL,
  DUNNING_CAMPAIGN_ID varchar(50) DEFAULT NULL,
  CUSTOMER_NUMBER varchar(50) DEFAULT NULL,
  UPDATE_TIMESTAMP timestamp NULL DEFAULT NULL,
  UPDATED_BY varchar(60) DEFAULT NULL,
  PRIMARY KEY (ID),
  KEY ENTITY_SPONSOR_INFO_FK1_idx (ENTITY_ID),
  KEY ENTITY_SPONSOR_INFO_FK2_idx (FEED_STATUS_CODE),
  CONSTRAINT ENTITY_SPONSOR_INFO_FK1 FOREIGN KEY (ENTITY_ID) REFERENCES entity (ENTITY_ID),
  CONSTRAINT ENTITY_SPONSOR_INFO_FK2 FOREIGN KEY (FEED_STATUS_CODE) REFERENCES entity_feed_status_type (FEED_STATUS_CODE)
);

DROP TABLE IF EXISTS entity_compliance_info;
CREATE TABLE entity_compliance_info (
  ID INT NOT NULL AUTO_INCREMENT,
  ENTITY_ID INT NULL,
  UPDATE_TIMESTAMP TIMESTAMP NULL DEFAULT NULL,
  UPDATED_BY VARCHAR(60) NULL DEFAULT NULL,
  PRIMARY KEY (ID),
  INDEX ENTITY_COMPLIANCE_INFO_FK1_idx (ENTITY_ID ASC) VISIBLE,
  CONSTRAINT ENTITY_COMPLIANCE_INFO_FK1
    FOREIGN KEY (ENTITY_ID)
    REFERENCES entity (ENTITY_ID)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION
);

DROP TABLE IF EXISTS entity_mailing_address;
CREATE TABLE entity_mailing_address (
  ENTITY_MAILING_ADDRESS_ID int NOT NULL AUTO_INCREMENT,
  ENTITY_ID int DEFAULT NULL,
  ADDRESS_TYPE_CODE varchar(10) DEFAULT NULL,
  ADDRESS_LINE_1 varchar(500) DEFAULT NULL,
  ADDRESS_LINE_2 varchar(500) DEFAULT NULL,
  CITY varchar(100) DEFAULT NULL,
  STATE varchar(100) DEFAULT NULL,
  POST_CODE varchar(20) DEFAULT NULL,
  COUNTRY_CODE varchar(3) DEFAULT NULL,
  LOCALITY varchar(100) DEFAULT NULL,
  REGION varchar(100) DEFAULT NULL,
  COUNTY varchar(100) DEFAULT NULL,
  UPDATE_TIMESTAMP timestamp NULL DEFAULT NULL,
  UPDATED_BY varchar(60) DEFAULT NULL,
  PRIMARY KEY (ENTITY_MAILING_ADDRESS_ID),
  KEY ENTITY_MAILING_ADDRESS_FK1_idx (ENTITY_ID),
  KEY ENTITY_MAILING_ADDRESS_FK2_idx (COUNTRY_CODE),
  KEY ENTITY_MAILING_ADDRESS_FK3_idx (ADDRESS_TYPE_CODE),
  CONSTRAINT ENTITY_MAILING_ADDRESS_FK1 FOREIGN KEY (ENTITY_ID) REFERENCES entity (ENTITY_ID),
  CONSTRAINT ENTITY_MAILING_ADDRESS_FK2 FOREIGN KEY (COUNTRY_CODE) REFERENCES country (COUNTRY_CODE),
  CONSTRAINT ENTITY_MAILING_ADDRESS_FK3 FOREIGN KEY (ADDRESS_TYPE_CODE) REFERENCES entity_address_type (ADDRESS_TYPE_CODE)
) ;

DROP TABLE IF EXISTS entity_telephone;
CREATE TABLE entity_telephone (
  ENTITY_TELEPHONE_ID INT NOT NULL AUTO_INCREMENT,
  ENTITY_ID INT NULL,
  INT_DIALING_CODE VARCHAR(5) NULL,
  TELEPHONE_NUMBER BIGINT NULL,
  UPDATE_TIMESTAMP TIMESTAMP NULL DEFAULT NULL,
  UPDATED_BY VARCHAR(60) NULL DEFAULT NULL,
  PRIMARY KEY (ENTITY_TELEPHONE_ID),
  INDEX ENTITY_TELEPHONE_FK1_idx (ENTITY_ID ASC) VISIBLE,
  CONSTRAINT ENTITY_TELEPHONE_FK1
    FOREIGN KEY (ENTITY_ID)
    REFERENCES entity (ENTITY_ID)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION
);

DROP TABLE IF EXISTS entity_registration;
CREATE TABLE entity_registration (
  ENTITY_REGISTRATION_ID INT NOT NULL AUTO_INCREMENT,
  ENTITY_ID INT NULL,
  REG_TYPE_CODE VARCHAR(10) NULL,
  REG_NUMBER VARCHAR(50) NULL,
  IS_ACTIVE VARCHAR(1) NULL DEFAULT NULL,
  UPDATE_TIMESTAMP TIMESTAMP NULL DEFAULT NULL,
  UPDATED_BY VARCHAR(60) NULL DEFAULT NULL,
  PRIMARY KEY (ENTITY_REGISTRATION_ID),
  INDEX ENTITY_REGISTRATION_FK1_idx (ENTITY_ID ASC) VISIBLE,
  INDEX ENTITY_REGISTRATION_FK2_idx (REG_TYPE_CODE ASC) VISIBLE,
  CONSTRAINT ENTITY_REGISTRATION_FK1
    FOREIGN KEY (ENTITY_ID)
    REFERENCES entity (ENTITY_ID)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION,
  CONSTRAINT ENTITY_REGISTRATION_FK2
    FOREIGN KEY (REG_TYPE_CODE)
    REFERENCES entity_registration_type (REG_TYPE_CODE)
    ON DELETE NO ACTION
    ON UPDATE NO ACTION
);

DROP TABLE IF EXISTS ENTITY_FILE_DATA;
CREATE TABLE ENTITY_FILE_DATA (
    FILE_DATA_ID varchar(100),
    FILE_PATH varchar(255),
	ORIGINAL_FILE_NAME varchar(500),
    FILE_NAME VARCHAR(100),
    IS_ARCHIVED char(1),
    FILE longblob,
    UPDATE_TIMESTAMP timestamp,
	UPDATED_BY varchar(60),
	PRIMARY KEY (FILE_DATA_ID)
);

DROP TABLE IF EXISTS entity_ownership_type;
CREATE TABLE entity_ownership_type (
  OWNERSHIP_TYPE_CODE varchar(10) NOT NULL,
  DESCRIPTION varchar(200) DEFAULT NULL,
  IS_ACTIVE varchar(1) DEFAULT 'Y',
  UPDATE_TIMESTAMP datetime DEFAULT NULL,
  UPDATED_BY varchar(40) DEFAULT NULL,
  PRIMARY KEY (OWNERSHIP_TYPE_CODE)
);

DROP TABLE IF EXISTS entity_address_type;
CREATE TABLE entity_address_type (
  ADDRESS_TYPE_CODE varchar(10) NOT NULL,
  DESCRIPTION varchar(200) DEFAULT NULL,
  IS_ACTIVE varchar(1) DEFAULT 'Y',
  UPDATE_TIMESTAMP datetime DEFAULT NULL,
  UPDATED_BY varchar(40) DEFAULT NULL,
  PRIMARY KEY (ADDRESS_TYPE_CODE)
);

DROP TABLE IF EXISTS entity_external_id_type;
CREATE TABLE entity_external_id_type (
  EXTERNAL_ID_TYPE_CODE varchar(10) NOT NULL,
  DESCRIPTION varchar(200) DEFAULT NULL,
  IS_ACTIVE varchar(1) DEFAULT 'Y',
  UPDATE_TIMESTAMP datetime DEFAULT NULL,
  UPDATED_BY varchar(40) DEFAULT NULL,
  PRIMARY KEY (EXTERNAL_ID_TYPE_CODE)
);

DROP TABLE IF EXISTS entity_foreign_name;
CREATE TABLE entity_foreign_name (
  ID int NOT NULL AUTO_INCREMENT,
  ENTITY_ID int DEFAULT NULL,
  FOREIGN_NAME varchar(500) DEFAULT NULL,
  UPDATED_BY varchar(40) DEFAULT NULL,
  UPDATE_TIMESTAMP datetime DEFAULT NULL,
  PRIMARY KEY (ID),
  KEY ENTITY_FOREIGN_NAME_FK1_idx (ENTITY_ID),
  CONSTRAINT ENTITY_FOREIGN_NAME_FK1 FOREIGN KEY (ENTITY_ID) REFERENCES entity (ENTITY_ID)
);

DROP TABLE IF EXISTS entity_business_type;
CREATE TABLE entity_business_type (
  BUSINESS_TYPE_CODE varchar(10) NOT NULL,
  DESCRIPTION varchar(500) DEFAULT NULL,
  IS_ACTIVE varchar(1) DEFAULT NULL,
  UPDATE_TIMESTAMP timestamp NULL DEFAULT NULL,
  UPDATED_BY varchar(60) DEFAULT NULL,
  PRIMARY KEY (BUSINESS_TYPE_CODE)
);

DROP TABLE IF EXISTS entity_organization_type;
CREATE TABLE entity_organization_type (
  ORGANIZATION_TYPE_CODE varchar(3) NOT NULL,
  DESCRIPTION varchar(500) DEFAULT NULL,
  UPDATE_TIMESTAMP datetime DEFAULT NULL,
  UPDATED_BY varchar(60) DEFAULT NULL,
  IS_ACTIVE varchar(1) DEFAULT 'Y',
  PRIMARY KEY (ORGANIZATION_TYPE_CODE)
);

DROP TABLE IF EXISTS entity_feed_status_type;
CREATE TABLE entity_feed_status_type (
  FEED_STATUS_CODE varchar(3) NOT NULL,
  DESCRIPTION varchar(500) DEFAULT NULL,
  IS_ACTIVE varchar(1) DEFAULT NULL,
  UPDATE_TIMESTAMP timestamp NULL DEFAULT NULL,
  UPDATED_BY varchar(60) DEFAULT NULL,
  PRIMARY KEY (FEED_STATUS_CODE)
);
 
DROP TABLE IF EXISTS valid_entity_risk_level;
CREATE TABLE valid_entity_risk_level (
  VALID_ENTITY_RISK_LVL_ID int NOT NULL AUTO_INCREMENT,
  RISK_TYPE_CODE varchar(3) DEFAULT NULL,
  RISK_LEVEL_CODE varchar(3) DEFAULT NULL,
  PRIMARY KEY (VALID_ENTITY_RISK_LVL_ID),
  KEY VALID_ENTITY_RISK_LVL_FK1_idx (RISK_TYPE_CODE),
  KEY VALID_ENTITY_RISK_LVL_FK2_idx (RISK_LEVEL_CODE),
  CONSTRAINT VALID_ENTITY_RISK_LVL_FK1 FOREIGN KEY (RISK_TYPE_CODE) REFERENCES entity_risk_type (RISK_TYPE_CODE),
  CONSTRAINT VALID_ENTITY_RISK_LVL_FK2 FOREIGN KEY (RISK_LEVEL_CODE) REFERENCES entity_risk_level (RISK_LEVEL_CODE)
);

DROP TABLE IF EXISTS valid_entity_attach_type;
CREATE TABLE valid_entity_attach_type (
  VALID_ENTITY_ATTACH_TYPE_ID int NOT NULL AUTO_INCREMENT,
  ATTACHMENT_TYPE_CODE varchar(3) DEFAULT NULL,
  ENTITY_SECTION_CODE varchar(3) DEFAULT NULL,
  PRIMARY KEY (VALID_ENTITY_ATTACH_TYPE_ID),
  KEY VALID_ENTITY_ATTACH_TYPE_FK1_idx (ATTACHMENT_TYPE_CODE),
  KEY VALID_ENTITY_ATTACH_TYPE_FK2_idx (ENTITY_SECTION_CODE),
  CONSTRAINT VALID_ENTITY_ATTACH_TYPE_FK1 FOREIGN KEY (ATTACHMENT_TYPE_CODE) REFERENCES entity_attachment_type (ATTACHMENT_TYPE_CODE),
  CONSTRAINT VALID_ENTITY_ATTACH_TYPE_FK2 FOREIGN KEY (ENTITY_SECTION_CODE) REFERENCES entity_section (ENTITY_SECTION_CODE)
);

set foreign_key_checks = 1;
