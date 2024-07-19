ALTER TABLE PERSON ADD COLUMN ALIAS_NAME VARCHAR (30) DEFAULT NULL;
ALTER TABLE PERSON ADD COLUMN GENDER_CODE varchar(3) DEFAULT NULL;
ALTER TABLE PERSON ADD KEY `PERSON_FK2` (`GENDER_CODE`);
ALTER TABLE PERSON ADD FOREIGN KEY (GENDER_CODE) REFERENCES GENDER(GENDER_CODE);
ALTER TABLE PERSON ADD COLUMN ALTERNATE_MOBILE_NUMBER varchar(20) DEFAULT NULL;
ALTER TABLE PERSON ADD COLUMN SECONDARY_EMAIL_ADDRESS varchar(60) DEFAULT NULL;

CREATE TABLE `GENDER` (
  `GENDER_CODE` VARCHAR(3) NOT NULL,
  `GENDER_NAME` VARCHAR(200) DEFAULT NULL,
  `UPDATE_TIMESTAMP` DATETIME DEFAULT NULL,
  `UPDATE_USER` VARCHAR(60) DEFAULT NULL,
  PRIMARY KEY (`GENDER_CODE`)
);
 
ALTER TABLE GENDER RENAME COLUMN GENDER_NAME TO DESCRIPTION;
 
INSERT INTO `gender` (`GENDER_CODE`,`DESCRIPTION`,`UPDATE_TIMESTAMP`,`UPDATE_USER`) VALUES ('F','Female',now(),'admin');
INSERT INTO `gender` (`GENDER_CODE`,`DESCRIPTION`,`UPDATE_TIMESTAMP`,`UPDATE_USER`) VALUES ('M','Male',now(),'admin');
 
 
UPDATE person AS t1
SET t1.GENDER_CODE = (
    SELECT t2.GENDER_CODE
    FROM gender AS t2
    WHERE LOWER(t2.DESCRIPTION) = LOWER(t1.GENDER)
)
WHERE GENDER_CODE is null;
 
 
ALTER TABLE UNIT RENAME COLUMN ACTIVE_FLAG TO IS_ACTIVE;
ALTER TABLE UNIT_LEVEL RENAME COLUMN ACTIVE_FLAG TO IS_ACTIVE;
ALTER TABLE ROLODEX RENAME COLUMN ACTV_IND TO IS_ACTIVE;
ALTER TABLE SPONSOR RENAME COLUMN ACTV_IND TO IS_ACTIVE;