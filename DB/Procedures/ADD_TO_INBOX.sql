CREATE DEFINER=`root`@`%` PROCEDURE `coi`.`ADD_TO_INBOX`(
AV_MODULE_CODE            	INT(3),
AV_MODULE_ITEM_ID         	VARCHAR(20),
AV_PERSON_ID              	VARCHAR(40),
AV_SUBJECT_TYPE           	VARCHAR(1),
AV_USER_MESSAGE           	VARCHAR(4000),
AV_UPDATE_USER            	VARCHAR(60),
AV_MESSAGE_TYPE_CODE     	VARCHAR(20),
AV_SUBMODULE_CODE         	INT(3),
AV_SUB_MODULE_ITEM_KEY    	VARCHAR(20),
AV_ALERT_TYPE		      	VARCHAR(20),
AV_EXPIRATION_DATE			DATETIME
)
    DETERMINISTIC
BEGIN

DECLARE LI_SEQ_INBOX_ID          INT(12);


SET SQL_SAFE_UPDATES = 0;

SELECT IFNULL(MAX(NEXT_VAL),0)+1 INTO LI_SEQ_INBOX_ID 
FROM INBOX_ID_GENERATOR;

UPDATE INBOX_ID_GENERATOR SET NEXT_VAL = LI_SEQ_INBOX_ID+1;
                        

INSERT INTO INBOX(
					INBOX_ID,
					MESSAGE_TYPE_CODE,
					USER_MESSAGE,
					MODULE_CODE,
					MODULE_ITEM_KEY,
					OPENED_FLAG,
					SUBJECT_TYPE,
					TO_PERSON_ID,
					ARRIVAL_DATE,
					UPDATE_TIMESTAMP,
					UPDATE_USER,
					SUB_MODULE_CODE,
					SUB_MODULE_ITEM_KEY,
					ALERT_TYPE,
					EXPIRATION_DATE
)
VALUES(
					LI_SEQ_INBOX_ID,
					AV_MESSAGE_TYPE_CODE,
					AV_USER_MESSAGE,
					AV_MODULE_CODE,
					AV_MODULE_ITEM_ID,
					'N',
					AV_SUBJECT_TYPE,
					AV_PERSON_ID,
					UTC_TIMESTAMP(),
					UTC_TIMESTAMP(),
					AV_UPDATE_USER,
					AV_SUBMODULE_CODE,
					AV_SUB_MODULE_ITEM_KEY,
					AV_ALERT_TYPE,
					AV_EXPIRATION_DATE
);

END