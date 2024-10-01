DELIMITER //
CREATE PROCEDURE `COI_INT_KC_SPON_ORG_RESPONSE`(
AV_ENTITY_ID INT,
AV_ORG_FEED_STATUS LONGTEXT,
AV_SPON_FEED_STATUS LONGTEXT,
AV_SPONSOR_CODE VARCHAR(10),
AV_ORGANIZATION_ID VARCHAR(100)
)
BEGIN
DECLARE LS_ENTITY_NAME VARCHAR(2000) DEFAULT '';
DECLARE LS_PREV_STATUS VARCHAR(2000) DEFAULT '';
DECLARE LS_ACTION_MSGE VARCHAR(2000) DEFAULT '';
DECLARE LS_CURRENT_STATUS VARCHAR(2000) DEFAULT '';

    IF AV_ORG_FEED_STATUS IS NOT NULL THEN
		IF UPPER(TRIM(AV_ORG_FEED_STATUS)) = 'SYNC_SUCCESS' THEN
			SET AV_ORG_FEED_STATUS = '3'; 
		ELSE
			SET AV_ORG_FEED_STATUS = '4'; 
		END IF;
	END IF;
    
	IF AV_SPON_FEED_STATUS IS NOT NULL THEN
		IF UPPER(TRIM(AV_SPON_FEED_STATUS)) = 'SYNC_SUCCESS' THEN
			SET AV_SPON_FEED_STATUS = '3'; 
		ELSE
			SET AV_SPON_FEED_STATUS = '4'; 
		END IF;
	END IF;

SET SQL_SAFE_UPDATES = 0;

IF AV_SPON_FEED_STATUS IS NOT NULL THEN

	SELECT E.PRIMARY_NAME,EFST.DESCRIPTION INTO LS_ENTITY_NAME,LS_PREV_STATUS FROM ENTITY E
	INNER JOIN ENTITY_SPONSOR_INFO ESI ON ESI.ENTITY_ID = E.ENTITY_ID
	INNER JOIN ENTITY_FEED_STATUS_TYPE EFST ON EFST.FEED_STATUS_CODE = ESI.FEED_STATUS_CODE
	WHERE E.ENTITY_ID = AV_ENTITY_ID;

	SELECT DESCRIPTION INTO LS_CURRENT_STATUS FROM ENTITY_FEED_STATUS_TYPE WHERE FEED_STATUS_CODE = AV_SPON_FEED_STATUS;

	IF LS_CURRENT_STATUS <> LS_PREV_STATUS THEN

		SET LS_ACTION_MSGE = CONCAT('Sponsor feed status of Entity <b>updated</b> from <b>',LS_PREV_STATUS,'</b> to <b>',LS_CURRENT_STATUS,'</b>');

		INSERT INTO ENTITY_ACTION_LOG 
		(ENTITY_ID, ENTITY_NUMBER, ACTION_TYPE_CODE, DESCRIPTION, COMMENT, UPDATE_TIMESTAMP, UPDATE_USER)
		VALUES 
		(AV_ENTITY_ID, AV_ENTITY_ID, 10, LS_ACTION_MSGE, NULL, UTC_TIMESTAMP(), 'willsmith');

	END IF;

	UPDATE ENTITY_SPONSOR_INFO
	SET SPONSOR_CODE = AV_SPONSOR_CODE,
	FEED_STATUS_CODE = AV_SPON_FEED_STATUS
	WHERE ENTITY_ID = AV_ENTITY_ID;

END IF;
 
SET LS_ENTITY_NAME = NULL;
SET LS_PREV_STATUS = NULL;
SET LS_CURRENT_STATUS = NULL;

IF AV_ORG_FEED_STATUS IS NOT NULL THEN

	SELECT E.PRIMARY_NAME,EFST.DESCRIPTION INTO LS_ENTITY_NAME,LS_PREV_STATUS FROM ENTITY E
	INNER JOIN ENTITY_SUB_ORG_INFO ESI ON ESI.ENTITY_ID = E.ENTITY_ID
	INNER JOIN ENTITY_FEED_STATUS_TYPE EFST ON EFST.FEED_STATUS_CODE = ESI.FEED_STATUS_CODE
	WHERE E.ENTITY_ID = AV_ENTITY_ID;

	SELECT DESCRIPTION INTO LS_CURRENT_STATUS FROM ENTITY_FEED_STATUS_TYPE WHERE FEED_STATUS_CODE = AV_ORG_FEED_STATUS;

	IF LS_CURRENT_STATUS <> LS_PREV_STATUS THEN

		SET LS_ACTION_MSGE = CONCAT('Organization feed status of Entity <b>updated</b> from <b>',LS_PREV_STATUS,'</b> to <b>',LS_CURRENT_STATUS,'</b>');

		INSERT INTO ENTITY_ACTION_LOG 
		(ENTITY_ID, ENTITY_NUMBER, ACTION_TYPE_CODE, DESCRIPTION, COMMENT, UPDATE_TIMESTAMP, UPDATE_USER)
		VALUES 
		(AV_ENTITY_ID, AV_ENTITY_ID, 11, LS_ACTION_MSGE,NULL, UTC_TIMESTAMP(), 'willsmith');

	END IF;

	UPDATE ENTITY_SUB_ORG_INFO
	SET ORGANIZATION_ID = AV_ORGANIZATION_ID,
	FEED_STATUS_CODE = AV_ORG_FEED_STATUS
	WHERE ENTITY_ID = AV_ENTITY_ID;

END IF;

SET SQL_SAFE_UPDATES = 1;
END
//