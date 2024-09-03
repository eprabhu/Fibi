DELIMITER //
CREATE PROCEDURE COI_INT_KC_SPON_ORG_RESPONSE(
AV_ENTITY_ID INT,
AV_ORG_FEED_STATUS VARCHAR(4000),
AV_SPON_FEED_STATUS VARCHAR(4000),
AV_SPONSOR_CODE VARCHAR(10),
AV_ORGANIZATION_ID VARCHAR(100)
)
BEGIN

    IF AV_ORG_FEED_STATUS IS NOT NULL AND UPPER(TRIM(AV_ORG_FEED_STATUS)) = 'SYNC_SUCCESS' THEN
       SET AV_ORG_FEED_STATUS = '3'; 
    ELSE
       SET AV_ORG_FEED_STATUS = '4'; 
    END IF;
    
	IF AV_SPON_FEED_STATUS IS NOT NULL AND UPPER(TRIM(AV_SPON_FEED_STATUS)) = 'SYNC_SUCCESS' THEN
       SET AV_SPON_FEED_STATUS = '3'; 
    ELSE
       SET AV_SPON_FEED_STATUS = '4'; 
    END IF;

SET SQL_SAFE_UPDATES = 0;

IF AV_SPONSOR_CODE IS NOT NULL THEN 
UPDATE ENTITY_SPONSOR_INFO
SET SPONSOR_CODE = AV_SPONSOR_CODE,
FEED_STATUS_CODE = AV_SPON_FEED_STATUS
WHERE ENTITY_ID = AV_ENTITY_ID;
END IF;

IF AV_ORGANIZATION_ID IS NOT NULL THEN 
UPDATE ENTITY_SUB_ORG_INFO
SET ORGANIZATION_ID = AV_ORGANIZATION_ID,
FEED_STATUS_CODE = AV_ORG_FEED_STATUS
WHERE ENTITY_ID = AV_ENTITY_ID;

END IF;

SET SQL_SAFE_UPDATES = 1;
END;
//

