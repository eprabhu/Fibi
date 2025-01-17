DELIMITER //
CREATE PROCEDURE `COI_DISCL_VOID_DEACTIVATED_PROJECT`(
AV_MODULE_CODE		VARCHAR(40),
AV_MODULE_ITEM_KEY	VARCHAR(100),
AV_PERSON_ID 		VARCHAR(40),
AV_REMARK	 		VARCHAR(225)
)
BEGIN

	IF AV_PERSON_ID IS NOT NULL THEN
        UPDATE COI_DISCLOSURE t1
        INNER JOIN COI_DISCL_PROJECTS t2 ON t2.DISCLOSURE_ID = t1.DISCLOSURE_ID
        SET t1.DISPOSITION_STATUS_CODE = 2, t1.REMARK = AV_REMARK
        WHERE t1.FCOI_TYPE_CODE = 2 AND t2.MODULE_ITEM_KEY = AV_MODULE_ITEM_KEY AND t2.MODULE_CODE = AV_MODULE_CODE AND t1.PERSON_ID = AV_PERSON_ID ;
	ELSE
		UPDATE COI_DISCLOSURE t1
		INNER JOIN COI_DISCL_PROJECTS t2 ON t2.DISCLOSURE_ID = t1.DISCLOSURE_ID
        SET t1.DISPOSITION_STATUS_CODE = 2, t1.REMARK = AV_REMARK
        WHERE t1.FCOI_TYPE_CODE = 2 AND t2.MODULE_ITEM_KEY = AV_MODULE_ITEM_KEY AND t2.MODULE_CODE = AV_MODULE_CODE;
	END IF;

END

//