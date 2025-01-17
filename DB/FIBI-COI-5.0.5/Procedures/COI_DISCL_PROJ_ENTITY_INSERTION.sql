DELIMITER //
CREATE PROCEDURE `COI_DISCL_PROJ_ENTITY_INSERTION`(
	AV_COI_DISCL_PROJECTS_ID          	INT,
	AV_DISCLOSURE_ID 					INT,
	AV_DISCLOSURE_NUMBER 				INT,
	AV_MODULE_CODE						INT(10),
	AV_MODULE_ITEM_KEY					VARCHAR(100),
	AV_PERSON_ID        				VARCHAR(40),
    AV_UPDATED_BY        				VARCHAR(40),
	AV_SFI_JSON_ARRAY      				JSON
)
BEGIN

	DECLARE i INT DEFAULT 0;
    DECLARE array_length INT DEFAULT JSON_LENGTH(AV_SFI_JSON_ARRAY);

	DECLARE LS_COMMENT TEXT;
	DECLARE LS_PROJECT_CONFLICT_STATUS_CODE VARCHAR(10);


    WHILE i < array_length DO

        SET @json_obj = JSON_EXTRACT(AV_SFI_JSON_ARRAY, CONCAT('$[', i, ']'));

        SET @LI_ENTITY_ID = JSON_UNQUOTE(JSON_EXTRACT(@json_obj, '$.ENTITY_ID'));
        SET @LI_PERSON_ENTITY_ID = JSON_UNQUOTE(JSON_EXTRACT(@json_obj, '$.PERSON_ENTITY_ID'));
        SET @LI_PERSON_ENTITY_NUMBER = JSON_UNQUOTE(JSON_EXTRACT(@json_obj, '$.PERSON_ENTITY_NUMBER'));


		SET LS_COMMENT = null;
		SET LS_PROJECT_CONFLICT_STATUS_CODE = null;

		SELECT DISTINCT T1.PROJECT_CONFLICT_STATUS_CODE, T3.COMMENT INTO LS_PROJECT_CONFLICT_STATUS_CODE, LS_COMMENT
			FROM COI_DISCL_PROJECT_ENTITY_REL T1
			INNER JOIN COI_DISCL_PROJECTS T2 ON T2.COI_DISCL_PROJECTS_ID =T1.COI_DISCL_PROJECTS_ID
			INNER JOIN (
						SELECT COMMENT, MODULE_ITEM_KEY, MODULE_ITEM_NUMBER, SUB_MODULE_ITEM_KEY
						FROM DISCL_COMMENT WHERE COMPONENT_TYPE_CODE = '1' AND MODULE_CODE = 8
				) T3 ON T3.MODULE_ITEM_KEY = T2.DISCLOSURE_ID AND T3.SUB_MODULE_ITEM_KEY = T1.COI_DISCL_PROJECT_ENTITY_REL_ID
			WHERE T1.PERSON_ENTITY_NUMBER = @LI_PERSON_ENTITY_NUMBER AND T1.ENTITY_ID = @LI_ENTITY_ID
				AND T2.MODULE_CODE =AV_MODULE_CODE AND T2.MODULE_ITEM_KEY = AV_MODULE_ITEM_KEY;

		INSERT INTO COI_DISCL_PROJECT_ENTITY_REL(COI_DISCL_PROJECTS_ID, PERSON_ENTITY_ID, PERSON_ENTITY_NUMBER, ENTITY_ID,
			PROJECT_CONFLICT_STATUS_CODE, UPDATED_BY, UPDATE_TIMESTAMP)
			values (AV_COI_DISCL_PROJECTS_ID, @LI_PERSON_ENTITY_ID, @LI_PERSON_ENTITY_NUMBER, @LI_ENTITY_ID,
			LS_PROJECT_CONFLICT_STATUS_CODE, AV_UPDATED_BY, UTC_TIMESTAMP());

        SET i = i + 1;
    END WHILE;
	UPDATE COI_DISCLOSURE SET SYNC_NEEDED = "N" WHERE DISCLOSURE_ID = AV_DISCLOSURE_ID;
END
//