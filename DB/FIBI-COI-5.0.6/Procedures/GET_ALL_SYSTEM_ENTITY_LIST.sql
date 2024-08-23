DELIMITER //
CREATE PROCEDURE `GET_ALL_SYSTEM_ENTITY_LIST`(
AV_TAB_TYPE                     VARCHAR(30),
AV_SORT_TYPE                    VARCHAR(500),
AV_LIMIT                        INT(10),
AV_PAGED                        INT(10),
AV_COUNTRY_NAME                 VARCHAR(60),
AV_UNLIMITED                    BOOLEAN,
AV_TYPE                         VARCHAR(1),
AV_ENTITY_STATUS                VARCHAR(20),
AV_ENTITY_TYPE               	VARCHAR(30),
AV_ENTITY_RISK					VARCHAR(500),
AV_HAS_SFI						BOOLEAN,
AV_HAS_DISCLOSURE				BOOLEAN,
AV_ENTITY_NAME					VARCHAR(500),
AV_VERIFICATION_STATUS			VARCHAR(10)
)
BEGIN

DECLARE LS_DYN_SQL LONGTEXT;
DECLARE LS_FILTER_CONDITION LONGTEXT;
DECLARE LS_OFFSET_CONDITION VARCHAR(600);
DECLARE LS_OFFSET INT(11);
DECLARE TAB_QUERY LONGTEXT;
DECLARE JOIN_CONDITION LONGTEXT;
DECLARE SELECTED_FIELD_LIST LONGTEXT;

SET LS_OFFSET = (AV_LIMIT * AV_PAGED);
SET LS_FILTER_CONDITION ='';
SET LS_DYN_SQL ='';
SET JOIN_CONDITION = '';
SET SELECTED_FIELD_LIST= '';

IF AV_COUNTRY_NAME IS NOT NULL AND AV_COUNTRY_NAME <> '' THEN
                SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' T.COUNTRY_CODE = ''',AV_COUNTRY_NAME,''' AND ');
END IF;

IF AV_ENTITY_STATUS IS NOT NULL  AND AV_ENTITY_STATUS <> '' THEN
				SET @LS_ENTITY_STATUS = REPLACE(AV_ENTITY_STATUS, ',', ''',''');
                SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' T.IS_ACTIVE IN (''',@LS_ENTITY_STATUS,''') AND ');
END IF;

IF AV_ENTITY_NAME IS NOT NULL  AND AV_ENTITY_NAME <> '' THEN
                SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' T.ENTITY_NAME = ''',AV_ENTITY_NAME,''' AND ');
END IF;

IF AV_VERIFICATION_STATUS IS NOT NULL  AND AV_VERIFICATION_STATUS <> '' THEN
                SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' T.ENTITY_STATUS_TYPE_CODE IN (',AV_VERIFICATION_STATUS,') AND ');
END IF;

IF AV_ENTITY_TYPE IS NOT NULL  AND AV_ENTITY_TYPE <> '' THEN
                SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' T.ENTITY_TYPE_CODE IN (',AV_ENTITY_TYPE,') AND ');
END IF;

IF AV_ENTITY_RISK IS NOT NULL  AND AV_ENTITY_RISK <> '' THEN
                SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' T.RISK_CATEGORY_CODE IN (',AV_ENTITY_RISK,') AND ');
END IF;

IF AV_HAS_SFI = TRUE THEN
				SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' T.ENTITY_ID IN (SELECT T1.ENTITY_ID FROM PERSON_ENTITY T1) AND ');
END IF;

IF AV_HAS_DISCLOSURE = TRUE THEN
				SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' T.ENTITY_ID IN (SELECT T1.ENTITY_ID FROM COI_DISCL_PROJECT_ENTITY_REL T1) AND ');
END IF;

IF AV_TAB_TYPE = 'ALL_ENTITIES' THEN

               --  SET TAB_QUERY = CONCAT(' AND T1.ENTITY_STATUS_TYPE_CODE IN (1,2)' );
               SET TAB_QUERY = CONCAT('' );

        ELSEIF AV_TAB_TYPE = 'UNVERIFIED' THEN

		SET TAB_QUERY = CONCAT(' AND T1.ENTITY_STATUS_TYPE_CODE = 2' );
                
        ELSEIF AV_TAB_TYPE = 'HIGH_RISK' THEN

                SET TAB_QUERY = CONCAT(' AND  T1.RISK_CATEGORY_CODE = 1' );
END IF;

IF AV_SORT_TYPE IS NULL OR AV_SORT_TYPE = '' THEN
        SET AV_SORT_TYPE =  CONCAT(' ORDER BY T.UPDATE_TIMESTAMP DESC ');
ELSE
    SET AV_SORT_TYPE = CONCAT(' ORDER BY ',AV_SORT_TYPE);
END IF;

IF AV_UNLIMITED = TRUE THEN
        SET LS_OFFSET_CONDITION = '';
ELSE
        SET LS_OFFSET_CONDITION = CONCAT(' LIMIT ',AV_LIMIT,' OFFSET ',LS_OFFSET);
END IF;

IF LS_FILTER_CONDITION <>'' THEN
        SET LS_FILTER_CONDITION = CONCAT(' WHERE ',LS_FILTER_CONDITION);
        SELECT TRIM(TRAILING 'AND ' FROM LS_FILTER_CONDITION) into LS_FILTER_CONDITION from dual;
END IF;

SET LS_DYN_SQL = CONCAT('SELECT DISTINCT *
FROM (
    SELECT DISTINCT
        T1.ENTITY_ID,
        T1.ENTITY_NUMBER,
        T1.PRIMARY_NAME as ENTITY_NAME,
        T1.ENTITY_STATUS_TYPE_CODE,
        T1.ENTITY_OWNERSHIP_TYPE_CODE,
        T2.COUNTRY_NAME AS COUNTRY,
        T2.COUNTRY_CODE,
        T3.DESCRIPTION AS ENTITY_TYPE,
        T5.DESCRIPTION AS STATUS,
        T1.IS_ACTIVE,
        T1.CREATE_TIMESTAMP,
        T1.UPDATE_TIMESTAMP
    FROM ENTITY T1
    LEFT OUTER JOIN entity_status_type T5 ON T5.ENTITY_STATUS_TYPE_CODE = T1.ENTITY_STATUS_TYPE_CODE
    LEFT OUTER JOIN COUNTRY T2 ON T2.COUNTRY_CODE = T1.COUNTRY_CODE
    LEFT OUTER JOIN entity_ownership_type T3 ON T3.OWNERSHIP_TYPE_CODE = T1.ENTITY_OWNERSHIP_TYPE_CODE'
    , JOIN_CONDITION, ' WHERE T1.VERSION_STATUS != ''ARCHIVE'' ', TAB_QUERY, ') T ', LS_FILTER_CONDITION, ' ', AV_SORT_TYPE, ' ', LS_OFFSET_CONDITION, ';');

SET @QUERY_STATEMENT = LS_DYN_SQL;
PREPARE EXECUTABLE_STAEMENT FROM @QUERY_STATEMENT;
EXECUTE EXECUTABLE_STAEMENT;

END
//