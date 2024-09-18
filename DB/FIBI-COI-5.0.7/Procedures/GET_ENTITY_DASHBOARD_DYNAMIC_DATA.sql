DELIMITER //
CREATE PROCEDURE GET_ENTITY_DASHBOARD_DYNAMIC_DATA(
JSON_FORMAT JSON
)
BEGIN

DECLARE LS_DYN_SQL LONGTEXT DEFAULT '';
DECLARE LS_FILTER_CONDITION LONGTEXT DEFAULT '';
DECLARE LS_OFFSET_CONDITION VARCHAR(600);
DECLARE LS_OFFSET INT(11) ;
DECLARE TAB_QUERY LONGTEXT DEFAULT '';
DECLARE JOIN_CONDITION LONGTEXT DEFAULT '';

DECLARE AV_TAB_TYPE                     VARCHAR(30);
DECLARE AV_SORT_TYPE                    VARCHAR(500);
DECLARE AV_LIMIT                        INT(10);
DECLARE AV_PAGED                        INT(10);
DECLARE AV_UNLIMITED                    BOOLEAN;
DECLARE AV_TYPE                         VARCHAR(1);

DECLARE AV_PERSON_ID					VARCHAR(40);
DECLARE AV_ENTITY_NAME					VARCHAR(500);
DECLARE AV_OWNERSHIP_TYPE_CODE  		varchar(500);
DECLARE AV_PRIMARY_ADDRESS_LINE_1		varchar(500);
DECLARE AV_PRIMARY_ADDRESS_LINE_2		varchar(500);
DECLARE AV_CITY 						varchar(30);
DECLARE AV_STATE						varchar(30);
DECLARE AV_COUNTRY_CODE 				varchar(3);
DECLARE AV_DUNS_NUMBER					varchar(20);
DECLARE AV_UEI_NUMBER					varchar(20);
DECLARE AV_CAGE_NUMBER 					varchar(20);
DECLARE AV_WEBSITE_ADDRESS 				varchar(500);
DECLARE AV_CERTIFIED_EMAIL 				varchar(200);
DECLARE AV_ENTITY_STATUS_TYPE_CODE  	varchar(10);
DECLARE AV_VERIFICATION_STATUS 			varchar(30);
DECLARE AV_FOREIGN_NAME					varchar(500);
DECLARE AV_PRIOR_NAME 					varchar(200);
DECLARE LS_ERROR_MSG 					VARCHAR(1000);
DECLARE CONTINUE HANDLER FOR SQLEXCEPTION
		BEGIN
			GET DIAGNOSTICS CONDITION 1 
					@sqlstate = RETURNED_SQLSTATE, 
					@errno = MYSQL_ERRNO, 
					@msg = MESSAGE_TEXT;

			SET @full_error = CONCAT("ERROR in GET_ENTITY_DASHBOARD_DYNAMIC_DATA ", @errno, " (", @sqlstate, "): ", @msg);
			
			SELECT @full_error INTO LS_ERROR_MSG;

			select LS_ERROR_MSG;
		
		END;

SET AV_TAB_TYPE = JSON_UNQUOTE(JSON_EXTRACT(JSON_FORMAT,'$.TAB_TYPE'));
SET AV_SORT_TYPE = JSON_UNQUOTE(JSON_EXTRACT(JSON_FORMAT,'$.SORT_TYPE'));
SET AV_LIMIT = JSON_UNQUOTE(JSON_EXTRACT(JSON_FORMAT,'$.LIMIT'));
SET AV_PAGED = JSON_UNQUOTE(JSON_EXTRACT(JSON_FORMAT,'$.PAGED'));
SET AV_UNLIMITED  = JSON_EXTRACT(JSON_FORMAT,'$.UNLIMITED');
SET AV_TYPE = JSON_UNQUOTE(JSON_EXTRACT(JSON_FORMAT,'$.TYPE'));

SET AV_PERSON_ID = JSON_UNQUOTE(JSON_EXTRACT(JSON_FORMAT,'$.PERSON_ID'));
SET AV_ENTITY_NAME = JSON_UNQUOTE(JSON_EXTRACT(JSON_FORMAT,'$.PRIMARY_NAME'));
SET AV_OWNERSHIP_TYPE_CODE = JSON_UNQUOTE(JSON_EXTRACT(JSON_FORMAT,'$.OWNERSHIP_TYPE_CODE'));
SET AV_PRIMARY_ADDRESS_LINE_1 = JSON_UNQUOTE(JSON_EXTRACT(JSON_FORMAT,'$.PRIMARY_ADDRESS_LINE_1'));
SET AV_PRIMARY_ADDRESS_LINE_2 = JSON_UNQUOTE(JSON_EXTRACT(JSON_FORMAT,'$.PRIMARY_ADDRESS_LINE_2'));
SET AV_CITY = JSON_UNQUOTE(JSON_EXTRACT(JSON_FORMAT,'$.CITY'));
SET AV_STATE = JSON_UNQUOTE(JSON_EXTRACT(JSON_FORMAT,'$.STATE'));
SET AV_COUNTRY_CODE = JSON_UNQUOTE(JSON_EXTRACT(JSON_FORMAT,'$.COUNTRY'));
SET AV_DUNS_NUMBER = JSON_UNQUOTE(JSON_EXTRACT(JSON_FORMAT,'$.DUNS_NUMBER'));
SET AV_UEI_NUMBER = JSON_UNQUOTE(JSON_EXTRACT(JSON_FORMAT,'$.UEI_NUMBER'));
SET AV_CAGE_NUMBER = JSON_UNQUOTE(JSON_EXTRACT(JSON_FORMAT,'$.CAGE_NUMBER'));
SET AV_WEBSITE_ADDRESS = JSON_UNQUOTE(JSON_EXTRACT(JSON_FORMAT,'$.WEBSITE_ADDRESS'));
SET AV_CERTIFIED_EMAIL = JSON_UNQUOTE(JSON_EXTRACT(JSON_FORMAT,'$.CERTIFIED_EMAIL'));
SET AV_ENTITY_STATUS_TYPE_CODE = JSON_UNQUOTE(JSON_EXTRACT(JSON_FORMAT,'$.ENTITY_STATUS_TYPE_CODE'));
SET AV_VERIFICATION_STATUS = JSON_UNQUOTE(JSON_EXTRACT(JSON_FORMAT,'$.VERIFICATION_STATUS'));
SET AV_FOREIGN_NAME = JSON_UNQUOTE(JSON_EXTRACT(JSON_FORMAT,'$.FOREIGN_NAME'));
SET AV_PRIOR_NAME = JSON_UNQUOTE(JSON_EXTRACT(JSON_FORMAT,'$.PRIOR_NAME'));

SET LS_OFFSET = (AV_LIMIT * AV_PAGED);
SET LS_FILTER_CONDITION ='';
SET LS_DYN_SQL ='';
SET JOIN_CONDITION = '';

IF AV_TYPE = 'A' THEN
	IF AV_ENTITY_NAME IS NOT NULL  AND AV_ENTITY_NAME <> '' THEN
			SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' T.PRIMARY_NAME = ''',AV_ENTITY_NAME,''' AND ');
	END IF;
	IF AV_OWNERSHIP_TYPE_CODE IS NOT NULL  AND AV_OWNERSHIP_TYPE_CODE <> '' THEN
			SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' T.ENTITY_OWNERSHIP_TYPE_CODE IN( ''',replacE(replace(AV_OWNERSHIP_TYPE_CODE,' ' , ''), ',', ''',''') ,''') AND ');
	END IF;
	IF AV_PRIMARY_ADDRESS_LINE_1 IS NOT NULL  AND AV_PRIMARY_ADDRESS_LINE_1 <> '' THEN
			SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' T.PRIMARY_ADDRESS_LINE_1 = ''',AV_PRIMARY_ADDRESS_LINE_1,''' AND ');
	END IF;
	IF AV_PRIMARY_ADDRESS_LINE_2 IS NOT NULL  AND AV_PRIMARY_ADDRESS_LINE_2 <> '' THEN
			SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' T.PRIMARY_ADDRESS_LINE_1 = ''',AV_PRIMARY_ADDRESS_LINE_2,''' AND ');
	END IF;
	IF AV_CITY IS NOT NULL AND AV_CITY <> '' THEN
			SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' T.CITY = ''',AV_CITY,''' AND ');
	END IF;
	IF AV_STATE IS NOT NULL AND AV_STATE <> '' THEN
			SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' T.STATE = ''',AV_STATE,''' AND ');
	END IF; 
	IF AV_COUNTRY_CODE IS NOT NULL AND AV_COUNTRY_CODE <> '' THEN
			SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' T.COUNTRY_CODE = ''',AV_COUNTRY_CODE,''' AND ');
	END IF;
	IF AV_DUNS_NUMBER IS NOT NULL AND AV_DUNS_NUMBER <> '' THEN
			SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' T.DUNS_NUMBER = ''',AV_DUNS_NUMBER,''' AND ');
	END IF;
	IF AV_UEI_NUMBER IS NOT NULL AND AV_UEI_NUMBER <> '' THEN
			SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' T.UEI_NUMBER = ''',AV_UEI_NUMBER,''' AND ');
	END IF;
	IF AV_CAGE_NUMBER IS NOT NULL AND AV_CAGE_NUMBER <> '' THEN
			SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' T.CAGE_NUMBER = ''',AV_CAGE_NUMBER,''' AND ');
	END IF;
	IF AV_WEBSITE_ADDRESS IS NOT NULL AND AV_WEBSITE_ADDRESS <> '' THEN
			SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' T.WEBSITE_ADDRESS = ''',AV_WEBSITE_ADDRESS,''' AND ');
	END IF;
	IF AV_CERTIFIED_EMAIL IS NOT NULL AND AV_CERTIFIED_EMAIL <> '' THEN
			SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' T.CERTIFIED_EMAIL = ''',AV_CERTIFIED_EMAIL,''' AND ');
	END IF;
		IF AV_ENTITY_STATUS_TYPE_CODE IS NOT NULL  AND AV_ENTITY_STATUS_TYPE_CODE <> '' THEN
			SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' T.ENTITY_STATUS IN (''',replacE(replace(AV_ENTITY_STATUS_TYPE_CODE,' ' , ''), ',', ''',''') ,''') AND ');
	END IF;
	IF AV_VERIFICATION_STATUS IS NOT NULL  AND AV_VERIFICATION_STATUS <> '' THEN
			SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' T.ENTITY_STATUS_TYPE_CODE IN (''',replacE(replace(AV_VERIFICATION_STATUS,' ' , ''), ',', ''','''),''') AND ');
	END IF;
	IF AV_FOREIGN_NAME IS NOT NULL  AND AV_FOREIGN_NAME <> '' THEN
			SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' T.FOREIGN_NAME LIKE ''%',AV_FOREIGN_NAME,'%'' AND ');
	END IF;
	IF AV_PRIOR_NAME IS NOT NULL  AND AV_PRIOR_NAME <> '' THEN
			SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' T.PRIOR_NAME LIKE ''%',AV_PRIOR_NAME,'%'' AND ');
	END IF;
END IF;

	IF AV_TAB_TYPE = 'ALL_ENTITY' THEN
		SET TAB_QUERY = CONCAT('' );
	ELSEIF AV_TAB_TYPE = 'UNVERIFIED' THEN
		SET TAB_QUERY = CONCAT(' AND T1.ENTITY_STATUS_TYPE_CODE = 2' );
	/*ELSEIF AV_TAB_TYPE = 'UNVERIFIED' THEN
		SET TAB_QUERY = CONCAT(' AND T1.ENTITY_STATUS_TYPE_CODE = 2' );*/
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

	IF LS_FILTER_CONDITION <> '' THEN
			SET LS_FILTER_CONDITION = CONCAT(' WHERE ',LS_FILTER_CONDITION);
			SET LS_FILTER_CONDITION = TRIM(TRAILING 'AND ' FROM LS_FILTER_CONDITION);
	END IF;

	SET LS_DYN_SQL = CONCAT('
	With RC as (SELECT COUNT(DISTINCT RLE.RIGHT_NAME) as RIGHTS_COUNT
					FROM PERSON_ROLES PR
					JOIN (
						SELECT ROLE_ID, RT.RIGHT_NAME 
						FROM ROLE_RIGHTS RR
						JOIN RIGHTS RT ON RR.RIGHT_ID = RT.RIGHT_ID
						WHERE RT.RIGHT_NAME IN (
							''MANAGE_ENTITY'', ''VIEW_ENTITY'', ''VERIFY_ENTITY'', 
							''MANAGE_ENTITY_SPONSOR'', ''MANAGE_ENTITY_ORGANIZATION'', 
							''MANAGE_ENTITY_COMPLIANCE''
						)
					) RLE ON PR.ROLE_ID = RLE.ROLE_ID
					WHERE PR.PERSON_ID =''', AV_PERSON_ID,''')
			SELECT DISTINCT
					ENTITY_ID,
					ENTITY_NUMBER,
					PRIMARY_NAME,
					OWNERSHIP_TYPE,
					PRIMARY_ADDRESS_LINE_1,
					PRIMARY_ADDRESS_LINE_2,
					COUNTRY,
					CITY,
					STATE,
					DUNS_NUMBER,
					UEI_NUMBER,
					CAGE_NUMBER,
					WEBSITE_ADDRESS,
					CERTIFIED_EMAIL,
					VERIFICATION_STATUS,
					CASE ENTITY_STATUS WHEN ''Y'' THEN ''Active'' WHEN ''N'' THEN ''Inactive'' WHEN ''D'' THEN ''Duplicate'' END  as ENTITY_STATUS
			FROM (
				SELECT DISTINCT
					T1.ENTITY_ID,
					T1.ENTITY_NUMBER,
					T1.PRIMARY_NAME,
					T3.DESCRIPTION AS OWNERSHIP_TYPE,
					T3.OWNERSHIP_TYPE_CODE as ENTITY_OWNERSHIP_TYPE_CODE,
					T1.PRIMARY_ADDRESS_LINE_1,
					T1.PRIMARY_ADDRESS_LINE_2,
					T2.COUNTRY_NAME AS COUNTRY,
					T2.COUNTRY_CODE,
					T1.CITY,
					T1.STATE,
					T1.DUNS_NUMBER,
					T1.UEI_NUMBER,
					T1.CAGE_NUMBER,
					T1.WEBSITE_ADDRESS,
					T1.CERTIFIED_EMAIL,
					T5.ENTITY_STATUS_TYPE_CODE,
					T5.DESCRIPTION AS VERIFICATION_STATUS,
					T1.IS_ACTIVE AS ENTITY_STATUS,
					T1.UPDATE_TIMESTAMP,
					EPN.PRIOR_NAME,
					EFN.FOREIGN_NAME
				FROM ENTITY T1
				LEFT OUTER JOIN entity_status_type T5 ON T5.ENTITY_STATUS_TYPE_CODE = T1.ENTITY_STATUS_TYPE_CODE
				LEFT OUTER JOIN COUNTRY T2 ON T2.COUNTRY_CODE = T1.COUNTRY_CODE
				LEFT OUTER JOIN entity_ownership_type T3 ON T3.OWNERSHIP_TYPE_CODE = T1.ENTITY_OWNERSHIP_TYPE_CODE
				LEFT OUTER JOIN entity_prior_name EPN on EPN.ENTITY_ID = T1.ENTITY_ID
				LEFT OUTER JOIN entity_foreign_name EFN on EFN.ENTITY_ID = T1.ENTITY_ID'
				, JOIN_CONDITION, ' WHERE T1.VERSION_STATUS != ''ARCHIVE'' AND  (select rights_count from RC) > 0 ', TAB_QUERY, ') T '
				, LS_FILTER_CONDITION, ' ', AV_SORT_TYPE, ' ', LS_OFFSET_CONDITION, ';');
				
        SET @QUERY_STATEMENT = LS_DYN_SQL;
		PREPARE EXECUTABLE_STAEMENT FROM @QUERY_STATEMENT;
		EXECUTE EXECUTABLE_STAEMENT;
END
//