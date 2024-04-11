DELIMITER //
CREATE PROCEDURE `GET_COI_DISCLOSURE_ADMIN_DASHBOARD_COUNT`(
		AV_DISCLOSURE_NUMBER                VARCHAR(20),
		AV_DISCLOSURE_PERSON_ID         VARCHAR(40),
		AV_HOME_UNIT                    VARCHAR(8),
		AV_CONFLICT_STATUS_CODE       VARCHAR(30),
		AV_DISCLOSURE_CATEOGORY_TYPE    VARCHAR(30),
		AV_START_DATE                   VARCHAR(30),
		AV_END_DATE                     VARCHAR(30),
		AV_ENTITY_NAME                  VARCHAR(90),
		AV_ENTITY_COUNTRY               VARCHAR(3),
		AV_PROPOSAL_ID 					VARCHAR(20),
		AV_TITLE				        VARCHAR(1000),
		AV_AWARD_ID					    VARCHAR(20),
		AV_PERSON_ID                    VARCHAR(200),
        AV_SORT_TYPE                    VARCHAR(500),
        AV_PAGED                        INT(10),
        AV_LIMIT                        INT(10),
        AV_TAB_TYPE                     VARCHAR(30),
        AV_UNLIMITED                    BOOLEAN,
        AV_TYPE                         VARCHAR(1),
        AV_PROJECT_TYPE                 VARCHAR(3),
        AV_HAS_SFI_FLAG   				VARCHAR(3),
		AV_DISPOSITION_STATUS_CODE      VARCHAR(30),
		AV_REVIEW_STATUS_CODE       	VARCHAR(30),
		AV_CERTIFICATION_DATE           VARCHAR(30)
)
BEGIN
DECLARE LS_DYN_SQL LONGTEXT;
DECLARE LS_FILTER_CONDITION LONGTEXT;
DECLARE LS_OFFSET_CONDITION VARCHAR(600);
DECLARE LS_OFFSET INT(11);
DECLARE TAB_QUERY LONGTEXT;
DECLARE JOIN_CONDITION LONGTEXT;
DECLARE SELECTED_FIELD_LIST LONGTEXT;
DECLARE LS_ADMIN_GROUP_FCOI_CONDITION LONGTEXT;
DECLARE LS_ADMIN_GROUP_PROJECT_CONDITION LONGTEXT;

SET LS_OFFSET = (AV_LIMIT * AV_PAGED);
SET LS_FILTER_CONDITION ='';
SET LS_DYN_SQL ='';
SET JOIN_CONDITION = '';
SET SELECTED_FIELD_LIST= '';
SET TAB_QUERY = '';
SET LS_ADMIN_GROUP_FCOI_CONDITION = '';
SET LS_ADMIN_GROUP_PROJECT_CONDITION = '';

                                         
SET JOIN_CONDITION =  CONCAT(JOIN_CONDITION,' LEFT JOIN (SELECT IFNULL(COUNT(DISTINCT T2.PERSON_ENTITY_ID),0) AS NO_OF_SFI_IN_ACTIVE,T2.DISCLOSURE_ID FROM PERSON_ENTITY T1 
										INNER JOIN COI_DISCL_ENT_PROJ_DETAILS T2 ON T2.PERSON_ENTITY_ID= T1.PERSON_ENTITY_ID
										INNER JOIN COI_DISCLOSURE T3 ON T3.PERSON_ID=T1.PERSON_ID
                                        GROUP BY T2.DISCLOSURE_ID) T17 ON T17.DISCLOSURE_ID = T1.DISCLOSURE_ID
                                        LEFT JOIN (SELECT IFNULL(COUNT(DISTINCT T1.MODULE_ITEM_KEY),0) AS NO_OF_ACTIVE_PROPOSAL,T1.DISCLOSURE_ID FROM COI_DISCL_ENT_PROJ_DETAILS T1 
                                        WHERE MODULE_CODE = 3 GROUP BY T1.DISCLOSURE_ID) T18 ON T18.DISCLOSURE_ID = T1.DISCLOSURE_ID 
                                        LEFT JOIN (SELECT IFNULL(COUNT(DISTINCT T1.MODULE_ITEM_KEY),0) AS NO_OF_ACTIVE_AWARD,T1.DISCLOSURE_ID FROM COI_DISCL_ENT_PROJ_DETAILS T1 
                                        WHERE MODULE_CODE = 1 GROUP BY T1.DISCLOSURE_ID) T19 ON T19.DISCLOSURE_ID = T1.DISCLOSURE_ID
                                        LEFT JOIN (SELECT IFNULL(COUNT(DISTINCT T1.PERSON_ENTITY_ID),0) AS NO_OF_SFI_IN_PENDING,T2.DISCLOSURE_ID,T2.PERSON_ID FROM PERSON_ENTITY T1 
										INNER JOIN COI_DISCLOSURE T2 ON T2.PERSON_ID=T1.PERSON_ID GROUP BY T2.DISCLOSURE_ID) T22 ON T22.PERSON_ID = T1.PERSON_ID 
										LEFT JOIN COI_REVIEW T34 ON T34.DISCLOSURE_ID = T1.DISCLOSURE_ID ');


-- Temporary tables for right check for FCOI & project diclosure

IF AV_TAB_TYPE = 'ALL_REVIEWS' THEN
	SET LS_DYN_SQL = CONCAT('WITH ACCESS_TMP AS (SELECT T1.UNIT_NUMBER FROM PERSON_ROLES T1 
                            INNER JOIN ADMIN_GROUP T2 ON T2.ROLE_ID = T1.ROLE_ID 
							WHERE T2.MODULE_CODE=8 AND T1.PERSON_ID = ''',AV_PERSON_ID,''') ' );

ELSEIF AV_TAB_TYPE = 'MY_REVIEWS'  THEN
	SET LS_DYN_SQL = CONCAT('');

ELSE

	SET LS_DYN_SQL = CONCAT('WITH FCOI_ACCESS_TMP AS ( SELECT UNIT_NUMBER
							FROM PERSON_ROLES T1
							INNER JOIN ROLE_RIGHTS T2 ON T1.ROLE_ID = T2.ROLE_ID
							INNER JOIN RIGHTS T3 ON T2.RIGHT_ID = T3.RIGHT_ID 
							WHERE T1.DESCEND_FLAG = ''N'' AND T1.PERSON_ID = ''',AV_PERSON_ID,'''
							AND RIGHT_NAME IN (''MANAGE_FCOI_DISCLOSURE'', ''VIEW_FCOI_DISCLOSURE'') 
							UNION SELECT T1.UNIT_NUMBER FROM PERSON_ROLES T1 
                            INNER JOIN ADMIN_GROUP T2 ON T2.ROLE_ID = T1.ROLE_ID 
							WHERE T2.MODULE_CODE=8 AND T1.PERSON_ID = ''',AV_PERSON_ID,''' 
							UNION SELECT CHILD_UNIT_NUMBER FROM UNIT_WITH_CHILDREN 
							WHERE UNIT_NUMBER IN ( SELECT UNIT_NUMBER
							FROM PERSON_ROLES T1
							INNER JOIN ROLE_RIGHTS T2 ON T1.ROLE_ID = T2.ROLE_ID
							INNER JOIN RIGHTS T3 ON T2.RIGHT_ID = T3.RIGHT_ID 
							WHERE T1.DESCEND_FLAG = ''Y'' AND T1.PERSON_ID = ''',AV_PERSON_ID,'''
							AND RIGHT_NAME IN (''MANAGE_FCOI_DISCLOSURE'', ''VIEW_FCOI_DISCLOSURE'') 
                            )),
							PROJECT_ACCESS_TMP AS
							( SELECT UNIT_NUMBER
							FROM PERSON_ROLES T1
							INNER JOIN ROLE_RIGHTS T2 ON T1.ROLE_ID = T2.ROLE_ID
							INNER JOIN RIGHTS T3 ON T2.RIGHT_ID = T3.RIGHT_ID 
							WHERE T1.DESCEND_FLAG = ''N'' AND T1.PERSON_ID = ''',AV_PERSON_ID,'''
							AND RIGHT_NAME IN (''MANAGE_PROJECT_DISCLOSURE'', ''VIEW_PROJECT_DISCLOSURE'')  
							UNION SELECT T1.UNIT_NUMBER FROM PERSON_ROLES T1 
                            INNER JOIN ADMIN_GROUP T2 ON T2.ROLE_ID = T1.ROLE_ID 
							WHERE T2.MODULE_CODE=8 AND T1.PERSON_ID = ''',AV_PERSON_ID,''' 
							UNION SELECT CHILD_UNIT_NUMBER FROM UNIT_WITH_CHILDREN 
							WHERE UNIT_NUMBER IN (
							SELECT UNIT_NUMBER
							FROM PERSON_ROLES T1
							INNER JOIN ROLE_RIGHTS T2 ON T1.ROLE_ID = T2.ROLE_ID
							INNER JOIN RIGHTS T3 ON T2.RIGHT_ID = T3.RIGHT_ID 
							WHERE T1.DESCEND_FLAG = ''Y'' AND T1.PERSON_ID = ''',AV_PERSON_ID,'''
							AND RIGHT_NAME IN (''MANAGE_PROJECT_DISCLOSURE'', ''VIEW_PROJECT_DISCLOSURE'') 
                            ))');
END IF;

IF AV_TAB_TYPE = 'ALL_DISCLOSURES' THEN

				SET TAB_QUERY = CONCAT(TAB_QUERY,' AND T1.VERSION_STATUS != ''ARCHIVE'' AND T1.REVIEW_STATUS_CODE != 1 ');

                SET LS_ADMIN_GROUP_FCOI_CONDITION = CONCAT(' AND (T1.HOME_UNIT IN (SELECT DISTINCT UNIT_NUMBER FROM FCOI_ACCESS_TMP) OR ADMIN_PERSON_ID = ''', AV_PERSON_ID ,''') ');	
				SET LS_ADMIN_GROUP_PROJECT_CONDITION = CONCAT(' AND (T1.HOME_UNIT IN (SELECT DISTINCT UNIT_NUMBER FROM PROJECT_ACCESS_TMP) OR ADMIN_PERSON_ID = ''', AV_PERSON_ID ,''') ');
               
ELSEIF AV_TAB_TYPE = 'NEW_SUBMISSIONS' THEN

               
				SET TAB_QUERY = CONCAT(TAB_QUERY,' AND T1.VERSION_STATUS = ''PENDING'' AND T1.REVIEW_STATUS_CODE = 2 AND T1.CONFLICT_STATUS_CODE != 4   
				AND T1.ADMIN_GROUP_ID IS NULL AND T1.ADMIN_PERSON_ID IS NULL ');
				
				SET LS_ADMIN_GROUP_FCOI_CONDITION = CONCAT(' AND T1.HOME_UNIT IN (SELECT DISTINCT UNIT_NUMBER FROM FCOI_ACCESS_TMP) ');	
				SET LS_ADMIN_GROUP_PROJECT_CONDITION = CONCAT(' AND T1.HOME_UNIT IN (SELECT DISTINCT UNIT_NUMBER FROM PROJECT_ACCESS_TMP) ');
																	
ELSEIF AV_TAB_TYPE = 'NEW_SUBMISSIONS_WITHOUT_SFI' THEN

               
				SET TAB_QUERY = CONCAT(TAB_QUERY,' AND T1.VERSION_STATUS = ''PENDING'' AND T1.REVIEW_STATUS_CODE = 2 AND T1.CONFLICT_STATUS_CODE = 4 
				AND T1.ADMIN_GROUP_ID IS NULL AND T1.ADMIN_PERSON_ID IS NULL ');
				
				SET LS_ADMIN_GROUP_FCOI_CONDITION = CONCAT(' AND T1.HOME_UNIT IN (SELECT DISTINCT UNIT_NUMBER FROM FCOI_ACCESS_TMP) ');	
				SET LS_ADMIN_GROUP_PROJECT_CONDITION = CONCAT(' AND T1.HOME_UNIT IN (SELECT DISTINCT UNIT_NUMBER FROM PROJECT_ACCESS_TMP) ');


ELSEIF AV_TAB_TYPE = 'ALL_REVIEWS' THEN

				SET TAB_QUERY = CONCAT(TAB_QUERY,' AND T1.DISPOSITION_STATUS_CODE = 1 AND T1.REVIEW_STATUS_CODE IN (3,7,8) AND T1.VERSION_STATUS = ''PENDING'' 
					AND( T1.ADMIN_PERSON_ID = ''', AV_PERSON_ID ,'''
                    OR (T1.HOME_UNIT IN (SELECT DISTINCT UNIT_NUMBER FROM ACCESS_TMP)))');
																	
ELSEIF AV_TAB_TYPE = 'MY_REVIEWS' THEN

				SET TAB_QUERY = CONCAT(TAB_QUERY,' AND T1.REVIEW_STATUS_CODE IN (3,7,8) 
				AND ((''', AV_PERSON_ID ,''' IN (SELECT T34.ASSIGNEE_PERSON_ID WHERE T34.REVIEW_STATUS_TYPE_CODE IN (1,3)))
                OR (''', AV_PERSON_ID ,''' =  T1.ADMIN_PERSON_ID ))
                AND T1.VERSION_STATUS = ''PENDING'' ');

END IF;

IF AV_TYPE ='A' THEN
				IF AV_DISCLOSURE_NUMBER IS NOT NULL AND AV_DISCLOSURE_NUMBER <> '' THEN

					SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' T.DISCLOSURE_NUMBER = ''',AV_DISCLOSURE_NUMBER,''' AND ');

				END IF;

				IF AV_DISCLOSURE_PERSON_ID IS NOT NULL AND AV_DISCLOSURE_PERSON_ID <> '' THEN

					SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' T.PERSON_ID = ''',AV_DISCLOSURE_PERSON_ID,''' AND ');

				END IF;

				IF AV_HOME_UNIT IS NOT NULL AND AV_HOME_UNIT <> '' THEN

					SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' T.HOME_UNIT = ''',AV_HOME_UNIT,''' AND ');

				END IF;

				IF AV_CONFLICT_STATUS_CODE IS NOT NULL  AND AV_CONFLICT_STATUS_CODE <> '' THEN

					SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' T.CONFLICT_STATUS_CODE IN (',AV_CONFLICT_STATUS_CODE,') AND ');
				  
				END IF;

				 IF AV_DISPOSITION_STATUS_CODE IS NOT NULL  AND AV_DISPOSITION_STATUS_CODE <> '' THEN
					SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' T.DISPOSITION_STATUS_CODE IN (',AV_DISPOSITION_STATUS_CODE,') AND ');
				END IF;
                
                IF AV_REVIEW_STATUS_CODE IS NOT NULL  AND AV_REVIEW_STATUS_CODE <> '' THEN
					SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' T.REVIEW_STATUS_CODE IN (',AV_REVIEW_STATUS_CODE,') AND ');
				END IF;

				IF AV_DISCLOSURE_CATEOGORY_TYPE IS NOT NULL  AND AV_DISCLOSURE_CATEOGORY_TYPE <> '' THEN

					SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' T.FCOI_TYPE_CODE IN (',AV_DISCLOSURE_CATEOGORY_TYPE,') AND ');
				  
				END IF;

				 IF AV_START_DATE IS NOT NULL AND AV_START_DATE <> '' THEN

					 SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' DATE(T.CREATE_TIMESTAMP) = ''', STR_TO_DATE(AV_START_DATE,'%Y-%m-%d'),''' AND ');
				  
				 END IF;

				 IF AV_END_DATE IS NOT NULL AND AV_END_DATE <> '' THEN

					 SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' DATE(T.EXPIRATION_DATE) = ''', STR_TO_DATE(AV_END_DATE,'%Y-%m-%d'),''' AND ');
				  
				 END IF;
				
				IF AV_CERTIFICATION_DATE IS NOT NULL AND AV_CERTIFICATION_DATE <> '' THEN

					SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' DATE(T.CERTIFIED_AT) = ''', STR_TO_DATE(AV_CERTIFICATION_DATE,'%Y-%m-%d'),''' AND ');
				  
				END IF;
				 
				IF AV_ENTITY_NAME IS NOT NULL  AND AV_ENTITY_NAME <> '' THEN

					SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' T.ENTITY_NAMES LIKE ''%',AV_ENTITY_NAME, '%'' AND ');

				END IF;

				IF AV_ENTITY_COUNTRY IS NOT NULL  AND AV_ENTITY_COUNTRY <> '' THEN

					SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' (T.ENTITY_COUNTRIES LIKE ''%,',AV_ENTITY_COUNTRY, ''' OR T.ENTITY_COUNTRIES LIKE ''', 
																			AV_ENTITY_COUNTRY, ',%'' OR T.ENTITY_COUNTRIES LIKE ''%,', AV_ENTITY_COUNTRY, 
																			',%'' OR T.ENTITY_COUNTRIES = ''', AV_ENTITY_COUNTRY, ''') AND ');

				END IF;

				IF AV_PROPOSAL_ID IS NOT NULL  AND AV_PROPOSAL_ID <> '' THEN

					SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' (T.PROPOSAL_IDS LIKE ''%,',AV_PROPOSAL_ID, ''' OR T.PROPOSAL_IDS LIKE ''', 
																			AV_PROPOSAL_ID, ',%'' OR T.PROPOSAL_IDS LIKE ''%,', AV_PROPOSAL_ID, 
																			',%'' OR T.PROPOSAL_IDS = ''', AV_PROPOSAL_ID, ''') AND ');

				END IF;

				IF AV_TITLE IS NOT NULL AND AV_TITLE <> '' THEN

					SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' (T.PROPOSAL_TITLES LIKE ''%',AV_TITLE, '%'' OR 
					T.AWARD_TITLES LIKE ''%', AV_TITLE, '%'') AND ');

				END IF;

				IF AV_AWARD_ID IS NOT NULL  AND AV_AWARD_ID <> '' THEN

					SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' (T.AWARD_IDS LIKE ''%,',AV_AWARD_ID, ''' OR T.AWARD_IDS LIKE ''', 
																			AV_AWARD_ID, ',%'' OR T.AWARD_IDS LIKE ''%,', AV_AWARD_ID, 
																			',%'' OR T.AWARD_IDS = ''', AV_AWARD_ID, ''') AND ');

				END IF;

				IF AV_PROJECT_TYPE IS NOT NULL  AND AV_PROJECT_TYPE <> '' THEN

					SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' (T.MODULE_CODES LIKE ''%,',AV_PROJECT_TYPE, ''' OR T.MODULE_CODES LIKE ''', 
																			AV_PROJECT_TYPE, ',%'' OR T.MODULE_CODES LIKE ''%,', AV_PROJECT_TYPE, 
																			',%'' OR T.MODULE_CODES = ''', AV_PROJECT_TYPE, ''') AND ');
				END IF;

				IF AV_HAS_SFI_FLAG IS NOT NULL AND AV_HAS_SFI_FLAG <> '' THEN

						SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' T.HAS_SFI_FLAG = ''', AV_HAS_SFI_FLAG, ''' AND ');

				END IF;

END IF;


IF AV_SORT_TYPE IS NULL THEN
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
        
        SET SELECTED_FIELD_LIST= CONCAT(SELECTED_FIELD_LIST,'T1.DISCLOSURE_ID, T1.DISCLOSURE_NUMBER, T1.VERSION_NUMBER, T1.PERSON_ID, T7.FULL_NAME AS DISCLOSURE_PERSON_FULL_NAME,
										T1.HOME_UNIT, T1.FCOI_TYPE_CODE, T8.DESCRIPTION AS DISCLOSURE_CATEGORY_TYPE, T1.CONFLICT_STATUS_CODE, T2.DESCRIPTION AS DISCLOSURE_STATUS,
                                        T1.DISPOSITION_STATUS_CODE, T3.DESCRIPTION AS DISPOSITION_STATUS, T1.REVIEW_STATUS_CODE, T4.DESCRIPTION AS REVIEW_STATUS,
                                        T1.VERSION_STATUS, T1.CREATE_TIMESTAMP, T1.CERTIFICATION_TEXT, T1.CERTIFIED_AT, T1.EXPIRATION_DATE,
										T6.FULL_NAME AS UPDATE_USER_FULL_NAME, T1.UPDATE_TIMESTAMP, T11.VERSION_NUMBER AS LAST_APPROVED_VERSION,
										T11.UPDATE_TIMESTAMP AS LAST_APPROVED_DATE, CASE WHEN (T12.DISCLOSURE_ID IS NOT NULL) THEN "YES" ELSE "NO" END AS HAS_SFI_FLAG,
										T13.ENTITY_NAMES, T13.ENTITY_COUNTRIES, T14.PROPOSAL_IDS, T14.PROPOSAL_TITLES, T15.AWARD_NUMBERS,
										T15.AWARD_TITLES, T16.MODULE_CODES, T1.REVISION_COMMENT, T17.NO_OF_SFI_IN_ACTIVE, T18.NO_OF_ACTIVE_PROPOSAL,
                                        T19.NO_OF_ACTIVE_AWARD, T22.NO_OF_SFI_IN_PENDING, T30.UNIT_NAME AS HOME_UNIT_NAME, T30.ORGANIZATION_ID, T30.PARENT_UNIT_NUMBER,		
                                        T30.IS_ACTIVE, T30.ACRONYM, T30.IS_FUNDING_UNIT, T31.ADMIN_GROUP_NAME, T32.FULL_NAME AS ADMINISTRATOR  ');

		SET JOIN_CONDITION =  CONCAT(' LEFT JOIN COI_CONFLICT_STATUS_TYPE T2 ON T2.CONFLICT_STATUS_CODE=T1.CONFLICT_STATUS_CODE
										LEFT JOIN COI_DISPOSITION_STATUS_TYPE T3 ON T3.DISPOSITION_STATUS_CODE = T1.DISPOSITION_STATUS_CODE
										LEFT JOIN COI_REVIEW_STATUS_TYPE T4 ON T4.REVIEW_STATUS_CODE = T1.REVIEW_STATUS_CODE
										INNER JOIN PERSON T6 ON T6.USER_NAME = T1.UPDATE_USER 
										INNER JOIN PERSON T7 ON T7.PERSON_ID = T1.PERSON_ID 
										LEFT JOIN COI_DISCLOSURE_FCOI_TYPE T8 ON T8.FCOI_TYPE_CODE = T1.FCOI_TYPE_CODE 
                                        LEFT JOIN (SELECT DISCLOSURE_NUMBER, VERSION_NUMBER, UPDATE_TIMESTAMP FROM COI_DISCLOSURE 
													WHERE (DISCLOSURE_NUMBER, VERSION_NUMBER) IN 
															(SELECT S1.DISCLOSURE_NUMBER, MAX(S1.VERSION_NUMBER) FROM COI_DISCLOSURE S1
															 WHERE S1.VERSION_STATUS = ''ACTIVE'' group by S1.DISCLOSURE_NUMBER)) T11 ON T11.DISCLOSURE_NUMBER = T1.DISCLOSURE_NUMBER            
										LEFT JOIN (SELECT T1.DISCLOSURE_ID FROM COI_DISCLOSURE T1 
													INNER JOIN COI_DISCL_ENT_PROJ_DETAILS T2 ON T2.DISCLOSURE_ID = T1.DISCLOSURE_ID 
													GROUP BY T1.DISCLOSURE_ID) T12 ON T12.DISCLOSURE_ID = T1.DISCLOSURE_ID 
										LEFT JOIN (SELECT T1.DISCLOSURE_ID,GROUP_CONCAT(DISTINCT(T4.ENTITY_NAME) SEPARATOR ", ") AS ENTITY_NAMES, 
													GROUP_CONCAT(DISTINCT(T4.COUNTRY_CODE) SEPARATOR ",") AS ENTITY_COUNTRIES FROM COI_DISCLOSURE T1 
													INNER JOIN COI_DISCL_ENT_PROJ_DETAILS T2 ON T2.DISCLOSURE_ID = T1.DISCLOSURE_ID 
													INNER JOIN PERSON_ENTITY T3 ON T3.PERSON_ENTITY_ID = T2.PERSON_ENTITY_ID 
													INNER JOIN ENTITY T4 ON T4.ENTITY_ID = T3.ENTITY_ID 
													GROUP BY T1.DISCLOSURE_ID) T13 ON T13.DISCLOSURE_ID = T1.DISCLOSURE_ID 
										LEFT JOIN (SELECT EXTERNAL_SYSTEM_REF_ID AS PROPOSAL_IDS, 
												  TITLE  AS PROPOSAL_TITLES, EXTERNAL_SYSTEM_REF_ID FROM COI_PROJECT_PROPOSAL_V ) T14 ON T14.EXTERNAL_SYSTEM_REF_ID = T1.MODULE_ITEM_KEY  
										LEFT  JOIN (SELECT AWARD_NUMBER  AS AWARD_NUMBERS, EXTERNAL_SYSTEM_REF_ID,
												   TITLE AS AWARD_TITLES FROM COI_PROJECT_AWARD_V )T15  ON T15.EXTERNAL_SYSTEM_REF_ID = T1.MODULE_ITEM_KEY 
										LEFT JOIN (SELECT DISCLOSURE_ID, GROUP_CONCAT(DISTINCT(MODULE_CODE) SEPARATOR ",") AS MODULE_CODES 
													FROM COI_DISCL_ENT_PROJ_DETAILS GROUP BY DISCLOSURE_ID) T16 ON T16.DISCLOSURE_ID = T1.DISCLOSURE_ID 
										LEFT JOIN UNIT T30 ON T30.UNIT_NUMBER = T1.HOME_UNIT LEFT JOIN ADMIN_GROUP T31 ON T31.ADMIN_GROUP_ID = T1.ADMIN_GROUP_ID 
										LEFT JOIN PERSON T32 ON T32.PERSON_ID = T1.ADMIN_PERSON_ID ', JOIN_CONDITION);
        
        SET LS_DYN_SQL = CONCAT(LS_DYN_SQL, 'SELECT DISTINCT COUNT(*)  FROM(SELECT DISTINCT ',SELECTED_FIELD_LIST,'
										FROM COI_DISCLOSURE T1 ', JOIN_CONDITION, ' WHERE T1.FCOI_TYPE_CODE =1  ',LS_ADMIN_GROUP_FCOI_CONDITION, TAB_QUERY, 
                                        ' GROUP BY T1.DISCLOSURE_ID 
										UNION 
										SELECT DISTINCT ',SELECTED_FIELD_LIST,'
										FROM COI_DISCLOSURE T1 ', JOIN_CONDITION, ' WHERE T1.FCOI_TYPE_CODE != 1 ',LS_ADMIN_GROUP_PROJECT_CONDITION, TAB_QUERY, 
                                        ' GROUP BY T1.DISCLOSURE_ID) T ',LS_FILTER_CONDITION,' ',AV_SORT_TYPE,' ',LS_OFFSET_CONDITION);


SET @QUERY_STATEMENT = LS_DYN_SQL;
PREPARE EXECUTABLE_STAEMENT FROM @QUERY_STATEMENT;
EXECUTE EXECUTABLE_STAEMENT;

END
//