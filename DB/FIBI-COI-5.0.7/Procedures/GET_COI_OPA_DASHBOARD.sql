DELIMITER //
CREATE PROCEDURE `GET_COI_OPA_DASHBOARD`(
	AV_PERSON_ID                    VARCHAR(60),
	AV_FILTER_TYPE					VARCHAR(10),
    AV_IS_COUNT                    	BOOLEAN,
    AV_SORT_TYPE                    VARCHAR(500),
    AV_PAGED                        INT(10),
    AV_LIMIT                        INT(10),
    AV_TAB_TYPE                     VARCHAR(30),
    AV_DISCLOSURE_STATUS_CODES      VARCHAR(30),
    AV_DISPOSITION_STATUS_CODES     VARCHAR(30),
    AV_SUBMISSION_TIMESTAMP         VARCHAR(30),
    AV_UNIT_NUMBER                  VARCHAR(8),
    AV_FETCH_ALL_RECORDS            BOOLEAN,
    AV_SEARCH_PERSON_ID             VARCHAR(60),
    AV_ENTITY_ID                    VARCHAR(60),
    AV_IS_FACULTY                   VARCHAR(10),
    AV_PERIOD_START_DATE            VARCHAR(30),
    AV_PERIOD_END_DATE              VARCHAR(30)
)
BEGIN

    

	DECLARE LS_DYN_SQL 					LONGTEXT;
	DECLARE LS_FILTER_CONDITION 		LONGTEXT;
    DECLARE OPA_SELECTED_FIELDS 	    LONGTEXT;
    DECLARE OPA_JOINS               	LONGTEXT;
    DECLARE LS_OFFSET 			        INT(11);
    DECLARE LS_OFFSET_CONDITION         VARCHAR(600);

	SET LS_DYN_SQL ='';     
    SET OPA_SELECTED_FIELDS = '';
    SET OPA_JOINS = '';
    SET LS_OFFSET = (AV_LIMIT * AV_PAGED);
    SET LS_OFFSET_CONDITION = '';

    

IF AV_TAB_TYPE = 'ALL_REVIEWS' THEN
	SET LS_DYN_SQL = CONCAT('WITH RW_ACCESS_TMP AS (SELECT T1.UNIT_NUMBER FROM PERSON_ROLES T1 
                            INNER JOIN ADMIN_GROUP T2 ON T2.ROLE_ID = T1.ROLE_ID 
							WHERE T2.MODULE_CODE=23 AND T1.PERSON_ID = ''',AV_PERSON_ID,''') ' );

ELSEIF AV_TAB_TYPE = 'MY_REVIEWS'  THEN
	SET LS_DYN_SQL = CONCAT('');

ELSE

	SET LS_DYN_SQL = CONCAT('WITH ACCESS_TMP AS ( SELECT UNIT_NUMBER
							FROM PERSON_ROLES T1
							INNER JOIN ROLE_RIGHTS T2 ON T1.ROLE_ID = T2.ROLE_ID
							INNER JOIN RIGHTS T3 ON T2.RIGHT_ID = T3.RIGHT_ID 
							WHERE T1.DESCEND_FLAG = ''N'' AND T1.PERSON_ID = ''',AV_PERSON_ID,'''
							AND RIGHT_NAME IN (''MANAGE_OPA_DISCLOSURE'', ''VIEW_OPA_DISCLOSURE'') 
							UNION SELECT T1.UNIT_NUMBER FROM PERSON_ROLES T1 
                            INNER JOIN ADMIN_GROUP T2 ON T2.ROLE_ID = T1.ROLE_ID 
							WHERE T2.MODULE_CODE=23 AND T1.PERSON_ID = ''',AV_PERSON_ID,''' 
							UNION SELECT CHILD_UNIT_NUMBER FROM UNIT_WITH_CHILDREN 
							WHERE UNIT_NUMBER IN ( SELECT UNIT_NUMBER
							FROM PERSON_ROLES T1
							INNER JOIN ROLE_RIGHTS T2 ON T1.ROLE_ID = T2.ROLE_ID
							INNER JOIN RIGHTS T3 ON T2.RIGHT_ID = T3.RIGHT_ID 
							WHERE T1.DESCEND_FLAG = ''Y'' AND T1.PERSON_ID = ''',AV_PERSON_ID,'''
							AND RIGHT_NAME IN (''MANAGE_OPA_DISCLOSURE'', ''VIEW_OPA_DISCLOSURE'') 
                            ))');
END IF;


    

    IF AV_TAB_TYPE = 'MY_DASHBOARD' THEN

        SET LS_FILTER_CONDITION = CONCAT(' T1.PERSON_ID = ''', AV_PERSON_ID,'''');

    ELSEIF AV_TAB_TYPE = 'NEW_SUBMISSIONS' THEN

        SET LS_FILTER_CONDITION = CONCAT(' T1.REVIEW_STATUS_CODE = 2   
				AND T1.ADMIN_GROUP_ID IS NULL AND T1.ADMIN_PERSON_ID IS NULL ');
				
		SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION, ' AND (T1.HOME_UNIT IN (SELECT DISTINCT UNIT_NUMBER FROM ACCESS_TMP) )');	

    ELSEIF AV_TAB_TYPE = 'MY_REVIEWS' THEN
       
        SET LS_FILTER_CONDITION = CONCAT(' T1.REVIEW_STATUS_CODE IN (3,7,8) AND ((T8.ASSIGNEE_PERSON_ID = ''', AV_PERSON_ID ,'''
                    AND T8.REVIEW_STATUS_TYPE_CODE IN (1,2)) 
                    OR (T1.ADMIN_PERSON_ID = ''', AV_PERSON_ID ,''')) ');

    ELSEIF AV_TAB_TYPE = 'ALL_REVIEWS' THEN

      SET LS_FILTER_CONDITION = CONCAT(' T1.REVIEW_STATUS_CODE IN (4,7,8)AND ((''', AV_PERSON_ID ,''' IN (SELECT T8.ASSIGNEE_PERSON_ID WHERE T8.REVIEW_STATUS_TYPE_CODE = 3))) ');
        SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION, ' OR(T1.REVIEW_STATUS_CODE IN (3,7,8) AND (T1.ADMIN_PERSON_ID = ''', AV_PERSON_ID ,''' 
                            OR (T1.HOME_UNIT IN (SELECT DISTINCT UNIT_NUMBER FROM RW_ACCESS_TMP))))');



    ELSEIF AV_TAB_TYPE = 'ALL_DISCLOSURES' THEN
    
		SET LS_FILTER_CONDITION = CONCAT('(T1.HOME_UNIT IN (SELECT DISTINCT UNIT_NUMBER FROM ACCESS_TMP) OR T1.ADMIN_PERSON_ID = ''', AV_PERSON_ID ,''')  ');
    END IF;

    

    IF AV_DISPOSITION_STATUS_CODES IS NOT NULL  AND AV_DISPOSITION_STATUS_CODES <> '' THEN
		SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' AND T1.DISPOSITION_STATUS_CODE IN (',AV_DISPOSITION_STATUS_CODES,') ');
	END IF;

    IF AV_DISCLOSURE_STATUS_CODES IS NOT NULL  AND AV_DISCLOSURE_STATUS_CODES <> '' THEN
		SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' AND T1.REVIEW_STATUS_CODE IN (',AV_DISCLOSURE_STATUS_CODES,') ');
	END IF;
    
    IF AV_UNIT_NUMBER IS NOT NULL AND AV_UNIT_NUMBER <> '' THEN
        SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' AND T1.HOME_UNIT = ''',AV_UNIT_NUMBER,''' ');
    END IF;

    IF AV_SUBMISSION_TIMESTAMP IS NOT NULL AND AV_SUBMISSION_TIMESTAMP <> '' THEN
        SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' AND DATE(T1.SUBMISSION_TIMESTAMP) = ''', STR_TO_DATE(AV_SUBMISSION_TIMESTAMP,'%Y-%m-%d'),''' ');
	END IF;

    IF AV_SEARCH_PERSON_ID IS NOT NULL AND AV_SEARCH_PERSON_ID <> '' THEN 
        SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' AND T1.PERSON_ID = ''',AV_SEARCH_PERSON_ID,''' ');
    END IF;

    IF AV_ENTITY_ID IS NOT NULL AND AV_ENTITY_ID <> '' THEN 
        SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' AND T11.ENTITY_ID = ''',AV_ENTITY_ID,''' ');
    END IF;

    IF AV_IS_FACULTY IS NOT NULL AND AV_IS_FACULTY <> '' THEN 
        SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' AND T1.IS_FACULTY IN (', AV_IS_FACULTY, ') ');
    END IF;

    IF AV_PERIOD_START_DATE IS NOT NULL AND AV_PERIOD_START_DATE <> '' THEN
        SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' AND T2.PERIOD_START_DATE >= ''',AV_PERIOD_START_DATE,''' ');
    END IF;

    IF AV_PERIOD_END_DATE IS NOT NULL AND AV_PERIOD_END_DATE <> '' THEN
        SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' AND T2.PERIOD_END_DATE <= ''',AV_PERIOD_END_DATE,''' ');
    END IF;

    

    IF AV_IS_COUNT THEN
        SET LS_DYN_SQL = CONCAT(LS_DYN_SQL, ' SELECT COUNT(*) FROM ( SELECT DISTINCT ');
        SET OPA_SELECTED_FIELDS = CONCAT(' T1.OPA_DISCLOSURE_ID ');
    ELSE
        SET LS_DYN_SQL = CONCAT(LS_DYN_SQL, ' SELECT * FROM ( SELECT DISTINCT ');
        SET OPA_SELECTED_FIELDS= CONCAT('T1.OPA_DISCLOSURE_ID, T1.OPA_DISCLOSURE_NUMBER, T1.OPA_CYCLE_NUMBER,
            T2.PERIOD_START_DATE, T2.PERIOD_END_DATE, T2.OPA_CYCLE_STATUS, T2.OPEN_DATE, T2.CLOSE_DATE,
            T1.PERSON_NAME, T1.HOME_UNIT AS UNIT_NUMBER, T3.UNIT_NAME, T1.IS_FACULTY, T1.IS_FALL_SABATICAL,
            T1.IS_SPRING_SABATICAL, T1.RECEIVED_SUMMER_COMP, T1.SUMMER_COMP_MONTHS, T1.HAS_POTENTIAL_CONFLICT,
            T1.CONFLICT_DESCRIPTION, T1.CREATE_TIMESTAMP, T1.CREATE_USER, T1.SUBMISSION_TIMESTAMP, T1.UPDATE_TIMESTAMP,
            T1.UPDATE_USER, T5.FULL_NAME AS UPDATE_USER_FULL_NAME , T1.DISPOSITION_STATUS_CODE, T1.REVIEW_STATUS_CODE,
            T6.DESCRIPTION AS OPA_DISCLOSURE_STATUS, T7.DESCRIPTION AS DISPOSITION_STATUS, T9.FULL_NAME AS ADMIN_FULL_NAME,
            T10.ADMIN_GROUP_NAME, GROUP_CONCAT(DISTINCT 
            CASE    WHEN T8.ASSIGNEE_PERSON_ID IS NULL THEN CONCAT(T15.DESCRIPTION, " : ", "null", " : ",T14.DESCRIPTION, " : ",T14.REVIEW_STATUS_CODE)
					ELSE CONCAT(T15.DESCRIPTION, " : ", T13.FULL_NAME, " : ", T14.DESCRIPTION, " : ",T14.REVIEW_STATUS_CODE)
					END
			SEPARATOR ";") AS REVIEWERS ');
        IF NOT AV_FETCH_ALL_RECORDS THEN
			SET LS_OFFSET_CONDITION = CONCAT(' LIMIT ',AV_LIMIT,' OFFSET ',LS_OFFSET);
        END IF;
    END IF;

    

    SET OPA_JOINS = CONCAT(' INNER JOIN OPA_CYCLES T2 ON T2.OPA_CYCLE_NUMBER = T1.OPA_CYCLE_NUMBER 
        INNER JOIN UNIT T3 ON T3.UNIT_NUMBER = T1.HOME_UNIT 
        INNER JOIN PERSON T5 ON T5.USER_NAME = T1.UPDATE_USER
        INNER JOIN OPA_REVIEW_STATUS_TYPE T6 ON T6.REVIEW_STATUS_CODE = T1.REVIEW_STATUS_CODE 
        LEFT JOIN OPA_DISPOSITION_STATUS_TYPE T7 ON T7.DISPOSITION_STATUS_CODE = T1.DISPOSITION_STATUS_CODE
        LEFT JOIN OPA_REVIEW T8 ON T8.OPA_DISCLOSURE_ID = T1.OPA_DISCLOSURE_ID
        LEFT JOIN PERSON T9 ON T9.PERSON_ID = T1.ADMIN_PERSON_ID
        LEFT JOIN ADMIN_GROUP T10 ON T10.ADMIN_GROUP_ID = T1.ADMIN_GROUP_ID 
        LEFT JOIN OPA_DISCL_PERSON_ENTITY T11 ON T11.OPA_DISCLOSURE_ID = T1.OPA_DISCLOSURE_ID 
		LEFT JOIN PERSON T13 ON T13.PERSON_ID = T8.ASSIGNEE_PERSON_ID 
        LEFT JOIN opa_review_reviewer_status_type T14 ON T14.REVIEW_STATUS_CODE = T8.REVIEW_STATUS_TYPE_CODE
        LEFT JOIN opa_review_location_type T15 ON T15.LOCATION_TYPE_CODE = T8.LOCATION_TYPE_CODE ');

    

    IF AV_IS_COUNT THEN
		SET AV_SORT_TYPE =   '';
    ELSEIF AV_SORT_TYPE IS NULL THEN
        SET AV_SORT_TYPE =  CONCAT(' ORDER BY T.UPDATE_TIMESTAMP DESC ');
    ELSE
        SET AV_SORT_TYPE = CONCAT(' ORDER BY ',AV_SORT_TYPE);
    END IF;

    

    SET LS_DYN_SQL = CONCAT(LS_DYN_SQL, OPA_SELECTED_FIELDS, ' FROM OPA_DISCLOSURE T1 ', 
            OPA_JOINS,  ' WHERE ', LS_FILTER_CONDITION, ' GROUP BY T1.OPA_DISCLOSURE_ID ) T ', AV_SORT_TYPE, LS_OFFSET_CONDITION);


    SET @QUERY_STATEMENT = LS_DYN_SQL;
	PREPARE EXECUTABLE_STAEMENT FROM @QUERY_STATEMENT;
	EXECUTE EXECUTABLE_STAEMENT;
END
//