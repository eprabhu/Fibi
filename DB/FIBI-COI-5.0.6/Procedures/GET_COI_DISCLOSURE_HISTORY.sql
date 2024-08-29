DELIMITER //
CREATE  PROCEDURE `GET_COI_DISCLOSURE_HISTORY`(
	AV_PERSON_ID                    VARCHAR(200),
	AV_FILTER_TYPE					        VARCHAR(10),
    AV_IS_COUNT                    	BOOLEAN
)
BEGIN
		
	DECLARE LS_DYN_SQL 					LONGTEXT;
    DECLARE LS_FILTER_CONDITION 		LONGTEXT;
    DECLARE DISC_JOIN_CONDITION 		LONGTEXT;
    DECLARE TRAVEL_DISC_JOIN_CONDITION 	LONGTEXT;
    DECLARE DISC_SELECTED_FIELD_LIST 	LONGTEXT;
    DECLARE TRAVEL_SELECTED_FIELD_LIST 	LONGTEXT;
    DECLARE DISC_TAB_QUERY 				LONGTEXT;
    DECLARE TRAVEL_TAB_QUERY 			LONGTEXT;
    DECLARE CONSULTING_SELECTED_FIELD_LIST 	LONGTEXT;
    DECLARE CONSULTING_DISC_JOIN_CONDITION 				LONGTEXT;
    DECLARE CONSULTING_TAB_QUERY 			LONGTEXT;

    SET LS_FILTER_CONDITION ='';
    SET LS_DYN_SQL ='';
    SET DISC_JOIN_CONDITION = '';
    SET TRAVEL_DISC_JOIN_CONDITION = '';
    SET DISC_SELECTED_FIELD_LIST= '';
    SET TRAVEL_SELECTED_FIELD_LIST= '';
    SET DISC_TAB_QUERY = '';
    SET TRAVEL_TAB_QUERY = '';
    SET CONSULTING_SELECTED_FIELD_LIST = '';
    SET CONSULTING_DISC_JOIN_CONDITION = '';
    SET CONSULTING_TAB_QUERY = '';


    	IF AV_FILTER_TYPE = 'FCOI' THEN

    		SET DISC_TAB_QUERY = CONCAT(' AND T1.FCOI_TYPE_CODE = 1 ');

    	ELSEIF AV_FILTER_TYPE = 'PROJECT' THEN

    		SET DISC_TAB_QUERY = CONCAT(' AND T1.FCOI_TYPE_CODE != 1 ');

    	END IF;

        IF AV_FILTER_TYPE = 'ALL' OR AV_FILTER_TYPE = 'FCOI' OR AV_FILTER_TYPE = 'PROJECT' THEN

    		SET DISC_SELECTED_FIELD_LIST= CONCAT('T1.DISCLOSURE_ID, NULL AS TRAVEL_DISCLOSURE_ID, NULL AS CONSULTING_DISCLOSURE_ID, T1.UPDATE_TIMESTAMP, T1.VERSION_STATUS, T1.FCOI_TYPE_CODE,
                                            T2.DESCRIPTION AS FCOI_TYPE, T1.HOME_UNIT, T3.UNIT_NAME, T1.EXPIRATION_DATE, T1.CERTIFIED_AT, T1.CONFLICT_STATUS_CODE,
                                            T4.DESCRIPTION AS CONFLICT_STATUS, T1.DISPOSITION_STATUS_CODE, T5.DESCRIPTION AS DISPOSITION_STATUS, T1.REVIEW_STATUS_CODE,
                                            T6.DESCRIPTION AS REVIEW_STATUS, NULL AS ENTITY_NAME, NULL AS DESTINATION_COUNTRY, NULL AS STATE, NULL AS DESTINATION_CITY,
                                            NULL AS PURPOSE_OF_THE_TRIP, NULL AS TRAVEL_START_DATE, NULL AS TRAVEL_END_DATE, NULL AS TRAVEL_STATUS_CODE, NULL AS TRAVEL_STATUS,
                                            CASE T11.MODULE_CODE WHEN 3 THEN T7.TITLE WHEN 1 THEN T8.TITLE ELSE NULL END AS PROJECT_TITLE,
                                            CASE T11.MODULE_CODE WHEN 3 THEN T7.EXTERNAL_SYSTEM_REF_ID WHEN 1 THEN T8.AWARD_NUMBER ELSE NULL END AS PROJECT_NUMBER');

    		SET DISC_JOIN_CONDITION = CONCAT(' FROM COI_DISCLOSURE T1
    		                            INNER JOIN COI_DISCL_PROJECTS T11 ON T11.DISCLOSURE_ID = T1.DISCLOSURE_ID
    		                            INNER JOIN COI_DISCLOSURE_FCOI_TYPE T2 ON T2.FCOI_TYPE_CODE = T1.FCOI_TYPE_CODE
                                        INNER JOIN UNIT T3 ON T3.UNIT_NUMBER = T1.HOME_UNIT
                                        LEFT JOIN COI_CONFLICT_STATUS_TYPE T4 ON T4.CONFLICT_STATUS_CODE = T1.CONFLICT_STATUS_CODE
                                        INNER JOIN COI_DISPOSITION_STATUS_TYPE T5 ON T5.DISPOSITION_STATUS_CODE = T1.DISPOSITION_STATUS_CODE
                                        INNER JOIN COI_REVIEW_STATUS_TYPE T6 ON T6.REVIEW_STATUS_CODE = T1.REVIEW_STATUS_CODE
                                        LEFT JOIN COI_PROJECT_PROPOSAL_V T7 ON (T7.EXTERNAL_SYSTEM_REF_ID = CAST(T11.MODULE_ITEM_KEY AS UNSIGNED) AND T11.MODULE_CODE = 3)
                                        LEFT JOIN COI_PROJECT_AWARD_V T8 ON (T8.EXTERNAL_SYSTEM_REF_ID = CAST(T11.MODULE_ITEM_KEY AS UNSIGNED) AND T11.MODULE_CODE = 1)
                                        WHERE T1.PERSON_ID = ''', AV_PERSON_ID,'''');
        END IF;
    	IF AV_FILTER_TYPE = 'ALL' OR AV_FILTER_TYPE = 'TRAVEL' THEN

    		SET TRAVEL_SELECTED_FIELD_LIST= CONCAT('NULL AS DISCLOSURE_ID, T21.TRAVEL_DISCLOSURE_ID, NULL AS CONSULTING_DISCLOSURE_ID, T21.UPDATE_TIMESTAMP, T21.VERSION_STATUS, NULL AS  FCOI_TYPE_CODE,
                                            NULL AS FCOI_TYPE, NULL AS HOME_UNIT, NULL AS UNIT_NAME, T21.EXPIRATION_DATE,  NULL AS CERTIFIED_AT, T30.TRAVEL_DISCLOSURE_STATUS_CODE AS CONFLICT_STATUS_CODE,
                                            T30.DESCRIPTION AS CONFLICT_STATUS, NULL AS DISPOSITION_STATUS_CODE, NULL AS DISPOSITION_STATUS, NULL AS REVIEW_STATUS_CODE, NULL AS REVIEW_STATUS,
                                            T22.PRIMARY_NAME as ENTITY_NAME, T21.DESTINATION_COUNTRY, T21.STATE, T21.DESTINATION_CITY, T21.PURPOSE_OF_THE_TRIP,
                                            T21.TRAVEL_START_DATE, T21.TRAVEL_END_DATE, T23.TRAVEL_STATUS_CODE, T23.DESCRIPTION TRAVEL_STATUS,
                                            NULL AS PROJECT_TITLE, NULL AS PROJECT_NUMBER ');
    		SET TRAVEL_DISC_JOIN_CONDITION = CONCAT(' FROM COI_TRAVEL_DISCLOSURE T21 INNER JOIN ENTITY T22 ON T22.ENTITY_ID = T21.ENTITY_ID
    										INNER JOIN COI_TRAVEL_DISCLOSURE_STATUS T30 ON T21.TRAVEL_DISCLOSURE_STATUS_CODE = T30.TRAVEL_DISCLOSURE_STATUS_CODE
                                            LEFT JOIN COI_TRAVEL_STATUS T23 ON T23.TRAVEL_STATUS_CODE = T21.TRAVEL_STATUS_CODE WHERE T21.PERSON_ID = ''',AV_PERSON_ID,'''');
    	END IF;
    	IF AV_FILTER_TYPE = 'ALL' OR AV_FILTER_TYPE = 'CONSULTING' THEN

    		SET CONSULTING_SELECTED_FIELD_LIST= CONCAT('NULL AS DISCLOSURE_ID, NULL AS TRAVEL_DISCLOSURE_ID, T50.DISCLOSURE_ID AS CONSULTING_DISCLOSURE_ID,
    										T50.UPDATE_TIMESTAMP, NULL AS VERSION_STATUS, NULL AS  FCOI_TYPE_CODE,
                                            NULL AS FCOI_TYPE, T51.UNIT_NUMBER AS HOME_UNIT, T51.UNIT_NAME, NULL AS EXPIRATION_DATE,  T50.CERTIFIED_AT,
    										NULL AS CONFLICT_STATUS_CODE, NULL AS CONFLICT_STATUS, T56.DISPOSITION_STATUS_CODE, T56.DESCRIPTION AS DISPOSITION_STATUS,
    										T52.REVIEW_STATUS_CODE, T52.DESCRIPTION AS REVIEW_STATUS,
                                            T54.PRIMARY_NAME as ENTITY_NAME, NULL AS DESTINATION_COUNTRY, NULL AS STATE, NULL AS DESTINATION_CITY, NULL AS PURPOSE_OF_THE_TRIP,
                                            NULL AS TRAVEL_START_DATE, NULL AS TRAVEL_END_DATE, NULL AS TRAVEL_STATUS_CODE, NULL AS TRAVEL_STATUS,
                                            NULL AS PROJECT_TITLE, NULL AS PROJECT_NUMBER ');
    		SET CONSULTING_DISC_JOIN_CONDITION = CONCAT(' FROM CONSULTING_DISCLOSURE T50
    										LEFT JOIN PERSON_ENTITY T53 ON T53.PERSON_ENTITY_ID = T50.PERSON_ENTITY_ID
    										LEFT JOIN ENTITY T54 on T54.ENTITY_ID = T53.ENTITY_ID
    										INNER JOIN CONSULTING_DISCL_REVIEW_STATUS_TYPE T52 ON T52.REVIEW_STATUS_CODE = T50.REVIEW_STATUS_CODE
    										INNER JOIN UNIT T51 ON T51.UNIT_NUMBER = T50.HOME_UNIT
    										INNER JOIN CONSULTING_DISCL_DISPOSITION_STATUS_TYPE T56 ON T56.DISPOSITION_STATUS_CODE = T50.DISPOSITION_STATUS_CODE
    										WHERE T50.PERSON_ID = ''',AV_PERSON_ID,'''');

        END IF;

        IF AV_IS_COUNT THEN
          SET LS_DYN_SQL = CONCAT('SELECT COUNT(*) FROM ');
        ELSE
          SET LS_DYN_SQL = CONCAT('SELECT * FROM ');
        END IF;

        IF AV_FILTER_TYPE = 'ALL' THEN
    		SET LS_DYN_SQL = CONCAT(LS_DYN_SQL, '(SELECT DISTINCT ',DISC_SELECTED_FIELD_LIST, DISC_JOIN_CONDITION, DISC_TAB_QUERY, ' UNION SELECT DISTINCT ',
    							TRAVEL_SELECTED_FIELD_LIST, TRAVEL_DISC_JOIN_CONDITION, TRAVEL_TAB_QUERY, ' UNION SELECT DISTINCT ',
    							CONSULTING_SELECTED_FIELD_LIST, CONSULTING_DISC_JOIN_CONDITION, CONSULTING_TAB_QUERY, ' ) T ORDER BY UPDATE_TIMESTAMP DESC ');

        ELSEIF AV_FILTER_TYPE = 'TRAVEL' THEN
    		SET LS_DYN_SQL = CONCAT(LS_DYN_SQL, '(SELECT DISTINCT ',TRAVEL_SELECTED_FIELD_LIST, TRAVEL_DISC_JOIN_CONDITION, TRAVEL_TAB_QUERY, ')T ORDER BY T.UPDATE_TIMESTAMP DESC');

    	ELSEIF AV_FILTER_TYPE = 'CONSULTING' THEN
    		SET LS_DYN_SQL = CONCAT(LS_DYN_SQL, '(SELECT DISTINCT ',CONSULTING_SELECTED_FIELD_LIST, CONSULTING_DISC_JOIN_CONDITION, CONSULTING_TAB_QUERY, ')T ORDER BY T.UPDATE_TIMESTAMP DESC');

        ELSE
    		SET LS_DYN_SQL = CONCAT(LS_DYN_SQL, '(SELECT DISTINCT ',DISC_SELECTED_FIELD_LIST, DISC_JOIN_CONDITION, DISC_TAB_QUERY, ') T ORDER BY T.UPDATE_TIMESTAMP DESC');
        END IF;

        SET @QUERY_STATEMENT = LS_DYN_SQL;
    	PREPARE EXECUTABLE_STAEMENT FROM @QUERY_STATEMENT;
    	EXECUTE EXECUTABLE_STAEMENT;
    END

//
