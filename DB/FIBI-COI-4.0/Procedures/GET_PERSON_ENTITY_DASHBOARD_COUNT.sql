
DELIMITER //
CREATE PROCEDURE `GET_PERSON_ENTITY_DASHBOARD_COUNT`(
		AV_ENTITY_ID                    INT(11),
        AV_TYPE                         VARCHAR(30),
        AV_INVOLMENT_START_DATE         VARCHAR(30),
        AV_INVOLMENT_END_DATE           VARCHAR(30),
        AV_PERSON_NAME                  VARCHAR(30),
        AV_STATUS_FLAG                  VARCHAR(3)
)
BEGIN
DECLARE LS_DYN_SQL LONGTEXT;
DECLARE LS_FILTER_CONDITION LONGTEXT;
DECLARE LS_OFFSET_CONDITION VARCHAR(600);
DECLARE LS_OFFSET INT(11);
DECLARE TAB_QUERY LONGTEXT;
DECLARE JOIN_CONDITION LONGTEXT;
DECLARE SELECTED_FIELD_LIST LONGTEXT;

SET LS_FILTER_CONDITION ='';
SET LS_DYN_SQL ='';
SET JOIN_CONDITION = '';
SET SELECTED_FIELD_LIST= '';
SET TAB_QUERY = '';

IF AV_TYPE IS NOT NULL AND AV_TYPE <> '' AND  AV_TYPE = 'PERSON' THEN 

    SET TAB_QUERY = CONCAT(TAB_QUERY,' WHERE T1.ENTITY_NUMBER =(SELECT ENTITY_NUMBER from entity where ENTITY_ID =',AV_ENTITY_ID,')  AND T1.VERSION_STATUS = ''ACTIVE'' ');

END IF;

IF AV_TYPE IS NOT NULL AND AV_TYPE <> '' AND  AV_TYPE = 'FINANCIAL_DISCLOSURES' THEN 

    SET TAB_QUERY = CONCAT(TAB_QUERY,' WHERE T13.ENTITY_NUMBER =(SELECT ENTITY_NUMBER from entity where ENTITY_ID =',AV_ENTITY_ID,')  AND T13.VERSION_STATUS = ''ACTIVE'' ');
    SET JOIN_CONDITION =  CONCAT(JOIN_CONDITION,' LEFT JOIN (SELECT IFNULL(COUNT(DISTINCT T2.PERSON_ENTITY_ID),0) AS NO_OF_SFI,T2.DISCLOSURE_ID FROM PERSON_ENTITY T1 
										INNER JOIN COI_DISCL_ENT_PROJ_DETAILS T2 ON T2.PERSON_ENTITY_ID= T1.PERSON_ENTITY_ID
                                        GROUP BY T2.DISCLOSURE_ID) T7 ON T7.DISCLOSURE_ID = T1.DISCLOSURE_ID
                                        LEFT JOIN (SELECT IFNULL(COUNT(DISTINCT T1.MODULE_ITEM_KEY),0) AS NO_OF_PROPOSAL,T1.DISCLOSURE_ID FROM COI_DISCL_ENT_PROJ_DETAILS T1 
                                        WHERE MODULE_CODE = 3 GROUP BY T1.DISCLOSURE_ID) T8 ON T8.DISCLOSURE_ID = T1.DISCLOSURE_ID 
                                        LEFT JOIN (SELECT IFNULL(COUNT(DISTINCT T1.MODULE_ITEM_KEY),0) AS NO_OF_AWARD,T1.DISCLOSURE_ID FROM COI_DISCL_ENT_PROJ_DETAILS T1 
                                        WHERE MODULE_CODE = 1 GROUP BY T1.DISCLOSURE_ID) T9 ON T9.DISCLOSURE_ID = T1.DISCLOSURE_ID 
										LEFT JOIN (SELECT T1.DISCLOSURE_ID, GROUP_CONCAT(DISTINCT(T3.EXTERNAL_SYSTEM_REF_ID) SEPARATOR ",") AS PROPOSAL_IDS, 
												   GROUP_CONCAT(DISTINCT(T3.TITLE) SEPARATOR ",") AS PROPOSAL_TITLES FROM COI_DISCLOSURE T1 
												   INNER JOIN COI_DISCL_ENT_PROJ_DETAILS T2 ON T2.DISCLOSURE_ID = T1.DISCLOSURE_ID 
										           INNER JOIN COI_PROJECT_PROPOSAL_V T3 ON T3.EXTERNAL_SYSTEM_REF_ID = CAST(T2.MODULE_ITEM_KEY AS UNSIGNED) 
										           WHERE T2.MODULE_CODE = 3 GROUP BY T1.DISCLOSURE_ID) T14 ON T14.DISCLOSURE_ID = T1.DISCLOSURE_ID 
										LEFT JOIN (SELECT T1.DISCLOSURE_ID ,GROUP_CONCAT(DISTINCT(T3.EXTERNAL_SYSTEM_REF_ID) SEPARATOR ",") AS AWARD_IDS, 
												   GROUP_CONCAT(DISTINCT(T3.TITLE) SEPARATOR ",") AS AWARD_TITLES FROM COI_DISCLOSURE T1 
												   INNER JOIN COI_DISCL_ENT_PROJ_DETAILS T2 ON T2.DISCLOSURE_ID = T1.DISCLOSURE_ID 
												   INNER JOIN COI_PROJECT_AWARD_V T3 ON T3.EXTERNAL_SYSTEM_REF_ID = CAST(T2.MODULE_ITEM_KEY AS UNSIGNED) 
												   WHERE T2.MODULE_CODE = 1 GROUP BY T1.DISCLOSURE_ID) T15 ON T15.DISCLOSURE_ID = T1.DISCLOSURE_ID');

END IF;
IF AV_TYPE IS NOT NULL AND AV_TYPE <> '' AND  AV_TYPE = 'TRAVEL_DISCLOSURES' THEN
SET TAB_QUERY = CONCAT(TAB_QUERY,' WHERE T21.ENTITY_NUMBER =(SELECT ENTITY_NUMBER from entity where ENTITY_ID =',AV_ENTITY_ID,')  AND T21.VERSION_STATUS = ''ACTIVE'' ');
END IF;
IF AV_INVOLMENT_START_DATE IS NOT NULL THEN

    SET LS_FILTER_CONDITION = CONCAT(' AND T1.INVOLVEMENT_START_DATE >= ',AV_INVOLMENT_START_DATE);

END IF;

IF AV_INVOLMENT_END_DATE IS NOT NULL THEN

    SET LS_FILTER_CONDITION = CONCAT(' AND T1.INVOLVEMENT_END_DATE <= ',AV_INVOLMENT_END_DATE);

END IF;

IF AV_PERSON_NAME IS NOT NULL THEN
    SET LS_FILTER_CONDITION = CONCAT(' AND T2.FULL_NAME LIKE %',AV_PERSON_NAME, '%');
END IF;

/* IF AV_STATUS_FLAG IS NOT NULL THEN

    SET LS_FILTER_CONDITION = CONCAT(' AND T1.IS_RELATIONSHIP_ACTIVE IN (',AV_STATUS_FLAG,')');

END IF; */
	
    IF AV_TYPE IS NOT NULL AND AV_TYPE <> '' AND  AV_TYPE = 'PERSON' THEN 
    SET LS_DYN_SQL = CONCAT('SELECT  DISTINCT COUNT(*) FROM (SELECT DISTINCT T1.PERSON_ENTITY_ID, 
															 T1.PERSON_ID, 
                                                             T2.FULL_NAME,
                                                             T2.PRIMARY_TITLE AS DESIGNATION,
                                                             T5.UNIT_NUMBER AS UNIT,
															 T5.UNIT_NAME,
															 T5.ORGANIZATION_ID,
															 T5.PARENT_UNIT_NUMBER,
															 T5.IS_ACTIVE,
															 T5.ACRONYM,
															 T5.IS_FUNDING_UNIT,
															 GROUP_CONCAT(DISTINCT CONCAT(t3.DESCRIPTION, \':\', grouped_descriptions) ORDER BY t3.DESCRIPTION SEPARATOR \':;:\') AS RELATIONSHIP_TYPES,
                                                             T1.ENTITY_ID, 
                                                             T1.ENTITY_NUMBER, 
                                                             T1.IS_FORM_COMPLETED, 
															 T1.VERSION_NUMBER, 
                                                             T1.VERSION_STATUS, 
                                                             T1.INVOLVEMENT_START_DATE, 
                                                             T1.INVOLVEMENT_END_DATE, 
                                                             T1.STUDENT_INVOLVEMENT, 
                                                             T1.STAFF_INVOLVEMENT, 
                                                             T1.UPDATE_TIMESTAMP,
															 T1.INSTITUTE_RESOURCE_INVOLVEMENT 
                                                             FROM PERSON_ENTITY T1 
                                                             LEFT JOIN PERSON T2 ON T2.PERSON_ID = T1.PERSON_ID
                                                             LEFT JOIN UNIT T5 ON T5.UNIT_NUMBER = T2.HOME_UNIT
                                                             LEFT JOIN COI_DISCLOSURE T6 ON T6.PERSON_ID = T2.PERSON_ID
                                                             LEFT JOIN person_entity_relationship t4 ON t4.PERSON_ENTITY_ID = T1.PERSON_ENTITY_ID
                                                             LEFT JOIN VALID_PERSON_ENTITY_REL_TYPE t7 ON t4.VALID_PERS_ENTITY_REL_TYP_CODE = t7.VALID_PERS_ENTITY_REL_TYP_CODE
															 LEFT JOIN coi_disclosure_type t3 ON t7.DISCLOSURE_TYPE_CODE = t3.DISCLOSURE_TYPE_CODE
                                                             LEFT JOIN (
        SELECT t0.PERSON_ENTITY_ID, t3.DESCRIPTION, GROUP_CONCAT(DISTINCT t4.DESCRIPTION ORDER BY t4.DESCRIPTION SEPARATOR \'/\') AS grouped_descriptions
        FROM person_entity t0
        INNER JOIN person_entity_relationship t1 ON t1.PERSON_ENTITY_ID = t0.PERSON_ENTITY_ID
        INNER JOIN VALID_PERSON_ENTITY_REL_TYPE t2 ON t1.VALID_PERS_ENTITY_REL_TYP_CODE = t2.VALID_PERS_ENTITY_REL_TYP_CODE
        INNER JOIN coi_disclosure_type t3 ON t2.DISCLOSURE_TYPE_CODE = t3.DISCLOSURE_TYPE_CODE
        INNER JOIN person_entity_rel_type t4 ON t4.RELATIONSHIP_TYPE_CODE = t2.RELATIONSHIP_TYPE_CODE
        GROUP BY t0.PERSON_ENTITY_ID, t3.DESCRIPTION
    ) AS T8 ON T1.PERSON_ENTITY_ID = T8.PERSON_ENTITY_ID AND t3.DESCRIPTION = T8.DESCRIPTION
                                                             ',
                                                             JOIN_CONDITION,'', TAB_QUERY,' GROUP BY T1.PERSON_ENTITY_ID) T', 
                                                             LS_FILTER_CONDITION);
    END IF;                                                      
	IF AV_TYPE IS NOT NULL AND AV_TYPE <> '' AND  AV_TYPE = 'FINANCIAL_DISCLOSURES' THEN
    SET LS_DYN_SQL =CONCAT('SELECT  DISTINCT COUNT(*) FROM(SELECT DISTINCT
										T1.DISCLOSURE_ID,
										T1.DISCLOSURE_NUMBER,
										T1.VERSION_NUMBER,
										T1.PERSON_ID,
                                        T13.ENTITY_ID,
                                        T1.HOME_UNIT AS UNIT,
                                        T20.UNIT_NAME,
                                        T20.ORGANIZATION_ID,
                                        T20.PARENT_UNIT_NUMBER,
                                        T20.IS_ACTIVE,
                                        T20.ACRONYM,
                                        T20.IS_FUNDING_UNIT,
                                        T1.CONFLICT_STATUS_CODE,
										T2.DESCRIPTION AS DISCLOSURE_STATUS,
                                        T1.DISPOSITION_STATUS_CODE, 
										T3.DESCRIPTION AS DISPOSITION_STATUS,
                                        T1.FCOI_TYPE_CODE,
                                        T1.REVISION_COMMENT AS DESCRIPTION,
										T10.DESCRIPTION AS DISCLOSURE_CATEGORY_TYPE,
                                        T1.REVIEW_STATUS_CODE,
										T4.DESCRIPTION AS REVIEW_STATUS,
                                        T1.VERSION_STATUS,
										T1.CERTIFICATION_TEXT, 
										T1.CERTIFIED_AT,
										T6.FULL_NAME AS DISCLOSURE_PERSON_FULL_NAME,
                                        T1.UPDATE_USER,
                                        T1.CREATE_USER,
										T1.UPDATE_TIMESTAMP,
                                        T1.CREATE_TIMESTAMP,
										T1.EXPIRATION_DATE,
										T14.PROPOSAL_TITLES,
										T14.PROPOSAL_IDS,
										T15.AWARD_TITLES,
										T15.AWARD_IDS,
                                        T7.NO_OF_SFI,
                                        T8.NO_OF_PROPOSAL,
                                        T9.NO_OF_AWARD,	
										T31.ADMIN_GROUP_NAME, 	
                                        T32.FULL_NAME AS ADMINISTRATOR ' ,SELECTED_FIELD_LIST,'
                                        FROM COI_DISCLOSURE T1
                                        LEFT JOIN COI_DISCL_ENT_PROJ_DETAILS T30 ON T1.DISCLOSURE_ID = T30.DISCLOSURE_ID
                                        LEFT JOIN PERSON_ENTITY T13 ON T13.PERSON_ENTITY_ID = T30.PERSON_ENTITY_ID
										LEFT JOIN COI_CONFLICT_STATUS_TYPE T2 ON T2.CONFLICT_STATUS_CODE=T1.CONFLICT_STATUS_CODE
										INNER JOIN COI_DISPOSITION_STATUS_TYPE T3 ON T3.DISPOSITION_STATUS_CODE = T1.DISPOSITION_STATUS_CODE
										INNER JOIN COI_REVIEW_STATUS_TYPE T4 ON T4.REVIEW_STATUS_CODE = T1.REVIEW_STATUS_CODE
										LEFT JOIN PERSON T6 ON T6.PERSON_ID = T1.PERSON_ID
                                        LEFT JOIN UNIT T20 ON T20.UNIT_NUMBER = T1.HOME_UNIT
                                        LEFT JOIN ADMIN_GROUP T31 ON T31.ADMIN_GROUP_ID = T1.ADMIN_GROUP_ID 	
										LEFT JOIN PERSON T32 ON T32.PERSON_ID = T1.ADMIN_PERSON_ID
                                        INNER JOIN COI_DISCLOSURE_FCOI_TYPE T10 ON T10.FCOI_TYPE_CODE = T1.FCOI_TYPE_CODE ',JOIN_CONDITION, TAB_QUERY,
										' GROUP BY T1.DISCLOSURE_ID)T ',LS_FILTER_CONDITION);
	END IF;
	IF AV_TYPE IS NOT NULL AND AV_TYPE <> '' AND  AV_TYPE = 'TRAVEL_DISCLOSURES' THEN
	SET LS_DYN_SQL = CONCAT(' SELECT  DISTINCT COUNT(*) FROM(SELECT DISTINCT
								T21.TRAVEL_DISCLOSURE_ID,
	                            T21.TRAVEL_NUMBER,
	                            T21.ENTITY_ID,
								T21.VERSION_STATUS AS TRAVEL_DISCLOSURE_STATUS,
	                            T5.UNIT_NUMBER AS UNIT,
								T5.UNIT_NAME,
								T5.ORGANIZATION_ID,
								T5.PARENT_UNIT_NUMBER,
								T5.IS_ACTIVE,
								T5.ACRONYM,
								T5.IS_FUNDING_UNIT,
								T21.TRAVEL_START_DATE,
								T21.TRAVEL_END_DATE,
	                            CASE WHEN T21.DESTINATION_CITY IS NOT NULL THEN T21.DESTINATION_CITY ELSE T21.DESTINATION_COUNTRY END AS DESTINATION,
								T21.SUBMISSION_DATE AS CERTIFICATION_DATE,
								T21.UPDATE_USER,
								T21.ACKNOWLEDGE_BY,
	                            T21.ACKNOWLEDGE_AT AS ACKNOWLEDGE_DATE,
	                            T21.UPDATE_TIMESTAMP,
	                            T21.PURPOSE_OF_THE_TRIP,
								T22.ENTITY_NAME AS TRAVEL_ENTITY_NAME,
								T23.FULL_NAME AS TRAVELLER_NAME
								FROM COI_TRAVEL_DISCLOSURE T21
								LEFT JOIN ENTITY T22 ON T22.ENTITY_ID=T21.ENTITY_ID
								LEFT JOIN PERSON T23 ON T23.PERSON_ID=T21.PERSON_ID 
	                            LEFT JOIN UNIT T5 ON T5.UNIT_NUMBER = T21.HOME_UNIT
	                            LEFT JOIN PERSON_ENTITY T30 ON T30.PERSON_ENTITY_ID = T21.PERSON_ENTITY_ID',TAB_QUERY,'
	                            GROUP BY T21.TRAVEL_DISCLOSURE_ID)T ',LS_FILTER_CONDITION);
	END IF;
SET @QUERY_STATEMENT = LS_DYN_SQL;
PREPARE EXECUTABLE_STAEMENT FROM @QUERY_STATEMENT;
EXECUTE EXECUTABLE_STAEMENT;

END
//
