DELIMITER //
CREATE PROCEDURE `GET_PERSON_ENTITIES`(
    AV_PERSON_ID                    VARCHAR(40),
    AV_DISCLOSURE_ID                INT(30),
    AV_REVIEW_STATUS_CODE           VARCHAR(4),
    AV_FILTER_TYPE                  VARCHAR(10),
    AV_PAGED                        INT(10),
    AV_LIMIT                        INT(10),
    AV_IS_COUNT                     BOOLEAN,
    AV_SEARCHWORD                   VARCHAR(100)
)
BEGIN

DECLARE LS_DYN_SQL          LONGTEXT;
DECLARE LS_FILTER_CONDITION LONGTEXT;
DECLARE LS_OFFSET_CONDITION VARCHAR(600);
DECLARE LS_OFFSET           INT(11);
DECLARE SELECTED_FIELD_LIST LONGTEXT;
DECLARE SEARCH_CONDITION    LONGTEXT;
DECLARE JOIN_CONDITION      LONGTEXT;

SET LS_FILTER_CONDITION ='';
SET LS_DYN_SQL ='';
SET LS_OFFSET = (AV_LIMIT * AV_PAGED);
SET SELECTED_FIELD_LIST= '';
SET SEARCH_CONDITION = '';
SET JOIN_CONDITION = '';

    IF AV_DISCLOSURE_ID IS NOT NULL AND (AV_REVIEW_STATUS_CODE IS NULL OR AV_REVIEW_STATUS_CODE NOT IN ('1', '6', '5')) THEN

        SET LS_DYN_SQL = CONCAT('SELECT DISTINCT t1.PERSON_ENTITY_ID, t1.PERSON_ENTITY_NUMBER, t1.PERSON_ID, t1.ENTITY_ID, t1.ENTITY_NUMBER, t1.INVOLVEMENT_START_DATE, t1.INVOLVEMENT_END_DATE, 
            t2.ENTITY_NAME, t2.ENTITY_TYPE_CODE, t2.IS_ACTIVE AS ENTITY_ACTIVE, t2.VERSION_STATUS AS ENTITY_VERSION_STATUS,
            t4.COUNTRY_NAME, t3.DESCRIPTION AS ENTITY_TYPE, t1.VERSION_STATUS, t1.IS_FORM_COMPLETED, false AS CAN_DELETE, t1.VERSION_NUMBER  FROM COI_DISCL_ENT_PROJ_DETAILS dd 
            INNER JOIN PERSON_ENTITY t1 ON t1.PERSON_ENTITY_ID = dd.PERSON_ENTITY_ID INNER JOIN ENTITY t2 
            ON t2.ENTITY_ID = t1.ENTITY_ID INNER JOIN ENTITY_TYPE t3 ON t3.ENTITY_TYPE_CODE = t2.ENTITY_TYPE_CODE 
            LEFT JOIN COUNTRY t4 ON t4.COUNTRY_CODE = t2.COUNTRY_CODE WHERE dd.DISCLOSURE_ID =', AV_DISCLOSURE_ID, ' ORDER BY t1.UPDATE_TIMESTAMP DESC');

    ELSE 
        IF AV_FILTER_TYPE = 'COMPLETE' THEN

            SET LS_FILTER_CONDITION = CONCAT(' AND t1.VERSION_STATUS = ''ACTIVE'' AND t1.IS_FORM_COMPLETED = ''Y'' ');

        ELSEIF AV_FILTER_TYPE = 'INCOMPLETE' THEN

                    SET LS_FILTER_CONDITION = CONCAT(' AND t1.VERSION_STATUS = ''ACTIVE'' AND t1.IS_FORM_COMPLETED = ''N'' ');

        ELSEIF AV_FILTER_TYPE = 'INACTIVE' THEN

            SET LS_FILTER_CONDITION = CONCAT(' AND t1.VERSION_STATUS = ''INACTIVE'' ');

        END IF;

        IF AV_SEARCHWORD IS NOT NULL THEN
            SET AV_SEARCHWORD = CONCAT('''%',AV_SEARCHWORD, '%''');
            SET SEARCH_CONDITION = CONCAT(' AND (t1.PERSON_ENTITY_ID LIKE ',AV_SEARCHWORD, ' OR t2.ENTITY_NAME LIKE ', AV_SEARCHWORD, ' OR t4.COUNTRY_NAME LIKE ',
                AV_SEARCHWORD, ' )');
        END IF;

        IF AV_IS_COUNT = TRUE THEN
            SET LS_OFFSET_CONDITION = '';
            SET SELECTED_FIELD_LIST= CONCAT( ' COUNT(DISTINCT t1.PERSON_ENTITY_ID) ');
        ELSE
            SET LS_OFFSET_CONDITION = CONCAT(' LIMIT ',AV_LIMIT,' OFFSET ',LS_OFFSET);
            SET SELECTED_FIELD_LIST= CONCAT( ' DISTINCT t1.PERSON_ENTITY_ID, t1.PERSON_ENTITY_NUMBER, t1.PERSON_ID, t1.ENTITY_ID, t1.ENTITY_NUMBER, t1.INVOLVEMENT_START_DATE, t1.INVOLVEMENT_END_DATE, 
                                    t2.ENTITY_NAME, t2.ENTITY_TYPE_CODE, t2.IS_ACTIVE AS ENTITY_ACTIVE, t2.VERSION_STATUS AS ENTITY_VERSION_STATUS, 
                                    t4.COUNTRY_NAME, t3.DESCRIPTION AS ENTITY_TYPE, t1.VERSION_STATUS, t1.IS_FORM_COMPLETED, 
                                    CASE WHEN ( t5.PERSON_ENTITY_ID IS NOT NULL OR t1.VERSION_NUMBER > 1 ) THEN false ELSE  true END AS CAN_DELETE, t1.VERSION_NUMBER ');
            set JOIN_CONDITION = CONCAT(' LEFT JOIN (SELECT t1.PERSON_ENTITY_ID FROM PERSON_ENTITY t1 INNER JOIN coi_discl_ent_proj_details t2 ON t2.PERSON_ENTITY_ID = t1.PERSON_ENTITY_ID
                                 INNER JOIN coi_disclosure t3 ON t3.DISCLOSURE_ID = t2.DISCLOSURE_ID WHERE t1.PERSON_ID = ''', AV_PERSON_ID, '''
                                 UNION SELECT t1.PERSON_ENTITY_ID FROM PERSON_ENTITY t1 INNER JOIN opa_discl_person_entity t2 ON t2.PERSON_ENTITY_ID = t1.PERSON_ENTITY_ID 
                                 INNER JOIN OPA_DISCLOSURE t3 ON  t3.OPA_DISCLOSURE_ID = t2.OPA_DISCLOSURE_ID WHERE t1.PERSON_ID = ''', AV_PERSON_ID, '''
                                 UNION SELECT t1.PERSON_ENTITY_ID FROM PERSON_ENTITY t1 INNER JOIN consulting_disclosure t2 ON t2.PERSON_ENTITY_ID = t1.PERSON_ENTITY_ID 
								 WHERE t1.PERSON_ID  = ''', AV_PERSON_ID, '''
                                 UNION SELECT t1.PERSON_ENTITY_ID FROM PERSON_ENTITY t1 INNER JOIN coi_travel_disclosure t2 ON t2.PERSON_ENTITY_ID = t1.PERSON_ENTITY_ID 
                                 WHERE t1.PERSON_ID = ''', AV_PERSON_ID, ''') t5 ON t5.PERSON_ENTITY_ID = t1.PERSON_ENTITY_ID ');
        END IF;


        SET LS_DYN_SQL = CONCAT(' SELECT', SELECTED_FIELD_LIST, '  FROM PERSON_ENTITY t1 INNER JOIN ENTITY t2 
            ON t2.ENTITY_ID = t1.ENTITY_ID INNER JOIN ENTITY_TYPE t3 ON t3.ENTITY_TYPE_CODE = t2.ENTITY_TYPE_CODE LEFT JOIN COUNTRY t4 ON t4.COUNTRY_CODE = t2.COUNTRY_CODE ', JOIN_CONDITION, '
            WHERE t1.VERSION_STATUS != ''ARCHIVE'' AND t1.PERSON_ID = ''', AV_PERSON_ID,'''', LS_FILTER_CONDITION, SEARCH_CONDITION,' ORDER BY t1.UPDATE_TIMESTAMP DESC', LS_OFFSET_CONDITION);

    END IF;


SET @QUERY_STATEMENT = LS_DYN_SQL;
PREPARE EXECUTABLE_STAEMENT FROM @QUERY_STATEMENT;
EXECUTE EXECUTABLE_STAEMENT;

END
//