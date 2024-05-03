DELIMITER //
CREATE PROCEDURE `GET_COI_PERSON_ENTITY_DETAILS`(
IN AV_COI_DISCLOSURE_ID INT,
IN AV_PERSON_ID	VARCHAR(40),
IN AV_FETCH_NON_ARCHIVE BOOLEAN
)
BEGIN

DECLARE LS_DYN_SQL 	 				LONGTEXT;
DECLARE LS_FILTER_CONDITION_1 		LONGTEXT;
DECLARE LS_FILTER_CONDITION_2		LONGTEXT;
DECLARE JOIN_CONDITION 				LONGTEXT;
DECLARE SELECTED_FIELD_LIST 		LONGTEXT;

SET LS_FILTER_CONDITION_1 ='';
SET LS_FILTER_CONDITION_2 ='';
SET SELECTED_FIELD_LIST ='';
SET JOIN_CONDITION ='';

IF AV_COI_DISCLOSURE_ID IS NOT NULL THEN
		SET LS_FILTER_CONDITION_1 = CONCAT(LS_FILTER_CONDITION_1,' t.DISCLOSURE_ID = ',AV_COI_DISCLOSURE_ID,' ');
        SET SELECTED_FIELD_LIST = CONCAT(SELECTED_FIELD_LIST,' t.DISCLOSURE_DETAILS_ID,' );
        SET JOIN_CONDITION = CONCAT(JOIN_CONDITION,' coi_discl_ent_proj_details t
													INNER JOIN person_entity t0  ON t.PERSON_ENTITY_ID = t0.PERSON_ENTITY_ID' );
ELSEIF AV_PERSON_ID IS NOT NULL THEN
		SET LS_FILTER_CONDITION_1 = CONCAT(LS_FILTER_CONDITION_1,' t0.PERSON_ID = ''',AV_PERSON_ID,'''');
        SET JOIN_CONDITION = CONCAT(JOIN_CONDITION,' person_entity t0 ' );
END IF;

IF AV_FETCH_NON_ARCHIVE IS NOT NULL THEN
		SET LS_FILTER_CONDITION_2 = CONCAT(LS_FILTER_CONDITION_2,' AND t0.VERSION_STATUS  != ''ARCHIVE'' ');
END IF;

 SET LS_DYN_SQL = CONCAT(' SELECT',SELECTED_FIELD_LIST,'
        t0.PERSON_ENTITY_ID,
        t0.PERSON_ID,
        t0.ENTITY_ID,
        t0.ENTITY_NUMBER,
        t5.ENTITY_NAME,
        t6.DESCRIPTION AS ENTITY_TYPE,
        t7.COUNTRY_NAME,
        GROUP_CONCAT(DISTINCT CONCAT(t3.DESCRIPTION, \': \', grouped_descriptions) ORDER BY t3.DESCRIPTION SEPARATOR \':;:\') AS REL,
        t0.INVOLVEMENT_START_DATE,
        t0.INVOLVEMENT_END_DATE,
		t8.DESCRIPTION as ENTITY_STATUS,
        t0.IS_FORM_COMPLETED,
        t0.VERSION_STATUS,
        t9.DESCRIPTION AS RISK_CATEGORY
    FROM ',JOIN_CONDITION,'
    INNER JOIN person_entity_relationship t1 ON t1.PERSON_ENTITY_ID = t0.PERSON_ENTITY_ID
    INNER JOIN VALID_PERSON_ENTITY_REL_TYPE t2 ON t1.VALID_PERS_ENTITY_REL_TYP_CODE = t2.VALID_PERS_ENTITY_REL_TYP_CODE
    INNER JOIN coi_disclosure_type t3 ON t2.DISCLOSURE_TYPE_CODE = t3.DISCLOSURE_TYPE_CODE
    INNER JOIN person_entity_rel_type t4 ON t4.RELATIONSHIP_TYPE_CODE = t2.RELATIONSHIP_TYPE_CODE
    INNER JOIN entity t5 ON t5.ENTITY_ID = t0.ENTITY_ID
    INNER JOIN entity_type t6 ON t6.ENTITY_TYPE_CODE = t5.ENTITY_TYPE_CODE
    INNER JOIN country t7 ON t7.COUNTRY_CODE = t5.COUNTRY_CODE
	INNER JOIN entity_status t8 on t8.ENTITY_STATUS_CODE = t5.ENTITY_STATUS_CODE
    INNER JOIN ENTITY_RISK_CATEGORY t9 ON t9.RISK_CATEGORY_CODE = t5.RISK_CATEGORY_CODE
    LEFT JOIN (
        SELECT t0.PERSON_ENTITY_ID, t3.DESCRIPTION, GROUP_CONCAT(DISTINCT t4.DESCRIPTION ORDER BY t4.DESCRIPTION SEPARATOR \'/\') AS grouped_descriptions
        FROM ',JOIN_CONDITION,'
        INNER JOIN person_entity_relationship t1 ON t1.PERSON_ENTITY_ID = t0.PERSON_ENTITY_ID
        INNER JOIN VALID_PERSON_ENTITY_REL_TYPE t2 ON t1.VALID_PERS_ENTITY_REL_TYP_CODE = t2.VALID_PERS_ENTITY_REL_TYP_CODE
        INNER JOIN coi_disclosure_type t3 ON t2.DISCLOSURE_TYPE_CODE = t3.DISCLOSURE_TYPE_CODE
        INNER JOIN person_entity_rel_type t4 ON t4.RELATIONSHIP_TYPE_CODE = t2.RELATIONSHIP_TYPE_CODE	
        WHERE', LS_FILTER_CONDITION_1, LS_FILTER_CONDITION_2, '
        GROUP BY t0.PERSON_ENTITY_ID, t3.DESCRIPTION
    ) AS subquery ON t0.PERSON_ENTITY_ID = subquery.PERSON_ENTITY_ID AND t3.DESCRIPTION = subquery.DESCRIPTION
    WHERE ',LS_FILTER_CONDITION_1, LS_FILTER_CONDITION_2, '
    GROUP BY t0.PERSON_ENTITY_ID, t0.PERSON_ID, t0.ENTITY_NUMBER, t5.ENTITY_NAME') ;
    
    
    SET @QUERY_STATEMENT = LS_DYN_SQL;
	PREPARE EXECUTABLE_STATEMENT FROM @QUERY_STATEMENT;
	EXECUTE EXECUTABLE_STATEMENT;

END
//