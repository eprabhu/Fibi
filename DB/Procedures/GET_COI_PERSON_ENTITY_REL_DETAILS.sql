DELIMITER //
CREATE PROCEDURE `GET_COI_PERSON_ENTITY_REL_DETAILS`(
AV_PERSON_ID	VARCHAR(40)
)
BEGIN

DECLARE LS_DYN_SQL 	 				LONGTEXT;
DECLARE LS_FILTER_CONDITION 		LONGTEXT;

SET LS_FILTER_CONDITION ='';
SET LS_DYN_SQL ='';

	SET LS_DYN_SQL = CONCAT('SELECT * FROM (SELECT DISTINCT T1.ENTITY_ID, T2.PERSON_ENTITY_ID,T2.INVOLVEMENT_START_DATE, T2.INVOLVEMENT_END_DATE, T1.ENTITY_NAME, T2.PERSON_ID, T5.COUNTRY_NAME, 
	T3.DESCRIPTION AS ENTITY_TYPE, REPLACE (REPLACE (REPLACE (group_concat(r.DISCLOSURE_TYPE,'': ['', r.RELATIONSHIP, '']'' 
	ORDER BY r.DISCLOSURE_TYPE ASC SEPARATOR '',''), ''FCOI : '',''''), ''OPA : '', ''''), ''Travel : '', '''') AS RELATIONSHIPS, T4.DESCRIPTION AS RISK, T1.ENTITY_NUMBER,
    T2.IS_RELATIONSHIP_ACTIVE, T2.VERSION_STATUS FROM PERSON_ENTITY T2 
	LEFT JOIN ENTITY T1 ON T1.ENTITY_ID = T2.ENTITY_ID INNER JOIN ENTITY_TYPE T3 ON T3.ENTITY_TYPE_CODE = T1.ENTITY_TYPE_CODE 
	LEFT JOIN ENTITY_RISK_CATEGORY T4 ON T4.RISK_CATEGORY_CODE = T1.RISK_CATEGORY_CODE 
    LEFT JOIN COUNTRY T5 ON T5.COUNTRY_CODE = T1.COUNTRY_CODE 
	LEFT JOIN (SELECT DISTINCT T6.PERSON_ENTITY_ID, T7.DESCRIPTION AS DISCLOSURE_TYPE, 
	GROUP_CONCAT(vl.DESCRIPTION ORDER BY vl.DESCRIPTION ASC) as RELATIONSHIP
	FROM PERSON_ENTITY_RELATIONSHIP T6 INNER JOIN VALID_PERSON_ENTITY_REL_TYPE vl 
	ON vl.VALID_PERS_ENTITY_REL_TYP_CODE = T6.VALID_PERS_ENTITY_REL_TYP_CODE 
	INNER JOIN COI_DISCLOSURE_TYPE T7 ON T7.DISCLOSURE_TYPE_CODE = vl.DISCLOSURE_TYPE_CODE 
	GROUP BY T7.DISCLOSURE_TYPE_CODE, T6.PERSON_ENTITY_ID) r ON r.PERSON_ENTITY_ID = T2.PERSON_ENTITY_ID 
	WHERE T2.PERSON_ID = ',AV_PERSON_ID,'  AND T2.VERSION_STATUS  != ''ARCHIVE'' GROUP BY T2.PERSON_ENTITY_ID, T1.ENTITY_ID )T 
    ORDER BY CASE WHEN T.PERSON_ENTITY_ID IS NULL THEN 1 ELSE 0 END ') ;
	
    SET @QUERY_STATEMENT = LS_DYN_SQL;
	PREPARE EXECUTABLE_STAEMENT FROM @QUERY_STATEMENT;
	EXECUTE EXECUTABLE_STAEMENT;
END
//
