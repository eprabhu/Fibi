DELIMITER //
CREATE PROCEDURE `GET_COI_DISCLOSURE_DASHBOARD`(
AV_PERSON_ID                    VARCHAR(200),
        AV_SORT_TYPE                    VARCHAR(500),
        AV_PAGED                        INT(10),
        AV_LIMIT                        INT(10),
        AV_TAB_TYPE                     VARCHAR(40),
        AV_UNLIMITED                    BOOLEAN,
        AV_TYPE                         VARCHAR(1),
        AV_FILTER_TYPE					VARCHAR(10)
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

IF AV_TAB_TYPE = 'IN_PROGRESS_DISCLOSURES' THEN

		IF AV_FILTER_TYPE = 'ALL' THEN

                SET TAB_QUERY = CONCAT(' T1.DISCLOSURE_CATEGORY_TYPE_CODE IN (1,2,3) AND T1.DISCLOSURE_STATUS_CODE IN (1,2,4,5,6) 
								AND T1.DISCLOSURE_SEQUENCE_STATUS_CODE = 1 AND T1.PERSON_ID = ''',AV_PERSON_ID,'''');
                                
		ELSEIF AV_FILTER_TYPE = 'OPA' THEN
        
				SET TAB_QUERY = CONCAT(' T1.DISCLOSURE_CATEGORY_TYPE_CODE = 2 AND T1.DISCLOSURE_STATUS_CODE IN (1,2,4,5,6) 
								AND T1.DISCLOSURE_SEQUENCE_STATUS_CODE = 1 AND T1.PERSON_ID = ''',AV_PERSON_ID,'''');
                                
		ELSEIF AV_FILTER_TYPE = 'FCOI' THEN
        
				SET TAB_QUERY = CONCAT(' T1.DISCLOSURE_CATEGORY_TYPE_CODE = 1 AND T1.DISCLOSURE_STATUS_CODE IN (1,2,4,5,6) 
								AND T1.DISCLOSURE_SEQUENCE_STATUS_CODE = 1 AND T1.PERSON_ID = ''',AV_PERSON_ID,'''');
		
        ELSEIF AV_FILTER_TYPE = 'PROJECT' THEN
			
				SET TAB_QUERY = CONCAT(' T1.DISCLOSURE_CATEGORY_TYPE_CODE = 3 AND T1.DISCLOSURE_STATUS_CODE IN (1,2,4,5,6) 
								AND T1.DISCLOSURE_SEQUENCE_STATUS_CODE = 1 AND T1.PERSON_ID = ''',AV_PERSON_ID,'''');
            
        END IF;
                
ELSEIF AV_TAB_TYPE = 'APPROVED_DISCLOSURES' THEN

		IF AV_FILTER_TYPE = 'ALL' THEN

				SET TAB_QUERY = CONCAT(' T1.DISCLOSURE_CATEGORY_TYPE_CODE IN (1,2,3) AND T1.DISCLOSURE_STATUS_CODE = 3 
								AND T1.DISCLOSURE_SEQUENCE_STATUS_CODE = 2 AND T1.PERSON_ID = ''',AV_PERSON_ID,'''');
                                
		ELSEIF AV_FILTER_TYPE = 'OPA' THEN
        
				SET TAB_QUERY = CONCAT(' T1.DISCLOSURE_CATEGORY_TYPE_CODE = 2 AND T1.DISCLOSURE_STATUS_CODE = 3 
								AND T1.DISCLOSURE_SEQUENCE_STATUS_CODE = 2 AND T1.PERSON_ID = ''',AV_PERSON_ID,'''');
                                
		ELSEIF AV_FILTER_TYPE = 'FCOI' THEN
        
				SET TAB_QUERY = CONCAT(' T1.DISCLOSURE_CATEGORY_TYPE_CODE = 1 AND T1.DISCLOSURE_STATUS_CODE = 3 
								AND T1.DISCLOSURE_SEQUENCE_STATUS_CODE = 2 AND T1.PERSON_ID = ''',AV_PERSON_ID,'''');
		
        ELSEIF AV_FILTER_TYPE = 'PROJECT' THEN
			
				SET TAB_QUERY = CONCAT(' T1.DISCLOSURE_CATEGORY_TYPE_CODE = 3 AND T1.DISCLOSURE_STATUS_CODE IN (1,2,4,5,6) 
								AND T1.DISCLOSURE_SEQUENCE_STATUS_CODE = 1 AND T1.PERSON_ID = ''',AV_PERSON_ID,'''');
        
        END IF;
				
ELSEIF AV_TAB_TYPE = 'TRAVEL_DISCLOSURES' THEN

                SET TAB_QUERY = CONCAT(' T1.DISCLOSURE_CATEGORY_TYPE_CODE = 4 AND T1.DISCLOSURE_SEQUENCE_STATUS_CODE IN (1,2) AND T1.PERSON_ID = ''',AV_PERSON_ID,'''');
								
ELSEIF AV_TAB_TYPE = 'DISCLOSURE_HISTORY' THEN

				SET TAB_QUERY = CONCAT(' T1.DISCLOSURE_SEQUENCE_STATUS_CODE = 3 AND T1.PERSON_ID = ''',AV_PERSON_ID,'''');

END IF;
				
                SET SELECTED_FIELD_LIST= CONCAT(SELECTED_FIELD_LIST,' , T7.NO_OF_SFI, T8.NO_OF_PROPOSAL,T9.NO_OF_AWARD');
                                        
				SET JOIN_CONDITION =  CONCAT(JOIN_CONDITION,' LEFT JOIN (SELECT IFNULL(COUNT(DISTINCT T2.COI_FINANCIAL_ENTITY_ID),0) AS NO_OF_SFI,T2.DISCLOSURE_ID FROM COI_FINANCIAL_ENTITY T1 
										INNER JOIN COI_DISCLOSURE_DETAILS T2 ON T2.COI_FINANCIAL_ENTITY_ID= T1.COI_FINANCIAL_ENTITY_ID
                                        GROUP BY T2.DISCLOSURE_ID) T7 ON T7.DISCLOSURE_ID = T1.DISCLOSURE_ID
                                        LEFT JOIN (SELECT IFNULL(COUNT(DISTINCT T1.MODULE_ITEM_KEY),0) AS NO_OF_PROPOSAL,T1.DISCLOSURE_ID FROM COI_DISCLOSURE_DETAILS T1 
                                        WHERE MODULE_CODE = 3 GROUP BY T1.DISCLOSURE_ID) T8 ON T8.DISCLOSURE_ID = T1.DISCLOSURE_ID 
                                        LEFT JOIN (SELECT IFNULL(COUNT(DISTINCT T1.MODULE_ITEM_KEY),0) AS NO_OF_AWARD,T1.DISCLOSURE_ID FROM COI_DISCLOSURE_DETAILS T1 
                                        WHERE MODULE_CODE = 1 GROUP BY T1.DISCLOSURE_ID) T9 ON T9.DISCLOSURE_ID = T1.DISCLOSURE_ID');


IF AV_SORT_TYPE IS NULL THEN
        SET AV_SORT_TYPE =  CONCAT(' ORDER BY T.CREATE_TIMESTAMP DESC ');
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
        
        SET LS_DYN_SQL =CONCAT('SELECT DISTINCT *  FROM(SELECT DISTINCT
										T1.DISCLOSURE_ID,
										T1.DISCLOSURE_NUMBER,
										T1.DISCLOSURE_VERSION_NUMBER,
										T1.PERSON_ID,
                                        T1.DISCLOSURE_STATUS_CODE,
										T2.DESCRIPTION AS DISCLOSURE_STATUS,
                                        T1.DISPOSITION_STATUS_TYPE_CODE, 
										T3.DESCRIPTION AS DISPOSITION_STATUS,
                                        T1.DISCLOSURE_CATEGORY_TYPE_CODE,
										T10.DESCRIPTION AS DISCLOSURE_CATEGORY_TYPE,
                                        T1.REVIEW_STATUS_TYPE_CODE,
										T4.DESCRIPTION AS REVIEW_STATUS,
                                        T1.DISCLOSURE_SEQUENCE_STATUS_CODE,
										T5.DESCRIPTION AS DISCLOSURE_SEQUENCE_STATUS,
										T1.CERTIFICATION_TEXT, 
										T1.CERTIFICATION_TIMESTAMP,
										T6.FULL_NAME AS UPDATE_USER,
										T1.UPDATE_TIMESTAMP,
                                        T1.CREATE_TIMESTAMP,
										T1.EXPIRATION_DATE ' ,SELECTED_FIELD_LIST,'
                                        FROM COI_DISCLOSURE T1 
										INNER JOIN COI_DISCLOSURE_STATUS T2 ON T2.DISCLOSURE_STATUS_CODE=T1.DISCLOSURE_STATUS_CODE
										INNER JOIN COI_DISPOSITION_STATUS T3 ON T3.DISPOSITION_STATUS_TYPE_CODE = T1.DISPOSITION_STATUS_TYPE_CODE
										INNER JOIN COI_REVIEW_STATUS T4 ON T4.REVIEW_STATUS_TYPE_CODE = T1.REVIEW_STATUS_TYPE_CODE
										INNER JOIN COI_DISCLOSURE_SEQUENCE_STATUS T5 ON T5.DISCLOSURE_SEQUENCE_STATUS_CODE = T1.DISCLOSURE_SEQUENCE_STATUS_CODE
										LEFT JOIN PERSON T6 ON T6.USER_NAME = T1.UPDATE_USER 
                                        INNER JOIN COI_DISCLOSURE_CATEGORY_TYPE T10 ON T10.DISCLOSURE_CATEGORY_TYPE_CODE = T1.DISCLOSURE_CATEGORY_TYPE_CODE ',JOIN_CONDITION,' WHERE',TAB_QUERY,
										' GROUP BY T1.DISCLOSURE_ID)T ',LS_FILTER_CONDITION,' ',AV_SORT_TYPE,' ',LS_OFFSET_CONDITION);

SET @QUERY_STATEMENT = LS_DYN_SQL;
PREPARE EXECUTABLE_STAEMENT FROM @QUERY_STATEMENT;
EXECUTE EXECUTABLE_STAEMENT;

END
//
