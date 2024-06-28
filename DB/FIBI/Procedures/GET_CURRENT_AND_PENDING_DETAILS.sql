DELIMITER $$
CREATE PROCEDURE `GET_CURRENT_AND_PENDING_DETAILS`(
AV_MODULE_CODE 			    INT(10),
AV_MODULE_ITEM_ID  		VARCHAR(500),
AV_PERSON_ID			VARCHAR(60)
)
BEGIN
DECLARE LS_DYN_SQL LONGTEXT;
DECLARE LS_PERSON_ID VARCHAR(500);
DECLARE IS_REPORT_GENERATED INT(10);
DECLARE LS_FILTER_CONDITION VARCHAR(500);

SET LS_DYN_SQL ='';
SET LS_FILTER_CONDITION = '';
							
IF AV_PERSON_ID IS NOT NULL THEN 
	SET LS_FILTER_CONDITION = CONCAT( ' AND C0.PERSON_ID = ',AV_PERSON_ID,'');
END IF;                            
BEGIN
						
						
		DECLARE DONE1 INT DEFAULT FALSE;
		DECLARE CP_PERSON_DATA CURSOR FOR
						
		SELECT DISTINCT IFNULL(AV_PERSON_ID,PERSON_ID) FROM CP_REPORT_HEADER
		WHERE MODULE_CODE = AV_MODULE_CODE 
		AND MODULE_ITEM_ID = AV_MODULE_ITEM_ID;
							 
		DECLARE CONTINUE HANDLER FOR NOT FOUND SET DONE1 = TRUE;
							
							
							OPEN CP_PERSON_DATA;
								CP_PERSON_DATA_LOOP : LOOP
								FETCH CP_PERSON_DATA 
									  INTO LS_PERSON_ID;
									  
								IF DONE1 THEN
									LEAVE CP_PERSON_DATA_LOOP;
								END IF;
								
								SELECT COUNT(1) INTO IS_REPORT_GENERATED 
								FROM CP_REPORT_HEADER
								WHERE MODULE_CODE = AV_MODULE_CODE 
								AND MODULE_ITEM_ID = AV_MODULE_ITEM_ID
								AND PERSON_ID = LS_PERSON_ID;										 
								                            
																
								SET LS_DYN_SQL = CONCAT('SELECT LINKED_MODULE_ITEM_ID,LINKED_MODULE_CODE,TITLE,END_DATE,START_DATE,MODULE_ITEM_KEY,SPONSOR_AWARD_NUMBER,
													UNIT_NAME,MODULE_STATUS,SPONSOR_NAME,TOTAL_COST,TOTAL_DIRECT_COST,PERCENTAGE_OF_EFFORT,
                                                    LEAD_PI,LEAD_PI_NON_EMPLOYEE_FLAG,IS_EXCLUDED,CP_REPORT_HEADER_ID,CP_REPORT_PROJECT_DETAIL_ID,
													MODULE_CODE,MODULE_ITEM_ID,NON_EMPLOYEE_FLAG,ROLE_NAME,CREATE_USER,PERSON_NAME,',IS_REPORT_GENERATED,' AS IS_REPORT_GENERATED_COUNT,
													PERSON_ID,LAST_UPDATED_USER_NAME,UPDATE_TIMESTAMP,PERSON_ROLE_ID,LEAD_PI_PERSON_ID,SPONSOR_CODE,
													IS_MANUALLY_ADDED,GRANT_CALL_NAME,AMOUNT,SPONSOR_TYPE_CODE,FUNDING_STATUS_CODE,CURRENCY_CODE,
													PROJECT_GOALS,SPECIFIC_AIMS,PROJECT_SUMMARY,COMMENTS FROM
                                                    (SELECT T1.AWARD_ID AS LINKED_MODULE_ITEM_ID ,1 AS LINKED_MODULE_CODE,T1.TITLE ,T1.FINAL_EXPIRATION_DATE AS END_DATE,
													T1.BEGIN_DATE AS START_DATE,T1.AWARD_NUMBER AS MODULE_ITEM_KEY,T1.SPONSOR_AWARD_NUMBER,
                                                    T5.UNIT_NAME,T6.DESCRIPTION AS MODULE_STATUS,T7.SPONSOR_NAME,T4.TOTAL_AWARD_AMOUNT AS TOTAL_COST,T4.ANNUAL_DIRECT_COST AS TOTAL_DIRECT_COST,
                                                    T4.PERCENTAGE_OF_EFFORT,T8.FULL_NAME AS LEAD_PI,
                                                    T4.LEAD_PI_NON_EMPLOYEE_FLAG,
                                                    T3.IS_EXCLUDED,T3.CP_REPORT_HEADER_ID,T3.CP_REPORT_PROJECT_DETAIL_ID,
													C0.MODULE_CODE,C0.MODULE_ITEM_ID,C0.PERSON_ID,C0.NON_EMPLOYEE_FLAG,
													T10.DESCRIPTION AS ROLE_NAME,C0.CREATE_USER,T9.FULL_NAME AS PERSON_NAME,
													T11.FULL_NAME AS LAST_UPDATED_USER_NAME,C0.UPDATE_TIMESTAMP,
													T4.PERSON_ROLE_ID,
													T4.LEAD_PI_PERSON_ID,T1.SPONSOR_CODE,
													T3.IS_MANUALLY_ADDED,T3.GRANT_CALL_NAME,T3.AMOUNT,T3.SPONSOR_TYPE_CODE,T3.FUNDING_STATUS_CODE,T3.CURRENCY_CODE,
													T4.PROJECT_GOALS,T4.SPECIFIC_AIMS,T4.PROJECT_SUMMARY,T4.COMMENTS
													FROM AWARD T1
                                                    INNER JOIN CP_REPORT_HEADER C0 ON C0.MODULE_ITEM_ID= ',AV_MODULE_ITEM_ID,' 
                                                    AND C0.VERSION_NUMBER = (SELECT MAX(VERSION_NUMBER) FROM CP_REPORT_HEADER C1 WHERE C1.PERSON_ID = C0.PERSON_ID AND C1.MODULE_ITEM_ID = ',AV_MODULE_ITEM_ID,' )
													INNER JOIN CP_REPORT_PROJECT_DETAILS T3 ON T3.LINKED_MODULE_ITEM_ID = T1.AWARD_ID
													AND T3.CP_REPORT_HEADER_ID = C0.CP_REPORT_HEADER_ID
													INNER JOIN CP_REPORT_PROJECT_DETAILS_EXT T4 ON T4.CP_REPORT_PROJECT_DETAIL_ID = T3.CP_REPORT_PROJECT_DETAIL_ID
													LEFT JOIN UNIT T5 ON T5.UNIT_NUMBER = T1.LEAD_UNIT_NUMBER
													INNER JOIN AWARD_STATUS T6 ON T6.STATUS_CODE= T1.STATUS_CODE
													LEFT JOIN SPONSOR T7 ON T7.SPONSOR_CODE=T1.SPONSOR_CODE
													LEFT JOIN PERSON T8 ON T8.PERSON_ID = T4.LEAD_PI_PERSON_ID 
                                                    INNER JOIN EPS_PROPOSAL_PERSONS T9 ON T9.PROPOSAL_ID = C0.MODULE_ITEM_ID 
													AND (CASE WHEN C0.NON_EMPLOYEE_FLAG= ''Y'' THEN T9.ROLODEX_ID = C0.PERSON_ID 
																ELSE T9.PERSON_ID = C0.PERSON_ID END)
													INNER JOIN EPS_PROP_PERSON_ROLE T10 ON T10.PROP_PERSON_ROLE_ID = T9.PROP_PERSON_ROLE_ID
													INNER JOIN PERSON T11 ON T11.USER_NAME=C0.UPDATE_USER
													WHERE T3.LINKED_MODULE_CODE = 1 ',LS_FILTER_CONDITION,'
													AND ((AWARD_SEQUENCE_STATUS = ''PENDING'' AND AWARD_DOCUMENT_TYPE_CODE = 1) OR AWARD_SEQUENCE_STATUS = ''ACTIVE'') 
													AND T1.FINAL_EXPIRATION_DATE >= DATE_SUB(NOW(), INTERVAL 5 YEAR)
													UNION
													
													SELECT T1.PROPOSAL_ID AS LINKED_MODULE_ITEM_ID,3 AS LINKED_MODULE_CODE,T1.TITLE,T1.END_DATE,T1.START_DATE,T1.PROPOSAL_ID AS MODULE_ITEM_KEY,
													NULL AS SPONSOR_AWARD_NUMBER,T5.UNIT_NAME,T6.DESCRIPTION AS MODULE_STATUS,T7.SPONSOR_NAME,T4.TOTAL_AWARD_AMOUNT AS TOTAL_COST,T4.ANNUAL_DIRECT_COST AS TOTAL_DIRECT_COST,
													T4.PERCENTAGE_OF_EFFORT,T8.FULL_NAME AS LEAD_PI,T4.LEAD_PI_NON_EMPLOYEE_FLAG,
													T3.IS_EXCLUDED,T3.CP_REPORT_HEADER_ID,T3.CP_REPORT_PROJECT_DETAIL_ID,
													C0.MODULE_CODE,C0.MODULE_ITEM_ID,C0.PERSON_ID,C0.NON_EMPLOYEE_FLAG,
													T10.DESCRIPTION AS ROLE_NAME,C0.CREATE_USER,T9.FULL_NAME AS PERSON_NAME,
													T11.FULL_NAME AS LAST_UPDATED_USER_NAME,C0.UPDATE_TIMESTAMP,
													T4.PERSON_ROLE_ID,
													T4.LEAD_PI_PERSON_ID,T1.SPONSOR_CODE,
													T3.IS_MANUALLY_ADDED,T3.GRANT_CALL_NAME,T3.AMOUNT,T3.SPONSOR_TYPE_CODE,T3.FUNDING_STATUS_CODE,T3.CURRENCY_CODE,
													T4.PROJECT_GOALS,T4.SPECIFIC_AIMS,T4.PROJECT_SUMMARY,T4.COMMENTS
													FROM 
                                                    EPS_PROPOSAL T1
                                                    INNER JOIN CP_REPORT_HEADER C0 ON C0.MODULE_ITEM_ID= ',AV_MODULE_ITEM_ID,'
													AND C0.VERSION_NUMBER = (SELECT MAX(VERSION_NUMBER) FROM CP_REPORT_HEADER C1 WHERE C1.PERSON_ID = C0.PERSON_ID AND
													C1.MODULE_ITEM_ID = ',AV_MODULE_ITEM_ID,' )													
													INNER JOIN CP_REPORT_PROJECT_DETAILS T3 ON T3.LINKED_MODULE_ITEM_ID = T1.PROPOSAL_ID
													AND T3.CP_REPORT_HEADER_ID = C0.CP_REPORT_HEADER_ID
													INNER JOIN CP_REPORT_PROJECT_DETAILS_EXT T4 ON T4.CP_REPORT_PROJECT_DETAIL_ID = T3.CP_REPORT_PROJECT_DETAIL_ID
                                                    LEFT JOIN UNIT T5 ON T5.UNIT_NUMBER = T1.HOME_UNIT_NUMBER
													INNER JOIN EPS_PROPOSAL_STATUS T6 ON T6.STATUS_CODE= T1.STATUS_CODE
													LEFT JOIN SPONSOR T7 ON T7.SPONSOR_CODE=T1.SPONSOR_CODE
													LEFT JOIN PERSON T8 ON T8.PERSON_ID = T4.LEAD_PI_PERSON_ID 
													INNER JOIN EPS_PROPOSAL_PERSONS T9 ON T9.PROPOSAL_ID = C0.MODULE_ITEM_ID 
													AND (CASE WHEN C0.NON_EMPLOYEE_FLAG= ''Y'' THEN T9.ROLODEX_ID = C0.PERSON_ID 
																ELSE T9.PERSON_ID = C0.PERSON_ID END)
													INNER JOIN EPS_PROP_PERSON_ROLE T10 ON T10.PROP_PERSON_ROLE_ID = T9.PROP_PERSON_ROLE_ID
													INNER JOIN PERSON T11 ON T11.USER_NAME=C0.UPDATE_USER
													WHERE  T3.LINKED_MODULE_CODE = 3 AND T1.DOCUMENT_STATUS_CODE <> 3 ',LS_FILTER_CONDITION,'
													UNION
													SELECT NULL AS LINKED_MODULE_ITEM_ID,T2.LINKED_MODULE_CODE,T2.PROJECT_TITLE AS TITLE,T2.END_DATE,T2.START_DATE,''External'' AS MODULE_ITEM_KEY,
													NULL AS SPONSOR_AWARD_NUMBER,NULL AS UNIT_NAME,T6.DESCRIPTION AS MODULE_STATUS,T3.SPONSOR_NAME,T2.AMOUNT AS TOTAL_COST,NULL AS TOTAL_DIRECT_COST,
													T2.PERCENTAGE_OF_EFFORT,NULL AS LEAD_PI,NULL AS LEAD_PI_NON_EMPLOYEE_FLAG,
													T2.IS_EXCLUDED,C0.CP_REPORT_HEADER_ID,T2.CP_REPORT_PROJECT_DETAIL_ID,
													C0.MODULE_CODE,C0.MODULE_ITEM_ID,C0.PERSON_ID,C0.NON_EMPLOYEE_FLAG,
													T4.DESCRIPTION AS ROLE_NAME,C0.CREATE_USER,NULL AS PERSON_NAME,
													T5.FULL_NAME AS LAST_UPDATED_USER_NAME,C0.UPDATE_TIMESTAMP,
													T2.PROP_PERSON_ROLE_ID AS PERSON_ROLE_ID,
													NULL AS LEAD_PI_PERSON_ID,T2.SPONSOR_CODE,
													T2.IS_MANUALLY_ADDED,T2.GRANT_CALL_NAME,T2.AMOUNT,T2.SPONSOR_TYPE_CODE,T2.FUNDING_STATUS_CODE,T2.CURRENCY_CODE,
													NULL AS PROJECT_GOALS,NULL AS SPECIFIC_AIMS,NULL AS PROJECT_SUMMARY,NULL AS COMMENTS
													FROM CP_REPORT_HEADER C0
													INNER JOIN CP_REPORT_PROJECT_DETAILS T2 ON T2.CP_REPORT_HEADER_ID = C0.CP_REPORT_HEADER_ID
													LEFT JOIN SPONSOR T3 ON T3.SPONSOR_CODE=T2.SPONSOR_CODE
													LEFT JOIN EPS_PROP_PERSON_ROLE T4 ON T4.PROP_PERSON_ROLE_ID = T2.PROP_PERSON_ROLE_ID
													INNER JOIN PERSON T5 ON T5.USER_NAME=C0.UPDATE_USER
													LEFT JOIN PROPOSAL_FUNDING_STATUS T6 ON T6.FUNDING_STATUS_CODE=T2.FUNDING_STATUS_CODE
													WHERE LINKED_MODULE_ITEM_ID IS NULL AND C0.MODULE_ITEM_ID= ',AV_MODULE_ITEM_ID,' ',LS_FILTER_CONDITION,'
                                                    AND C0.CP_REPORT_HEADER_ID = (SELECT MAX(CP_REPORT_HEADER_ID) FROM CP_REPORT_HEADER C1 WHERE C1.PERSON_ID = C0.PERSON_ID AND
													C1.MODULE_ITEM_ID = ',AV_MODULE_ITEM_ID,' )
                                                    UNION 
                                                    SELECT NULL AS LINKED_MODULE_ITEM_ID,NULL AS LINKED_MODULE_CODE,NULL AS TITLE,NULL AS END_DATE,NULL AS START_DATE,NULL AS MODULE_ITEM_KEY,
													NULL AS SPONSOR_AWARD_NUMBER,NULL AS UNIT_NAME,NULL AS MODULE_STATUS,NULL AS SPONSOR_NAME,NULL AS TOTAL_COST,NULL AS TOTAL_DIRECT_COST,
													NULL AS PERCENTAGE_OF_EFFORT,NULL AS LEAD_PI,NULL AS LEAD_PI_NON_EMPLOYEE_FLAG,
													NULL AS IS_EXCLUDED,C0.CP_REPORT_HEADER_ID,T5.CP_REPORT_PROJECT_DETAIL_ID,
													C0.MODULE_CODE,C0.MODULE_ITEM_ID,C0.PERSON_ID,C0.NON_EMPLOYEE_FLAG,
													T4.DESCRIPTION AS ROLE_NAME,C0.CREATE_USER,T3.FULL_NAME AS PERSON_NAME,T2.FULL_NAME AS LAST_UPDATED_USER_NAME,C0.UPDATE_TIMESTAMP,
													NULL AS PERSON_ROLE_ID,NULL AS LEAD_PI_PERSON_ID,NULL AS SPONSOR_CODE,
													NULL AS IS_MANUALLY_ADDED,NULL AS GRANT_CALL_NAME,NULL AS AMOUNT,NULL AS SPONSOR_TYPE_CODE,NULL AS FUNDING_STATUS_CODE,NULL AS CURRENCY_CODE,
													NULL AS PROJECT_GOALS,NULL AS SPECIFIC_AIMS,NULL AS PROJECT_SUMMARY,NULL AS COMMENTS
													FROM CP_REPORT_HEADER C0
													INNER JOIN PERSON T2 ON T2.USER_NAME=C0.UPDATE_USER
                                                    INNER JOIN EPS_PROPOSAL_PERSONS T3 ON T3.PROPOSAL_ID = C0.MODULE_ITEM_ID 
													AND (CASE WHEN C0.NON_EMPLOYEE_FLAG= ''Y'' THEN T3.ROLODEX_ID = C0.PERSON_ID 
																ELSE T3.PERSON_ID = C0.PERSON_ID END)
													INNER JOIN EPS_PROP_PERSON_ROLE T4 ON T4.PROP_PERSON_ROLE_ID = T3.PROP_PERSON_ROLE_ID
                                                    LEFT JOIN CP_REPORT_PROJECT_DETAILS T5 ON T5.CP_REPORT_HEADER_ID=C0.CP_REPORT_HEADER_ID
													WHERE T5.CP_REPORT_PROJECT_DETAIL_ID  IS NULL AND C0.MODULE_ITEM_ID= ',AV_MODULE_ITEM_ID,' ',LS_FILTER_CONDITION,'
                                                    AND C0.CP_REPORT_HEADER_ID = (SELECT MAX(CP_REPORT_HEADER_ID) FROM CP_REPORT_HEADER C1 WHERE C1.PERSON_ID = C0.PERSON_ID AND
													C1.MODULE_ITEM_ID = ',AV_MODULE_ITEM_ID,' ))T');
																								
									
							
		END LOOP;
		CLOSE CP_PERSON_DATA; 
END;

SET @QUERY_STATEMENT = LS_DYN_SQL;
PREPARE EXECUTABLE_STATEMENT FROM @QUERY_STATEMENT;
EXECUTE EXECUTABLE_STATEMENT;
END$$