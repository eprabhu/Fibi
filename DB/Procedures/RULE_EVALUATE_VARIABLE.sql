DELIMITER //
CREATE PROCEDURE `RULE_EVALUATE_VARIABLE`(
AV_RULE_EXPRESSION_ID    DECIMAL(38,0),
AV_MODULE_CODE           DECIMAL(3,0),
AV_SUBMODULE_CODE        DECIMAL(3,0),
AV_MODULE_ITEM_KEY       VARCHAR(20),
AV_SUB_MODULE_ITEM_KEY	 VARCHAR(20),
OUT EXPR_RESULT VARCHAR(2000)
)
    DETERMINISTIC
BEGIN
DECLARE LS_LVALUE           VARCHAR(4000);
DECLARE LS_OPERATOR         VARCHAR(25);
DECLARE LS_RVALUE           VARCHAR(4000);
DECLARE LS_TABLE_NAME       VARCHAR(30);
DECLARE LS_COLUMN_NAME      VARCHAR(30);
DECLARE LS_VARIABLE         LONGTEXT;
DECLARE LS_FILTER_CONDTION  VARCHAR(1000);
DECLARE LS_RESULT           VARCHAR(1000);
DECLARE LS_PROPOSAL_ID INT(10);
DECLARE LS_LOOKUP_WINDOW_NAME   VARCHAR(100);  

SET LS_FILTER_CONDTION = '';
SET LS_VARIABLE = '';

SELECT T1.LVALUE,
    CASE WHEN TRIM(UPPER(T1.CONDITION_OPERATOR)) = 'EQUAL TO' THEN '='
         WHEN TRIM(UPPER(T1.CONDITION_OPERATOR)) = 'GREATER THAN' THEN '>'
         WHEN TRIM(UPPER(T1.CONDITION_OPERATOR)) = 'GREATER THAN EQUAL TO' THEN '>='
         WHEN TRIM(UPPER(T1.CONDITION_OPERATOR)) = 'LESS THAN' THEN '<'
         WHEN TRIM(UPPER(T1.CONDITION_OPERATOR)) = 'LESS THAN EQUAL TO' THEN '<='
         WHEN TRIM(UPPER(T1.CONDITION_OPERATOR)) = 'NOT EQUAL TO' THEN '<>'
         WHEN TRIM(UPPER(T1.CONDITION_OPERATOR)) = 'CONTAINS' THEN '%'
		 WHEN TRIM(UPPER(T1.CONDITION_OPERATOR)) = 'IS EMPTY' THEN 'EMPTY'
         WHEN TRIM(UPPER(T1.CONDITION_OPERATOR)) = 'IS NOT EMPTY' THEN 'NOT EMPTY'
    END,
    T1.RVALUE 
    INTO LS_LVALUE,LS_OPERATOR,LS_RVALUE
    FROM BUSINESS_RULES_EXPERSSION T1
    WHERE T1.RULES_EXPERSSION_ID = AV_RULE_EXPRESSION_ID;
    
 
    SELECT T1.TABLE_NAME,T1.COLUMN_NAME,T1.MODULE_CODE,T1.SUB_MODULE_CODE, T1.LOOKUP_WINDOW_NAME  
    INTO LS_TABLE_NAME,LS_COLUMN_NAME,AV_MODULE_CODE,AV_SUBMODULE_CODE, LS_LOOKUP_WINDOW_NAME
    FROM BUSINESS_RULE_VARIABLE T1
    WHERE LOWER(T1.VARIABLE_NAME) = LOWER(LS_LVALUE);
  
    IF AV_MODULE_CODE = 1 AND AV_SUBMODULE_CODE = 0 THEN 

      SET  LS_FILTER_CONDTION = CONCAT(' WHERE AWARD_ID =''',AV_MODULE_ITEM_KEY,'''' );

    ELSEIF AV_MODULE_CODE IN (2,3) AND AV_SUBMODULE_CODE IN (0,4) THEN 
        
        IF LS_TABLE_NAME = 'GRANT_CALL_HEADER' THEN 
        
			IF AV_SUBMODULE_CODE = 4 THEN 
            
				SELECT PROPOSAL_ID INTO LS_PROPOSAL_ID
				FROM PROPOSAL_REVIEW 
				WHERE REVIEW_ID = AV_MODULE_ITEM_KEY;           
				 
			ELSE
				SET LS_PROPOSAL_ID = AV_MODULE_ITEM_KEY;
            END IF;		
			
			SET LS_FILTER_CONDTION = CONCAT(' T1 INNER JOIN EPS_PROPOSAL T2 ON T1.GRANT_HEADER_ID = T2.GRANT_HEADER_ID
												  WHERE T2.PROPOSAL_ID =''',LS_PROPOSAL_ID,'''');
        ELSE                                      
	
			SET LS_FILTER_CONDTION = CONCAT(' WHERE PROPOSAL_ID =''',AV_MODULE_ITEM_KEY,''''); 
        
       END IF;
    
    ELSEIF AV_MODULE_CODE = 5 AND AV_SUBMODULE_CODE = 0 THEN 
		
        SET LS_FILTER_CONDTION = CONCAT(' WHERE NEGOTIATION_ID =''',AV_MODULE_ITEM_KEY,'''');
        
    ELSEIF AV_MODULE_CODE = 13 AND AV_SUBMODULE_CODE = 0 THEN 
		
        SET LS_FILTER_CONDTION = CONCAT(' WHERE AGREEMENT_REQUEST_ID =''',AV_MODULE_ITEM_KEY,'''');
	
    ELSEIF AV_MODULE_CODE = 1 AND AV_SUBMODULE_CODE = 2 THEN  
	
		SET LS_FILTER_CONDTION = CONCAT(' WHERE TASK_ID =''',AV_SUB_MODULE_ITEM_KEY,'''');

	ELSEIF AV_MODULE_CODE = 20 AND AV_SUBMODULE_CODE = 0 THEN 
		
        SET LS_FILTER_CONDTION = CONCAT(' WHERE SR_HEADER_ID =''',AV_MODULE_ITEM_KEY,'''');
	
    ELSEIF AV_MODULE_CODE = 8 AND AV_SUBMODULE_CODE = 801 THEN 
		
        SET LS_FILTER_CONDTION = CONCAT(' WHERE PERSON_ENTITY_ID =''',AV_MODULE_ITEM_KEY,''' AND VALID_PERS_ENTITY_REL_TYP_CODE =''',AV_SUB_MODULE_ITEM_KEY,'''');

    END IF;
    
    IF AV_MODULE_CODE IN (1,2,3,5,13,20) AND AV_SUBMODULE_CODE IN (0,4,1,2) THEN

        IF LS_OPERATOR = '%' THEN 

			SET LS_VARIABLE = CONCAT('SELECT COUNT(1) INTO @RESULT FROM ',LS_TABLE_NAME,LS_FILTER_CONDTION,' AND ',LS_COLUMN_NAME,' LIKE ''',LS_RVALUE,'%''');
        
        ELSE
		
			IF LS_LOOKUP_WINDOW_NAME IS NOT NULL AND LS_LOOKUP_WINDOW_NAME <> '' AND LS_LOOKUP_WINDOW_NAME = 'DATE' THEN  

		 		SET LS_VARIABLE = CONCAT('SELECT DATE_FORMAT(',LS_COLUMN_NAME, ',"%Y-%m-%d") INTO @RESULT FROM ',LS_TABLE_NAME,LS_FILTER_CONDTION);
				SET LS_RVALUE = CONCAT('''',LS_RVALUE,'''');

			ELSE 
            
				SET LS_VARIABLE = CONCAT('SELECT ',LS_COLUMN_NAME,' INTO @RESULT FROM ',LS_TABLE_NAME,LS_FILTER_CONDTION);
			END IF;

        END IF;
		 	
    ELSEIF AV_MODULE_CODE = 3 AND AV_SUBMODULE_CODE = 1 THEN 

        IF UPPER(LS_TABLE_NAME) = 'BUDGET_HEADER' THEN

					IF LS_OPERATOR = '%' THEN

					  SET LS_VARIABLE = CONCAT('SELECT COUNT(1) INTO @RESULT FROM EPS_PROPOSAL T1
										INNER JOIN BUDGET_HEADER T2 ON T1.BUDGET_HEADER_ID = T2.BUDGET_HEADER_ID
										WHERE T1.PROPOSAL_ID =',AV_MODULE_ITEM_KEY,' AND T2.',LS_COLUMN_NAME,' LIKE ''%''',LS_RVALUE,'''%''');

					ELSE

					  SET LS_VARIABLE = CONCAT('SELECT T2.',LS_COLUMN_NAME,' INTO @RESULT FROM EPS_PROPOSAL T1
										  INNER JOIN BUDGET_HEADER T2 ON T1.BUDGET_HEADER_ID = T2.BUDGET_HEADER_ID
										  WHERE T1.PROPOSAL_ID =',AV_MODULE_ITEM_KEY);                  
					END IF;

        ELSEIF UPPER(LS_TABLE_NAME) = 'BUDGET_PERIOD' THEN

					IF LS_OPERATOR = '%' THEN

						SET LS_VARIABLE = CONCAT('SELECT COUNT(1) INTO @RESULT FROM FIBI_PROPOSAL T1
										  INNER JOIN BUDGET_HEADER T2 ON T1.BUDGET_HEADER_ID = T2.BUDGET_HEADER_ID
										  INNER JOIN BUDGET_PERIOD T3 ON T2.BUDGET_HEADER_ID = T3.BUDGET_HEADER_ID
										  WHERE T1.PROPOSAL_ID =',AV_MODULE_ITEM_KEY,' 
										  AND T3.BUDGET_PERIOD IN (SELECT MAX(T4.BUDGET_PERIOD)
																   FROM BUDGET_PERIOD T4
																   WHERE T3.BUDGET_HEADER_ID = T4.BUDGET_HEADER_ID)
										  AND T3.',LS_COLUMN_NAME,' LIKE ''%''',LS_RVALUE,'''%''');
					ELSE
						SET LS_VARIABLE = CONCAT('SELECT T3.',LS_COLUMN_NAME,' INTO @RESULT FROM EPS_PROPOSAL T1
										  INNER JOIN BUDGET_HEADER T2 ON T1.BUDGET_HEADER_ID = T2.BUDGET_HEADER_ID
										  INNER JOIN BUDGET_PERIOD T3 ON T2.BUDGET_HEADER_ID = T3.BUDGET_HEADER_ID
										  WHERE T1.PROPOSAL_ID =',AV_MODULE_ITEM_KEY,'
										  AND T3.BUDGET_PERIOD IN (SELECT MAX(T4.BUDGET_PERIOD)
																   FROM BUDGET_PERIOD T4
																   WHERE T3.BUDGET_HEADER_ID = T4.BUDGET_HEADER_ID
																  )');
					END IF;
		END IF;

	ELSEIF AV_MODULE_CODE = 16 AND AV_SUBMODULE_CODE = 0 THEN 

				IF UPPER(LS_TABLE_NAME) = 'FUNDING_SCHEME' THEN
					IF LS_OPERATOR = '=' THEN
                        SET LS_VARIABLE = CONCAT('SELECT T4.',LS_COLUMN_NAME,' INTO @RESULT FROM AWARD_PROGRESS_REPORT T1 
						INNER JOIN AWARD T2 ON T1.AWARD_ID = T2.AWARD_ID 
						INNER JOIN GRANT_CALL_HEADER T3 ON T2.GRANT_HEADER_ID = T3.GRANT_HEADER_ID
						INNER JOIN SPONSOR_FUNDING_SCHEME T4 ON T3.FUNDING_SCHEME_ID = T4.FUNDING_SCHEME_ID
						WHERE T1.PROGRESS_REPORT_ID = ',AV_MODULE_ITEM_KEY);

                    END IF;
				END IF;   

	ELSEIF AV_MODULE_CODE = 8 AND AV_SUBMODULE_CODE = 801 THEN
    
        SET LS_VARIABLE = CONCAT('SELECT ',LS_COLUMN_NAME,' INTO @RESULT FROM ',LS_TABLE_NAME,LS_FILTER_CONDTION);
        
    ELSEIF AV_MODULE_CODE = 8 AND AV_SUBMODULE_CODE = 0 THEN
                
        SET LS_VARIABLE = CONCAT('SELECT ', LS_COLUMN_NAME, ' INTO @RESULT FROM COI_DISCLOSURE WHERE DISCLOSURE_ID = ', AV_MODULE_ITEM_KEY);
	
	ELSEIF AV_MODULE_CODE = 24 AND AV_SUBMODULE_CODE = 0 THEN
                
        SET LS_VARIABLE = CONCAT('SELECT ', LS_COLUMN_NAME, ' INTO @RESULT FROM COI_TRAVEL_DISCLOSURE WHERE TRAVEL_DISCLOSURE_ID = ', AV_MODULE_ITEM_KEY);
	
	ELSEIF AV_MODULE_CODE = 23 AND AV_SUBMODULE_CODE = 0 THEN
                
        SET LS_VARIABLE = CONCAT('SELECT ', LS_COLUMN_NAME, ' INTO @RESULT FROM OPA_DISCLOSURE WHERE OPA_DISCLOSURE_ID = ', AV_MODULE_ITEM_KEY);

    END IF;

   SET @RESULT ='';
   SET @STATEMENT = LS_VARIABLE;
   
   PREPARE QUERY_STATEMENT FROM @STATEMENT;
   EXECUTE QUERY_STATEMENT;
   
   IF LS_LOOKUP_WINDOW_NAME IS NOT NULL AND LS_LOOKUP_WINDOW_NAME <> '' AND LS_LOOKUP_WINDOW_NAME = 'DATE' THEN 
 		SET LS_RESULT = CONCAT('''',@RESULT,'''');
 	ELSE 
		SET LS_RESULT = @RESULT;
 	END IF;
    IF LS_RESULT IS NOT NULL AND LS_RESULT <> '' THEN
	   IF LS_OPERATOR = '%' THEN   
			IF LS_RESULT > 0  THEN
			   SET LS_RESULT = 'TRUE';
			ELSE
			   SET LS_RESULT = 'FALSE';
			END IF;
		ELSEIF LS_OPERATOR = 'NOT EMPTY'  THEN
			IF (LS_RESULT IS NOT NULL OR LS_RESULT <>'' ) THEN
			SET LS_RESULT = 'TRUE';
            ELSE 
            SET LS_RESULT = 'FALSE';
            END IF;
		ELSEIF LS_OPERATOR = 'EMPTY'  THEN
			IF (LS_RESULT IS NULL OR LS_RESULT ='' ) THEN
			SET LS_RESULT = 'TRUE';
            ELSE 
            SET LS_RESULT = 'FALSE';
            END IF;
		ELSE
			SET LS_VARIABLE = CONCAT(LS_RESULT,LS_OPERATOR,LS_RVALUE); 
			CALL RULE_VERIFY_EXPRESSION(LS_VARIABLE,@RESULT_EXP);
			SET LS_RESULT = @RESULT_EXP;
		END IF;
         
	ELSE
		IF LS_OPERATOR = 'EMPTY' THEN
			SET LS_RESULT = 'TRUE';
		ELSE SET LS_RESULT = 'FALSE';
        END IF;
	END IF;

   SELECT LS_RESULT INTO EXPR_RESULT;  

END
//
