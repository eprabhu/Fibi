DELIMITER $$
CREATE PROCEDURE `RULE_EVALUATE_QUESTION`(
AV_RULE_EXPRESSION_ID     DECIMAL(38,0),
AV_MODULE_CODE            DECIMAL(38,0),
AV_SUBMODULE_CODE         DECIMAL(38,0),
AV_MODULE_ITEM_KEY        VARCHAR(200),
AV_SUB_MODULE_ITEM_KEY	 VARCHAR(20),
OUT RESULT_EXP VARCHAR(200)
)
    DETERMINISTIC
BEGIN
DECLARE LS_LVALUE               VARCHAR(4000);
DECLARE LS_OPERATOR             VARCHAR(25);
DECLARE LS_RVALUE               VARCHAR(4000);
DECLARE LS_ANSWER_COUNT         DECIMAL(3,0) ;
DECLARE LS_ANSWER               VARCHAR(1000);
DECLARE LS_EXPR                 VARCHAR(100);
DECLARE LS_RESULT               VARCHAR(10);
DECLARE LI_ANSWER_HEADER_ID			INT;
    SELECT 
    T1.LVALUE,
    CASE WHEN UPPER(T1.CONDITION_OPERATOR) = 'EQUAL TO' THEN '='
         WHEN UPPER(T1.CONDITION_OPERATOR) = 'GREATER THAN' THEN '>'
         WHEN UPPER(T1.CONDITION_OPERATOR) = 'GREATER THAN EQUAL TO' THEN '>='
         WHEN UPPER(T1.CONDITION_OPERATOR) = 'LESS THAN' THEN '<'
         WHEN UPPER(T1.CONDITION_OPERATOR) = 'LESS THAN EQUAL TO' THEN '<='
         WHEN UPPER(T1.CONDITION_OPERATOR) = 'NOT EQUAL TO' THEN '<>'
         WHEN UPPER(T1.CONDITION_OPERATOR) = 'CONTAINS' THEN '%'
         WHEN TRIM(UPPER(T1.CONDITION_OPERATOR)) = 'IS EMPTY' THEN 'EMPTY'
         WHEN TRIM(UPPER(T1.CONDITION_OPERATOR)) = 'IS NOT EMPTY' THEN 'NOT EMPTY'
    END,
    T1.RVALUE 
    INTO LS_LVALUE,LS_OPERATOR,LS_RVALUE
    FROM BUSINESS_RULES_EXPERSSION T1
    WHERE T1.RULES_EXPERSSION_ID = AV_RULE_EXPRESSION_ID;
    IF LS_OPERATOR = '%' THEN 
        SELECT COUNT(T4.ANSWER)
        INTO LS_ANSWER_COUNT
        FROM QUEST_USAGE T1
        INNER JOIN QUEST_QUESTION T2 ON T1.QUESTIONNAIRE_ID = T2.QUESTIONNAIRE_ID
        INNER JOIN QUEST_ANSWER_HEADER T3 ON T2.QUESTIONNAIRE_ID = T3.QUESTIONNAIRE_ID
        INNER JOIN QUEST_ANSWER T4 ON T3.QUESTIONNAIRE_ANS_HEADER_ID = T4.QUESTIONNAIRE_ANS_HEADER_ID 
                                   AND T4.QUESTION_ID = T2.QUESTION_ID
		INNER JOIN QUEST_HEADER T5 ON T5.QUESTIONNAIRE_ID = T1.QUESTIONNAIRE_ID AND T5.IS_FINAL = 'Y'
        WHERE T1.MODULE_ITEM_CODE = AV_MODULE_CODE 
        AND T3.MODULE_ITEM_KEY = AV_MODULE_ITEM_KEY
        AND T2.QUESTION_NUMBER = LS_LVALUE
        AND T4.ANSWER LIKE CONCAT('%',LS_RVALUE,'%');
    ELSE
		SELECT MAX(T3.QUESTIONNAIRE_ANS_HEADER_ID) INTO LI_ANSWER_HEADER_ID
        FROM QUEST_USAGE T1
        INNER JOIN QUEST_QUESTION T2 ON T1.QUESTIONNAIRE_ID = T2.QUESTIONNAIRE_ID
        INNER JOIN QUEST_ANSWER_HEADER T3 ON T2.QUESTIONNAIRE_ID = T3.QUESTIONNAIRE_ID 
        AND T3.MODULE_ITEM_CODE = T1.MODULE_ITEM_CODE AND T3.MODULE_SUB_ITEM_CODE = T1.MODULE_SUB_ITEM_CODE
		INNER JOIN QUEST_HEADER T5 ON T5.QUESTIONNAIRE_ID = T1.QUESTIONNAIRE_ID AND T5.IS_FINAL = 'Y'
        WHERE T1.MODULE_ITEM_CODE = AV_MODULE_CODE 
        AND T3.MODULE_ITEM_KEY = AV_MODULE_ITEM_KEY
        AND T2.QUESTION_NUMBER = LS_LVALUE;
		IF LI_ANSWER_HEADER_ID IS NOT NULL THEN
			SELECT GROUP_CONCAT(T4.ANSWER SEPARATOR ',')
			INTO LS_ANSWER
			FROM QUEST_USAGE T1
			INNER JOIN QUEST_QUESTION T2 ON T1.QUESTIONNAIRE_ID = T2.QUESTIONNAIRE_ID
			INNER JOIN QUEST_ANSWER_HEADER T3 ON T2.QUESTIONNAIRE_ID = T3.QUESTIONNAIRE_ID
            AND T3.MODULE_ITEM_CODE = T1.MODULE_ITEM_CODE AND T3.MODULE_SUB_ITEM_CODE = T1.MODULE_SUB_ITEM_CODE
			INNER JOIN QUEST_ANSWER T4 ON T3.QUESTIONNAIRE_ANS_HEADER_ID = T4.QUESTIONNAIRE_ANS_HEADER_ID 
									   AND T4.QUESTION_ID = T2.QUESTION_ID
			INNER JOIN QUEST_HEADER T5 ON T5.QUESTIONNAIRE_ID = T1.QUESTIONNAIRE_ID AND T5.IS_FINAL = 'Y'
			WHERE T1.MODULE_ITEM_CODE = AV_MODULE_CODE 
			AND T3.MODULE_ITEM_KEY = AV_MODULE_ITEM_KEY
			AND T2.QUESTION_NUMBER = LS_LVALUE
			AND T3.QUESTIONNAIRE_ANS_HEADER_ID = LI_ANSWER_HEADER_ID;
        END IF;
    END IF;
    IF LS_OPERATOR = '%' THEN   
        IF LS_ANSWER_COUNT > 0 THEN
           SET  LS_RESULT = 'TRUE';
        ELSE
           SET  LS_RESULT = 'FALSE';
        END IF;
	ELSEIF LS_OPERATOR = 'EMPTY' THEN
		IF LS_ANSWER IS NULL THEN
			SET LS_RESULT = 'TRUE';
		ELSE 
			SET LS_RESULT = 'FALSE';
		END IF;
	ELSEIF LS_OPERATOR = 'NOT EMPTY' THEN
		IF LS_ANSWER IS NULL THEN
			SET LS_RESULT = 'FALSE';
		ELSE 
			SET LS_RESULT = 'TRUE';
		END IF;	
    ELSE
		IF LS_ANSWER IS NULL THEN 
		SET LS_ANSWER = 'NULL';
		END IF;
		SET LS_EXPR = CONCAT('''',LS_ANSWER,'''',LS_OPERATOR,'''',LS_RVALUE,'''');
        CALL RULE_VERIFY_EXPRESSION(LS_EXPR,@RESULT_EXP);
		SET LS_RESULT = @RESULT_EXP;
   END IF;
   SELECT LS_RESULT INTO RESULT_EXP;
END$$
