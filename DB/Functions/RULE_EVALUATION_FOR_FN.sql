DELIMITER //
CREATE PROCEDURE `RULE_EVALUATION_FOR_FN`(
AV_MODULE_CODE_DUMMY  INT,
AV_MODULE_CODE 	      INT,
AV_SUBMODULE_CODE     INT,
AV_MODULE_ITEM_KEY    VARCHAR(20),
AV_RULE_ID 	          INT,
AV_LOGGIN_PERSON_ID   VARCHAR(40),
AV_UPDATE_USER        VARCHAR(60),
AV_SUB_MODULE_ITEM_KEY    VARCHAR(20),
OUT rule_result INT
)
    DETERMINISTIC
BEGIN
DECLARE LS_RULE_EXPR VARCHAR(300);
DECLARE  LS_RESULT VARCHAR(10);

SET rule_result = 0;

    SELECT 
    T1.RULE_EXPRESSION 
    INTO LS_RULE_EXPR
    FROM BUSINESS_RULES T1
    WHERE T1.RULE_ID = AV_RULE_ID;
    
  
     CALL RULE_EVALUATE_EXPRESSION(AV_MODULE_CODE,
                                             AV_SUBMODULE_CODE,
                                             AV_MODULE_ITEM_KEY,
                                             AV_RULE_ID,
                                             LS_RULE_EXPR,
                                             AV_UPDATE_USER,
                                             AV_LOGGIN_PERSON_ID,
											 AV_SUB_MODULE_ITEM_KEY,
                                             @LS_RESULT_EXP
                                             );
	SET LS_RESULT = @LS_RESULT_EXP;

    IF upper(LS_RESULT) = 'TRUE' THEN
     set rule_result = 1;   
    END IF;

END
//