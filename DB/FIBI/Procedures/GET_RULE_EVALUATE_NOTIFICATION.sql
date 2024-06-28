DELIMITER $$
CREATE PROCEDURE `GET_RULE_EVALUATE_NOTIFICATION`(
AV_MODULE_CODE           DECIMAL(38,0),
AV_SUBMODULE_CODE        DECIMAL(38,0),
AV_MODULE_ITEM_KEY       VARCHAR(200),
AV_UPDATE_USER           VARCHAR(200),
AV_LOGGIN_PERSON_ID      VARCHAR(200),
AV_SUB_MODULE_ITEM_KEY    VARCHAR(20)
)
    DETERMINISTIC
BEGIN
DECLARE LS_RESULT             VARCHAR(10);
DECLARE LS_FILTER_CONDITION   VARCHAR(1000);
DECLARE LS_DYNAMIC_QRY        VARCHAR(4000);
DECLARE LS_FLAG               DECIMAL(1,0);
DECLARE  LS_RULE_ID INT(6);
DECLARE  LS_RULE_TYPE VARCHAR(2);
DECLARE  LS_RULE_EXPRESSION VARCHAR(300);
DECLARE  LS_NOTIFICATION_ID INT(3);
DECLARE LS_PARENT_UNIT_NUMBER VARCHAR(20);
DECLARE LS_LEAD_UNIT_NUMBER VARCHAR(20);
DECLARE LS_COUNT INT;
DECLARE DONE1 INT DEFAULT FALSE;
DECLARE RULE_CURSOR CURSOR FOR 
SELECT T1.RULE_ID,T1.RULE_TYPE,T1.RULE_EXPRESSION,T1.NOTIFICATION_ID FROM BUSINESS_RULES T1
                       WHERE T1.MODULE_CODE = AV_MODULE_CODE 
                       AND T1.SUB_MODULE_CODE = AV_SUBMODULE_CODE
					   AND T1.UNIT_NUMBER = LS_PARENT_UNIT_NUMBER
                       AND T1.RULE_TYPE = 'N'
                       AND T1.IS_ACTIVE = 'Y';
DECLARE RULE_NEGOTIATION_CURSOR CURSOR FOR 
SELECT T1.RULE_ID,T1.RULE_TYPE,T1.RULE_EXPRESSION,T1.NOTIFICATION_ID FROM BUSINESS_RULES T1
                       WHERE T1.MODULE_CODE = AV_MODULE_CODE 
                       AND T1.SUB_MODULE_CODE = AV_SUBMODULE_CODE
                       AND T1.RULE_TYPE = 'N'
                       AND T1.IS_ACTIVE = 'Y';
DECLARE CONTINUE HANDLER FOR NOT FOUND SET DONE1 = TRUE;
SET LS_FILTER_CONDITION ='';
SET LS_DYNAMIC_QRY = '';
if AV_MODULE_CODE = 1 then
 	 SELECT LEAD_UNIT_NUMBER INTO LS_LEAD_UNIT_NUMBER 
	 FROM AWARD WHERE AWARD_ID = AV_MODULE_ITEM_KEY;
elseif AV_MODULE_CODE = 3 then
	 SELECT HOME_UNIT_NUMBER INTO LS_LEAD_UNIT_NUMBER 
	 FROM EPS_PROPOSAL WHERE PROPOSAL_ID = AV_MODULE_ITEM_KEY;
end if;
SET LS_PARENT_UNIT_NUMBER = LS_LEAD_UNIT_NUMBER ;
IF AV_MODULE_CODE = 5 THEN
		OPEN RULE_NEGOTIATION_CURSOR;
		RULE_NEGO_CURSOR_LOOP : LOOP
				FETCH RULE_NEGOTIATION_CURSOR INTO LS_RULE_ID,LS_RULE_TYPE,LS_RULE_EXPRESSION,LS_NOTIFICATION_ID;
				IF DONE1 THEN
					LEAVE RULE_NEGO_CURSOR_LOOP;
				END IF;
		 CALL RULE_EVALUATE_EXPRESSION(AV_MODULE_CODE,
										   AV_SUBMODULE_CODE,
										   AV_MODULE_ITEM_KEY,
										   LS_RULE_ID,
										   LS_RULE_EXPRESSION,
										   AV_UPDATE_USER,
										   AV_LOGGIN_PERSON_ID,
										   AV_SUB_MODULE_ITEM_KEY,
                                           @RESULT_EXP
										   );  
				SET LS_RESULT = @RESULT_EXP;
				IF LS_RESULT = 'TRUE' THEN
					SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,LS_RULE_ID,',');
					SET LS_FLAG = 1;
				 END IF;
		 END LOOP;
		CLOSE RULE_NEGOTIATION_CURSOR;
ELSE
		WHILE LS_PARENT_UNIT_NUMBER IS NOT NULL  DO
			OPEN RULE_CURSOR;
			RULE_CURSOR_LOOP : LOOP
					FETCH RULE_CURSOR INTO LS_RULE_ID,LS_RULE_TYPE,LS_RULE_EXPRESSION,LS_NOTIFICATION_ID;
					IF DONE1 THEN
						LEAVE RULE_CURSOR_LOOP;
					END IF;
					 CALL RULE_EVALUATE_EXPRESSION(AV_MODULE_CODE,
													   AV_SUBMODULE_CODE,
													   AV_MODULE_ITEM_KEY,
													   LS_RULE_ID,
													   LS_RULE_EXPRESSION,
													   AV_UPDATE_USER,
													   AV_LOGGIN_PERSON_ID,
													   AV_SUB_MODULE_ITEM_KEY,
													   @RESULT_EXP
													   );  
					SET LS_RESULT = @RESULT_EXP;
					IF LS_RESULT = 'TRUE' THEN
						SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,LS_RULE_ID,',');
						SET LS_FLAG = 1;
					 END IF;
			 END LOOP;
			CLOSE RULE_CURSOR;
			SET DONE1 = FALSE;
			SELECT COUNT(PARENT_UNIT_NUMBER) into LS_COUNT  FROM UNIT WHERE UNIT_NUMBER = LS_PARENT_UNIT_NUMBER;
			IF LS_COUNT = 0 THEN
				SET LS_PARENT_UNIT_NUMBER = NULL;
			ELSE
				SELECT PARENT_UNIT_NUMBER INTO LS_PARENT_UNIT_NUMBER FROM UNIT WHERE UNIT_NUMBER = LS_PARENT_UNIT_NUMBER;
			END IF;
		END WHILE;
END IF;
SELECT TRIM( TRAILING ',' FROM TRIM(LS_FILTER_CONDITION))
INTO LS_FILTER_CONDITION FROM DUAL;
    IF LS_FLAG = 1 THEN
        SET LS_DYNAMIC_QRY = CONCAT('SELECT NOTIFICATION_ID FROM BUSINESS_RULES WHERE RULE_ID IN (',LS_FILTER_CONDITION,')'); 
        SET @STATEMENT = LS_DYNAMIC_QRY;
        PREPARE QUERY_STATEMENT FROM @STATEMENT;
        EXECUTE QUERY_STATEMENT;
    END IF;
END$$