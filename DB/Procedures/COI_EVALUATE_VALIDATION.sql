DELIMITER //
CREATE PROCEDURE `COI_EVALUATE_VALIDATION`(
  AV_DISCLOSURE_ID INT,
  AV_PERSON_ID VARCHAR(45)
)
BEGIN
  DECLARE LI_QUESTIONNAIRE_FLAG INT; 
  DECLARE LI_SFI_FLAG INT;
  DECLARE LI_RELATIONSHIP_FLAG INT;
  DECLARE LS_ERROR_MSG VARCHAR(2000);
  
  -- Temporary table to store the error messages
  CREATE TEMPORARY TABLE IF NOT EXISTS ERROR_MESSAGE (MESSAGE VARCHAR(2000));
  
  SET LI_QUESTIONNAIRE_FLAG = FN_EVAL_DISCLOSURE_QUESTIONNAIRE(8, 0, AV_DISCLOSURE_ID);

  IF LI_QUESTIONNAIRE_FLAG = 1 THEN
  
		-- If questionnaire evaluates to true, then check if SFI is added
		SELECT COUNT(*) > 0 INTO LI_SFI_FLAG 
		FROM PERSON_ENTITY 
		WHERE PERSON_ID = AV_PERSON_ID
		AND VERSION_STATUS = 'ACTIVE';

		IF NOT LI_SFI_FLAG THEN
    
			  SET LS_ERROR_MSG = "You have not added any SFI(s). Based on your screening questionnaire, you have to add at least one SFI to continue with disclosure.";
			  
			  INSERT INTO ERROR_MESSAGE (MESSAGE) VALUES (LS_ERROR_MSG);
      
		END IF;
    
  END IF;
  
  SET LI_SFI_FLAG = 0;

  -- Check if any draft SFI is present
  SELECT COUNT(*) > 0 INTO LI_SFI_FLAG 
  FROM PERSON_ENTITY 
  WHERE PERSON_ID = AV_PERSON_ID 
  AND VERSION_STATUS = 'PENDING';
            
  IF LI_SFI_FLAG THEN
    
  	SET LS_ERROR_MSG = "You have draft SFI(s) in your SFI list, either define relationships and activate the SFI or delete the draft SFI (if not required) to certify the disclosure.";
			  
	INSERT INTO ERROR_MESSAGE (MESSAGE) VALUES (LS_ERROR_MSG);
              
  END IF;
  
  -- If any SFI is present, check if relationship is defined
  SELECT COUNT(*) = 0 INTO LI_RELATIONSHIP_FLAG
  FROM COI_DISCL_ENT_PROJ_DETAILS
  WHERE DISCLOSURE_ID = AV_DISCLOSURE_ID
  AND PROJECT_CONFLICT_STATUS_CODE IS NULL
  AND PERSON_ENTITY_ID IS NOT NULL;

  IF NOT LI_RELATIONSHIP_FLAG THEN
  
		SET LS_ERROR_MSG = "You have undefined Project-SFI relationships. Kindly complete the Relationships section to certify the disclosure.";
		
		INSERT INTO ERROR_MESSAGE (MESSAGE) VALUES (LS_ERROR_MSG);
    
  END IF;

  SELECT MESSAGE FROM ERROR_MESSAGE;
  
  DROP TEMPORARY TABLE IF EXISTS ERROR_MESSAGE;
  
END
//
