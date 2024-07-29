DELIMITER //
CREATE FUNCTION `FN_EVAL_DISCLOSURE_STATUS`(
AV_PERSON_ID    		 VARCHAR(40),
AV_PERSON_ROLE_CODE    	 VARCHAR(20),
AV_MODULE_CODE   VARCHAR(20),
AV_MODULEITEM_KEY  INT
) RETURNS int
    DETERMINISTIC
BEGIN

DECLARE LI_COUNT INT DEFAULT 0;

	SELECT count(T1.MODULE_ITEM_KEY) INTO LI_COUNT 
    FROM coi_disclosure T1
    WHERE T1.MODULE_ITEM_KEY = AV_MODULEITEM_KEY 
    AND T1.MODULE_CODE = AV_MODULE_CODE 
    AND T1.PERSON_ID =  AV_PERSON_ID;
    
    IF LI_COUNT > 0 THEN
		return 1;
	END IF;
    
    SELECT count(*) INTO LI_COUNT
	FROM sponsor_disclosure_requirements T1 
	LEFT JOIN coi_project_proposal_v T2 
		ON T2.SPONSOR_CODE = T1.SPONSOR_CODE
	LEFT JOIN coi_project_proposal_v T3 
		ON T3.PRIME_SPONSOR_CODE = T1.SPONSOR_CODE
	WHERE 
		(
			(T2.PROPOSAL_NUMBER = AV_MODULEITEM_KEY AND T2.COI_PROJECT_TYPE_CODE = AV_MODULE_CODE)
			OR 
			(T3.PROPOSAL_NUMBER = AV_MODULEITEM_KEY AND T3.COI_PROJECT_TYPE_CODE = AV_MODULE_CODE)
		)
		AND 
		(
			T1.KEY_PERSON_DISCL_REQUIREMENT = 'ALL'
			OR 
			(T1.KEY_PERSON_DISCL_REQUIREMENT = 'PI-COI' 
			AND 
			(
				T2.KEY_PERSON_ROLE_CODE = AV_PERSON_ROLE_CODE 
				OR T3.KEY_PERSON_ROLE_CODE = AV_PERSON_ROLE_CODE
			))
		);

	IF LI_COUNT > 0 THEN
		return 1;
	END IF;

	SELECT count(*) INTO LI_COUNT 
    FROM coi_project_proposal_qnr_ans_v T1
    WHERE T1.PROPOSAL_NUMBER = AV_MODULEITEM_KEY
    AND T1.KEY_PERSON_ID =  AV_PERSON_ID; 
    
    IF LI_COUNT > 0 THEN
		return -1;
	END IF;

RETURN 0;
END
//