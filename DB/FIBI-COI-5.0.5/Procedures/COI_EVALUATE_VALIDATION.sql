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
  DECLARE LS_SFI_LIST VARCHAR(2000);
  DECLARE LS_PROJ_SFI_LIST LONGTEXT;
  DECLARE LI_LOCK_ACQUIRED INT DEFAULT 0;

  START TRANSACTION;
  SELECT GET_LOCK('coi_sfi_projects_lock', 30) INTO LI_LOCK_ACQUIRED;
  IF LI_LOCK_ACQUIRED = 1 THEN

  /*
      VALIDATION_MSG_TYPE
        QM : questionnaire error message
        RS : unrevised SFI
        PS : project SFI(unparalleled SFI/unmarked conflicts on project relation )
        US : unsynced SFI

      VALIDATION_TYPE
        VE : Validation error
        VW : Validation warning
  */


  -- Temporary table to store the error messages
  -- CREATE TEMPORARY TABLE IF NOT EXISTS ERROR_MESSAGE (MESSAGE VARCHAR(2000));
  CREATE TEMPORARY TABLE IF NOT EXISTS ERROR_MESSAGE (
    VALIDATION_TYPE VARCHAR(3),
    VALIDATION_MSG_TYPE VARCHAR(3),
    MESSAGE VARCHAR(2000),
    SFIs VARCHAR(2000),
    PROJ_SFI_DETAILS LONGTEXT
);
  
  SET LI_QUESTIONNAIRE_FLAG = FN_EVAL_DISCLOSURE_QUESTIONNAIRE(8, 0, AV_DISCLOSURE_ID);

  IF LI_QUESTIONNAIRE_FLAG = 1 THEN
  
		-- If questionnaire evaluates to true, then check if SFI is added
		SELECT COUNT(*) > 0 INTO LI_SFI_FLAG 
		FROM PERSON_ENTITY 
		WHERE PERSON_ID = AV_PERSON_ID
		AND VERSION_STATUS = 'ACTIVE';

		IF NOT LI_SFI_FLAG THEN
    
			  SET LS_ERROR_MSG = "You have not added any SFI(s). Based on your screening questionnaire, you have to add at least one SFI to continue with disclosure.";
			  
			  INSERT INTO ERROR_MESSAGE(VALIDATION_TYPE, VALIDATION_MSG_TYPE, MESSAGE) VALUES ('VE', 'QM', LS_ERROR_MSG);
      
		END IF;
    
  END IF;
  
  SET LI_SFI_FLAG = 0;
  
  -- If any SFI is present, check if relationship is defined
	  SELECT COUNT(*) = 0 INTO LI_RELATIONSHIP_FLAG
	  FROM COI_DISCL_PROJECT_ENTITY_REL T1
	  INNER JOIN COI_DISCL_PROJECTS T4 ON T4.COI_DISCL_PROJECTS_ID = T1.COI_DISCL_PROJECTS_ID
	  INNER JOIN PERSON_ENTITY_RELATIONSHIP T2 ON T1.PERSON_ENTITY_ID = T2.PERSON_ENTITY_ID
      INNER JOIN VALID_PERSON_ENTITY_REL_TYPE T3 ON T2.VALID_PERS_ENTITY_REL_TYP_CODE = T3.VALID_PERS_ENTITY_REL_TYP_CODE
	  WHERE T4.DISCLOSURE_ID = AV_DISCLOSURE_ID
	  AND T1.PROJECT_CONFLICT_STATUS_CODE IS NULL
	  AND T1.PERSON_ENTITY_ID IS NOT NULL
	  AND T3.DISCLOSURE_TYPE_CODE = 1;

  IF NOT LI_RELATIONSHIP_FLAG THEN

		SET LS_SFI_LIST = '';

		INSERT INTO ERROR_MESSAGE (VALIDATION_TYPE, VALIDATION_MSG_TYPE, MESSAGE, PROJ_SFI_DETAILS)
		SELECT DISTINCT 'VE' AS VALIDATION_TYPE, 'PS' AS VALIDATION_MSG_TYPE, 'You have undefined Project-SFI relationships. Kindly complete the Relationships section to certify the disclosure.' AS LS_ERROR_MSG,
                CONCAT(
					'DisclDetailId:: ', T1.COI_DISCL_PROJECT_ENTITY_REL_ID, '||', 'Title:: ', COALESCE(T2.TITLE, T3.TITLE, T6.TITLE), '||', 'Entity:: ', T4.ENTITY_NAME, '||',
                    'ModuleCode:: ', COALESCE(T2.COI_PROJECT_TYPE_CODE, T3.COI_PROJECT_TYPE_CODE, T6.COI_PROJECT_TYPE_CODE), '||',
                    'ModuleItemKey:: ', COALESCE(T2.EXTERNAL_SYSTEM_REF_ID, T3.EXTERNAL_SYSTEM_REF_ID, T6.EXTERNAL_SYSTEM_REF_ID))
                    as LS_PROJ_SFI_LIST
		FROM COI_DISCL_PROJECT_ENTITY_REL T1
		INNER JOIN COI_DISCL_PROJECTS T7 ON T7.COI_DISCL_PROJECTS_ID = T1.COI_DISCL_PROJECTS_ID
        INNER JOIN PERSON_ENTITY_RELATIONSHIP T5 ON T5.PERSON_ENTITY_ID = T1.PERSON_ENTITY_ID AND T5.VALID_PERS_ENTITY_REL_TYP_CODE IN (1,2,3)
		LEFT JOIN COI_PROJECT_AWARD_V T2 ON T2.EXTERNAL_SYSTEM_REF_ID = T7.MODULE_ITEM_KEY AND T7.MODULE_CODE = 1
		LEFT JOIN COI_PROJECT_PROPOSAL_V T3 ON T3.EXTERNAL_SYSTEM_REF_ID = T7.MODULE_ITEM_KEY AND T7.MODULE_CODE = 3
		LEFT JOIN coi_project_institute_proposal_v T6 ON T6.EXTERNAL_SYSTEM_REF_ID = T7.MODULE_ITEM_KEY AND T7.MODULE_CODE = 2
		INNER JOIN ENTITY T4 ON T4.ENTITY_ID = T1.ENTITY_ID
		WHERE T7.DISCLOSURE_ID = AV_DISCLOSURE_ID
		AND T1.PROJECT_CONFLICT_STATUS_CODE IS NULL
		AND T1.PERSON_ENTITY_ID IS NOT NULL;

		SET LS_ERROR_MSG = "You have undefined Project-SFI relationships. Kindly complete the Relationships section to certify the disclosure.";

  END IF;
-- Unrevised SFI check
   SELECT COUNT(*) > 0 INTO LI_SFI_FLAG
		FROM COI_DISCL_PROJECT_ENTITY_REL t1
		INNER JOIN COI_DISCL_PROJECTS t4 ON t4.COI_DISCL_PROJECTS_ID = t1.COI_DISCL_PROJECTS_ID
		INNER JOIN PERSON_ENTITY t2 ON t2.PERSON_ENTITY_ID = t1.PERSON_ENTITY_ID
    INNER JOIN ENTITY t3 ON t3.ENTITY_NUMBER = t2.ENTITY_NUMBER
		WHERE t4.DISCLOSURE_ID = AV_DISCLOSURE_ID AND t2.ENTITY_ID != t3.ENTITY_ID AND t3.IS_ACTIVE IN ('N', 'Y')
    AND t3.VERSION_STATUS = 'ACTIVE';

	IF LI_SFI_FLAG THEN

			SET LS_PROJ_SFI_LIST = '';

			SELECT GROUP_CONCAT(DISTINCT CONCAT(t2.PERSON_ENTITY_ID, '||', t4.ENTITY_NAME) SEPARATOR ':;:') INTO LS_PROJ_SFI_LIST
			FROM COI_DISCL_PROJECT_ENTITY_REL t1
			INNER JOIN COI_DISCL_PROJECTS t5 ON t5.COI_DISCL_PROJECTS_ID = t1.COI_DISCL_PROJECTS_ID
			INNER JOIN PERSON_ENTITY t2 ON t2.PERSON_ENTITY_ID = t1.PERSON_ENTITY_ID
			INNER JOIN ENTITY t3 ON t3.ENTITY_NUMBER = t2.ENTITY_NUMBER
            INNER JOIN ENTITY t4 ON t4.ENTITY_ID = t1.ENTITY_ID
			WHERE t5.DISCLOSURE_ID = AV_DISCLOSURE_ID AND t2.ENTITY_ID != t3.ENTITY_ID AND t3.IS_ACTIVE IN ('N', 'Y') AND t3.VERSION_STATUS = 'ACTIVE';

			SET LS_ERROR_MSG = "You have unrevised SFI (s) in your SFI list for recently modified Entity (s). Kindly revise the SFI or inactivate the SFI to proceed and certify the disclosure.";
            INSERT INTO ERROR_MESSAGE (VALIDATION_TYPE, VALIDATION_MSG_TYPE, MESSAGE, SFIs) VALUES ('VE','RS', LS_ERROR_MSG, LS_PROJ_SFI_LIST);

	END IF;

	--  SFIs are changed but not synced check

    /* INSERT INTO ERROR_MESSAGE (VALIDATION_TYPE, VALIDATION_MSG_TYPE, MESSAGE, PROJ_SFI_DETAILS) (SELECT DISTINCT 'VE' AS VALIDATION_TYPE, 'PS' AS VALIDATION_MSG_TYPE,  "Your SFIs are changed. Kindly make necessary changes" AS LS_ERROR_MSG,
        CONCAT('DisclDetailId:: ', T1.DISCLOSURE_DETAILS_ID, '||', 'Title:: ', COALESCE(T5.TITLE, T6.TITLE, T8.TITLE), '||', 'Entity:: ', T7.ENTITY_NAME, '||',
        'ModuleCode:: ', COALESCE(T5.COI_PROJECT_TYPE_CODE, T6.COI_PROJECT_TYPE_CODE, T8.COI_PROJECT_TYPE_CODE), '||', 'ModuleItemKey:: ', COALESCE(T5.EXTERNAL_SYSTEM_REF_ID, T6.EXTERNAL_SYSTEM_REF_ID, T8.EXTERNAL_SYSTEM_REF_ID
        )) as LS_PROJ_SFI_LIST
        FROM COI_DISCL_ENT_PROJ_DETAILS T1
        INNER JOIN PERSON_ENTITY T2 ON T2.PERSON_ENTITY_NUMBER = T1.PERSON_ENTITY_NUMBER
        INNER JOIN PERSON_ENTITY_RELATIONSHIP T3 ON T3.PERSON_ENTITY_ID = T2.PERSON_ENTITY_ID
        INNER JOIN VALID_PERSON_ENTITY_REL_TYPE T4 ON T4.VALID_PERS_ENTITY_REL_TYP_CODE = T3.VALID_PERS_ENTITY_REL_TYP_CODE
        LEFT JOIN COI_PROJECT_AWARD_V T5 ON T5.EXTERNAL_SYSTEM_REF_ID = T1.MODULE_ITEM_KEY AND T1.MODULE_CODE = 1
        LEFT JOIN COI_PROJECT_PROPOSAL_V T6 ON T6.EXTERNAL_SYSTEM_REF_ID = T1.MODULE_ITEM_KEY AND T1.MODULE_CODE = 3
        LEFT JOIN coi_project_institute_proposal_v T8 ON T8.EXTERNAL_SYSTEM_REF_ID = T1.MODULE_ITEM_KEY AND T1.MODULE_CODE = 2
        INNER JOIN ENTITY T7 ON T7.ENTITY_ID = T1.ENTITY_ID
        WHERE T2.VERSION_STATUS='ACTIVE' AND T4.DISCLOSURE_TYPE_CODE = '1' AND T1.PERSON_ENTITY_ID != T2.PERSON_ENTITY_ID
        AND T1.DISCLOSURE_ID = AV_DISCLOSURE_ID); */

   -- check SFIs were altered or modified

   INSERT INTO ERROR_MESSAGE (VALIDATION_TYPE, VALIDATION_MSG_TYPE, MESSAGE, PROJ_SFI_DETAILS) (SELECT DISTINCT 'VW' AS VALIDATION_TYPE, 'PS' AS VALIDATION_MSG_TYPE,  "Your SFIs were altered or modified. If required, please make necessary changes." AS LS_ERROR_MSG,
           CONCAT('DisclDetailId:: ', T1.COI_DISCL_PROJECT_ENTITY_REL_ID, '||', 'Title:: ', COALESCE(T5.TITLE, T6.TITLE, T8.TITLE), '||', 'Entity:: ', T7.ENTITY_NAME, '||',
           'ModuleCode:: ', COALESCE(T5.COI_PROJECT_TYPE_CODE, T6.COI_PROJECT_TYPE_CODE, T8.COI_PROJECT_TYPE_CODE), '||', 'ModuleItemKey:: ', COALESCE(T5.EXTERNAL_SYSTEM_REF_ID, T6.EXTERNAL_SYSTEM_REF_ID, T8.EXTERNAL_SYSTEM_REF_ID)) as LS_PROJ_SFI_LIST
           FROM COI_DISCL_PROJECT_ENTITY_REL T1
		   INNER JOIN COI_DISCL_PROJECTS T9 ON T9.COI_DISCL_PROJECTS_ID = T1.COI_DISCL_PROJECTS_ID
           INNER JOIN PERSON_ENTITY T2 ON T2.PERSON_ENTITY_NUMBER = T1.PERSON_ENTITY_NUMBER
           LEFT JOIN COI_PROJECT_AWARD_V T5 ON T5.EXTERNAL_SYSTEM_REF_ID = T9.MODULE_ITEM_KEY AND T9.MODULE_CODE = 1
           LEFT JOIN COI_PROJECT_PROPOSAL_V T6 ON T6.EXTERNAL_SYSTEM_REF_ID = T9.MODULE_ITEM_KEY AND T9.MODULE_CODE = 3
           LEFT JOIN coi_project_institute_proposal_v T8 ON T8.EXTERNAL_SYSTEM_REF_ID = T9.MODULE_ITEM_KEY AND T9.MODULE_CODE = 2
           INNER JOIN ENTITY T7 ON T7.ENTITY_ID = T1.ENTITY_ID
           WHERE  (T1.PREVIOUS_PERSON_ENTITY_ID IS NOT NULL AND T1.PERSON_ENTITY_ID != T1.PREVIOUS_PERSON_ENTITY_ID ) AND T9.DISCLOSURE_ID = AV_DISCLOSURE_ID);

    -- not synced active sfi check
   SELECT COUNT(T2.PERSON_ENTITY_ID) > 0  INTO LI_SFI_FLAG
			FROM COI_DISCL_PROJECT_ENTITY_REL T1
			INNER JOIN COI_DISCL_PROJECTS T4 ON T4.COI_DISCL_PROJECTS_ID = T1.COI_DISCL_PROJECTS_ID
           INNER JOIN COI_DISCLOSURE T0 ON T0.DISCLOSURE_ID = T4.DISCLOSURE_ID
           RIGHT JOIN (SELECT t1.PERSON_ENTITY_ID, t1.PERSON_ENTITY_NUMBER, t1.ENTITY_ID, t1.ENTITY_NUMBER FROM PERSON_ENTITY t1
           INNER JOIN PERSON_ENTITY_RELATIONSHIP t2 ON t2.PERSON_ENTITY_ID = t1.PERSON_ENTITY_ID
           INNER JOIN VALID_PERSON_ENTITY_REL_TYPE t3 ON t3.VALID_PERS_ENTITY_REL_TYP_CODE = t2.VALID_PERS_ENTITY_REL_TYP_CODE
           WHERE t1.PERSON_ENTITY_NUMBER NOT IN (
				SELECT DISTINCT t5.PERSON_ENTITY_NUMBER FROM COI_DISCL_PROJECT_ENTITY_REL t5
				INNER JOIN COI_DISCL_PROJECTS t6 ON t6.COI_DISCL_PROJECTS_ID = t5.COI_DISCL_PROJECTS_ID
				WHERE t6.DISCLOSURE_ID = AV_DISCLOSURE_ID AND t5.PERSON_ENTITY_NUMBER IS NOT NULL)
           AND t1.VERSION_STATUS='ACTIVE' AND t3.DISCLOSURE_TYPE_CODE = '1' AND t1.PERSON_ID = AV_PERSON_ID) T2
           ON (T1.PERSON_ENTITY_NUMBER IS NULL OR T1.PERSON_ENTITY_NUMBER != T2.PERSON_ENTITY_NUMBER)
           INNER JOIN ENTITY T3 ON T3.ENTITY_ID = T2.ENTITY_ID
           WHERE T4.DISCLOSURE_ID = AV_DISCLOSURE_ID AND T0.REVIEW_STATUS_CODE IN (1,5,6) ;
   IF LI_SFI_FLAG THEN
        INSERT INTO ERROR_MESSAGE (VALIDATION_TYPE, VALIDATION_MSG_TYPE, MESSAGE, SFIs) (SELECT DISTINCT 'VE' AS VALIDATION_TYPE, 'US' AS VALIDATION_MSG_TYPE, "You have Active SFI (s) in your SFI list against Inactive Entity (s). Kindly inactivate/sync the SFI to proceed and certify the disclosure." AS LS_ERROR_MSG,
            GROUP_CONCAT(DISTINCT CONCAT(T2.PERSON_ENTITY_ID, '||', T3.ENTITY_NAME) SEPARATOR ':;:') INTO LS_SFI_LIST
            FROM COI_DISCL_PROJECT_ENTITY_REL T1
			INNER JOIN COI_DISCL_PROJECTS T4 ON T4.COI_DISCL_PROJECTS_ID = T1.COI_DISCL_PROJECTS_ID
            INNER JOIN COI_DISCLOSURE T0 ON T0.DISCLOSURE_ID = T4.DISCLOSURE_ID
            RIGHT JOIN (SELECT t1.PERSON_ENTITY_ID, t1.PERSON_ENTITY_NUMBER, t1.ENTITY_ID, t1.ENTITY_NUMBER FROM PERSON_ENTITY t1
            INNER JOIN PERSON_ENTITY_RELATIONSHIP t2 ON t2.PERSON_ENTITY_ID = t1.PERSON_ENTITY_ID
            INNER JOIN VALID_PERSON_ENTITY_REL_TYPE t3 ON t3.VALID_PERS_ENTITY_REL_TYP_CODE = t2.VALID_PERS_ENTITY_REL_TYP_CODE
            WHERE t1.PERSON_ENTITY_NUMBER NOT IN (SELECT DISTINCT t5.PERSON_ENTITY_NUMBER FROM COI_DISCL_PROJECT_ENTITY_REL t5
				INNER JOIN COI_DISCL_PROJECTS t6 ON t6.COI_DISCL_PROJECTS_ID = t5.COI_DISCL_PROJECTS_ID
				WHERE t6.DISCLOSURE_ID = AV_DISCLOSURE_ID AND t5.PERSON_ENTITY_NUMBER IS NOT NULL)
            AND t1.VERSION_STATUS='ACTIVE' AND t3.DISCLOSURE_TYPE_CODE = '1' AND t1.PERSON_ID = AV_PERSON_ID) T2
            ON (T1.PERSON_ENTITY_NUMBER IS NULL OR T1.PERSON_ENTITY_NUMBER != T2.PERSON_ENTITY_NUMBER)
            INNER JOIN ENTITY T3 ON T3.ENTITY_ID = T2.ENTITY_ID
            WHERE T4.DISCLOSURE_ID = AV_DISCLOSURE_ID AND T0.REVIEW_STATUS_CODE IN (1,5,6) );
   END IF;

   -- incomplete sfi check

   SELECT COUNT(T2.PERSON_ENTITY_ID) > 0  INTO LI_SFI_FLAG
		  FROM COI_DISCL_PROJECT_ENTITY_REL T1
		  INNER JOIN COI_DISCL_PROJECTS T8 ON T8.COI_DISCL_PROJECTS_ID = T1.COI_DISCL_PROJECTS_ID
          INNER JOIN PERSON_ENTITY T2 ON T2.PERSON_ENTITY_ID = T1.PERSON_ENTITY_ID
          WHERE  T2.IS_FORM_COMPLETED = 'N'  AND T8.DISCLOSURE_ID = AV_DISCLOSURE_ID;

   IF LI_SFI_FLAG THEN
        INSERT INTO ERROR_MESSAGE (VALIDATION_TYPE, VALIDATION_MSG_TYPE, MESSAGE, SFIs) (SELECT DISTINCT 'VE' AS VALIDATION_TYPE, 'INS' AS VALIDATION_MSG_TYPE,  "You have incomplete SFIs, please make necessary changes and complete." AS LS_ERROR_MSG,
                   GROUP_CONCAT(DISTINCT CONCAT(T1.PERSON_ENTITY_ID, '||', T3.ENTITY_NAME) SEPARATOR ':;:') INTO LS_SFI_LIST
                   FROM COI_DISCL_PROJECT_ENTITY_REL T1
				   INNER JOIN COI_DISCL_PROJECTS T8 ON T8.COI_DISCL_PROJECTS_ID = T1.COI_DISCL_PROJECTS_ID
                   INNER JOIN PERSON_ENTITY T2 ON T2.PERSON_ENTITY_ID = T1.PERSON_ENTITY_ID
                   INNER JOIN ENTITY T3 ON T3.ENTITY_ID = T2.ENTITY_ID
                   WHERE  T2.IS_FORM_COMPLETED = 'N'  AND T8.DISCLOSURE_ID = AV_DISCLOSURE_ID);
   END IF;
  /* SET LI_SFI_FLAG = 0;

  SELECT COUNT(*) > 0 INTO LI_SFI_FLAG 
		FROM COI_DISCL_ENT_PROJ_DETAILS t1 INNER JOIN PERSON_ENTITY t2 ON t2.PERSON_ENTITY_ID = t1.PERSON_ENTITY_ID 
    INNER JOIN ENTITY t3 ON t3.ENTITY_NUMBER = t2.ENTITY_NUMBER 
		WHERE t1.DISCLOSURE_ID = AV_DISCLOSURE_ID AND t2.ENTITY_ID = t3.ENTITY_ID AND t3.IS_ACTIVE = 'N' AND t3.VERSION_STATUS = 'ACTIVE';

	IF LI_SFI_FLAG THEN
    
			SET LS_SFI_LIST = '';
    
			SELECT GROUP_CONCAT(DISTINCT CONCAT(t2.PERSON_ENTITY_ID, '||', t3.ENTITY_NAME) SEPARATOR ':;:') INTO LS_SFI_LIST
			FROM COI_DISCL_ENT_PROJ_DETAILS t1 
			INNER JOIN PERSON_ENTITY t2 ON t2.PERSON_ENTITY_ID = t1.PERSON_ENTITY_ID 
			INNER JOIN ENTITY t3 ON t3.ENTITY_NUMBER = t2.ENTITY_NUMBER 
			WHERE t1.DISCLOSURE_ID = AV_DISCLOSURE_ID AND t2.ENTITY_ID = t3.ENTITY_ID AND t3.IS_ACTIVE = 'N' AND t3.VERSION_STATUS = 'ACTIVE';
    
			SET LS_ERROR_MSG = "You have Active SFI (s) in your SFI list against Inactive Entity (s). Kindly inactivate the SFI to proceed and certify the disclosure.";
			INSERT INTO ERROR_MESSAGE (MESSAGE, SFIs) VALUES (LS_ERROR_MSG, LS_SFI_LIST);
            
	END IF; 
    */

  SELECT VALIDATION_TYPE, VALIDATION_MSG_TYPE, MESSAGE, SFIs, PROJ_SFI_DETAILS FROM ERROR_MESSAGE;
  
  DROP TEMPORARY TABLE IF EXISTS ERROR_MESSAGE;
 COMMIT;
 SET LI_LOCK_ACQUIRED = RELEASE_LOCK('coi_sfi_projects_lock');
 	SELECT '1';
 ELSE
         SELECT 'Unable to acquire lock. Another process may sync the projects vs entity.';

 END IF;
END
//
