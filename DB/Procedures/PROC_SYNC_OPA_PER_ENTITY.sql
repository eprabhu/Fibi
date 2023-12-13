DELIMITER //
CREATE PROCEDURE `PROC_SYNC_OPA_PER_ENTITY`(IN AV_OPA_DISCLOSURE_ID INT, IN AV_UPDATE_USER VARCHAR(60))
BEGIN
    DECLARE LS_PERSON_ID VARCHAR(20);
    DECLARE LS_OPA_STATUS VARCHAR(3);
    
    SELECT PERSON_ID, OPA_DISCLOSURE_STATUS_CODE
	INTO LS_PERSON_ID, LS_OPA_STATUS
    FROM OPA_DISCLOSURE
    WHERE OPA_DISCLOSURE_ID = AV_OPA_DISCLOSURE_ID;
	
	-- Sync Primary consition
    IF FIND_IN_SET(LS_OPA_STATUS, '1,5,6') < 1 THEN
        SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'Status code is not In Progress state. Exiting procedure.';
    END IF;



    -- Insert missing rows into OPA_DISCL_PERSON_ENTITY from PERSON_ENTITY
    INSERT INTO OPA_DISCL_PERSON_ENTITY (OPA_DISCLOSURE_ID, PERSON_ENTITY_ID, PERSON_ENTITY_NUMBER, ENTITY_ID, ENTITY_NUMBER,UPDATE_TIMESTAMP, UPDATE_USER)
	 SELECT
        AV_OPA_DISCLOSURE_ID,
        T.PERSON_ENTITY_ID,
		T.PERSON_ENTITY_NUMBER,
        T.ENTITY_ID,
        T.ENTITY_NUMBER,
        NOW(),
        AV_UPDATE_USER
	
	FROM
	(
		
		SELECT DISTINCT
			t0.PERSON_ENTITY_ID,
			t0.ENTITY_ID,
			t0.ENTITY_NUMBER,
			t0.PERSON_ENTITY_NUMBER
		FROM PERSON_ENTITY t0
		INNER JOIN person_entity_relationship t1 ON t1.PERSON_ENTITY_ID = t0.PERSON_ENTITY_ID
		INNER JOIN VALID_PERSON_ENTITY_REL_TYPE t2 ON t1.VALID_PERS_ENTITY_REL_TYP_CODE = t2.VALID_PERS_ENTITY_REL_TYP_CODE
		WHERE t2.RELATIONSHIP_TYPE_CODE IN ('1' ,'5')  -- Self, Commitment
		AND t0.VERSION_STATUS = 'ACTIVE'
		AND t0.IS_RELATIONSHIP_ACTIVE = 'Y'
		AND t0.PERSON_ID = LS_PERSON_ID
		AND t0.PERSON_ENTITY_NUMBER NOT IN (
											SELECT s1.PERSON_ENTITY_NUMBER 
											FROM OPA_DISCL_PERSON_ENTITY s1													
											WHERE s1.OPA_DISCLOSURE_ID = AV_OPA_DISCLOSURE_ID
									   )
	) T;
                                              
	
	
	
	-- loading Comp & Uncomp data default by commitment entities
	INSERT INTO OPA_DISCL_ACTIVITIES(OPA_DISCLOSURE_ID,OPA_DISCL_PERSON_ENTITY_ID,IS_COMPENSATED,NUM_OF_DAYS_SUMMER,NUM_OF_DAYS_ACADEMIC,NUM_OF_DAYS_IN_YEAR,NATURE_OF_WORK,UPDATE_TIMESTAMP,UPDATE_USER)
	
	SELECT distinct AV_OPA_DISCLOSURE_ID,t.OPA_DISCL_PERSON_ENTITY_ID,'Y',NULL,NULL,NULL,
	CONCAT(t0.STUDENT_INVOLVEMENT,t0.INSTITUTE_RESOURCE_INVOLVEMENT, t0.STAFF_INVOLVEMENT) AS NATURE_OF_WORK,
	NOW(),AV_UPDATE_USER	
    FROM OPA_DISCL_PERSON_ENTITY t
    INNER JOIN person_entity t0 ON t.PERSON_ENTITY_ID = t0.PERSON_ENTITY_ID
    INNER JOIN person_entity_relationship t1 ON t1.PERSON_ENTITY_ID = t0.PERSON_ENTITY_ID
    INNER JOIN VALID_PERSON_ENTITY_REL_TYPE t2 ON t1.VALID_PERS_ENTITY_REL_TYP_CODE = t2.VALID_PERS_ENTITY_REL_TYP_CODE
    WHERE t2.RELATIONSHIP_TYPE_CODE = '5' -- commitment
    AND t.OPA_DISCLOSURE_ID = AV_OPA_DISCLOSURE_ID
    AND t.OPA_DISCL_PERSON_ENTITY_ID NOT IN (
											SELECT OPA_DISCL_PERSON_ENTITY_ID	
                                            FROM OPA_DISCL_ACTIVITIES 
                                            WHERE OPA_DISCLOSURE_ID = AV_OPA_DISCLOSURE_ID
									);


	-- loading Outside financial interest and relationships data default by commitment and Self entities
	INSERT INTO OPA_OUTSIDE_FIN_INTERESTS(OPA_DISCLOSURE_ID,OPA_DISCL_PERSON_ENTITY_ID,PERSON_REL_WITH_ENTITY,ENTITY_REL_WITH_INSTITUTE,UPDATE_TIMESTAMP,UPDATE_USER)
	
	SELECT distinct AV_OPA_DISCLOSURE_ID,t.OPA_DISCL_PERSON_ENTITY_ID,NULL,NULL,NOW(),AV_UPDATE_USER	
    FROM OPA_DISCL_PERSON_ENTITY t
    INNER JOIN person_entity t0 ON t.PERSON_ENTITY_ID = t0.PERSON_ENTITY_ID
    INNER JOIN person_entity_relationship t1 ON t1.PERSON_ENTITY_ID = t0.PERSON_ENTITY_ID
    INNER JOIN VALID_PERSON_ENTITY_REL_TYPE t2 ON t1.VALID_PERS_ENTITY_REL_TYP_CODE = t2.VALID_PERS_ENTITY_REL_TYP_CODE
    WHERE t2.RELATIONSHIP_TYPE_CODE IN ('1' ,'5') -- commitment , Self
    AND t.OPA_DISCLOSURE_ID = AV_OPA_DISCLOSURE_ID
    AND t.OPA_DISCL_PERSON_ENTITY_ID NOT IN (
											SELECT OPA_DISCL_PERSON_ENTITY_ID	
                                            FROM OPA_OUTSIDE_FIN_INTERESTS 
                                            WHERE OPA_DISCLOSURE_ID = AV_OPA_DISCLOSURE_ID
									);
		
	-- Update the PERSON_ENTITY_ID , if there is any change in the SFI
	
	UPDATE opa_discl_person_entity s0
	JOIN (
	SELECT 
		t0.PERSON_ENTITY_NUMBER,
		t0.PERSON_ENTITY_ID,
		t0.ENTITY_ID
	FROM person_entity t0
	WHERE t0.VERSION_STATUS = 'ACTIVE'
	AND t0.IS_RELATIONSHIP_ACTIVE = 'Y'
	AND t0.PERSON_ID = LS_PERSON_ID
	GROUP BY t0.PERSON_ENTITY_NUMBER
	) AS s1
	ON s0.PERSON_ENTITY_NUMBER = s1.PERSON_ENTITY_NUMBER 

	SET s0.PERSON_ENTITY_ID = s1.PERSON_ENTITY_ID, s0.ENTITY_ID = s1.ENTITY_ID
	WHERE s0.OPA_DISCLOSURE_ID = AV_OPA_DISCLOSURE_ID
	AND	s0.PERSON_ENTITY_ID <> s1.PERSON_ENTITY_ID;								
	
	 
END
//
