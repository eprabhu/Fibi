DELIMITER //
CREATE PROCEDURE `SYNC_SFIS_AND_DISCLOSURE`(
AV_DISCLOSURE_ID   	     INT(30),
AV_DISCLOSURE_NUMBER   	 INT(30),
AV_PERSON_ID             VARCHAR(45),
AV_UPDATE_USER           VARCHAR(45),
AV_PERSON_ENTITY_ID      VARCHAR(30),
AV_MODULE_CODE           INT(3),
AV_MODULE_ITEM_KEY       VARCHAR(30),
AV_TYPE                  VARCHAR(3)
)
BEGIN

/**
    AV_TYPE = F   // Create FCOI
        AV_TYPE = P   // Project Disclosure
    AV_TYPE = SD  // Sync SFIs with Disclosure
*/

DECLARE LI_PERSON_ENTITY_ID     VARCHAR(30);
DECLARE LI_ENTITY_ID            VARCHAR(30);
DECLARE LI_ENTITY_NUMBER        VARCHAR(30);
DECLARE LI_MODULE_CODE          VARCHAR(30);
DECLARE LI_MODULE_ITEM_KEY      VARCHAR(30);
DECLARE LI_IS_VALUE             INT DEFAULT 0;
DECLARE LI_DISCLOSURE_ID   	    INT(30);
DECLARE LI_DISCLOSURE_NUMBER   	INT(30);
DECLARE LI_PERSON_ID            VARCHAR(40);
DECLARE LI_PERSON_ENTITY_NUMBER     VARCHAR(30);
DECLARE LI_COUNT   	            INT(30);
DECLARE LI_DISCLOSURE_STATUS    INT(3);
DECLARE done BOOLEAN DEFAULT FALSE;
DECLARE LI_LOCK_ACQUIRED INT DEFAULT 0;

START TRANSACTION;
SELECT GET_LOCK('coi_sfi_projects_lock', 30) INTO LI_LOCK_ACQUIRED;
IF LI_LOCK_ACQUIRED = 1 THEN

    BEGIN 
        /**
            For FCOI creation this check has been done. AlL the active SFIs against a person will sync with the proposals and awards
        */
        IF AV_TYPE = 'F' AND AV_DISCLOSURE_ID IS NOT NULL THEN
            BEGIN 
                DECLARE DONE1 INT DEFAULT FALSE;
                                            
                DECLARE CUR_ENTITIES CURSOR FOR SELECT DISTINCT T1.PERSON_ENTITY_ID, T1.PERSON_ENTITY_NUMBER, T1.ENTITY_ID, T1.ENTITY_NUMBER FROM PERSON_ENTITY T1
                INNER JOIN PERSON_ENTITY_RELATIONSHIP T2 ON T2.PERSON_ENTITY_ID = T1.PERSON_ENTITY_ID
                INNER JOIN VALID_PERSON_ENTITY_REL_TYPE T3 ON T3.VALID_PERS_ENTITY_REL_TYP_CODE = T2.VALID_PERS_ENTITY_REL_TYP_CODE
                    WHERE T1.VERSION_STATUS='ACTIVE'
                    AND T3.DISCLOSURE_TYPE_CODE = '1' AND T1.PERSON_ID = AV_PERSON_ID;

                DECLARE CONTINUE HANDLER FOR NOT FOUND SET DONE1 = TRUE;
                                
                OPEN CUR_ENTITIES;
                ENTITIES_LOOP: LOOP 
                FETCH CUR_ENTITIES INTO  LI_PERSON_ENTITY_ID, LI_PERSON_ENTITY_NUMBER, LI_ENTITY_ID, LI_ENTITY_NUMBER;

                    IF DONE1 THEN
                        IF LI_IS_VALUE = 0 THEN
                            
                            INSERT INTO COI_DISCL_ENT_PROJ_DETAILS(`DISCLOSURE_ID`, `DISCLOSURE_NUMBER`, `PERSON_ENTITY_ID`, `ENTITY_ID`, 
										`ENTITY_NUMBER`, `MODULE_CODE`, `MODULE_ITEM_KEY`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) 
										(SELECT AV_DISCLOSURE_ID, AV_DISCLOSURE_NUMBER, null, null, 
										null, 1, T1.EXTERNAL_SYSTEM_REF_ID,  now(), AV_UPDATE_USER FROM COI_PROJECT_AWARD_V T1 
										WHERE T1.AWARD_STATUS IN ('Active','Pending') and T1.KEY_PERSON_ID = AV_PERSON_ID);
										
					        /* INSERT INTO COI_DISCL_ENT_PROJ_DETAILS(`DISCLOSURE_ID`, `DISCLOSURE_NUMBER`, `PERSON_ENTITY_ID`, `ENTITY_ID`,
										`ENTITY_NUMBER`, `MODULE_CODE`, `MODULE_ITEM_KEY`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) 
										(SELECT AV_DISCLOSURE_ID, AV_DISCLOSURE_NUMBER, null, null, 
										null, 3, T1.EXTERNAL_SYSTEM_REF_ID,  now(), AV_UPDATE_USER FROM COI_PROJECT_PROPOSAL_V T1 WHERE T1.PROPOSAL_STATUS not IN ("In Progress","Unsuccessful","Inactive","Revision Requested",
										"ORT Director Review Completed","Pending Revisions By PI","Pending Revisions By PI","Not Submitted","Returned","Withdrawn","Awarded") 
										AND T1.KEY_PERSON_ID = AV_PERSON_ID); */

						    INSERT INTO COI_DISCL_ENT_PROJ_DETAILS(`DISCLOSURE_ID`, `DISCLOSURE_NUMBER`, `PERSON_ENTITY_ID`, `ENTITY_ID`,
                            										`ENTITY_NUMBER`, `MODULE_CODE`, `MODULE_ITEM_KEY`, `UPDATE_TIMESTAMP`, `UPDATE_USER`)
                            										(SELECT AV_DISCLOSURE_ID, AV_DISCLOSURE_NUMBER, null, null,
                            										null, 2, T1.EXTERNAL_SYSTEM_REF_ID,  now(), AV_UPDATE_USER FROM coi_project_institute_proposal_v T1 WHERE T1.KEY_PERSON_ID = AV_PERSON_ID);
                        END IF;
                        LEAVE ENTITIES_LOOP;
                    END IF;
                    SET LI_IS_VALUE = 1;
                    INSERT INTO COI_DISCL_ENT_PROJ_DETAILS(`DISCLOSURE_ID`, `DISCLOSURE_NUMBER`, `PERSON_ENTITY_ID`, `PERSON_ENTITY_NUMBER`, `ENTITY_ID`,
										`ENTITY_NUMBER`, `MODULE_CODE`, `MODULE_ITEM_KEY`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) 
										(SELECT AV_DISCLOSURE_ID, AV_DISCLOSURE_NUMBER, LI_PERSON_ENTITY_ID, LI_PERSON_ENTITY_NUMBER, LI_ENTITY_ID,
										LI_ENTITY_NUMBER, 1, T1.EXTERNAL_SYSTEM_REF_ID,  now(), AV_UPDATE_USER FROM COI_PROJECT_AWARD_V T1 
										WHERE T1.AWARD_STATUS IN ('Active','Pending') and T1.KEY_PERSON_ID = AV_PERSON_ID);
										
					/* INSERT INTO COI_DISCL_ENT_PROJ_DETAILS(`DISCLOSURE_ID`, `DISCLOSURE_NUMBER`, `PERSON_ENTITY_ID`, `PERSON_ENTITY_NUMBER`, `ENTITY_ID`,
										`ENTITY_NUMBER`, `MODULE_CODE`, `MODULE_ITEM_KEY`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) 
										(SELECT AV_DISCLOSURE_ID, AV_DISCLOSURE_NUMBER, LI_PERSON_ENTITY_ID, LI_PERSON_ENTITY_NUMBER, LI_ENTITY_ID,
										LI_ENTITY_NUMBER, 3, T1.EXTERNAL_SYSTEM_REF_ID,  now(), AV_UPDATE_USER FROM COI_PROJECT_PROPOSAL_V T1 WHERE T1.PROPOSAL_STATUS not IN ("In Progress","Unsuccessful","Inactive","Revision Requested",
										"ORT Director Review Completed","Pending Revisions By PI","Pending Revisions By PI","Not Submitted","Returned","Withdrawn","Awarded") 
										AND T1.KEY_PERSON_ID = AV_PERSON_ID); */

                    INSERT INTO COI_DISCL_ENT_PROJ_DETAILS(`DISCLOSURE_ID`, `DISCLOSURE_NUMBER`, `PERSON_ENTITY_ID`, `PERSON_ENTITY_NUMBER`, `ENTITY_ID`,
										`ENTITY_NUMBER`, `MODULE_CODE`, `MODULE_ITEM_KEY`, `UPDATE_TIMESTAMP`, `UPDATE_USER`)
										(SELECT AV_DISCLOSURE_ID, AV_DISCLOSURE_NUMBER, LI_PERSON_ENTITY_ID, LI_PERSON_ENTITY_NUMBER, LI_ENTITY_ID,
										LI_ENTITY_NUMBER, 2, T1.EXTERNAL_SYSTEM_REF_ID,  now(), AV_UPDATE_USER FROM coi_project_institute_proposal_v T1 WHERE T1.KEY_PERSON_ID = AV_PERSON_ID);
                END LOOP;
                CLOSE CUR_ENTITIES;
            END;

        /**
            This check is cretate new project disclosure, for project disclosre creation AV_MODULE_ITEM_KEY will not be null
        */
        ELSEIF AV_TYPE = 'P' AND AV_MODULE_ITEM_KEY IS NOT NULL AND AV_DISCLOSURE_ID IS NOT NULL THEN

            BEGIN 
                DECLARE DONE1 INT DEFAULT FALSE;
                                            
                DECLARE CUR_ENTITIES CURSOR FOR SELECT DISTINCT T1.PERSON_ENTITY_ID, T1.PERSON_ENTITY_NUMBER, T1.ENTITY_ID, T1.ENTITY_NUMBER FROM PERSON_ENTITY T1
                    INNER JOIN PERSON_ENTITY_RELATIONSHIP T2 ON T2.PERSON_ENTITY_ID = T1.PERSON_ENTITY_ID
                    INNER JOIN VALID_PERSON_ENTITY_REL_TYPE T3 ON T3.VALID_PERS_ENTITY_REL_TYP_CODE = T2.VALID_PERS_ENTITY_REL_TYP_CODE
                    WHERE T1.VERSION_STATUS='ACTIVE'
                    AND T3.DISCLOSURE_TYPE_CODE = '1' AND T1.PERSON_ID = AV_PERSON_ID;

                DECLARE CONTINUE HANDLER FOR NOT FOUND SET DONE1 = TRUE;
                                
                OPEN CUR_ENTITIES;
                ENTITIES_LOOP: LOOP 
                FETCH CUR_ENTITIES INTO  LI_PERSON_ENTITY_ID, LI_PERSON_ENTITY_NUMBER, LI_ENTITY_ID, LI_ENTITY_NUMBER;

                    IF DONE1 THEN
                        IF LI_IS_VALUE = 0 THEN
                            INSERT INTO COI_DISCL_ENT_PROJ_DETAILS(`DISCLOSURE_ID`, `DISCLOSURE_NUMBER`, `MODULE_CODE`, `MODULE_ITEM_KEY`, 
                                `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES(AV_DISCLOSURE_ID, AV_DISCLOSURE_NUMBER, AV_MODULE_CODE,
                                AV_MODULE_ITEM_KEY, now(), AV_UPDATE_USER);
                        END IF;
                        LEAVE ENTITIES_LOOP;
                    END IF;
                    SET LI_IS_VALUE = 1;
                    INSERT INTO COI_DISCL_ENT_PROJ_DETAILS(`DISCLOSURE_ID`, `DISCLOSURE_NUMBER`, `PERSON_ENTITY_ID`, `ENTITY_ID`, `ENTITY_NUMBER`, `MODULE_CODE`, `MODULE_ITEM_KEY`, 
                            `UPDATE_TIMESTAMP`, `UPDATE_USER`, `PERSON_ENTITY_NUMBER`) VALUES(AV_DISCLOSURE_ID, AV_DISCLOSURE_NUMBER, LI_PERSON_ENTITY_ID, LI_ENTITY_ID, LI_ENTITY_NUMBER, AV_MODULE_CODE,
                            AV_MODULE_ITEM_KEY, now(), AV_UPDATE_USER, LI_PERSON_ENTITY_NUMBER);
                END LOOP;
                CLOSE CUR_ENTITIES;
            END;

        ELSEIF AV_TYPE = 'SD' AND AV_DISCLOSURE_ID IS NOT NULL THEN
            BEGIN

                SELECT REVIEW_STATUS_CODE, PERSON_ID INTO LI_DISCLOSURE_STATUS, LI_PERSON_ID FROM COI_DISCLOSURE WHERE DISCLOSURE_ID = AV_DISCLOSURE_ID;

                IF LI_PERSON_ID != AV_PERSON_ID THEN
                    SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'Disclosure person id is not matching with attempting person id.';
                END IF;

                -- Sync Primary consition
                IF FIND_IN_SET(LI_DISCLOSURE_STATUS, '1,5,6') < 1 THEN
                    SIGNAL SQLSTATE '45000' SET MESSAGE_TEXT = 'Disclosure Status code is not In Progress/Withdrawn/Returned state. Exiting procedure.';
                END IF;

                CREATE TEMPORARY TABLE temp_project_details_table SELECT DISTINCT MODULE_ITEM_KEY, MODULE_CODE, DISCLOSURE_NUMBER, DISCLOSURE_ID
                FROM COI_DISCL_ENT_PROJ_DETAILS  WHERE DISCLOSURE_ID = AV_DISCLOSURE_ID;

                UPDATE COI_DISCL_ENT_PROJ_DETAILS AS T
                    INNER JOIN COI_DISCLOSURE T0 ON T0.DISCLOSURE_ID = T.DISCLOSURE_ID
                    JOIN(SELECT DISTINCT T1.PERSON_ENTITY_ID AS PREVIOUS_PERSON_ENTITY_ID,
                    T2.PERSON_ENTITY_ID, T1.PERSON_ENTITY_NUMBER FROM COI_DISCL_ENT_PROJ_DETAILS T1
                    INNER JOIN PERSON_ENTITY T2 ON T2.PERSON_ENTITY_NUMBER = T1.PERSON_ENTITY_NUMBER
                    INNER JOIN PERSON_ENTITY_RELATIONSHIP T3 ON T3.PERSON_ENTITY_ID = T2.PERSON_ENTITY_ID
                    INNER JOIN VALID_PERSON_ENTITY_REL_TYPE T4 ON T4.VALID_PERS_ENTITY_REL_TYP_CODE = T3.VALID_PERS_ENTITY_REL_TYP_CODE
                    WHERE T2.VERSION_STATUS='ACTIVE' AND T4.DISCLOSURE_TYPE_CODE = '1' AND T1.PERSON_ENTITY_ID != T2.PERSON_ENTITY_ID
                    AND T1.DISCLOSURE_ID = AV_DISCLOSURE_ID) AS T6 ON T6.PERSON_ENTITY_NUMBER = T.PERSON_ENTITY_NUMBER
                    SET T.PREVIOUS_PERSON_ENTITY_ID = T6.PREVIOUS_PERSON_ENTITY_ID, T.PERSON_ENTITY_ID = T6.PERSON_ENTITY_ID
                    WHERE T.PERSON_ENTITY_NUMBER = T6.PERSON_ENTITY_NUMBER AND T.DISCLOSURE_ID = AV_DISCLOSURE_ID
                    AND T0.REVIEW_STATUS_CODE IN (1,5,6);

                INSERT INTO COI_DISCL_ENT_PROJ_DETAILS(`PERSON_ENTITY_ID`,`PERSON_ENTITY_NUMBER`, `ENTITY_ID`, `ENTITY_NUMBER`,
                    `MODULE_ITEM_KEY`, `MODULE_CODE`, `DISCLOSURE_NUMBER`, `DISCLOSURE_ID`, `UPDATE_TIMESTAMP`, `UPDATE_USER`)
                    (SELECT DISTINCT T2.PERSON_ENTITY_ID, T2.PERSON_ENTITY_NUMBER, T2.ENTITY_ID, T2.ENTITY_NUMBER,
                     T1.MODULE_ITEM_KEY, T1.MODULE_CODE, T1.DISCLOSURE_NUMBER, AV_DISCLOSURE_ID, now(), AV_UPDATE_USER
                     FROM COI_DISCL_ENT_PROJ_DETAILS T1
                     INNER JOIN COI_DISCLOSURE T0 ON T0.DISCLOSURE_ID = T1.DISCLOSURE_ID
                     RIGHT JOIN (SELECT t1.PERSON_ENTITY_ID, t1.PERSON_ENTITY_NUMBER, t1.ENTITY_ID, t1.ENTITY_NUMBER FROM PERSON_ENTITY t1
                     INNER JOIN PERSON_ENTITY_RELATIONSHIP t2 ON t2.PERSON_ENTITY_ID = t1.PERSON_ENTITY_ID
                     INNER JOIN VALID_PERSON_ENTITY_REL_TYPE t3 ON t3.VALID_PERS_ENTITY_REL_TYP_CODE = t2.VALID_PERS_ENTITY_REL_TYP_CODE
                     WHERE t1.PERSON_ENTITY_NUMBER NOT IN (SELECT DISTINCT PERSON_ENTITY_NUMBER FROM COI_DISCL_ENT_PROJ_DETAILS
                     WHERE DISCLOSURE_ID = AV_DISCLOSURE_ID AND PERSON_ENTITY_NUMBER IS NOT NULL)
                     AND t1.VERSION_STATUS='ACTIVE' AND t3.DISCLOSURE_TYPE_CODE = '1' AND t1.PERSON_ID = AV_PERSON_ID) T2
                     ON (T1.PERSON_ENTITY_NUMBER IS NULL OR T1.PERSON_ENTITY_NUMBER != T2.PERSON_ENTITY_NUMBER)
                     WHERE T1.DISCLOSURE_ID = AV_DISCLOSURE_ID AND T0.REVIEW_STATUS_CODE IN (1,5,6));


                DELETE T2, T FROM COI_DISCL_ENT_PROJ_DETAILS T
                    INNER JOIN COI_DISCLOSURE T0 ON T0.DISCLOSURE_ID = T.DISCLOSURE_ID
                    LEFT JOIN  DISCL_COMMENT T2 ON T2.SUB_MODULE_ITEM_KEY = T.DISCLOSURE_DETAILS_ID AND (T2.COMPONENT_TYPE_CODE = '1' OR T2.COMPONENT_TYPE_CODE = '6')
                    INNER JOIN (SELECT DISTINCT T2.PERSON_ENTITY_ID, T1.PERSON_ENTITY_NUMBER FROM COI_DISCL_ENT_PROJ_DETAILS T1
                    INNER JOIN PERSON_ENTITY T2 ON T2.PERSON_ENTITY_NUMBER = T1.PERSON_ENTITY_NUMBER
                    LEFT JOIN PERSON_ENTITY_RELATIONSHIP T3 ON T3.PERSON_ENTITY_ID = T2.PERSON_ENTITY_ID
                    LEFT JOIN VALID_PERSON_ENTITY_REL_TYPE T4 ON T4.VALID_PERS_ENTITY_REL_TYP_CODE = T3.VALID_PERS_ENTITY_REL_TYP_CODE
                    WHERE (T2.VERSION_STATUS='INACTIVE' OR T3.PERSON_ENTITY_ID IS NULL OR (T4.DISCLOSURE_TYPE_CODE != '1' AND
                    NOT EXISTS (SELECT 1 FROM PERSON_ENTITY_RELATIONSHIP T3_1
                        INNER JOIN VALID_PERSON_ENTITY_REL_TYPE T4_1 ON T4_1.VALID_PERS_ENTITY_REL_TYP_CODE = T3_1.VALID_PERS_ENTITY_REL_TYP_CODE
                        WHERE T3_1.PERSON_ENTITY_ID = T2.PERSON_ENTITY_ID AND T4_1.DISCLOSURE_TYPE_CODE = '1'
                    )))
                    AND  (T1.PERSON_ENTITY_ID = T2.PERSON_ENTITY_ID OR T1.PERSON_ENTITY_ID != T2.PERSON_ENTITY_ID)
                    AND T2.VERSION_STATUS != 'ARCHIVE' AND T1.DISCLOSURE_ID = AV_DISCLOSURE_ID) AS T3 ON T3.PERSON_ENTITY_NUMBER = T.PERSON_ENTITY_NUMBER
                    WHERE T.PERSON_ENTITY_NUMBER = T3.PERSON_ENTITY_NUMBER AND T.DISCLOSURE_ID = AV_DISCLOSURE_ID
                    AND T0.REVIEW_STATUS_CODE IN (1,5,6);

                DELETE T1 FROM COI_DISCL_ENT_PROJ_DETAILS T1
                    INNER JOIN COI_DISCLOSURE T0 ON T0.DISCLOSURE_ID = T1.DISCLOSURE_ID
                    WHERE T1.PERSON_ENTITY_ID IS NULL AND T1.DISCLOSURE_ID = AV_DISCLOSURE_ID AND T0.REVIEW_STATUS_CODE IN (1,5,6);

                SELECT COUNT(*) INTO LI_COUNT FROM COI_DISCL_ENT_PROJ_DETAILS WHERE DISCLOSURE_ID = AV_DISCLOSURE_ID;
                IF LI_COUNT < 1 THEN

                    INSERT INTO COI_DISCL_ENT_PROJ_DETAILS(`DISCLOSURE_ID`, `DISCLOSURE_NUMBER`, `MODULE_CODE`, `MODULE_ITEM_KEY`,
                    `UPDATE_TIMESTAMP`, `UPDATE_USER`)(SELECT DISCLOSURE_ID, DISCLOSURE_NUMBER, MODULE_CODE, MODULE_ITEM_KEY, now(), AV_UPDATE_USER
                    FROM temp_project_details_table WHERE DISCLOSURE_ID = AV_DISCLOSURE_ID);

                END IF;
            END;
        END IF;	
	END;
COMMIT;
SET LI_LOCK_ACQUIRED = RELEASE_LOCK('coi_sfi_projects_lock');
	SELECT '1';
ELSE
        SELECT 'Unable to acquire lock. Another process may sync the projects vs entity.';

END IF;
END
//