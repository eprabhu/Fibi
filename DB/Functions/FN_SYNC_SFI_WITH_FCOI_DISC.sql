DELIMITER //
CREATE FUNCTION `FN_SYNC_SFI_WITH_FCOI_DISC`(
AV_MODULE_CODE    		 INT,
AV_PERSON_ID             VARCHAR(45),
AV_UPDATE_USER           VARCHAR(45),
AV_DISCLOSURE_ID   	     INT(30),
AV_DISCLOSURE_NUMBER   	 INT(30),
AV_PERSON_ENTITY_ID      VARCHAR(30),
AV_ENTITY_ID             VARCHAR(30),
AV_ENTITY_NUMBER         VARCHAR(30)
) RETURNS int(11)
    DETERMINISTIC
    BEGIN

        DECLARE LI_AWARD_ID             VARCHAR(30);
        DECLARE LI_AWARD_NUMBER         VARCHAR(30);
        DECLARE LI_PROPOSAL_ID          VARCHAR(30);

        /** IN AV_MODULE_CODE future this should be configurable using table*/

            IF AV_MODULE_CODE = 1 THEN
                BEGIN
                DECLARE DONE2 INT DEFAULT FALSE;
       
                    DECLARE CUR_AWARDS CURSOR FOR SELECT DISTINCT T1.EXTERNAL_SYSTEM_REF_ID AS AWARD_ID, T1.AWARD_NUMBER FROM COI_PROJECT_AWARD_V T1 
                    WHERE T1.AWARD_STATUS IN ('Active','Pending') and T1.PI_PERSON_ID = AV_PERSON_ID;

                    DECLARE CONTINUE HANDLER FOR NOT FOUND SET DONE2 = TRUE;

                    OPEN CUR_AWARDS;
                    AWARDS_LOOP: LOOP 
					FETCH CUR_AWARDS INTO LI_AWARD_ID, LI_AWARD_NUMBER;

                        IF DONE2 THEN
                            LEAVE AWARDS_LOOP;
                        END IF;

                        INSERT INTO COI_DISCL_ENT_PROJ_DETAILS(`DISCLOSURE_ID`, `DISCLOSURE_NUMBER`, `PERSON_ENTITY_ID`, `ENTITY_ID`, `ENTITY_NUMBER`, `MODULE_CODE`, `MODULE_ITEM_KEY`, 
                        `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES(AV_DISCLOSURE_ID, AV_DISCLOSURE_NUMBER, AV_PERSON_ENTITY_ID, AV_ENTITY_ID, AV_ENTITY_NUMBER, AV_MODULE_CODE,
                        LI_AWARD_ID, now(), AV_UPDATE_USER);
                    
					END LOOP;
					CLOSE CUR_AWARDS;
                END;
            END IF;
            
            IF AV_MODULE_CODE = 3 THEN
                BEGIN 
					DECLARE DONE3 INT DEFAULT FALSE;
                    DECLARE CUR_PROPOSALS CURSOR FOR SELECT DISTINCT T1.EXTERNAL_SYSTEM_REF_ID AS PROPOSAL_ID 
                    FROM COI_PROJECT_PROPOSAL_V T1 WHERE T1.PROPOSAL_STATUS not IN ("In Progress","Unsuccessful","Inactive","Revision Requested",
                    "ORT Director Review Completed","Pending Revisions By PI","Pending Revisions By PI","Not Submitted","Returned","Withdrawn","Awarded") 
                    AND T1.PI_PERSON_ID = AV_PERSON_ID;
                    
                    DECLARE CONTINUE HANDLER FOR NOT FOUND SET DONE3 = TRUE;

                    OPEN CUR_PROPOSALS;
                    PROPOSALS_LOOP: LOOP 
					FETCH CUR_PROPOSALS INTO LI_PROPOSAL_ID;

                        IF DONE3 THEN
                            LEAVE PROPOSALS_LOOP;
                        END IF;

                        INSERT INTO COI_DISCL_ENT_PROJ_DETAILS(`DISCLOSURE_ID`, `DISCLOSURE_NUMBER`, `PERSON_ENTITY_ID`, `ENTITY_ID`, `ENTITY_NUMBER`, `MODULE_CODE`, `MODULE_ITEM_KEY`, 
                        `UPDATE_TIMESTAMP`, `UPDATE_USER`) VALUES(AV_DISCLOSURE_ID, AV_DISCLOSURE_NUMBER, AV_PERSON_ENTITY_ID, AV_ENTITY_ID, AV_ENTITY_NUMBER, AV_MODULE_CODE,
                        LI_PROPOSAL_ID, now(), AV_UPDATE_USER);
            
					END LOOP;
					CLOSE CUR_PROPOSALS;
                END;
            END IF;
RETURN 1;
END
//