DELIMITER //
CREATE PROCEDURE `COI_VALIDATE_DISCLOSURE_CONFLICTS`(
    AV_DISCLOSURE_ID   	     INT(30),
    AV_PERSON_ID             VARCHAR(45),
    AV_UPDATE_USER           VARCHAR(45)
) 
    BEGIN

    /*
        PROJECT_CONFLICT_STATUS_CODE = 3 CONFLICT
        PROJECT_CONFLICT_STATUS_CODE = 2 POTENTIAL CONFLICT
        PROJECT_CONFLICT_STATUS_CODE = 1 NO CONFLICT

        CONFLICT_STATUS_CODE = 3 CONFLICT
        CONFLICT_STATUS_CODE = 2 POTENTIAL CONFLICT
        CONFLICT_STATUS_CODE = 1 NO CONFLICT
        CONFLICT_STATUS_CODE = 4 NO CONFLICT WITHOUT SFI
    */

    DECLARE LI_CONFLICT_COUNT               INT;
    DECLARE LI_POTENTIAL_CONFLICT_COUNT     INT;
    DECLARE LI_NO_CONFLICT_COUNT            INT;
    DECLARE LI_NO_SFI						INT;

    SELECT COUNT(PROJECT_CONFLICT_STATUS_CODE) INTO LI_CONFLICT_COUNT FROM COI_DISCL_ENT_PROJ_DETAILS WHERE PROJECT_CONFLICT_STATUS_CODE BETWEEN 300 AND 399 AND DISCLOSURE_ID = AV_DISCLOSURE_ID;
    /*
        If found any conflicts updates conflic status on disclosure table and return conflic status 
    */
    IF LI_CONFLICT_COUNT != 0 THEN

            UPDATE COI_DISCLOSURE SET CONFLICT_STATUS_CODE = 3, UPDATE_USER = AV_UPDATE_USER, UPDATE_TIMESTAMP = now() WHERE DISCLOSURE_ID = AV_DISCLOSURE_ID;
            SELECT CONFLICT_STATUS_CODE, DESCRIPTION FROM COI_CONFLICT_STATUS_TYPE WHERE CONFLICT_STATUS_CODE =3;
    ELSEIF LI_CONFLICT_COUNT = 0 THEN
        SELECT COUNT(PROJECT_CONFLICT_STATUS_CODE) INTO LI_POTENTIAL_CONFLICT_COUNT FROM COI_DISCL_ENT_PROJ_DETAILS WHERE PROJECT_CONFLICT_STATUS_CODE BETWEEN 200 AND 299 AND DISCLOSURE_ID = AV_DISCLOSURE_ID;
        /*
            If not found any conflicts and founds potential conflict updates potential conflic status on disclosure table return conflic status 
        */
        IF LI_POTENTIAL_CONFLICT_COUNT != 0 THEN

            UPDATE COI_DISCLOSURE SET CONFLICT_STATUS_CODE = 2, UPDATE_USER = AV_UPDATE_USER, UPDATE_TIMESTAMP = now() WHERE DISCLOSURE_ID = AV_DISCLOSURE_ID;
            SELECT CONFLICT_STATUS_CODE, DESCRIPTION FROM COI_CONFLICT_STATUS_TYPE WHERE CONFLICT_STATUS_CODE =2;
        END IF;
    END IF;
    IF LI_POTENTIAL_CONFLICT_COUNT = 0 THEN
        SELECT COUNT(PROJECT_CONFLICT_STATUS_CODE) INTO LI_NO_CONFLICT_COUNT FROM COI_DISCL_ENT_PROJ_DETAILS WHERE PROJECT_CONFLICT_STATUS_CODE BETWEEN 100 AND 199 AND DISCLOSURE_ID = AV_DISCLOSURE_ID;
        /*
            If not found any potential conflicts and founds no conflict updates no conflic status on disclosure table return conflic status 
        */
        IF LI_NO_CONFLICT_COUNT != 0 THEN

            UPDATE COI_DISCLOSURE SET CONFLICT_STATUS_CODE = 1, UPDATE_USER = AV_UPDATE_USER, UPDATE_TIMESTAMP = now() WHERE DISCLOSURE_ID = AV_DISCLOSURE_ID;
            SELECT CONFLICT_STATUS_CODE, DESCRIPTION FROM COI_CONFLICT_STATUS_TYPE WHERE CONFLICT_STATUS_CODE =1;
        END IF;
    END IF;

    IF LI_NO_CONFLICT_COUNT = 0 THEN
		/** No Conflict status set for SFIs */
		SELECT COUNT(PERSON_ENTITY_ID) INTO LI_NO_SFI FROM COI_DISCL_ENT_PROJ_DETAILS WHERE PERSON_ENTITY_ID is not NULL AND DISCLOSURE_ID = AV_DISCLOSURE_ID;
		IF LI_NO_SFI > 0 THEN
			SELECT NULL, NULL;
		ELSE
			/** Exempt disclosures(Disclosure without SFI)*/
			UPDATE COI_DISCLOSURE SET CONFLICT_STATUS_CODE = 4, UPDATE_USER = AV_UPDATE_USER, UPDATE_TIMESTAMP = now() WHERE DISCLOSURE_ID = AV_DISCLOSURE_ID;
			SELECT CONFLICT_STATUS_CODE, DESCRIPTION FROM COI_CONFLICT_STATUS_TYPE WHERE CONFLICT_STATUS_CODE =4;
		END IF;
    END IF;
    
END
//