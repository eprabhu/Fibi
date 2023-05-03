DELIMITER //
CREATE PROCEDURE `GET_DISCLOSURE_RELATIONS`(
	AV_MODULE_CODE            INT(3),
	AV_PERSON_ID              VARCHAR(40),
	AV_DISCLOSURE_ID          INT(10)
)
BEGIN
	
    IF AV_MODULE_CODE = 1 THEN
    
		IF AV_DISCLOSURE_ID is not null THEN
			SELECT DISTINCT T2.EXTERNAL_SYSTEM_REF_ID AS AWARD_ID, T2.AWARD_NUMBER, T2.TITLE, T2.AWARD_START_DATE AS BEGIN_DATE, 
            T2.AWARD_END_DATE AS FINAL_EXPIRATION_DATE, T2.LEAD_UNIT_NUMBER, T2.LEAD_UNIT_NAME AS UNIT_NAME, T2.SPONSOR_NAME ,
            T2.PI_NAME AS PI, T2.AWARD_STATUS AS STATUS FROM COI_DISCL_ENT_PROJ_DETAILS T1 INNER JOIN COI_PROJECT_AWARD_V T2 ON  
            T2.EXTERNAL_SYSTEM_REF_ID=T1.MODULE_ITEM_KEY WHERE T1.MODULE_CODE = 1 AND (T2.AWARD_STATUS IN ('Active','Pending') ) AND T1.DISCLOSURE_ID= AV_DISCLOSURE_ID;
            
		else
			SELECT DISTINCT T1.EXTERNAL_SYSTEM_REF_ID AS AWARD_ID, T1.AWARD_NUMBER, T1.TITLE, T1.AWARD_START_DATE AS BEGIN_DATE, 
            T1.AWARD_END_DATE AS FINAL_EXPIRATION_DATE, T1.LEAD_UNIT_NUMBER, T1.LEAD_UNIT_NAME AS UNIT_NAME, T1.SPONSOR_NAME ,
            T1.PI_NAME AS PI, T1.AWARD_STATUS AS STATUS FROM COI_PROJECT_AWARD_V T1 WHERE T1.AWARD_STATUS IN ('Active','Pending') and T1.PI_PERSON_ID = AV_PERSON_ID;
            
        END IF;

	ELSE IF AV_MODULE_CODE = 3 THEN

		IF AV_DISCLOSURE_ID is not null THEN
			SELECT DISTINCT T2.EXTERNAL_SYSTEM_REF_ID, T2.TITLE, T2.PROPOSAL_STATUS AS STATUS, T2.PROPOSAL_START_DATE AS START_DATE, T2.PROPOSAL_END_DATE AS END_DATE, 
            T2.LEAD_UNIT_NUMBER AS HOME_UNIT_NUMBER, T2.LEAD_UNIT_NAME AS HOME_UNIT_NAME,
            T2.SPONSOR_NAME, T2.PRIME_SPONSOR_NAME, T2.PI_NAME AS PI FROM COI_DISCL_ENT_PROJ_DETAILS T1 INNER JOIN COI_PROJECT_PROPOSAL_V T2 
            ON T2.EXTERNAL_SYSTEM_REF_ID=T1.MODULE_ITEM_KEY WHERE T1.MODULE_CODE = 3 AND T2.PROPOSAL_STATUS not IN ("In Progress","Unsuccessful","Inactive","Revision Requested",
            "ORT Director Review Completed","Pending Revisions By PI","Pending Revisions By PI","Not Submitted","Returned","Withdrawn","Awarded") AND T1.DISCLOSURE_ID= AV_DISCLOSURE_ID;
            
		else
			SELECT DISTINCT T1.EXTERNAL_SYSTEM_REF_ID, T1.TITLE, T1.PROPOSAL_STATUS AS STATUS, T1.PROPOSAL_START_DATE AS START_DATE, T1.PROPOSAL_END_DATE AS END_DATE, 
            T1.LEAD_UNIT_NUMBER AS HOME_UNIT_NUMBER, T1.LEAD_UNIT_NAME AS HOME_UNIT_NAME, T1.SPONSOR_NAME, T1.PRIME_SPONSOR_NAME, T1.PI_NAME AS PI 
            FROM COI_PROJECT_PROPOSAL_V T1 WHERE T1.PROPOSAL_STATUS not IN ("In Progress","Unsuccessful","Inactive","Revision Requested",
            "ORT Director Review Completed","Pending Revisions By PI","Pending Revisions By PI","Not Submitted","Returned","Withdrawn","Awarded") AND T1.PI_PERSON_ID = AV_PERSON_ID;
        END IF;
	END IF;
    end if;
END
//