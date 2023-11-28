DELIMITER //
CREATE PROCEDURE `GET_DISCLOSURE_RELATIONS`(
	AV_MODULE_CODE            INT(3),
	AV_PERSON_ID              VARCHAR(40),
	AV_DISCLOSURE_ID          INT(10),
    AV_SEARCH_STRING		  VARCHAR(40)
)
BEGIN

    IF AV_MODULE_CODE = 1 THEN

		IF AV_DISCLOSURE_ID is not null THEN
			SELECT DISTINCT T2.EXTERNAL_SYSTEM_REF_ID AS AWARD_ID, T2.AWARD_NUMBER, T2.TITLE, T2.AWARD_START_DATE AS BEGIN_DATE, 
            T2.AWARD_END_DATE AS FINAL_EXPIRATION_DATE, T2.LEAD_UNIT_NUMBER, T2.LEAD_UNIT_NAME AS UNIT_NAME, T2.SPONSOR_NAME ,
            T2.PI_NAME AS PI, T2.KEY_PERSON_NAME AS KEY_PERSON, T2.KEY_PERSON_ID, T3.DESCRIPTION AS REPORTER_ROLE, T2.AWARD_STATUS AS STATUS, T2.PRIME_SPONSOR_NAME,
            T2.ACCOUNT_NUMBER, T2.SPONSOR_AWARD_NUMBER, T6.DESCRIPTION AS CONFLICT_DESCRIPTION, T4.PROJECT_CONFLICT_STATUS_CODE 
            FROM COI_DISCL_ENT_PROJ_DETAILS T1 
            INNER JOIN COI_PROJECT_AWARD_V T2 ON T2.EXTERNAL_SYSTEM_REF_ID=T1.MODULE_ITEM_KEY 
            INNER JOIN EPS_PROP_PERSON_ROLE T3 ON T2.KEY_PERSON_ROLE_CODE = T3.PROP_PERSON_ROLE_ID 
            LEFT JOIN (SELECT DISCLOSURE_DETAILS_ID , MODULE_ITEM_KEY,
            MODULE_CODE, MAX(T5.PROJECT_CONFLICT_STATUS_CODE) AS PROJECT_CONFLICT_STATUS_CODE
            FROM COI_DISCL_ENT_PROJ_DETAILS T5  
            WHERE T5.DISCLOSURE_ID= AV_DISCLOSURE_ID AND MODULE_CODE = 1 GROUP BY MODULE_ITEM_KEY, MODULE_CODE )T4 ON T4.MODULE_ITEM_KEY = T2.EXTERNAL_SYSTEM_REF_ID 
            LEFT JOIN COI_PROJ_CONFLICT_STATUS_TYPE T6 ON T6.PROJECT_CONFLICT_STATUS_CODE = T4.PROJECT_CONFLICT_STATUS_CODE 
            WHERE T1.MODULE_CODE = 1 
            AND (T2.AWARD_STATUS IN ('Active','Pending') ) 
            AND T1.DISCLOSURE_ID= AV_DISCLOSURE_ID
            AND T2.KEY_PERSON_ID = AV_PERSON_ID;

		ELSE

			SELECT DISTINCT T1.EXTERNAL_SYSTEM_REF_ID AS AWARD_ID, T1.AWARD_NUMBER, T1.TITLE, T1.AWARD_START_DATE AS BEGIN_DATE, 
            T1.AWARD_END_DATE AS FINAL_EXPIRATION_DATE, T1.LEAD_UNIT_NUMBER, T1.LEAD_UNIT_NAME AS UNIT_NAME, T1.SPONSOR_NAME ,
            T1.PI_NAME AS PI, T1.KEY_PERSON_NAME AS KEY_PERSON, T1.KEY_PERSON_ID, T2.DESCRIPTION AS REPORTER_ROLE, T1.AWARD_STATUS AS STATUS, T1.PRIME_SPONSOR_NAME, 
			T1.ACCOUNT_NUMBER, T1.SPONSOR_AWARD_NUMBER , null as CONFLICT_DESCRIPTION, null as PROJECT_CONFLICT_STATUS_CODE
            FROM COI_PROJECT_AWARD_V T1 
            INNER JOIN EPS_PROP_PERSON_ROLE T2 ON T1.KEY_PERSON_ROLE_CODE = T2.PROP_PERSON_ROLE_ID
            WHERE T1.AWARD_STATUS IN ('Active','Pending')
            AND (LOWER(T1.EXTERNAL_SYSTEM_REF_ID) LIKE CONCAT('%', LOWER(AV_SEARCH_STRING), '%')
				OR LOWER(T1.AWARD_NUMBER) LIKE CONCAT('%', LOWER(AV_SEARCH_STRING), '%')
				OR LOWER(T1.TITLE) LIKE CONCAT('%', AV_SEARCH_STRING, '%')
				OR LOWER(T1.LEAD_UNIT_NUMBER) LIKE CONCAT('%', LOWER(AV_SEARCH_STRING), '%')
				OR LOWER(T1.LEAD_UNIT_NAME) LIKE CONCAT('%', LOWER(AV_SEARCH_STRING), '%')
				OR LOWER(T1.SPONSOR_NAME) LIKE CONCAT('%', LOWER(AV_SEARCH_STRING), '%')
				OR LOWER(T1.PI_NAME) LIKE CONCAT('%', LOWER(AV_SEARCH_STRING), '%')
				OR LOWER(T1.KEY_PERSON_NAME) LIKE CONCAT('%', LOWER(AV_SEARCH_STRING), '%')
				OR LOWER(T1.KEY_PERSON_ID) LIKE CONCAT('%', LOWER(AV_SEARCH_STRING), '%')
				OR LOWER(T1.PRIME_SPONSOR_NAME) LIKE CONCAT('%', LOWER(AV_SEARCH_STRING), '%')
				OR LOWER(T1.ACCOUNT_NUMBER) LIKE CONCAT('%', LOWER(AV_SEARCH_STRING), '%')
				OR LOWER(T1.SPONSOR_AWARD_NUMBER) LIKE CONCAT('%', LOWER(AV_SEARCH_STRING), '%')
			)
            AND T1.KEY_PERSON_ID = AV_PERSON_ID;

        END IF;

	ELSE IF AV_MODULE_CODE = 3 THEN

		IF AV_DISCLOSURE_ID is not null THEN
			SELECT DISTINCT T2.EXTERNAL_SYSTEM_REF_ID AS PROPOSAL_ID, T2.TITLE, T2.PROPOSAL_STATUS AS STATUS, T2.PROPOSAL_START_DATE AS START_DATE, T2.PROPOSAL_END_DATE AS END_DATE, 
            T2.LEAD_UNIT_NUMBER AS HOME_UNIT_NUMBER, T2.LEAD_UNIT_NAME AS HOME_UNIT_NAME,
            T2.SPONSOR_NAME, T2.PRIME_SPONSOR_NAME, T2.PI_NAME AS PI, T2.KEY_PERSON_NAME AS KEY_PERSON, T2.KEY_PERSON_ID, T3.DESCRIPTION AS REPORTER_ROLE,
            T6.DESCRIPTION AS CONFLICT_DESCRIPTION, T4.PROJECT_CONFLICT_STATUS_CODE 
            FROM COI_DISCL_ENT_PROJ_DETAILS T1 
            INNER JOIN COI_PROJECT_PROPOSAL_V T2 ON T2.EXTERNAL_SYSTEM_REF_ID=T1.MODULE_ITEM_KEY 
            INNER JOIN EPS_PROP_PERSON_ROLE T3 ON T2.KEY_PERSON_ROLE_CODE = T3.PROP_PERSON_ROLE_ID
            LEFT JOIN (SELECT DISCLOSURE_DETAILS_ID , MODULE_ITEM_KEY,
            MODULE_CODE, MAX(T5.PROJECT_CONFLICT_STATUS_CODE) AS PROJECT_CONFLICT_STATUS_CODE
            FROM COI_DISCL_ENT_PROJ_DETAILS T5 
            WHERE T5.DISCLOSURE_ID= AV_DISCLOSURE_ID AND MODULE_CODE = 3 GROUP BY MODULE_ITEM_KEY, MODULE_CODE )T4 ON T4.MODULE_ITEM_KEY = T2.EXTERNAL_SYSTEM_REF_ID 
            LEFT JOIN COI_PROJ_CONFLICT_STATUS_TYPE T6 ON T6.PROJECT_CONFLICT_STATUS_CODE = T4.PROJECT_CONFLICT_STATUS_CODE 
            WHERE T1.MODULE_CODE = 3 
            AND T2.PROPOSAL_STATUS not IN ("In Progress","Unsuccessful","Inactive","Revision Requested",
            "ORT Director Review Completed","Pending Revisions By PI","Pending Revisions By PI","Not Submitted","Returned","Withdrawn","Awarded") 
            AND T1.DISCLOSURE_ID= AV_DISCLOSURE_ID 
            AND T2.KEY_PERSON_ID = AV_PERSON_ID;
            
		ELSE

			SELECT DISTINCT T1.EXTERNAL_SYSTEM_REF_ID AS PROPOSAL_ID, T1.TITLE, T1.PROPOSAL_STATUS AS STATUS, T1.PROPOSAL_START_DATE AS START_DATE,
            T1.PROPOSAL_END_DATE AS END_DATE, T1.LEAD_UNIT_NUMBER AS HOME_UNIT_NUMBER, T1.LEAD_UNIT_NAME AS HOME_UNIT_NAME, T1.SPONSOR_NAME,
            T1.PRIME_SPONSOR_NAME, T1.PI_NAME AS PI, T1.KEY_PERSON_NAME AS KEY_PERSON, T1.KEY_PERSON_ID, T2.DESCRIPTION AS REPORTER_ROLE,
            null as CONFLICT_DESCRIPTION, null as PROJECT_CONFLICT_STATUS_CODE 
            FROM COI_PROJECT_PROPOSAL_V T1 
            INNER JOIN EPS_PROP_PERSON_ROLE T2 ON T1.KEY_PERSON_ROLE_CODE = T2.PROP_PERSON_ROLE_ID
            WHERE T1.PROPOSAL_STATUS not IN ("In Progress","Unsuccessful","Inactive","Revision Requested",
            "ORT Director Review Completed","Pending Revisions By PI","Pending Revisions By PI","Not Submitted","Returned","Withdrawn","Awarded") 
            AND (LOWER(T1.EXTERNAL_SYSTEM_REF_ID) LIKE CONCAT('%', LOWER(AV_SEARCH_STRING), '%')
				 OR LOWER(T1.TITLE) LIKE CONCAT('%', LOWER(AV_SEARCH_STRING), '%')
				 OR LOWER(T1.LEAD_UNIT_NUMBER) LIKE CONCAT('%', LOWER(AV_SEARCH_STRING), '%')
				 OR LOWER(T1.LEAD_UNIT_NAME) LIKE CONCAT('%', LOWER(AV_SEARCH_STRING), '%')
				 OR LOWER(T1.SPONSOR_NAME) LIKE CONCAT('%', LOWER(AV_SEARCH_STRING), '%')
				 OR LOWER(T1.PRIME_SPONSOR_NAME) LIKE CONCAT('%', LOWER(AV_SEARCH_STRING), '%')
				 OR LOWER(T1.PI_NAME) LIKE CONCAT('%', LOWER(AV_SEARCH_STRING), '%')
				 OR LOWER(T1.KEY_PERSON_NAME) LIKE CONCAT('%', LOWER(AV_SEARCH_STRING), '%')
				 OR LOWER(T1.KEY_PERSON_ID) LIKE CONCAT('%', LOWER(AV_SEARCH_STRING), '%')
			)
            AND T1.KEY_PERSON_ID = AV_PERSON_ID;

        END IF;

	END IF;
    END IF;
END
//
