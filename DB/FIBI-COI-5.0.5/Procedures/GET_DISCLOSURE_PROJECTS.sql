DELIMITER //
CREATE PROCEDURE `GET_DISCLOSURE_PROJECTS`(
	AV_DISCLOSURE_ID          INT
)
BEGIN

        SELECT DISTINCT
			COALESCE(T2.EXTERNAL_SYSTEM_REF_ID, T3.EXTERNAL_SYSTEM_REF_ID, T4.EXTERNAL_SYSTEM_REF_ID) AS PROJECT_ID,
			COALESCE(T2.AWARD_NUMBER, T3.PROPOSAL_NUMBER, T4.PROPOSAL_NUMBER) AS PROJECT_NUMBER,
            COALESCE(T2.TITLE, T3.TITLE, T4.TITLE) AS PROJECT_TITLE,
			COALESCE(T2.AWARD_STATUS, T3.PROPOSAL_STATUS, T4.PROPOSAL_STATUS) AS PROJECT_STATUS,
			COALESCE(T2.AWARD_START_DATE, T3.PROPOSAL_START_DATE, T4.PROPOSAL_START_DATE) AS PROJECT_START_DATE,
            COALESCE(T2.AWARD_END_DATE, T3.PROPOSAL_END_DATE, T4.PROPOSAL_END_DATE) AS PROJECT_END_DATE,
			COALESCE(T2.LEAD_UNIT_NUMBER, T3.LEAD_UNIT_NUMBER, T4.LEAD_UNIT_NUMBER) AS LEAD_UNIT_NUMBER,
			COALESCE(T2.LEAD_UNIT_NAME, T3.LEAD_UNIT_NAME, T4.LEAD_UNIT_NAME) AS LEAD_UNIT_NAME,
            COALESCE(T2.SPONSOR_NAME, T3.SPONSOR_NAME, T4.SPONSOR_NAME) AS PROJECT_SPONSOR_NAME,
			COALESCE(T2.SPONSOR_CODE, T3.SPONSOR_CODE, T4.SPONSOR_CODE) AS SPONSOR_CODE,
			COALESCE(T2.PRIME_SPONSOR_NAME, T3.PRIME_SPONSOR_NAME, T4.PRIME_SPONSOR_NAME) AS PROJECT_PRIME_SPONSOR_NAME,
			COALESCE(T2.PRIME_SPONSOR_CODE, T3.PRIME_SPONSOR_CODE, T4.PRIME_SPONSOR_CODE) AS PRIME_SPONSOR_CODE,
			COALESCE(T2.PI_NAME, T3.PI_NAME, T4.PI_NAME) AS PI_NAME,
			COALESCE(T2.KEY_PERSON_NAME, T3.KEY_PERSON_NAME, T4.KEY_PERSON_NAME) AS KEY_PERSON_NAME,
			COALESCE(T2.KEY_PERSON_ID, T3.KEY_PERSON_ID, T4.KEY_PERSON_ID) AS KEY_PERSON_ID,
			COALESCE(T2.KEY_PERSON_ROLE_NAME, T3.KEY_PERSON_ROLE_NAME, T4.KEY_PERSON_ROLE_NAME) AS REPORTER_ROLE,
			T1.MODULE_CODE,
			T9.COI_PROJECT_TYPE_CODE,
			T9.DESCRIPTION AS COI_PROJECT_TYPE,
            T9.BADGE_COLOR,
			T9.PROJECT_ICON,
			T1.COI_DISCL_PROJECTS_ID
			FROM COI_DISCLOSURE T0
			INNER JOIN COI_DISCL_PROJECTS T1 ON T1.DISCLOSURE_ID = T0.DISCLOSURE_ID
            LEFT JOIN COI_PROJECT_AWARD_V T2 ON (T2.EXTERNAL_SYSTEM_REF_ID = T1.MODULE_ITEM_KEY AND T1.MODULE_CODE = 1 AND T0.PERSON_ID = T2.KEY_PERSON_ID)
            LEFT JOIN COI_PROJECT_INSTITUTE_PROPOSAL_V T3 ON (T3.EXTERNAL_SYSTEM_REF_ID = T1.MODULE_ITEM_KEY AND T1.MODULE_CODE = 2 AND T0.PERSON_ID = T3.KEY_PERSON_ID)
            LEFT JOIN COI_PROJECT_PROPOSAL_V T4 ON (T4.EXTERNAL_SYSTEM_REF_ID = T1.MODULE_ITEM_KEY AND T1.MODULE_CODE = 3 AND T0.PERSON_ID = T4.KEY_PERSON_ID)
            LEFT JOIN COI_PROJECT_TYPE T9 ON (T9.COI_PROJECT_TYPE_CODE = T2.COI_PROJECT_TYPE_CODE
				OR T9.COI_PROJECT_TYPE_CODE = T3.COI_PROJECT_TYPE_CODE OR T9.COI_PROJECT_TYPE_CODE = T4.COI_PROJECT_TYPE_CODE)
            WHERE T0.DISCLOSURE_ID= AV_DISCLOSURE_ID;

END
//
