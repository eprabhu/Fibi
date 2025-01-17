DELIMITER //
CREATE PROCEDURE `GET_DISCLOSURE_PROJECTS`(
	AV_DISCLOSURE_ID          INT
)
BEGIN

DECLARE LS_PERSON_ID VARCHAR(40);

        SELECT PERSON_ID INTO LS_PERSON_ID FROM COI_DISCLOSURE WHERE DISCLOSURE_ID= AV_DISCLOSURE_ID;

        SELECT DISTINCT 
			T2.EXTERNAL_SYSTEM_REF_ID AS PROJECT_ID, 
			T2.AWARD_NUMBER AS PROJECT_NUMBER,
            T2.TITLE, 
			T2.AWARD_STATUS AS STATUS, 
			T2.AWARD_START_DATE AS START_DATE,
            T2.AWARD_END_DATE AS END_DATE, 
			T2.LEAD_UNIT_NUMBER AS HOME_UNIT_NUMBER, 
			T2.LEAD_UNIT_NAME AS HOME_UNIT_NAME,
            T2.SPONSOR_NAME, T2.PRIME_SPONSOR_NAME, 
			T2.PI_NAME AS PI, 
			T2.KEY_PERSON_NAME AS KEY_PERSON, 
			T2.KEY_PERSON_ID, 
			T3.DESCRIPTION AS REPORTER_ROLE,
            T6.DESCRIPTION AS CONFLICT_DESCRIPTION, 
			T4.PROJECT_CONFLICT_STATUS_CODE, 
			(CASE WHEN T5.RELATIONSHIP_COUNT > 0 THEN true ELSE false END) AS RELATIONSHIP_EXISTS , 
			T1.MODULE_CODE,
            (CASE WHEN T7.IS_PROJECT_COMPLETED_COUNT IS NOT NULL THEN false ELSE true END) AS IS_PROJECT_COMPLETED,
            T8.CONFLICT_STATUS_COUNT, 
			T2.COI_PROJECT_TYPE_CODE, 
			T9.DESCRIPTION AS COI_PROJECT_TYPE,
            T9.BADGE_COLOR 
			FROM COI_DISCL_ENT_PROJ_DETAILS T1
            INNER JOIN COI_PROJECT_AWARD_V T2 ON T2.EXTERNAL_SYSTEM_REF_ID=T1.MODULE_ITEM_KEY
            INNER JOIN EPS_PROP_PERSON_ROLE T3 ON T2.KEY_PERSON_ROLE_CODE = T3.PROP_PERSON_ROLE_ID
            LEFT JOIN (SELECT T5.DISCLOSURE_DETAILS_ID, T5.MODULE_ITEM_KEY, T5.MODULE_CODE, T5.PROJECT_CONFLICT_STATUS_CODE
				FROM COI_DISCL_ENT_PROJ_DETAILS T5
				JOIN (
				SELECT MODULE_ITEM_KEY, MODULE_CODE, MAX(PROJECT_CONFLICT_STATUS_CODE) AS MAX_CONFLICT_STATUS_CODE
				FROM COI_DISCL_ENT_PROJ_DETAILS
				WHERE DISCLOSURE_ID = AV_DISCLOSURE_ID AND MODULE_CODE = 1
				GROUP BY MODULE_ITEM_KEY, MODULE_CODE
				) subquery ON T5.MODULE_ITEM_KEY = subquery.MODULE_ITEM_KEY
				AND T5.MODULE_CODE = subquery.MODULE_CODE
				AND T5.PROJECT_CONFLICT_STATUS_CODE = subquery.MAX_CONFLICT_STATUS_CODE
				WHERE T5.DISCLOSURE_ID = AV_DISCLOSURE_ID AND T5.MODULE_CODE = 1 )T4 ON T4.MODULE_ITEM_KEY = T2.EXTERNAL_SYSTEM_REF_ID
            LEFT JOIN (SELECT MODULE_ITEM_KEY,
						MODULE_CODE, COUNT(PERSON_ENTITY_ID) AS RELATIONSHIP_COUNT
						FROM COI_DISCL_ENT_PROJ_DETAILS 
						WHERE DISCLOSURE_ID= AV_DISCLOSURE_ID 
						AND MODULE_CODE = 1 
						AND PERSON_ENTITY_ID IS NOT NULL
						GROUP BY MODULE_ITEM_KEY, MODULE_CODE )T5 ON T5.MODULE_ITEM_KEY = T2.EXTERNAL_SYSTEM_REF_ID
            LEFT JOIN COI_PROJ_CONFLICT_STATUS_TYPE T6 ON T6.PROJECT_CONFLICT_STATUS_CODE = T4.PROJECT_CONFLICT_STATUS_CODE
            LEFT JOIN (SELECT COUNT(*) AS IS_PROJECT_COMPLETED_COUNT, C2.MODULE_ITEM_KEY 
						FROM COI_DISCL_ENT_PROJ_DETAILS C2
						WHERE C2.PROJECT_CONFLICT_STATUS_CODE IS NULL 
						AND C2.DISCLOSURE_ID = AV_DISCLOSURE_ID
						AND C2.MODULE_CODE = 1 GROUP BY C2.MODULE_ITEM_KEY ) T7 ON T7.MODULE_ITEM_KEY = T2.EXTERNAL_SYSTEM_REF_ID
            LEFT JOIN (SELECT CONCAT('[', GROUP_CONCAT(CONCAT('{', CONFLICT_STATUS_GROUP, ': ', PROJECT_COUNT, '}')), ']') AS CONFLICT_STATUS_COUNT,
						MODULE_ITEM_KEY FROM ( SELECT CASE
											WHEN C2.PROJECT_CONFLICT_STATUS_CODE BETWEEN 100 AND 199 THEN 1
											WHEN C2.PROJECT_CONFLICT_STATUS_CODE BETWEEN 200 AND 299 THEN 2
											WHEN C2.PROJECT_CONFLICT_STATUS_CODE BETWEEN 300 AND 399 THEN 3
											END AS CONFLICT_STATUS_GROUP, 
											COUNT(*) AS PROJECT_COUNT , 
											C2.MODULE_ITEM_KEY
											FROM COI_DISCL_ENT_PROJ_DETAILS C2
											WHERE C2.PROJECT_CONFLICT_STATUS_CODE IS NOT NULL
											AND C2.DISCLOSURE_ID = AV_DISCLOSURE_ID
											AND C2.MODULE_CODE = 1 GROUP BY CONFLICT_STATUS_GROUP, C2.MODULE_ITEM_KEY ) AS grouped 
						GROUP BY MODULE_ITEM_KEY) T8 ON T8.MODULE_ITEM_KEY = T2.EXTERNAL_SYSTEM_REF_ID
            LEFT JOIN COI_PROJECT_TYPE T9 ON T9.COI_PROJECT_TYPE_CODE = T2.COI_PROJECT_TYPE_CODE
            WHERE T1.MODULE_CODE = 1 AND T1.DISCLOSURE_ID= AV_DISCLOSURE_ID AND T2.KEY_PERSON_ID = LS_PERSON_ID

            UNION

        SELECT DISTINCT T2.EXTERNAL_SYSTEM_REF_ID AS PROJECT_ID, 
		T2.PROPOSAL_NUMBER AS PROJECT_NUMBER,
         T2.TITLE, T2.PROPOSAL_STATUS AS STATUS, 
		 T2.PROPOSAL_START_DATE AS START_DATE,
        T2.PROPOSAL_END_DATE AS END_DATE, 
		T2.LEAD_UNIT_NUMBER AS HOME_UNIT_NUMBER, 
		T2.LEAD_UNIT_NAME AS HOME_UNIT_NAME,
            T2.SPONSOR_NAME, 
			T2.PRIME_SPONSOR_NAME, 
			T2.PI_NAME AS PI, 
			T2.KEY_PERSON_NAME AS KEY_PERSON, 
			T2.KEY_PERSON_ID, 
			T3.DESCRIPTION AS REPORTER_ROLE,
            T6.DESCRIPTION AS CONFLICT_DESCRIPTION, 
			T4.PROJECT_CONFLICT_STATUS_CODE, 
			(CASE WHEN T5.RELATIONSHIP_COUNT > 0 THEN true ELSE false END) AS RELATIONSHIP_EXISTS,
            T1.MODULE_CODE, 
			(CASE WHEN T7.IS_PROJECT_COMPLETED_COUNT IS NOT NULL THEN false ELSE true END) AS IS_PROJECT_COMPLETED,
            T8.CONFLICT_STATUS_COUNT, 
			T2.COI_PROJECT_TYPE_CODE, 
			T9.DESCRIPTION AS COI_PROJECT_TYPE,
            T9.BADGE_COLOR 
			FROM COI_DISCL_ENT_PROJ_DETAILS T1
            INNER JOIN COI_PROJECT_PROPOSAL_V T2 ON T2.EXTERNAL_SYSTEM_REF_ID=T1.MODULE_ITEM_KEY
            INNER JOIN EPS_PROP_PERSON_ROLE T3 ON T2.KEY_PERSON_ROLE_CODE = T3.PROP_PERSON_ROLE_ID
            LEFT JOIN (SELECT T5.DISCLOSURE_DETAILS_ID, T5.MODULE_ITEM_KEY, T5.MODULE_CODE, T5.PROJECT_CONFLICT_STATUS_CODE
				FROM COI_DISCL_ENT_PROJ_DETAILS T5
				JOIN (
				SELECT MODULE_ITEM_KEY, MODULE_CODE, MAX(PROJECT_CONFLICT_STATUS_CODE) AS MAX_CONFLICT_STATUS_CODE
				FROM COI_DISCL_ENT_PROJ_DETAILS
				WHERE DISCLOSURE_ID = AV_DISCLOSURE_ID AND MODULE_CODE = 3
				GROUP BY MODULE_ITEM_KEY, MODULE_CODE
				) subquery ON T5.MODULE_ITEM_KEY = subquery.MODULE_ITEM_KEY
				AND T5.MODULE_CODE = subquery.MODULE_CODE
				AND T5.PROJECT_CONFLICT_STATUS_CODE = subquery.MAX_CONFLICT_STATUS_CODE
				WHERE T5.DISCLOSURE_ID = AV_DISCLOSURE_ID AND T5.MODULE_CODE = 3  )T4 ON T4.MODULE_ITEM_KEY = T2.EXTERNAL_SYSTEM_REF_ID
            LEFT JOIN (SELECT MODULE_ITEM_KEY, 
						MODULE_CODE, 
						COUNT(T5.PERSON_ENTITY_ID) AS RELATIONSHIP_COUNT
                        FROM COI_DISCL_ENT_PROJ_DETAILS T5
                        WHERE T5.DISCLOSURE_ID= AV_DISCLOSURE_ID 
						AND MODULE_CODE = 3 
						AND T5.PERSON_ENTITY_ID IS NOT NULL
                        GROUP BY MODULE_ITEM_KEY, MODULE_CODE )T5 ON T5.MODULE_ITEM_KEY = T2.EXTERNAL_SYSTEM_REF_ID
            LEFT JOIN COI_PROJ_CONFLICT_STATUS_TYPE T6 ON T6.PROJECT_CONFLICT_STATUS_CODE = T4.PROJECT_CONFLICT_STATUS_CODE
            LEFT JOIN (SELECT COUNT(*) AS IS_PROJECT_COMPLETED_COUNT, 
						C2.MODULE_ITEM_KEY 
						FROM COI_DISCL_ENT_PROJ_DETAILS C2
                        WHERE C2.PROJECT_CONFLICT_STATUS_CODE IS NULL 
						AND C2.DISCLOSURE_ID = AV_DISCLOSURE_ID
                        AND C2.MODULE_CODE = 3 GROUP BY C2.MODULE_ITEM_KEY ) T7 ON T7.MODULE_ITEM_KEY = T2.EXTERNAL_SYSTEM_REF_ID
            LEFT JOIN (SELECT CONCAT('[', GROUP_CONCAT(CONCAT('{', CONFLICT_STATUS_GROUP, ': ', PROJECT_COUNT, '}')), ']') AS CONFLICT_STATUS_COUNT,
                            MODULE_ITEM_KEY FROM ( SELECT CASE
                                    WHEN C2.PROJECT_CONFLICT_STATUS_CODE BETWEEN 100 AND 199 THEN 1
                                    WHEN C2.PROJECT_CONFLICT_STATUS_CODE BETWEEN 200 AND 299 THEN 2
                                    WHEN C2.PROJECT_CONFLICT_STATUS_CODE BETWEEN 300 AND 399 THEN 3
									END AS CONFLICT_STATUS_GROUP, COUNT(*) AS PROJECT_COUNT , C2.MODULE_ITEM_KEY
									FROM COI_DISCL_ENT_PROJ_DETAILS C2
									WHERE C2.PROJECT_CONFLICT_STATUS_CODE IS NOT NULL
									AND C2.DISCLOSURE_ID = AV_DISCLOSURE_ID
									AND C2.MODULE_CODE = 3
									GROUP BY CONFLICT_STATUS_GROUP, C2.MODULE_ITEM_KEY
									) AS grouped 
							GROUP BY MODULE_ITEM_KEY) T8 ON T8.MODULE_ITEM_KEY = T2.EXTERNAL_SYSTEM_REF_ID
            LEFT JOIN COI_PROJECT_TYPE T9 ON T9.COI_PROJECT_TYPE_CODE = T2.COI_PROJECT_TYPE_CODE
            WHERE T1.MODULE_CODE = 3 AND T1.DISCLOSURE_ID= AV_DISCLOSURE_ID AND T2.KEY_PERSON_ID = LS_PERSON_ID

            UNION

            SELECT DISTINCT T2.EXTERNAL_SYSTEM_REF_ID AS PROJECT_ID, 
			T2.PROPOSAL_NUMBER AS PROJECT_NUMBER,
             T2.TITLE, 
			 T2.PROPOSAL_STATUS AS STATUS, 
			 T2.PROPOSAL_START_DATE AS START_DATE,
                    T2.PROPOSAL_END_DATE AS END_DATE, 
					T2.LEAD_UNIT_NUMBER AS HOME_UNIT_NUMBER, 
					T2.LEAD_UNIT_NAME AS HOME_UNIT_NAME,
                        T2.SPONSOR_NAME, 
						T2.PRIME_SPONSOR_NAME, 
						T2.PI_NAME AS PI, 
						T2.KEY_PERSON_NAME AS KEY_PERSON, 
						T2.KEY_PERSON_ID, 
						T3.DESCRIPTION AS REPORTER_ROLE,
                        T6.DESCRIPTION AS CONFLICT_DESCRIPTION, 
						T4.PROJECT_CONFLICT_STATUS_CODE, 
						(CASE WHEN T5.RELATIONSHIP_COUNT > 0 THEN true ELSE false END) AS RELATIONSHIP_EXISTS,
                        T1.MODULE_CODE, 
						(CASE WHEN T7.IS_PROJECT_COMPLETED_COUNT IS NOT NULL THEN false ELSE true END) AS IS_PROJECT_COMPLETED,
                        T8.CONFLICT_STATUS_COUNT, 
						T2.COI_PROJECT_TYPE_CODE, 
						T9.DESCRIPTION AS COI_PROJECT_TYPE,
                        T9.BADGE_COLOR 
						FROM COI_DISCL_ENT_PROJ_DETAILS T1
                        INNER JOIN coi_project_institute_proposal_v T2 ON T2.EXTERNAL_SYSTEM_REF_ID=T1.MODULE_ITEM_KEY
                        INNER JOIN EPS_PROP_PERSON_ROLE T3 ON T2.KEY_PERSON_ROLE_CODE = T3.PROP_PERSON_ROLE_ID
                        LEFT JOIN (SELECT T5.DISCLOSURE_DETAILS_ID, T5.MODULE_ITEM_KEY, T5.MODULE_CODE, T5.PROJECT_CONFLICT_STATUS_CODE
							FROM COI_DISCL_ENT_PROJ_DETAILS T5
							JOIN (
							SELECT MODULE_ITEM_KEY, MODULE_CODE, MAX(PROJECT_CONFLICT_STATUS_CODE) AS MAX_CONFLICT_STATUS_CODE
							FROM COI_DISCL_ENT_PROJ_DETAILS
							WHERE DISCLOSURE_ID = AV_DISCLOSURE_ID AND MODULE_CODE = 2
							GROUP BY MODULE_ITEM_KEY, MODULE_CODE
							) subquery ON T5.MODULE_ITEM_KEY = subquery.MODULE_ITEM_KEY
							AND T5.MODULE_CODE = subquery.MODULE_CODE
							AND T5.PROJECT_CONFLICT_STATUS_CODE = subquery.MAX_CONFLICT_STATUS_CODE
							WHERE T5.DISCLOSURE_ID = AV_DISCLOSURE_ID AND T5.MODULE_CODE = 2 )T4 ON T4.MODULE_ITEM_KEY = T2.EXTERNAL_SYSTEM_REF_ID
                        LEFT JOIN (SELECT MODULE_ITEM_KEY, 
									MODULE_CODE, 
									COUNT(T5.PERSON_ENTITY_ID) AS RELATIONSHIP_COUNT
                                    FROM COI_DISCL_ENT_PROJ_DETAILS T5
                                    WHERE T5.DISCLOSURE_ID= AV_DISCLOSURE_ID 
									AND MODULE_CODE = 2 
									AND T5.PERSON_ENTITY_ID IS NOT NULL
                                    GROUP BY MODULE_ITEM_KEY, MODULE_CODE )T5 ON T5.MODULE_ITEM_KEY = T2.EXTERNAL_SYSTEM_REF_ID
                        LEFT JOIN COI_PROJ_CONFLICT_STATUS_TYPE T6 ON T6.PROJECT_CONFLICT_STATUS_CODE = T4.PROJECT_CONFLICT_STATUS_CODE
                        LEFT JOIN (SELECT COUNT(*) AS IS_PROJECT_COMPLETED_COUNT, 
									C2.MODULE_ITEM_KEY FROM COI_DISCL_ENT_PROJ_DETAILS C2
                                    WHERE C2.PROJECT_CONFLICT_STATUS_CODE IS NULL 
									AND C2.DISCLOSURE_ID = AV_DISCLOSURE_ID
                                    AND C2.MODULE_CODE = 2 GROUP BY C2.MODULE_ITEM_KEY ) T7 ON T7.MODULE_ITEM_KEY = T2.EXTERNAL_SYSTEM_REF_ID
                        LEFT JOIN (SELECT CONCAT('[', GROUP_CONCAT(CONCAT('{', CONFLICT_STATUS_GROUP, ': ', PROJECT_COUNT, '}')), ']') AS CONFLICT_STATUS_COUNT,
                                        MODULE_ITEM_KEY FROM ( SELECT CASE
																WHEN C2.PROJECT_CONFLICT_STATUS_CODE BETWEEN 100 AND 199 THEN 1
																WHEN C2.PROJECT_CONFLICT_STATUS_CODE BETWEEN 200 AND 299 THEN 2
																WHEN C2.PROJECT_CONFLICT_STATUS_CODE BETWEEN 300 AND 399 THEN 3
																END AS CONFLICT_STATUS_GROUP, COUNT(*) AS PROJECT_COUNT , C2.MODULE_ITEM_KEY
																FROM COI_DISCL_ENT_PROJ_DETAILS C2
																WHERE C2.PROJECT_CONFLICT_STATUS_CODE IS NOT NULL
																AND C2.DISCLOSURE_ID = AV_DISCLOSURE_ID
																AND C2.MODULE_CODE = 2
																GROUP BY CONFLICT_STATUS_GROUP, C2.MODULE_ITEM_KEY
                                    ) AS grouped GROUP BY MODULE_ITEM_KEY) T8 ON T8.MODULE_ITEM_KEY = T2.EXTERNAL_SYSTEM_REF_ID
                        LEFT JOIN COI_PROJECT_TYPE T9 ON T9.COI_PROJECT_TYPE_CODE = T2.COI_PROJECT_TYPE_CODE
                        WHERE T1.MODULE_CODE = 2 AND T1.DISCLOSURE_ID= AV_DISCLOSURE_ID AND T2.KEY_PERSON_ID = LS_PERSON_ID; 
END
//

