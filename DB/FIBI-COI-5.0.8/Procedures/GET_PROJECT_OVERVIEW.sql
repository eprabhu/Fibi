DELIMITER //
CREATE PROCEDURE `GET_PROJECT_OVERVIEW`(
AV_FILTER_TYPE            VARCHAR(40),
AV_PROPOSAL_TITLE		  VARCHAR(1000),
AV_PROPOSAL_NUMBER		  VARCHAR(20),
AV_UNIT_NUMBER            VARCHAR(8),
AV_PERSON_ID    		  VARCHAR(40),
AV_PROPOSAL_STATUS        VARCHAR(200),
AV_REVIEW_STATUS 		  VARCHAR(200),
AV_SPONSOR 					VARCHAR(6),
AV_PRIME_SPONSOR 			VARCHAR(6),
AV_PROPOSAL_START_DATE 		VARCHAR(20),
AV_PROPOSAL_END_DATE 		VARCHAR(20),
AV_PAGED                        INT(10),
AV_LIMIT                        INT(10),
AV_UNLIMITED                    BOOLEAN,
AV_TYPE						 VARCHAR(1)
)
BEGIN

DECLARE LS_FILTER_CONDITION 		LONGTEXT;
DECLARE LS_JOIN_CONDITION			LONGTEXT;
DECLARE LS_PAGINATION_CONDITION 	VARCHAR(60);
DECLARE LS_DYN_SQL					LONGTEXT;
DECLARE LS_OFFSET_CONDITION 		VARCHAR(600);
DECLARE LS_OFFSET 					INT(11);

SET LS_DYN_SQL ='';
SET LS_FILTER_CONDITION = '';
SET LS_PAGINATION_CONDITION = '';
SET LS_JOIN_CONDITION = '';
SET LS_OFFSET = (AV_LIMIT * AV_PAGED);

    SET LS_JOIN_CONDITION = CONCAT(LS_JOIN_CONDITION, ' INNER JOIN PERSON T2 ON T1.KEY_PERSON_ID = T2.PERSON_ID
														LEFT JOIN SPONSOR T3 ON T3.SPONSOR_CODE = T1.SPONSOR_CODE
														LEFT JOIN SPONSOR T4 ON T4.SPONSOR_CODE = T1.PRIME_SPONSOR_CODE
														LEFT JOIN (
                                                        SELECT T13.EXTERNAL_SYSTEM_REF_ID, T22.DISCLOSURE_ID, T6.FCOI_TYPE_CODE, T6.REVIEW_STATUS_CODE,
															T6.DISPOSITION_STATUS_CODE, T6.PERSON_ID
															from coi_project_proposal_v T13
                                                            INNER JOIN COI_DISCL_PROJECTS T22
																ON T22.MODULE_ITEM_KEY = T13.EXTERNAL_SYSTEM_REF_ID
																AND T22.MODULE_CODE = T13.COI_PROJECT_TYPE_CODE
                                                            inner join
															COI_DISCLOSURE T6
                                                            on T22.DISCLOSURE_ID = T6.DISCLOSURE_ID
                                                            where T6.DISPOSITION_STATUS_CODE != 2
														) AS T55 ON (T55.EXTERNAL_SYSTEM_REF_ID = T1.EXTERNAL_SYSTEM_REF_ID AND T55.PERSON_ID = T1.KEY_PERSON_ID)
														INNER JOIN eps_prop_person_role T7 ON T7.PROP_PERSON_ROLE_ID = T1.KEY_PERSON_ROLE_CODE
														INNER JOIN UNIT T8 ON T8.UNIT_NUMBER = T2.HOME_UNIT
														LEFT JOIN coi_review_status_type T9 ON T9.REVIEW_STATUS_CODE = T55.REVIEW_STATUS_CODE
														INNER JOIN coi_project_type T10 ON T10.COI_PROJECT_TYPE_CODE = T1.COI_PROJECT_TYPE_CODE
                                                        INNER JOIN coi_project_proposal_status_v T11 ON T11.STATUS_CODE = T1.PROPOSAL_STATUS_CODE
                                                         ');

IF AV_TYPE ='A' THEN

    IF AV_PROPOSAL_TITLE IS NOT NULL AND AV_PROPOSAL_TITLE <> '' THEN
		 SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION, ' AND T1.TITLE LIKE ''%', AV_PROPOSAL_TITLE, '%'' ');
	END IF;

    IF AV_PROPOSAL_NUMBER IS NOT NULL  AND AV_PROPOSAL_NUMBER <> '' THEN
		SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' AND T1.PROPOSAL_NUMBER LIKE ''%',AV_PROPOSAL_NUMBER, '%'' ');
	END IF;
    
    IF AV_UNIT_NUMBER IS NOT NULL AND AV_UNIT_NUMBER <> '' THEN
        SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' AND T1.LEAD_UNIT_NUMBER = ''',AV_UNIT_NUMBER,''' ');
    END IF;

    IF AV_PERSON_ID IS NOT NULL AND AV_PERSON_ID <> '' THEN 
        SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' AND T1.KEY_PERSON_ID = ''',AV_PERSON_ID,''' ');
    END IF;

    IF AV_PROPOSAL_STATUS IS NOT NULL AND AV_PROPOSAL_STATUS <> '' THEN 
        SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' AND T11.STATUS_CODE IN (', AV_PROPOSAL_STATUS, ') ');
    END IF;

    IF AV_REVIEW_STATUS IS NOT NULL AND AV_REVIEW_STATUS <> '' THEN
        SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' AND T55.REVIEW_STATUS_CODE IN (', AV_REVIEW_STATUS, ') ');
    END IF;

    IF AV_SPONSOR IS NOT NULL AND AV_SPONSOR <> '' THEN 
        SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' AND T3.SPONSOR_CODE = ''',AV_SPONSOR,''' ');
    END IF;
    
    IF AV_PRIME_SPONSOR IS NOT NULL AND AV_PRIME_SPONSOR <> '' THEN 
        SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' AND T4.SPONSOR_CODE = ''',AV_PRIME_SPONSOR,''' ');
    END IF;

    IF AV_PROPOSAL_START_DATE IS NOT NULL AND AV_PROPOSAL_START_DATE <> '' THEN
        SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' AND DATE(T1.PROPOSAL_START_DATE) >= DATE_FORMAT(''',AV_PROPOSAL_START_DATE,''',''%Y-%m-%d'') ');
    END IF;

    IF AV_PROPOSAL_END_DATE IS NOT NULL AND AV_PROPOSAL_END_DATE <> '' THEN
        SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION,' AND DATE(T1.PROPOSAL_END_DATE) <= DATE_FORMAT(''',AV_PROPOSAL_END_DATE,''',''%Y-%m-%d'') ');
    END IF;

ELSE

	SET LS_FILTER_CONDITION = CONCAT(LS_FILTER_CONDITION, ' AND T11.STATUS_CODE IN (1,2) AND T1.PROPOSAL_TYPE_CODE = 1 AND (
      EXISTS (
          SELECT 1
          FROM COI_DISCL_PROJECTS T20
          WHERE T20.MODULE_ITEM_KEY = T1.PROPOSAL_NUMBER 
          AND T20.MODULE_CODE = T1.COI_PROJECT_TYPE_CODE
      ) 
      OR T1.SPONSOR_CODE IN (SELECT T21.SPONSOR_CODE FROM SPONSOR_DISCLOSURE_REQUIREMENTS T21)
      OR T1.PRIME_SPONSOR_CODE IN (SELECT T21.SPONSOR_CODE FROM SPONSOR_DISCLOSURE_REQUIREMENTS T21)
  )');

END IF;

IF AV_UNLIMITED = TRUE THEN
        SET LS_DYN_SQL = CONCAT(LS_DYN_SQL, 'SELECT COUNT(*) AS PROPOSAL_COUNT FROM ( SELECT DISTINCT T1.EXTERNAL_SYSTEM_REF_ID AS PROJECT_ID
    FROM coi_project_proposal_v T1 ', LS_JOIN_CONDITION, ' WHERE (T55.FCOI_TYPE_CODE = 2 OR T55.FCOI_TYPE_CODE IS NULL)
    ', LS_FILTER_CONDITION,' ) T');

ELSE


SET LS_DYN_SQL = CONCAT(
    LS_DYN_SQL,
    'WITH DistinctProjects AS (
        SELECT DISTINCT T1.EXTERNAL_SYSTEM_REF_ID AS PROJECT_ID
        FROM coi_project_proposal_v T1 ', LS_JOIN_CONDITION, '
        WHERE (T55.FCOI_TYPE_CODE = 2 OR T55.FCOI_TYPE_CODE IS NULL) ', LS_FILTER_CONDITION, '
        ORDER BY T1.UPDATE_TIMESTAMP DESC
        LIMIT ', AV_LIMIT, ' OFFSET ', LS_OFFSET, '),
	CommentsCount AS (
        SELECT MODULE_ITEM_KEY, MODULE_CODE, COUNT(*) AS COMMENT_COUNT
        FROM coi_project_comment
        where MODULE_ITEM_KEY in (SELECT PROJECT_ID FROM DistinctProjects)
        GROUP BY MODULE_ITEM_KEY, MODULE_CODE
    )'
);

SET LS_DYN_SQL = CONCAT(LS_DYN_SQL, '
SELECT DISTINCT 
    T1.EXTERNAL_SYSTEM_REF_ID AS PROJECT_ID,
    T1.EXTERNAL_SYSTEM_REF_ID AS PROJECT_NUMBER,
    T1.TITLE,
    T1.PI_NAME,
    T1.KEY_PERSON_ID, 
    T1.KEY_PERSON_NAME, 
    T1.KEY_PERSON_ROLE_CODE, 
    T7.DESCRIPTION AS KEY_PERSON_ROLE,
    T1.SPONSOR_NAME, 
    T3.SPONSOR_CODE,
    T1.PRIME_SPONSOR_NAME,
    T4.SPONSOR_CODE AS PRIME_SPONSOR_CODE,
    T1.LEAD_UNIT_NUMBER, 
    T1.LEAD_UNIT_NAME,
    T2.HOME_UNIT,
    T8.UNIT_NAME AS HOME_UNIT_NAME,
    T1.PROPOSAL_START_DATE,
    T1.PROPOSAL_END_DATE,
    T1.PROPOSAL_STATUS AS PROJECT_STATUS,
    T55.DISCLOSURE_ID,
    T9.DESCRIPTION AS DISCLOSURE_REVIEW_STATUS,
    T55.FCOI_TYPE_CODE,
    T1.COI_PROJECT_TYPE_CODE AS PROJECT_TYPE_CODE,
    T10.DESCRIPTION AS PROJECT_TYPE,
    T10.BADGE_COLOR,
    T1.UPDATE_TIMESTAMP,
    T1.CERTIFICATION_FLAG,
    T1.DISCLOSURE_REQUIRED_FLAG,
    COALESCE(CC.COMMENT_COUNT, 0) AS COMMENT_COUNT,
    CASE
        WHEN T55.REVIEW_STATUS_CODE IN (2, 3, 4, 7, 8) THEN "Yes"
        ELSE "No"
    END AS DISCLOSURE_COMPLETION_STATUS,
    T10.PROJECT_ICON
	FROM coi_project_proposal_v T1
	LEFT JOIN CommentsCount CC ON CC.MODULE_ITEM_KEY = T1.EXTERNAL_SYSTEM_REF_ID
		AND CC.MODULE_CODE = T1.COI_PROJECT_TYPE_CODE', LS_JOIN_CONDITION, '
	WHERE T1.EXTERNAL_SYSTEM_REF_ID IN (SELECT PROJECT_ID FROM DistinctProjects)
	AND (T55.FCOI_TYPE_CODE = 2 OR T55.FCOI_TYPE_CODE IS NULL)
    AND T1.KEY_PERSON_STATUS != ''I''
	', LS_FILTER_CONDITION, ' GROUP BY T1.EXTERNAL_SYSTEM_REF_ID , T1.KEY_PERSON_ID ORDER BY T1.UPDATE_TIMESTAMP DESC, T7.SORT_ID ASC;');
 
 END IF;

SET @QUERY_STATEMENT = LS_DYN_SQL;
PREPARE EXECUTABLE_STAEMENT FROM @QUERY_STATEMENT;
EXECUTE EXECUTABLE_STAEMENT;

END
//
