DELIMITER $$
CREATE PROCEDURE `INSERT_WORKFLOW_FOR_ROLE_TYPE`(
	AV_MODULE_CODE DECIMAL(38, 0),
	AV_MODULE_ITEM_ID VARCHAR(20),
	AV_ROLE_TYPE DECIMAL(38, 0),
	AV_WORKFLOW_ID INT(6),
	AV_MAPID INT(6),
	AV_MAP_NUMBER INT(3),
	AV_APPROVAL_STOP_NUMBER INT(3),
	AV_APPROVER_NUMBER INT(3),
	AV_PRIMARY_APPROVER_FLAG VARCHAR(1),
	AV_APPROVAL_STATUS VARCHAR(1),
	AV_UPDATE_USER VARCHAR(100),
	AV_SUBMODULE_CODE DECIMAL(3, 0),
	AV_SUB_MODULE_ITEM_KEY VARCHAR(20),
	AV_RULE_ID INT(6),
	AV_STOP_NAME VARCHAR(200),
	AV_MAP_NAME VARCHAR(200),
	AV_MAP_DESCRIPTION VARCHAR(200)
)
    DETERMINISTIC
BEGIN
DECLARE LI_RETURN DECIMAL(38, 0);
DECLARE LS_LEAD_UNIT VARCHAR(8);
DECLARE LS_UNIT_ADMIN_TYPE_CODE DECIMAL(38, 0);
DECLARE LL_PERSON_ID VARCHAR(40);
DECLARE LL_FULL_NAME VARCHAR(90);
DECLARE LL_EMAIL_ADDRESS VARCHAR(60);
DECLARE LI_LOOP_NUMBER DECIMAL(38, 0);
DECLARE LI_TEMP_PERSON_ID LONGTEXT;
DECLARE TMP_PERSON_LIST LONGTEXT;
DECLARE LI_PERSON VARCHAR(40);
DECLARE LS_MAP_TYPE VARCHAR(1);
DECLARE LS_CODE CHAR(5) DEFAULT '00000';
DECLARE LS_MSG TEXT;
DECLARE ls_create_user VARCHAR(60);
DECLARE LI_FLAG INT;
DECLARE LS_AWARD_ID VARCHAR(22);
DECLARE LS_ACTIVE_AWARD_ID VARCHAR(22);
DECLARE LI_FUNDING_SCHEME_ID INT;
DECLARE LI_PERSON_COUNT INT;
DECLARE LS_ADMIN_PERSON_ID VARCHAR(22);
DECLARE CONTINUE HANDLER FOR SQLEXCEPTION 
BEGIN
	GET DIAGNOSTICS CONDITION 1 LS_CODE = RETURNED_SQLSTATE,LS_MSG = MESSAGE_TEXT;
END;
SET SQL_SAFE_UPDATES = 0;
	SELECT MAP_TYPE INTO LS_MAP_TYPE
	FROM WORKFLOW_MAP
	WHERE MAP_ID = AV_MAPID;

IF AV_MODULE_CODE = 1 THEN
	SELECT T2.AWARD_ID INTO LS_ACTIVE_AWARD_ID
	FROM AWARD T1,
	AWARD T2
	WHERE T1.AWARD_ID = AV_MODULE_ITEM_ID
	AND T1.AWARD_NUMBER = T2.AWARD_NUMBER
	AND T2.AWARD_SEQUENCE_STATUS = 'ACTIVE';
	IF LS_ACTIVE_AWARD_ID IS NOT NULL THEN
		SET AV_MODULE_ITEM_ID = LS_ACTIVE_AWARD_ID;
	END IF;
		SELECT LEAD_UNIT_NUMBER INTO LS_LEAD_UNIT
		FROM AWARD
		WHERE AWARD_ID = AV_MODULE_ITEM_ID;

ELSEIF AV_MODULE_CODE = 3 THEN
	SELECT HOME_UNIT_NUMBER INTO LS_LEAD_UNIT
	FROM EPS_PROPOSAL
	WHERE PROPOSAL_ID = AV_MODULE_ITEM_ID;

ELSEIF AV_MODULE_CODE = 13 THEN
	SELECT UNIT_NUMBER INTO LS_LEAD_UNIT
	FROM AGREEMENT_HEADER
	WHERE AGREEMENT_REQUEST_ID = AV_MODULE_ITEM_ID;

ELSEIF AV_MODULE_CODE = 14 THEN
	SELECT LEAD_UNIT_NUMBER,AWARD_ID INTO LS_LEAD_UNIT,LS_AWARD_ID
	FROM AWARD
	WHERE AWARD_ID IN(
		SELECT AWARD_ID
		FROM CLAIM
		WHERE CLAIM_ID = AV_MODULE_ITEM_ID
	);

ELSEIF AV_MODULE_CODE = 16 THEN
	SELECT LEAD_UNIT_NUMBER,AWARD_ID INTO LS_LEAD_UNIT,LS_AWARD_ID
	FROM AWARD
	WHERE AWARD_ID IN(
		SELECT AWARD_ID
		FROM AWARD_PROGRESS_REPORT
		WHERE PROGRESS_REPORT_ID = AV_MODULE_ITEM_ID
	);
END IF;

IF AV_ROLE_TYPE IN (1, 3) THEN
	SELECT GROUP_CONCAT(T1.PERSON_ID) INTO LI_TEMP_PERSON_ID
	FROM UNIT_ADMINISTRATOR T1
	INNER JOIN PERSON T2 ON T2.PERSON_ID = T1.PERSON_ID
	WHERE T1.UNIT_NUMBER = LS_LEAD_UNIT
	AND T1.UNIT_ADMINISTRATOR_TYPE_CODE = (
		SELECT (
				CASE
					AV_ROLE_TYPE
					WHEN 1 THEN 1
					WHEN 3 THEN 4
				END
			) AS UNIT_ADMINISTRATOR_TYPE_CODE
		FROM DUAL
	)
	AND T2.STATUS = 'A';

ELSEIF AV_ROLE_TYPE = 2 THEN
	SELECT GROUP_CONCAT(distinct T1.PERSON_ID) INTO LI_TEMP_PERSON_ID
	FROM UNIT_ADMINISTRATOR T1
	INNER JOIN PERSON T2 ON T2.PERSON_ID = T1.PERSON_ID
	WHERE T1.UNIT_ADMINISTRATOR_TYPE_CODE = 5
	AND T1.UNIT_NUMBER = LS_LEAD_UNIT
	AND T2.STATUS = 'A';

ELSEIF AV_ROLE_TYPE = 20 THEN
	SELECT GROUP_CONCAT(distinct T1.PERSON_ID) INTO LI_TEMP_PERSON_ID
	FROM UNIT_ADMINISTRATOR T1
	INNER JOIN PERSON T2 ON T2.PERSON_ID = T1.PERSON_ID
	WHERE T1.UNIT_ADMINISTRATOR_TYPE_CODE = 3
	AND T1.UNIT_NUMBER = LS_LEAD_UNIT
	AND T2.STATUS = 'A';

ELSEIF AV_ROLE_TYPE = 21 THEN
	SELECT GROUP_CONCAT(distinct T1.PERSON_ID) INTO LI_TEMP_PERSON_ID
	FROM UNIT_ADMINISTRATOR T1
	INNER JOIN PERSON T2 ON T2.PERSON_ID = T1.PERSON_ID
	WHERE T1.UNIT_ADMINISTRATOR_TYPE_CODE = 5
	AND T1.UNIT_NUMBER = '208'
	AND T2.STATUS = 'A';

ELSEIF AV_ROLE_TYPE = 4 THEN 
	IF AV_MODULE_CODE = 5 THEN
		SELECT GROUP_CONCAT(T1.NEGOTIATOR_PERSON_ID) INTO LI_TEMP_PERSON_ID
		FROM NEGOTIATION T1
		INNER JOIN PERSON T2 ON T2.PERSON_ID = T1.NEGOTIATOR_PERSON_ID
		WHERE T1.NEGOTIATION_ID = AV_MODULE_ITEM_ID
		AND T2.STATUS = 'A';
	ELSEIF AV_MODULE_CODE = 13 THEN
		SELECT GROUP_CONCAT(T1.PERSON_ID) INTO LI_TEMP_PERSON_ID
		FROM AGREEMENT_PEOPLE T1
		INNER JOIN PERSON T2 ON T1.PERSON_ID = T2.PERSON_ID
		WHERE T1.AGREEMENT_REQUEST_ID = AV_MODULE_ITEM_ID
		AND T1.PEOPLE_TYPE_ID = 2
		AND T2.STATUS = 'A';
	END IF;

ELSEIF AV_ROLE_TYPE = 5 THEN 

	IF AV_MODULE_CODE = 1 THEN
		SELECT GROUP_CONCAT(T2.PERSON_ID) INTO LI_TEMP_PERSON_ID
		FROM AWARD T1
		INNER JOIN AWARD_PERSONS T2 ON T1.AWARD_ID = T2.AWARD_ID
		INNER JOIN PERSON T3 ON T3.PERSON_ID = T2.PERSON_ID
		WHERE T1.AWARD_ID = AV_MODULE_ITEM_ID
		AND T2.PERSON_ROLE_ID = 3
		AND T3.STATUS = 'A';

	ELSEIF AV_MODULE_CODE = 3 THEN
		SELECT GROUP_CONCAT(T2.PERSON_ID) INTO LI_TEMP_PERSON_ID
		FROM EPS_PROPOSAL T1
		INNER JOIN EPS_PROPOSAL_PERSONS T2 ON T1.PROPOSAL_ID = T2.PROPOSAL_ID
		AND T2.PROP_PERSON_ROLE_ID = 3
		INNER JOIN PERSON T3 ON T2.PERSON_ID = T3.PERSON_ID
		WHERE T1.PROPOSAL_ID = AV_MODULE_ITEM_ID
		AND T3.STATUS = 'A';

	ELSEIF AV_MODULE_CODE = 14 THEN
		SELECT GROUP_CONCAT(T2.PERSON_ID) INTO LI_TEMP_PERSON_ID
		FROM CLAIM T
		INNER JOIN AWARD T1 ON T.AWARD_ID = T1.AWARD_ID
		INNER JOIN AWARD_PERSONS T2 ON T1.AWARD_ID = T2.AWARD_ID
		INNER JOIN PERSON T3 ON T2.PERSON_ID = T3.PERSON_ID
		WHERE T.CLAIM_ID = AV_MODULE_ITEM_ID
		AND T2.PERSON_ROLE_ID = 3
		AND T3.STATUS = 'A';

	ELSEIF AV_MODULE_CODE = 16 THEN
		SELECT GROUP_CONCAT(T2.PERSON_ID) INTO LI_TEMP_PERSON_ID
		FROM AWARD_PROGRESS_REPORT T
		INNER JOIN AWARD T1 ON T.AWARD_ID = T1.AWARD_ID
		INNER JOIN AWARD_PERSONS T2 ON T1.AWARD_ID = T2.AWARD_ID
		INNER JOIN PERSON T3 ON T2.PERSON_ID = T3.PERSON_ID
		WHERE T.PROGRESS_REPORT_ID = AV_MODULE_ITEM_ID
		AND T2.PERSON_ROLE_ID = 3
		AND T3.STATUS = 'A';

	ELSEIF AV_MODULE_CODE = 13 THEN
		SELECT GROUP_CONCAT(T2.PERSON_ID) INTO LI_TEMP_PERSON_ID
		FROM AGREEMENT_PEOPLE T1
		INNER JOIN PERSON T2 ON T1.PERSON_ID = T2.PERSON_ID
		WHERE T1.AGREEMENT_REQUEST_ID = AV_MODULE_ITEM_ID
		AND T1.PEOPLE_TYPE_ID = 3
		AND T2.STATUS = 'A';
	END IF;

ELSEIF AV_ROLE_TYPE = 6 THEN 

	IF AV_MODULE_CODE = 1 THEN
		SELECT GROUP_CONCAT(T2.PERSON_ID) INTO LI_TEMP_PERSON_ID
		FROM AWARD T1
		INNER JOIN AWARD_PERSONS T2 ON T1.AWARD_ID = T2.AWARD_ID
		INNER JOIN PERSON T3 ON T2.PERSON_ID = T3.PERSON_ID
		WHERE T1.AWARD_ID = AV_MODULE_ITEM_ID
		AND T2.STATUS = 'A';

	ELSEIF AV_MODULE_CODE = 3 THEN
		SELECT GROUP_CONCAT(T1.PERSON_ID) INTO LI_TEMP_PERSON_ID
		FROM EPS_PROPOSAL_PERSONS T1
		INNER JOIN PERSON T2 ON T2.PERSON_ID = T1.PERSON_ID
		WHERE T1.PROPOSAL_ID = AV_MODULE_ITEM_ID
		AND T2.STATUS = 'A';
	END IF;

ELSEIF AV_ROLE_TYPE = 8 THEN 
	IF AV_MODULE_CODE = 3 THEN
	SELECT GROUP_CONCAT(T2.PERSON_ID) INTO LI_TEMP_PERSON_ID
	FROM PERSON T2
	WHERE T2.PERSON_ID in (
			SELECT S2.SUPERVISOR_PERSON_ID
			FROM EPS_PROPOSAL_PERSONS S1
				INNER JOIN PERSON S2 ON S1.PERSON_ID = S2.PERSON_ID
			WHERE S1.PROPOSAL_ID = AV_MODULE_ITEM_ID
				AND S1.PROP_PERSON_ROLE_ID = 3
				AND S2.STATUS = 'A'
		);
	END IF;

ELSEIF AV_ROLE_TYPE = 9 THEN
	SELECT GROUP_CONCAT(T2.PERSON_ID) INTO LI_TEMP_PERSON_ID
	FROM PERSON T2
	WHERE T2.DIRECTORY_TITLE = 'Faculty'
	OR T2.IS_FACULTY = 'Y'
	AND T2.STATUS = 'A';

ELSEIF AV_ROLE_TYPE = 10 THEN
	SELECT GROUP_CONCAT(T2.PERSON_ID) INTO LI_TEMP_PERSON_ID
	FROM PERSON T2
	WHERE T2.DIRECTORY_TITLE = 'Research Staff'
	OR T2.IS_RESEARCH_STAFF = 'Y'
	AND T2.STATUS = 'A';
ELSEIF AV_ROLE_TYPE = 11 THEN 

IF AV_MODULE_CODE IN(1, 14, 16) THEN 
	SELECT COUNT(1) INTO LI_FLAG
	FROM UNIT
	WHERE UNIT_NUMBER = LS_LEAD_UNIT
	AND UNIT_NUMBER NOT IN (
		SELECT UNIT_NUMBER
		FROM UNIT
		WHERE UNIT_NUMBER in ('495', '259', '396')
			OR PARENT_UNIT_NUMBER IN ('495', '259', '396')
	);
	IF LI_FLAG > 0 THEN
		SELECT GROUP_CONCAT(distinct T1.PERSON_ID) INTO LI_TEMP_PERSON_ID
		FROM PERSON_ROLES T1
		INNER JOIN PERSON T2 ON T1.PERSON_ID = T2.PERSON_ID
		WHERE T1.ROLE_ID = '660'
		AND T1.UNIT_NUMBER = '000001'
		AND T2.STATUS = 'A';
	ELSE 
		SELECT COUNT(1) INTO LI_FLAG
		FROM UNIT
		WHERE UNIT_NUMBER = LS_LEAD_UNIT
		AND UNIT_NUMBER IN (
		SELECT UNIT_NUMBER
		FROM UNIT
		WHERE UNIT_NUMBER = '495'
		OR PARENT_UNIT_NUMBER = '495'
		);
	IF LI_FLAG > 0 THEN
		SELECT GROUP_CONCAT(distinct T1.PERSON_ID) INTO LI_TEMP_PERSON_ID
		FROM PERSON_ROLES T1
		INNER JOIN PERSON T2 ON T1.PERSON_ID = T2.PERSON_ID
		WHERE T1.ROLE_ID = '660'
		AND T1.UNIT_NUMBER = '495'
		AND T2.STATUS = 'A';
	ELSE 
		SELECT COUNT(1) INTO LI_FLAG
		FROM UNIT
		WHERE UNIT_NUMBER = LS_LEAD_UNIT
		AND UNIT_NUMBER IN (
			SELECT UNIT_NUMBER
			FROM UNIT
			WHERE UNIT_NUMBER = '259'
			OR PARENT_UNIT_NUMBER = '259'
		);
	IF LI_FLAG > 0 THEN
		SELECT GROUP_CONCAT(distinct T1.PERSON_ID) INTO LI_TEMP_PERSON_ID
		FROM PERSON_ROLES T1
		INNER JOIN PERSON T2 ON T1.PERSON_ID = T2.PERSON_ID
		WHERE T1.ROLE_ID = '660'
		AND T1.UNIT_NUMBER = '259'
		AND T2.STATUS = 'A';
	ELSE 
		SELECT COUNT(1) INTO LI_FLAG
		FROM UNIT
		WHERE UNIT_NUMBER = LS_LEAD_UNIT
		AND UNIT_NUMBER IN (
		SELECT UNIT_NUMBER
		FROM UNIT
		WHERE UNIT_NUMBER = '396'
			OR PARENT_UNIT_NUMBER = '396'
	);
	IF LI_FLAG > 0 THEN
		SELECT GROUP_CONCAT(distinct T1.PERSON_ID) INTO LI_TEMP_PERSON_ID
		FROM PERSON_ROLES T1
		INNER JOIN PERSON T2 ON T1.PERSON_ID = T2.PERSON_ID
		WHERE T1.ROLE_ID = '660'
		AND T1.UNIT_NUMBER = '396'
		AND T2.STATUS = 'A';
	END IF;
END IF;
END IF;
END IF;
ELSE
	SELECT GROUP_CONCAT(distinct T1.PERSON_ID) INTO LI_TEMP_PERSON_ID
	FROM PERSON_ROLES T1
	INNER JOIN PERSON T2 ON T1.PERSON_ID = T2.PERSON_ID
	WHERE T1.ROLE_ID = 660
	AND T2.STATUS = 'A';
END IF;

ELSEIF AV_ROLE_TYPE = 12 THEN 

	IF AV_MODULE_CODE = 3 THEN
		SELECT T2.HOME_UNIT_NUMBER INTO LS_LEAD_UNIT
		FROM EPS_PROPOSAL T1
		LEFT JOIN GRANT_CALL_HEADER T2 ON T1.GRANT_HEADER_ID = T2.GRANT_HEADER_ID
		WHERE T1.PROPOSAL_ID = AV_MODULE_ITEM_ID;
		SELECT GROUP_CONCAT(distinct T1.PERSON_ID) INTO LI_TEMP_PERSON_ID
		FROM PERSON_ROLES T1
		LEFT JOIN PERSON T2 ON T1.PERSON_ID = T2.PERSON_ID
		WHERE ROLE_ID = 400
		AND T1.UNIT_NUMBER = LS_LEAD_UNIT
		AND T2.STATUS = 'A';

	ELSEIF AV_MODULE_CODE = 1 THEN
		SELECT GROUP_CONCAT(distinct T1.PERSON_ID) INTO LI_TEMP_PERSON_ID
		FROM PERSON_ROLES T1
		LEFT JOIN PERSON T2 ON T1.PERSON_ID = T2.PERSON_ID
		WHERE ROLE_ID = 400
		AND T1.UNIT_NUMBER = '252'
		AND T2.STATUS = 'A';
	END IF;

ELSEIF AV_ROLE_TYPE = 13 THEN 

IF AV_MODULE_CODE = 3 THEN
	SELECT T1.FUNDING_SCHEME_ID INTO LI_FUNDING_SCHEME_ID
	FROM GRANT_CALL_HEADER T1
	INNER JOIN EPS_PROPOSAL T2 ON T1.GRANT_HEADER_ID = T2.GRANT_HEADER_ID
	WHERE T2.PROPOSAL_ID = AV_MODULE_ITEM_ID;
	SELECT COUNT(1) INTO LI_PERSON_COUNT
	FROM GRANT_CALL_FUNDING_SCHEME_MANAGERS T1
	INNER JOIN PERSON T2 ON T1.PERSON_ID = T2.PERSON_ID
	WHERE T1.FUNDING_SCHEME_CODE IN (
		SELECT FUNDING_SCHEME_CODE
		FROM SPONSOR_FUNDING_SCHEME
		WHERE FUNDING_SCHEME_ID = LI_FUNDING_SCHEME_ID
	)
	AND T2.STATUS = 'A';
	IF LI_PERSON_COUNT = 0 THEN
		SELECT COUNT(1) INTO LI_FLAG
		FROM EPS_PROPOSAL T1
		INNER JOIN GRANT_CALL_HEADER T2 ON T1.GRANT_HEADER_ID = T2.GRANT_HEADER_ID
		WHERE T1.PROPOSAL_ID = AV_MODULE_ITEM_ID
		AND T1.GRANT_HEADER_ID IS NOT NULL
		AND T2.HOME_UNIT_NUMBER <> '000001';
		IF LI_FLAG > 0 THEN
			SELECT T2.HOME_UNIT_NUMBER INTO LS_LEAD_UNIT
			FROM EPS_PROPOSAL T1
			INNER JOIN GRANT_CALL_HEADER T2 ON T1.GRANT_HEADER_ID = T2.GRANT_HEADER_ID
			WHERE T1.PROPOSAL_ID = AV_MODULE_ITEM_ID;
		ELSE
			SELECT T1.HOME_UNIT_NUMBER INTO LS_LEAD_UNIT
			FROM EPS_PROPOSAL T1
			WHERE T1.PROPOSAL_ID = AV_MODULE_ITEM_ID;
		END IF;
		SELECT GROUP_CONCAT(distinct T1.PERSON_ID) INTO LI_TEMP_PERSON_ID
		FROM PERSON_ROLES T1
		INNER JOIN PERSON T2 ON T1.PERSON_ID = T2.PERSON_ID
		WHERE T1.ROLE_ID = 100
		AND T1.UNIT_NUMBER = LS_LEAD_UNIT
		AND T2.STATUS = 'A';
	ELSE
		SELECT GROUP_CONCAT(distinct T1.PERSON_ID) INTO LI_TEMP_PERSON_ID
		FROM GRANT_CALL_FUNDING_SCHEME_MANAGERS T1
		INNER JOIN PERSON T2 ON T1.PERSON_ID = T2.PERSON_ID
		WHERE T1.FUNDING_SCHEME_CODE IN (
			SELECT FUNDING_SCHEME_CODE
			FROM SPONSOR_FUNDING_SCHEME
			WHERE FUNDING_SCHEME_ID = LI_FUNDING_SCHEME_ID
		)
		AND T2.STATUS = 'A';
	END IF;

ELSEIF AV_MODULE_CODE = 1 THEN
	SELECT T1.FUNDING_SCHEME_ID INTO LI_FUNDING_SCHEME_ID
	FROM GRANT_CALL_HEADER T1
	INNER JOIN AWARD T2 ON T1.GRANT_HEADER_ID = T2.GRANT_HEADER_ID
	WHERE T2.AWARD_ID = AV_MODULE_ITEM_ID;
	SELECT COUNT(1) INTO LI_PERSON_COUNT
	FROM GRANT_CALL_FUNDING_SCHEME_MANAGERS T1
	INNER JOIN PERSON T2 ON T1.PERSON_ID = T2.PERSON_ID
	WHERE T1.FUNDING_SCHEME_CODE IN (
		SELECT FUNDING_SCHEME_CODE
		FROM SPONSOR_FUNDING_SCHEME
		WHERE FUNDING_SCHEME_ID = LI_FUNDING_SCHEME_ID
	)
	AND T2.STATUS = 'A';
	IF LI_PERSON_COUNT = 0 THEN
		SELECT COUNT(1) INTO LI_FLAG
		FROM AWARD T1
		INNER JOIN GRANT_CALL_HEADER T2 ON T1.GRANT_HEADER_ID = T2.GRANT_HEADER_ID
		WHERE T1.AWARD_ID = AV_MODULE_ITEM_ID
		AND T1.GRANT_HEADER_ID IS NOT NULL
		AND T2.HOME_UNIT_NUMBER <> '000001';
		IF LI_FLAG > 0 THEN
			SELECT T2.HOME_UNIT_NUMBER INTO LS_LEAD_UNIT
			FROM AWARD T1
			INNER JOIN GRANT_CALL_HEADER T2 ON T1.GRANT_HEADER_ID = T2.GRANT_HEADER_ID
			WHERE T1.AWARD_ID = AV_MODULE_ITEM_ID;
		ELSE
			SELECT T1.LEAD_UNIT_NUMBER INTO LS_LEAD_UNIT
			FROM AWARD T1
			WHERE T1.AWARD_ID = AV_MODULE_ITEM_ID;
		END IF;
		SELECT GROUP_CONCAT(distinct T1.PERSON_ID) INTO LI_TEMP_PERSON_ID
		FROM PERSON_ROLES T1
		LEFT JOIN PERSON T2 ON T1.PERSON_ID = T2.PERSON_ID
		WHERE T1.ROLE_ID = 100
		AND T1.UNIT_NUMBER = LS_LEAD_UNIT
		AND T2.STATUS = 'A';
	ELSE
		SELECT GROUP_CONCAT(distinct T1.PERSON_ID) INTO LI_TEMP_PERSON_ID
		FROM GRANT_CALL_FUNDING_SCHEME_MANAGERS T1
		INNER JOIN PERSON T2 ON T1.PERSON_ID = T2.PERSON_ID
		WHERE T1.FUNDING_SCHEME_CODE IN (
			SELECT FUNDING_SCHEME_CODE
			FROM SPONSOR_FUNDING_SCHEME
			WHERE FUNDING_SCHEME_ID = LI_FUNDING_SCHEME_ID
		)
		AND T2.STATUS = 'A';
	END IF;

ELSEIF AV_MODULE_CODE in (14, 16) THEN
	SELECT T1.FUNDING_SCHEME_ID INTO LI_FUNDING_SCHEME_ID
	FROM GRANT_CALL_HEADER T1
	INNER JOIN AWARD T2 ON T1.GRANT_HEADER_ID = T2.GRANT_HEADER_ID
	WHERE T2.AWARD_ID = LS_AWARD_ID;
	SELECT COUNT(1) INTO LI_PERSON_COUNT
	FROM GRANT_CALL_FUNDING_SCHEME_MANAGERS T1
	INNER JOIN PERSON T2 ON T1.PERSON_ID = T2.PERSON_ID
	WHERE T1.FUNDING_SCHEME_CODE IN (
		SELECT FUNDING_SCHEME_CODE
		FROM SPONSOR_FUNDING_SCHEME
		WHERE FUNDING_SCHEME_ID = LI_FUNDING_SCHEME_ID
	)
	AND T2.STATUS = 'A';
	IF LI_PERSON_COUNT = 0 THEN
		SELECT COUNT(1) INTO LI_FLAG
		FROM AWARD T1
		INNER JOIN GRANT_CALL_HEADER T2 ON T1.GRANT_HEADER_ID = T2.GRANT_HEADER_ID
		WHERE T1.AWARD_ID = LS_AWARD_ID
		AND T1.GRANT_HEADER_ID IS NOT NULL
		AND T2.HOME_UNIT_NUMBER <> '000001';
		IF LI_FLAG > 0 THEN
			SELECT T2.HOME_UNIT_NUMBER INTO LS_LEAD_UNIT
			FROM AWARD T1
			INNER JOIN GRANT_CALL_HEADER T2 ON T1.GRANT_HEADER_ID = T2.GRANT_HEADER_ID
			WHERE T1.AWARD_ID = LS_AWARD_ID;
		ELSE
			SELECT T1.LEAD_UNIT_NUMBER INTO LS_LEAD_UNIT
			FROM AWARD T1
			WHERE T1.AWARD_ID = LS_AWARD_ID;
		END IF;
		SELECT GROUP_CONCAT(distinct T1.PERSON_ID) INTO LI_TEMP_PERSON_ID
		FROM PERSON_ROLES T1
		LEFT JOIN PERSON T2 ON T1.PERSON_ID = T2.PERSON_ID
		WHERE T1.ROLE_ID = 100
		AND T1.UNIT_NUMBER = LS_LEAD_UNIT
		AND T2.STATUS = 'A';
	ELSE
		SELECT GROUP_CONCAT(distinct T1.PERSON_ID) INTO LI_TEMP_PERSON_ID
		FROM GRANT_CALL_FUNDING_SCHEME_MANAGERS T1
		INNER JOIN PERSON T2 ON T1.PERSON_ID = T2.PERSON_ID
		WHERE T1.FUNDING_SCHEME_CODE IN (
			SELECT FUNDING_SCHEME_CODE
			FROM SPONSOR_FUNDING_SCHEME
			WHERE FUNDING_SCHEME_ID = LI_FUNDING_SCHEME_ID
		)
		AND T2.STATUS = 'A';
	END IF;
END IF;
ELSEIF AV_ROLE_TYPE = 14 THEN
	SELECT GROUP_CONCAT(distinct T1.PERSON_ID) INTO LI_TEMP_PERSON_ID
	FROM PERSON_ROLES T1
	LEFT JOIN PERSON T2 ON T1.PERSON_ID = T2.PERSON_ID
	WHERE T1.ROLE_ID = 120
	AND T1.UNIT_NUMBER = LS_LEAD_UNIT
	AND T2.STATUS = 'A';

ELSEIF AV_ROLE_TYPE = 15 THEN
	SELECT GROUP_CONCAT(distinct T1.PERSON_ID) INTO LI_TEMP_PERSON_ID
	FROM PERSON_ROLES T1
	INNER JOIN PERSON T2 ON T1.PERSON_ID = T2.PERSON_ID
	WHERE T1.ROLE_ID = 28
	AND T1.UNIT_NUMBER = LS_LEAD_UNIT
	AND T2.STATUS = 'A';
ELSEIF AV_ROLE_TYPE = 16 THEN
	SELECT GROUP_CONCAT(distinct T1.PERSON_ID) INTO LI_TEMP_PERSON_ID
	FROM PERSON_ROLES T1
	INNER JOIN PERSON T2 ON T1.PERSON_ID = T2.PERSON_ID
	WHERE ROLE_ID = 100
	AND T1.UNIT_NUMBER = '252'
	AND T2.STATUS = 'A';
ELSEIF AV_ROLE_TYPE = 17 THEN
	SELECT GROUP_CONCAT(distinct T1.PERSON_ID) INTO LI_TEMP_PERSON_ID
	FROM UNIT_ADMINISTRATOR T1
	INNER JOIN PERSON T2 ON T1.PERSON_ID = T2.PERSON_ID
	WHERE UNIT_ADMINISTRATOR_TYPE_CODE = 5
	AND T1.UNIT_NUMBER = '252'
	AND T2.STATUS = 'A';
ELSEIF AV_ROLE_TYPE = 18 THEN
	SELECT GROUP_CONCAT(distinct T1.PERSON_ID) INTO LI_TEMP_PERSON_ID
	FROM PERSON_ROLES T1
	INNER JOIN PERSON T2 ON T1.PERSON_ID = T2.PERSON_ID
	WHERE ROLE_ID = 100
	AND T1.UNIT_NUMBER = '654'
	AND T2.STATUS = 'A';
ELSEIF AV_ROLE_TYPE = 19 THEN
	SELECT GROUP_CONCAT(distinct T1.PERSON_ID) INTO LI_TEMP_PERSON_ID
	FROM UNIT_ADMINISTRATOR T1
	INNER JOIN PERSON T2 ON T1.PERSON_ID = T2.PERSON_ID
	WHERE UNIT_ADMINISTRATOR_TYPE_CODE = 5
	AND T1.UNIT_NUMBER = '654'
	AND T2.STATUS = 'A';
ELSEIF AV_ROLE_TYPE = 35 THEN 
IF AV_MODULE_CODE = 13 THEN
	SELECT ADMIN_PERSON_ID INTO LS_ADMIN_PERSON_ID
	FROM AGREEMENT_HEADER
	WHERE AGREEMENT_REQUEST_ID = AV_MODULE_ITEM_ID;
	IF LS_ADMIN_PERSON_ID IS NULL THEN

		SELECT GROUP_CONCAT(distinct PR.PERSON_ID) INTO LI_TEMP_PERSON_ID
        FROM
        PERSON_ROLES PR,
        (SELECT ROLE_ID,RT.RIGHT_NAME 
           FROM ROLE_RIGHTS RR,
                RIGHTS RT
            WHERE RR.RIGHT_ID = RT.RIGHT_ID
        ) RLE,
        PERSON P
        WHERE (( PR. DESCEND_FLAG = 'Y' AND LS_LEAD_UNIT IN (SELECT CHILD_UNIT_NUMBER
                                                                 FROM UNIT_WITH_CHILDREN 
                                                                                  WHERE UNIT_NUMBER = PR.UNIT_NUMBER
             ))OR ( PR. DESCEND_FLAG = 'N' AND PR.UNIT_NUMBER = LS_LEAD_UNIT )
            )
         AND PR.PERSON_ID=P.PERSON_ID AND P.STATUS='A' AND RLE.ROLE_ID =1300  AND RLE.ROLE_ID = PR.ROLE_ID;
		 	 
	ELSE
		SELECT T1.ADMIN_PERSON_ID INTO LI_TEMP_PERSON_ID
		FROM AGREEMENT_HEADER T1
		INNER JOIN PERSON T2 ON T1.ADMIN_PERSON_ID = T2.PERSON_ID
		WHERE AGREEMENT_REQUEST_ID = AV_MODULE_ITEM_ID
		AND T2.STATUS = 'A';
	END IF;
END IF;

ELSEIF AV_ROLE_TYPE = 22 THEN
	SELECT GROUP_CONCAT(distinct T1.PERSON_ID) INTO LI_TEMP_PERSON_ID
	FROM PERSON_ROLES T1
	INNER JOIN PERSON T2 ON T1.PERSON_ID = T2.PERSON_ID
	WHERE T1.ROLE_ID = 668
	AND T1.UNIT_NUMBER = LS_LEAD_UNIT
	AND T2.STATUS = 'A';
END IF;

SELECT LI_TEMP_PERSON_ID INTO TMP_PERSON_LIST
FROM DUAL;
WHILE TMP_PERSON_LIST != '' DO
SET LI_PERSON = SUBSTRING_INDEX(TMP_PERSON_LIST, ',', 1);
SELECT FULL_NAME,EMAIL_ADDRESS INTO LL_FULL_NAME,LL_EMAIL_ADDRESS
FROM PERSON
WHERE PERSON_ID = LI_PERSON;
INSERT INTO WORKFLOW_DETAIL(
		WORKFLOW_ID,
		MAP_ID,
		MAP_NUMBER,
		APPROVAL_STOP_NUMBER,
		APPROVER_NUMBER,
		PRIMARY_APPROVER_FLAG,
		APPROVER_PERSON_ID,
		APPROVAL_STATUS,
		UPDATE_USER,
		UPDATE_TIMESTAMP,
		ROLE_TYPE_CODE,
		EMAIL_ADDRESS,
		APPROVER_PERSON_NAME,
		STOP_NAME,
		MAP_NAME,
		MAP_DESCRIPTION
	) VALUES(
		AV_WORKFLOW_ID,
		AV_MAPID,
		AV_MAP_NUMBER,
		AV_APPROVAL_STOP_NUMBER,
		AV_APPROVER_NUMBER,
		AV_PRIMARY_APPROVER_FLAG,
		LI_PERSON,
		AV_APPROVAL_STATUS,
		AV_UPDATE_USER,
		utc_timestamp(),
		AV_ROLE_TYPE,
		LL_EMAIL_ADDRESS,
		LL_FULL_NAME,
		AV_STOP_NAME,
		AV_MAP_NAME,
		AV_MAP_DESCRIPTION
	);
SET AV_PRIMARY_APPROVER_FLAG = 'N';
IF LOCATE(',', TMP_PERSON_LIST) > 0 THEN
SET TMP_PERSON_LIST = SUBSTRING(
		TMP_PERSON_LIST,
		LOCATE(',', TMP_PERSON_LIST) + 1
	);
ELSE
SET TMP_PERSON_LIST = '';
END IF;
END WHILE;
END$$
