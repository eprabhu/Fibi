DELIMITER //
CREATE PROCEDURE `GET_COI_DISCL_REVIEW_DETAILS` (
	AV_COLUMN_NAMES           	VARCHAR(4000),
    AV_MODULE_ITEM_KEY         	INT,
	AV_SUB_MODULE_ITEM_KEY      INT
)
BEGIN

DECLARE LS_DYN_SQL 					LONGTEXT;

SET LS_DYN_SQL = CONCAT('SELECT ', AV_COLUMN_NAMES, ' FROM
	(SELECT t2.FULL_NAME AS ASSIGNEE_NAME, t3.DESCRIPTION AS REVIEW_LOCATION, t4.DESCRIPTION AS REVIEWER_REVIEW_STATUS
	FROM COI_REVIEW t1
	LEFT JOIN PERSON t2 ON t2.PERSON_ID=t1.ASSIGNEE_PERSON_ID
	INNER JOIN COI_REVIEW_LOCATION_TYPE t3 ON t3.LOCATION_TYPE_CODE = t1.LOCATION_TYPE_CODE
	INNER JOIN COI_REVIEWER_STATUS_TYPE t4 ON t4.REVIEW_STATUS_CODE = t1.REVIEW_STATUS_TYPE_CODE
	WHERE t1.DISCLOSURE_ID = ', AV_MODULE_ITEM_KEY, ' AND t1.COI_REVIEW_ID = ',AV_SUB_MODULE_ITEM_KEY,') T');

SET @QUERY_STATEMENT = LS_DYN_SQL;
	PREPARE EXECUTABLE_STAEMENT FROM @QUERY_STATEMENT;
	EXECUTE EXECUTABLE_STAEMENT;
END
//