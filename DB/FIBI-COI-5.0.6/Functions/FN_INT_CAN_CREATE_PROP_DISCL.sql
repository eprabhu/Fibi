DELIMITER //
CREATE FUNCTION FN_INT_CAN_CREATE_PROP_DISCL(
AV_PROPOSAL_NUMBER  VARCHAR(20),
AV_PERSON_ID VARCHAR(40),
AV_QUESTIONNAIRE_ID INT

) RETURNS TINYINT
    DETERMINISTIC
BEGIN

DECLARE LS_COUNT INT;

SELECT COUNT(*) INTO LS_COUNT 
FROM COI_INT_STAGE_DEV_PROPOSAL_PERSON T1
WHERE PROPOSAL_NUMBER = AV_PROPOSAL_NUMBER
AND KEY_PERSON_ID = AV_PERSON_ID
AND DISCLOSURE_REQUIRED_FLAG = 'REQUIRED'; 

IF LS_COUNT = 0 THEN

	RETURN 0;

END IF;

SELECT COUNT(*)  INTO LS_COUNT
FROM COI_DISCLOSURE T1
INNER JOIN COI_DISCL_PROJECTS T2 ON T1.DISCLOSURE_ID = T2.DISCLOSURE_ID
WHERE T1.FCOI_TYPE_CODE = '2'
AND T1.PERSON_ID = AV_PERSON_ID
AND T2.MODULE_CODE = '3'
AND T2.MODULE_ITEM_KEY = AV_PROPOSAL_NUMBER;

IF LS_COUNT > 0 THEN

	RETURN 0;

END IF;

RETURN 1;


END
//
