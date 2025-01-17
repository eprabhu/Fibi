DELIMITER //
CREATE PROCEDURE `INSERT_QUESTIONNAIRE_ANSWERS`(
AV_QUESTIONNAIRE_ANS_HEADER_ID INT,
AV_QUESTION_ID INT,
AV_ANSWER_NUMBER INT,
AV_ANSWER VARCHAR(2000),
AV_ANSWER_LOOKUP_CODE VARCHAR(100),
AV_EXPLANATION VARCHAR(4000),
AV_UPDATE_USER VARCHAR(60)
)
BEGIN

DECLARE	LI_MODULE_ITEM_CODE int(3) ;

SELECT MODULE_ITEM_CODE INTO LI_MODULE_ITEM_CODE FROM QUEST_ANSWER_HEADER WHERE QUESTIONNAIRE_ANS_HEADER_ID = AV_QUESTIONNAIRE_ANS_HEADER_ID;

IF LI_MODULE_ITEM_CODE = 8 THEN

	INSERT INTO COI_QUEST_ANSWER (`QUESTIONNAIRE_ANS_HEADER_ID`, `QUESTION_ID`, `ANSWER_NUMBER`, `ANSWER`, `ANSWER_LOOKUP_CODE`, `EXPLANATION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) 
	VALUES (AV_QUESTIONNAIRE_ANS_HEADER_ID, AV_QUESTION_ID, AV_ANSWER_NUMBER, AV_ANSWER, AV_ANSWER_LOOKUP_CODE, AV_EXPLANATION, UTC_TIMESTAMP(), AV_UPDATE_USER);

	SELECT LAST_INSERT_ID();

ELSE

	INSERT INTO QUEST_ANSWER (`QUESTIONNAIRE_ANS_HEADER_ID`, `QUESTION_ID`, `ANSWER_NUMBER`, `ANSWER`, `ANSWER_LOOKUP_CODE`, `EXPLANATION`, `UPDATE_TIMESTAMP`, `UPDATE_USER`) 
	VALUES (AV_QUESTIONNAIRE_ANS_HEADER_ID, AV_QUESTION_ID, AV_ANSWER_NUMBER, AV_ANSWER, AV_ANSWER_LOOKUP_CODE, AV_EXPLANATION, UTC_TIMESTAMP(), AV_UPDATE_USER);

	SELECT LAST_INSERT_ID();

END IF;
END
//