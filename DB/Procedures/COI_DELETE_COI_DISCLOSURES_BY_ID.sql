DELIMITER //
CREATE PROCEDURE `COI_DELETE_COI_DISCLOSURES_BY_ID`(
AV_DISCLOSURE_ID VARCHAR(60)
)
BEGIN
/**
    To delete FCOI and Project disclosure and its associated data based on disclosure id
*/
	SET FOREIGN_KEY_CHECKS=0; 
	SET SQL_SAFE_UPDATES = 0;

	DELETE FROM COI_DISCL_ENT_PROJ_DETAILS 
    WHERE DISCLOSURE_ID = AV_DISCLOSURE_ID;
    
    DELETE FROM DISCLOSURE_ACTION_LOG 
    WHERE DISCLOSURE_ID = AV_DISCLOSURE_ID;
    
    DELETE FROM COI_CONFLICT_HISTORY 
	WHERE DISCLOSURE_ID = AV_DISCLOSURE_ID;
    
    DELETE FROM COI_CONFLICT_HISTORY 
    WHERE DISCLOSURE_ID = AV_DISCLOSURE_ID;
    
    DELETE FROM COI_REVIEW_COMMENT_TAGS 
    WHERE COI_REVIEW_ID IN (SELECT COI_REVIEW_ID FROM COI_REVIEW WHERE DISCLOSURE_ID = AV_DISCLOSURE_ID);
    
    DELETE FROM COI_REVIEW_ASSIGNEE_HISTORY 
    WHERE COI_REVIEW_ID IN (SELECT COI_REVIEW_ID FROM COI_REVIEW WHERE DISCLOSURE_ID = AV_DISCLOSURE_ID);
    
    DELETE FROM COI_REVIEW_COMMENT_ATTACHMENT 
    WHERE COI_REVIEW_ID IN (SELECT COI_REVIEW_ID FROM COI_REVIEW WHERE DISCLOSURE_ID = AV_DISCLOSURE_ID);

	DELETE FROM COI_REVIEW 
    WHERE DISCLOSURE_ID = AV_DISCLOSURE_ID;
    
    DELETE FROM COI_QUEST_ANSWER_ATTACHMENT 
    WHERE QUESTIONNAIRE_ANSWER_ID IN (select QUESTIONNAIRE_ANSWER_ID from COI_QUEST_ANSWER 
									WHERE QUESTIONNAIRE_ANS_HEADER_ID IN (select QUESTIONNAIRE_ANS_HEADER_ID FROM quest_answer_header 
																			WHERE MODULE_ITEM_CODE = 8 AND MODULE_SUB_ITEM_CODE=0 AND MODULE_ITEM_KEY = AV_DISCLOSURE_ID));
	DELETE FROM COI_QUEST_TABLE_ANSWER 
    WHERE QUESTIONNAIRE_ANS_HEADER_ID IN (select QUESTIONNAIRE_ANS_HEADER_ID FROM quest_answer_header 
											WHERE MODULE_ITEM_CODE = 8 AND MODULE_SUB_ITEM_CODE=0 AND MODULE_ITEM_KEY = AV_DISCLOSURE_ID);
												
	DELETE FROM COI_QUEST_ANSWER 
    WHERE QUESTIONNAIRE_ANS_HEADER_ID IN (select QUESTIONNAIRE_ANS_HEADER_ID FROM quest_answer_header 
											WHERE MODULE_ITEM_CODE = 8 AND MODULE_SUB_ITEM_CODE=0 AND MODULE_ITEM_KEY = AV_DISCLOSURE_ID);
    
    DELETE FROM COI_DISCLOSURE WHERE DISCLOSURE_ID = AV_DISCLOSURE_ID;
    
    DELETE FROM DISCL_COMMENT WHERE MODULE_CODE = 8 AND MODULE_ITEM_KEY = AV_DISCLOSURE_ID;
    
    SET FOREIGN_KEY_CHECKS=1; 
	SET SQL_SAFE_UPDATES = 1;

END
//