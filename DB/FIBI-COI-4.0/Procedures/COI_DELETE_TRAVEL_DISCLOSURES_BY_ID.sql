DELIMITER //
CREATE PROCEDURE `COI_DELETE_TRAVEL_DISCLOSURES_BY_ID`(
AV_TRAVEL_DISCLOSURE_ID VARCHAR(60)
)
BEGIN
/**
    To delete Travel disclosure and its associated data based on Travel disclosure id
*/

	SET FOREIGN_KEY_CHECKS=0; 
	SET SQL_SAFE_UPDATES = 0;
    
    DELETE FROM COI_TRAVEL_DISCLOSURE_TRAVELER 
    WHERE TRAVEL_DISCLOSURE_ID = AV_TRAVEL_DISCLOSURE_ID;
    
    DELETE FROM TRAVEL_DISCLOSURE_ACTION_LOG 
    WHERE TRAVEL_DISCLOSURE_ID = AV_TRAVEL_DISCLOSURE_ID;
    
    DELETE FROM COI_TRAVEL_CONFLICT_HISTORY 
    WHERE TRAVEL_DISCLOSURE_ID = AV_TRAVEL_DISCLOSURE_ID;
    
    DELETE FROM COI_QUEST_ANSWER_ATTACHMENT 
    WHERE QUESTIONNAIRE_ANSWER_ID IN (select QUESTIONNAIRE_ANSWER_ID from COI_QUEST_ANSWER 
									WHERE QUESTIONNAIRE_ANS_HEADER_ID IN (select QUESTIONNAIRE_ANS_HEADER_ID FROM quest_answer_header 
																			WHERE MODULE_ITEM_CODE = 24 AND MODULE_SUB_ITEM_CODE=0 AND MODULE_ITEM_KEY = AV_TRAVEL_DISCLOSURE_ID));
	DELETE FROM COI_QUEST_TABLE_ANSWER 
    WHERE QUESTIONNAIRE_ANS_HEADER_ID IN (select QUESTIONNAIRE_ANS_HEADER_ID FROM quest_answer_header 
											WHERE MODULE_ITEM_CODE = 24 AND MODULE_SUB_ITEM_CODE=0 AND MODULE_ITEM_KEY = AV_TRAVEL_DISCLOSURE_ID);
												
	DELETE FROM COI_QUEST_ANSWER 
    WHERE QUESTIONNAIRE_ANS_HEADER_ID IN (select QUESTIONNAIRE_ANS_HEADER_ID FROM quest_answer_header 
											WHERE MODULE_ITEM_CODE = 24 AND MODULE_SUB_ITEM_CODE=0 AND MODULE_ITEM_KEY = AV_TRAVEL_DISCLOSURE_ID);
    
    DELETE FROM COI_TRAVEL_DISCLOSURE WHERE TRAVEL_DISCLOSURE_ID = AV_TRAVEL_DISCLOSURE_ID;
    
    DELETE FROM DISCL_COMMENT WHERE MODULE_CODE = 24 AND MODULE_ITEM_KEY = AV_TRAVEL_DISCLOSURE_ID;
    
    SET FOREIGN_KEY_CHECKS=1; 
	SET SQL_SAFE_UPDATES = 1;

END
//