DELIMITER //

CREATE DEFINER=`root`@`%` PROCEDURE `coi`.`GET_COI_DISCLOSURES_FOR_BANNER`( NO_OF_DAYS INT)
BEGIN
		SELECT 8                                           					AS MODULE_CODE,
         0                                                 					AS SUB_MODULE_CODE,
         T1.DISCLOSURE_ID                                  					AS MODULE_ITEM_KEY,
         0                                                 					AS SUB_MODULE_ITEM_KEY,
         GROUP_CONCAT(DISTINCT T1.PERSON_ID SEPARATOR ',') 					AS PERSON_ID,
         T1.EXPIRATION_DATE                                					AS EXPIRATION_DATE,
         'Your FCOI Disclosure will expire in <b>{NO_OF_DAYS}</b> days'		AS USER_MESSAGE,
         '139' 																AS EXPIRING_MESSAGE_TYPE_CODE,
         T1.UPDATE_USER                                    					AS UPDATE_USER,
         T1.UPDATE_TIMESTAMP                               					AS UPDATE_TIMESTAMP
  FROM   COI_DISCLOSURE T1 
  WHERE  T1.VERSION_STATUS IN ('ACTIVE', 'PENDING')
  AND    DATE_SUB(T1.EXPIRATION_DATE, INTERVAL NO_OF_DAYS DAY) = CURRENT_DATE()
 
  UNION
  
  SELECT 24                                                					AS MODULE_CODE,
         0                                                 					AS SUB_MODULE_CODE,
         T3.TRAVEL_DISCLOSURE_ID                           					AS MODULE_ITEM_KEY,
         0                                                 					AS SUB_MODULE_ITEM_KEY,
         GROUP_CONCAT(DISTINCT T3.PERSON_ID SEPARATOR ',') 					AS PERSON_ID,
         T3.EXPIRATION_DATE                                					AS EXPIRATION_DATE,
         'Your Travel Disclosure will expire in <b>{NO_OF_DAYS}</b> days'	AS USER_MESSAGE,
         '140' 																AS EXPIRING_MESSAGE_TYPE_CODE,
         T3.UPDATE_USER                                    					AS UPDATE_USER,
         T3.UPDATE_TIMESTAMP                               					AS UPDATE_TIMESTAMP
  FROM   COI_TRAVEL_DISCLOSURE T3
  WHERE  T3.VERSION_STATUS IN ('ACTIVE','PENDING')
  AND    DATE_SUB(T3.EXPIRATION_DATE, INTERVAL NO_OF_DAYS DAY) = CURRENT_DATE();
 END
  
//
