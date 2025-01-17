DELIMITER //
CREATE PROCEDURE `GET_UNIT_HIERARCHY`()
    DETERMINISTIC
BEGIN


DECLARE LS_VAR LONGTEXT;
DECLARE LS_INTO_VAR LONGTEXT;
DECLARE LS_CHILD_LIST LONGTEXT;

declare li_count int;

SET LS_VAR = '000001'; 
SET LS_CHILD_LIST = '';

SET li_count = 0;
    
       
	 SELECT UNIT_NUMBER,UNIT_NAME,ORGANIZATION_ID,UPDATE_TIMESTAMP,UPDATE_USER,PARENT_UNIT_NUMBER,IS_ACTIVE,
      1 AS LVL 
      FROM UNIT WHERE PARENT_UNIT_NUMBER is null
      
      UNION ALL
      
      
	  SELECT UNIT_NUMBER,UNIT_NAME,ORGANIZATION_ID,UPDATE_TIMESTAMP,UPDATE_USER,PARENT_UNIT_NUMBER,IS_ACTIVE,
      FN_GET_UNIT_HIERARCHY_LEVEL(UNIT_NUMBER) AS LVL  
      FROM UNIT 
     WHERE PARENT_UNIT_NUMBER is not null
	  order by LVL,PARENT_UNIT_NUMBER,UNIT_NUMBER;



END
//