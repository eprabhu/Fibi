DELIMITER $$
CREATE PROCEDURE `GET_UNIT_WITH_CHILDREN`()
    DETERMINISTIC
BEGIN
DECLARE LS_VAR 			LONGTEXT;
DECLARE LS_INTO_VAR 	LONGTEXT;
DECLARE LS_CHILD_LIST   LONGTEXT;
DECLARE LS_CHILD VARCHAR(8);
DECLARE LS_UNIT_NUMBER VARCHAR(8);
DECLARE LI_COUNT INT;
DECLARE DONE1 INT DEFAULT FALSE;
DECLARE UNIT_CURSOR CURSOR FOR 
SELECT UNIT_NUMBER 
FROM UNIT;
DECLARE CONTINUE HANDLER FOR NOT FOUND SET DONE1 = TRUE;
OPEN UNIT_CURSOR;
UNIT_CURSOR_LOOP : LOOP
        FETCH UNIT_CURSOR INTO LS_UNIT_NUMBER;
		IF DONE1 THEN
			LEAVE UNIT_CURSOR_LOOP;
		END IF;
		SET LS_CHILD_LIST = '';
		SET  LS_VAR =  LS_UNIT_NUMBER  ;
		REPEAT
			SELECT GROUP_CONCAT(UNIT_NUMBER SEPARATOR ',') INTO LS_INTO_VAR FROM UNIT WHERE FIND_IN_SET(PARENT_UNIT_NUMBER, LS_VAR) > 0  ;
            IF LS_INTO_VAR IS NOT NULL  THEN 
				SET LS_CHILD_LIST = CONCAT(LS_CHILD_LIST,',',LS_INTO_VAR) ;
				SET LS_VAR = LS_INTO_VAR ;
			END IF;
        UNTIL LS_INTO_VAR IS NULL  END REPEAT;
		SELECT TRIM(LEADING   ',' FROM TRIM(LS_CHILD_LIST)) INTO LS_CHILD_LIST FROM DUAL;
		SELECT COUNT(*) INTO LI_COUNT
		FROM UNIT_WITH_CHILDREN WHERE UNIT_NUMBER = LS_UNIT_NUMBER AND CHILD_UNIT_NUMBER = LS_UNIT_NUMBER;
		IF LI_COUNT  = 0 THEN
			INSERT INTO UNIT_WITH_CHILDREN(UNIT_NUMBER,CHILD_UNIT_NUMBER) VALUES(LS_UNIT_NUMBER,LS_UNIT_NUMBER);
		END IF;
		WHILE LS_CHILD_LIST != '' DO
					SET LS_CHILD = SUBSTRING_INDEX(LS_CHILD_LIST, ',', 1);  
					SELECT COUNT(*) INTO LI_COUNT
					FROM UNIT_WITH_CHILDREN WHERE UNIT_NUMBER = LS_UNIT_NUMBER AND CHILD_UNIT_NUMBER = LS_CHILD;
					IF LI_COUNT  = 0 THEN
						INSERT INTO UNIT_WITH_CHILDREN(UNIT_NUMBER,CHILD_UNIT_NUMBER) VALUES(LS_UNIT_NUMBER,LS_CHILD);
					END IF;
					IF LOCATE(',', LS_CHILD_LIST) > 0 THEN
						SET LS_CHILD_LIST = SUBSTRING(LS_CHILD_LIST, LOCATE(',', LS_CHILD_LIST) + 1);
					ELSE
						SET LS_CHILD_LIST = '';
					END IF;
		END WHILE;
    END LOOP;
CLOSE UNIT_CURSOR;
END$$
