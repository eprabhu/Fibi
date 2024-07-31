DELIMITER //
CREATE PROCEDURE `GET_PERSON_HAS_PERMISSION`(AV_RIGHT_NAME VARCHAR(500),
AV_UNIT_NUMBER VARCHAR(40),
AV_PERSON_ID VARCHAR(40)
)
BEGIN
DECLARE LS_DYN_SQL VARCHAR(2000) default '';
DECLARE LS_RIGHT_NAME VARCHAR(500);
DECLARE LI_FLAG INT;

IF AV_RIGHT_NAME != '' THEN
	SET LS_RIGHT_NAME = CONCAT("'", REPLACE(AV_RIGHT_NAME, ",", "','"), "'");
ELSE
	SET LS_RIGHT_NAME = AV_RIGHT_NAME;
END IF;

IF AV_UNIT_NUMBER IS NULL OR AV_UNIT_NUMBER = '' THEN

 set LS_DYN_SQL = concat('SELECT COUNT(1) INTO @LI_FLAG
        FROM
        PERSON_ROLES PR,
        (SELECT ROLE_ID,RT.RIGHT_NAME
           FROM ROLE_RIGHTS RR,
                RIGHTS RT
            WHERE RR.RIGHT_ID = RT.RIGHT_ID
              AND RT.RIGHT_NAME IN (',LS_RIGHT_NAME,')
        ) RLE
        WHERE PR.PERSON_ID = ''',AV_PERSON_ID,'''
        AND RLE.ROLE_ID = PR.ROLE_ID');

ELSEIF AV_UNIT_NUMBER IS NOT NULL AND AV_PERSON_ID IS NOT NULL AND LS_RIGHT_NAME IS NOT NULL THEN

 set LS_DYN_SQL = concat('SELECT COUNT(1) INTO @LI_FLAG FROM PERSON_ROLES PR,
        (SELECT ROLE_ID,RT.RIGHT_NAME
           FROM ROLE_RIGHTS RR,
                RIGHTS RT
            WHERE RR.RIGHT_ID = RT.RIGHT_ID
              AND RT.RIGHT_NAME IN (',LS_RIGHT_NAME,')
        ) RLE
       WHERE PR.PERSON_ID = ''',AV_PERSON_ID,'''
        AND
          (( PR. DESCEND_FLAG = ''Y'' AND ''',AV_UNIT_NUMBER,''' IN (SELECT CHILD_UNIT_NUMBER
                                                                 FROM UNIT_WITH_CHILDREN
                                                                                  WHERE UNIT_NUMBER = PR.UNIT_NUMBER
             ))OR ( PR. DESCEND_FLAG = ''N'' AND PR.UNIT_NUMBER = ''',AV_UNIT_NUMBER,''' )
            )
         AND RLE.ROLE_ID = PR.ROLE_ID');

END IF;
				 SET @QUERY_STATEMENT = LS_DYN_SQL;
				 PREPARE EXECUTABLE_STAEMENT FROM @QUERY_STATEMENT;
				 EXECUTE EXECUTABLE_STAEMENT ;
				SET LI_FLAG = @LI_FLAG;
	 IF LI_FLAG > 0 THEN
		 SELECT 'TRUE' FROM DUAL;
	 ELSE
		 SELECT 'FALSE' FROM DUAL;
	 END IF;
END
//
