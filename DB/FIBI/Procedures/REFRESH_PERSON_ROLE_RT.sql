DELIMITER $$
CREATE PROCEDURE `REFRESH_PERSON_ROLE_RT`()
    DETERMINISTIC
BEGIN
	
INSERT INTO PERSON_ROLE_RT
(
PERSON_ID,
UNIT_NUMBER,
RIGHT_NAME,
ROLE_ID
)
SELECT DISTINCT T1.PERSON_ID,T1.UNIT_NUMBER,T3.RIGHT_NAME,T1.ROLE_ID
FROM PERSON_ROLES T1
INNER JOIN ROLE_RIGHTS T2 ON T1.ROLE_ID = T2.ROLE_ID
INNER JOIN RIGHTS T3 ON T2.RIGHT_ID = T3.RIGHT_ID 
WHERE T1.DESCEND_FLAG = 'N'
AND (T1.PERSON_ID,T1.UNIT_NUMBER,T3.RIGHT_NAME,T1.ROLE_ID) NOT IN(SELECT PERSON_ID,UNIT_NUMBER,RIGHT_NAME,ROLE_ID FROM PERSON_ROLE_RT)
UNION
SELECT DISTINCT T1.PERSON_ID,T4.CHILD_UNIT_NUMBER AS UNIT_NUMBER,T3.RIGHT_NAME,T1.ROLE_ID
FROM PERSON_ROLES T1
INNER JOIN ROLE_RIGHTS T2 ON T1.ROLE_ID = T2.ROLE_ID
INNER JOIN RIGHTS T3 ON T2.RIGHT_ID = T3.RIGHT_ID 
INNER JOIN UNIT_WITH_CHILDREN T4 ON T4.UNIT_NUMBER = T1.UNIT_NUMBER
WHERE T1.DESCEND_FLAG = 'Y'
AND (T1.PERSON_ID,T4.CHILD_UNIT_NUMBER,T3.RIGHT_NAME,T1.ROLE_ID) NOT IN(SELECT PERSON_ID,UNIT_NUMBER,RIGHT_NAME,ROLE_ID FROM PERSON_ROLE_RT);
COMMIT;
END$$
