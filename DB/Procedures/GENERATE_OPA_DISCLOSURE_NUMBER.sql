DELIMITER //
CREATE PROCEDURE `GENERATE_OPA_DISCLOSURE_NUMBER`(OUT NEXT_VALUE INT)
BEGIN

    DECLARE LOCK_ACQUIRED INT DEFAULT 0;

    START TRANSACTION;

    SELECT GET_LOCK('LOCK_ON_OPA_DISCLOSURE_NUMBER_COUNTER', 30) INTO LOCK_ACQUIRED;

    IF LOCK_ACQUIRED THEN

        SELECT COUNTER_VALUE INTO NEXT_VALUE FROM OPA_DISCLOSURE_NUMBER_COUNTER WHERE COUNTER_NAME = 'OPA_DISCLOSURE_NUMBER_COUNTER' FOR UPDATE;

        UPDATE OPA_DISCLOSURE_NUMBER_COUNTER SET COUNTER_VALUE = COUNTER_VALUE + 1 WHERE COUNTER_NAME = 'OPA_DISCLOSURE_NUMBER_COUNTER';

        SELECT RELEASE_LOCK('LOCK_ON_OPA_DISCLOSURE_NUMBER_COUNTER');

        COMMIT;

    END IF;

END
//