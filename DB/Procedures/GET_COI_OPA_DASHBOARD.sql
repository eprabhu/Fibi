DELIMITER //
CREATE  PROCEDURE `GET_COI_OPA_DASHBOARD`(
	AV_PERSON_ID                    VARCHAR(200),
	AV_FILTER_TYPE					VARCHAR(10),
    AV_IS_COUNT                    	BOOLEAN,
    AV_SORT_TYPE                    VARCHAR(500),
    AV_PAGED                        INT(10),
    AV_LIMIT                        INT(10),
    AV_TAB_TYPE                     VARCHAR(30)
)
BEGIN

    /*
        This is a single procedure for all opa related dashboard
        AV_TAB_TYPE : Tabs eg : All Disclosure tab,...
    	AV_FILTER_TYPE : can be consider as sub tab or sub session or major filter
    	    eg : in All Disclosure there is two sub tabs of a status/any field of true and false
    	AV_IS_COUNT : if true it will return on count without considering pagination else will return rows considering pagination

    */

	DECLARE LS_DYN_SQL 					LONGTEXT;
	DECLARE LS_FILTER_CONDITION 		LONGTEXT;
    DECLARE OPA_SELECTED_FIELDS 	    LONGTEXT;
    DECLARE OPA_JOINS               	LONGTEXT;
    DECLARE LS_OFFSET 			        INT(11);
    DECLARE LS_OFFSET_CONDITION         VARCHAR(600);

	SET LS_DYN_SQL ='';     
    SET OPA_SELECTED_FIELDS = '';
    SET OPA_JOINS = '';
    SET LS_OFFSET = (AV_LIMIT * AV_PAGED);
    SET LS_OFFSET_CONDITION = '';

    IF AV_TAB_TYPE = 'MY_DASHBOARD' THEN

        SET LS_FILTER_CONDITION = CONCAT(' T1.PERSON_ID = ', AV_PERSON_ID);

    ELSEIF AV_TAB_TYPE = 'NEW_SUBMISSIONS' THEN

        SET LS_FILTER_CONDITION = CONCAT('');

    ELSEIF AV_TAB_TYPE = 'MY_REVIEWS' THEN
       
        SET LS_FILTER_CONDITION = CONCAT('');

    ELSEIF AV_TAB_TYPE = 'ALL_REVIEWS' THEN

        SET LS_FILTER_CONDITION = CONCAT('');

    ELSEIF AV_TAB_TYPE = 'ALL_DISCLOSURES' THEN

        SET LS_FILTER_CONDITION = CONCAT('');

    END IF;
    

    IF AV_IS_COUNT THEN
        SET LS_DYN_SQL = CONCAT(' SELECT COUNT(*) FROM ( SELECT ');
        SET OPA_SELECTED_FIELDS = CONCAT(' T1.OPA_DISCLOSURE_ID ');
        SET LS_OFFSET_CONDITION = CONCAT(' LIMIT ',AV_LIMIT,' OFFSET ',LS_OFFSET);
    ELSE
        SET LS_DYN_SQL = CONCAT(' SELECT * FROM ( SELECT ');
        SET OPA_SELECTED_FIELDS= CONCAT('T1.OPA_DISCLOSURE_ID, T1.OPA_DISCLOSURE_NUMBER, T1.OPA_CYCLE_NUMBER,
            T2.PERIOD_START_DATE, T2.PERIOD_END_DATE, T2.OPA_CYCLE_STATUS, T2.OPEN_DATE, T2.CLOSE_DATE,
            T1.PERSON_NAME, T1.HOME_UNIT AS UNIT_NUMBER, T3.UNIT_NAME, T1.IS_FACULTY, T1.IS_FALL_SABATICAL,
            T1.IS_SPRING_SABATICAL, T1.RECEIVED_SUMMER_COMP, T1.SUMMER_COMP_MONTHS, T1.HAS_POTENTIAL_CONFLICT,
            T1.CONFLICT_DESCRIPTION, T1.CREATE_TIMESTAMP, T1.CREATE_USER, T1.SUBMISSION_TIMESTAMP, T1.UPDATE_TIMESTAMP,
            T1.UPDATE_USER, T5.FULL_NAME AS UPDATE_USER_FULL_NAME ');
    END IF;

    SET OPA_JOINS = CONCAT(' INNER JOIN OPA_CYCLES T2 ON T2.OPA_CYCLE_NUMBER = T1.OPA_CYCLE_NUMBER 
        INNER JOIN UNIT T3 ON T3.UNIT_NUMBER = T1.HOME_UNIT 
        INNER JOIN OPA_PERSON T4 ON T4.PERSON_ID = T1.PERSON_ID 
        INNER JOIN PERSON T5 ON T5.USER_NAME = T1.UPDATE_USER');


    IF AV_IS_COUNT THEN
		SET AV_SORT_TYPE =   '';
    ELSEIF AV_SORT_TYPE IS NULL THEN
        SET AV_SORT_TYPE =  CONCAT(' ORDER BY T.UPDATE_TIMESTAMP DESC ');
    ELSE
        SET AV_SORT_TYPE = CONCAT(' ORDER BY ',AV_SORT_TYPE);
    END IF;

    SET LS_DYN_SQL = CONCAT(LS_DYN_SQL, OPA_SELECTED_FIELDS, 'FROM OPA_DISCLOSURE T1 ', OPA_JOINS,  ' WHERE ', LS_FILTER_CONDITION, ' ) T ', AV_SORT_TYPE, LS_OFFSET_CONDITION);


    SET @QUERY_STATEMENT = LS_DYN_SQL;
	PREPARE EXECUTABLE_STAEMENT FROM @QUERY_STATEMENT;
	EXECUTE EXECUTABLE_STAEMENT;
END

//