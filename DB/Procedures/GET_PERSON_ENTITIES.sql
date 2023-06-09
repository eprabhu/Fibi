DELIMITER //
CREATE PROCEDURE `GET_PERSON_ENTITIES`(
AV_PERSON_ID varchar(40),
AV_DISCLOSURE_ID int(30)
)
BEGIN

    IF AV_PERSON_ID IS NOT NULL THEN

        SELECT DISTINCT t1.PERSON_ENTITY_ID, t1.PERSON_ID, t1.ENTITY_ID, t1.ENTITY_NUMBER, t1.INVOLVEMENT_START_DATE, t1.INVOLVEMENT_END_DATE, 
        t2.ENTITY_NAME, t2.ENTITY_TYPE_CODE, t4.COUNTRY_NAME, t3.DESCRIPTION AS ENTITY_TYPE, t1.VERSION_STATUS FROM PERSON_ENTITY t1 INNER JOIN ENTITY t2 
        ON t2.ENTITY_ID = t1.ENTITY_ID INNER JOIN ENTITY_TYPE t3 ON t3.ENTITY_TYPE_CODE = t2.ENTITY_TYPE_CODE LEFT JOIN COUNTRY t4 ON t4.COUNTRY_CODE = t2.COUNTRY_CODE 
        WHERE t1.PERSON_ID = AV_PERSON_ID AND t1.VERSION_STATUS = 'Active' ORDER BY t1.UPDATE_TIMESTAMP DESC;

    ELSEIF AV_DISCLOSURE_ID IS NOT NULL THEN

        SELECT DISTINCT t1.PERSON_ENTITY_ID, t1.PERSON_ID, t1.ENTITY_ID, t1.ENTITY_NUMBER, t1.INVOLVEMENT_START_DATE, t1.INVOLVEMENT_END_DATE, 
        t2.ENTITY_NAME, t2.ENTITY_TYPE_CODE, t4.COUNTRY_NAME, t3.DESCRIPTION AS ENTITY_TYPE, t1.VERSION_STATUS FROM COI_DISCL_ENT_PROJ_DETAILS dd 
        INNER JOIN PERSON_ENTITY t1 ON t1.PERSON_ENTITY_ID = dd.PERSON_ENTITY_ID INNER JOIN ENTITY t2 
        ON t2.ENTITY_ID = t1.ENTITY_ID INNER JOIN ENTITY_TYPE t3 ON t3.ENTITY_TYPE_CODE = t2.ENTITY_TYPE_CODE 
        LEFT JOIN COUNTRY t4 ON t4.COUNTRY_CODE = t2.COUNTRY_CODE WHERE dd.DISCLOSURE_ID = AV_DISCLOSURE_ID ORDER BY t1.UPDATE_TIMESTAMP DESC;

    END IF;

END
//