DELIMITER //
CREATE PROCEDURE CLENSEMATCH_ENT_INFO_BY_DUNS(IN AV_DUNS_NUMBER VARCHAR(20))
BEGIN
    SELECT 
        t1.ENTITY_ID, 
        t1.PRIMARY_NAME, 
        t1.DUNS_NUMBER,
        t1.UEI_NUMBER, 
        t1.CAGE_NUMBER,
        t1.ENTITY_STATUS_TYPE_CODE, 
        t4.DESCRIPTION AS ENTITY_STATUS,
        t1.PRIMARY_ADDRESS_LINE_1, 
        t1.PRIMARY_ADDRESS_LINE_2, 
        t1.CITY, 
        t1.STATE, 
        t1.POST_CODE,
        t1.COUNTRY_CODE, 
        t5.COUNTRY_NAME, 
        t1.WEBSITE_ADDRESS,
        t1.CERTIFIED_EMAIL, 
        t1.IS_ACTIVE, 
        t2.SPONSOR_CODE, 
        t1.PHONE_NUMBER,
        t3.ORGANIZATION_ID
    FROM entity t1
    INNER JOIN entity_status_type t4 ON t4.ENTITY_STATUS_TYPE_CODE = t1.ENTITY_STATUS_TYPE_CODE
    LEFT OUTER JOIN country t5 ON t5.COUNTRY_CODE = t1.COUNTRY_CODE
    LEFT OUTER JOIN entity_sponsor_info t2 ON t1.ENTITY_ID = t2.ENTITY_ID
    LEFT OUTER JOIN entity_sub_org_info t3 ON t1.ENTITY_ID = t3.ENTITY_ID 
    WHERE t1.DUNS_NUMBER = AV_DUNS_NUMBER;
END
//