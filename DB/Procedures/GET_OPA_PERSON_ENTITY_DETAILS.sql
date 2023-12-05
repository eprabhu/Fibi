DELIMITER //
CREATE PROCEDURE `GET_OPA_PERSON_ENTITY_DETAILS`(IN AV_OPA_DISCLOSURE_ID INT)
BEGIN
  SELECT
        t.OPA_DISCL_PERSON_ENTITY_ID,
        t0.PERSON_ENTITY_ID,
        t0.PERSON_ID,
        t0.ENTITY_NUMBER,
        t5.ENTITY_NAME,
        t6.DESCRIPTION AS ENTITY_TYPE,
        t7.COUNTRY_NAME,
        GROUP_CONCAT(DISTINCT CONCAT(t3.DESCRIPTION, ': ', grouped_descriptions) ORDER BY t3.DESCRIPTION SEPARATOR ':;:') AS REL,
        t0.INVOLVEMENT_START_DATE,
        t0.INVOLVEMENT_END_DATE,
		t8.DESCRIPTION as ENTITY_STATUS,
        t0.IS_RELATIONSHIP_ACTIVE,
        t0.VERSION_STATUS,
        t9.DESCRIPTION AS RISK_CATEGORY
    FROM OPA_DISCL_PERSON_ENTITY t
    INNER JOIN person_entity t0  ON t.PERSON_ENTITY_ID = t0.PERSON_ENTITY_ID
    INNER JOIN person_entity_relationship t1 ON t1.PERSON_ENTITY_ID = t0.PERSON_ENTITY_ID
    INNER JOIN VALID_PERSON_ENTITY_REL_TYPE t2 ON t1.VALID_PERS_ENTITY_REL_TYP_CODE = t2.VALID_PERS_ENTITY_REL_TYP_CODE
    INNER JOIN coi_disclosure_type t3 ON t2.DISCLOSURE_TYPE_CODE = t3.DISCLOSURE_TYPE_CODE
    INNER JOIN person_entity_rel_type t4 ON t4.RELATIONSHIP_TYPE_CODE = t2.RELATIONSHIP_TYPE_CODE
    INNER JOIN entity t5 ON t5.ENTITY_ID = t0.ENTITY_ID
    INNER JOIN entity_type t6 ON t6.ENTITY_TYPE_CODE = t5.ENTITY_TYPE_CODE
    INNER JOIN country t7 ON t7.COUNTRY_CODE = t5.COUNTRY_CODE
	INNER JOIN entity_status t8 on t8.ENTITY_STATUS_CODE = t5.ENTITY_STATUS_CODE
    INNER JOIN ENTITY_RISK_CATEGORY t9 ON t9.RISK_CATEGORY_CODE = t5.RISK_CATEGORY_CODE
    LEFT JOIN (
        SELECT t0.PERSON_ENTITY_ID, t3.DESCRIPTION, GROUP_CONCAT(DISTINCT t4.DESCRIPTION ORDER BY t4.DESCRIPTION SEPARATOR '/') AS grouped_descriptions
        FROM OPA_DISCL_PERSON_ENTITY t
        INNER JOIN person_entity t0 ON t.PERSON_ENTITY_ID = t0.PERSON_ENTITY_ID
        INNER JOIN person_entity_relationship t1 ON t1.PERSON_ENTITY_ID = t0.PERSON_ENTITY_ID
        INNER JOIN VALID_PERSON_ENTITY_REL_TYPE t2 ON t1.VALID_PERS_ENTITY_REL_TYP_CODE = t2.VALID_PERS_ENTITY_REL_TYP_CODE
        INNER JOIN coi_disclosure_type t3 ON t2.DISCLOSURE_TYPE_CODE = t3.DISCLOSURE_TYPE_CODE
        INNER JOIN person_entity_rel_type t4 ON t4.RELATIONSHIP_TYPE_CODE = t2.RELATIONSHIP_TYPE_CODE	
        WHERE t.OPA_DISCLOSURE_ID = AV_OPA_DISCLOSURE_ID
        GROUP BY t.OPA_DISCL_PERSON_ENTITY_ID, t0.PERSON_ENTITY_ID, t3.DESCRIPTION
    ) AS subquery ON t0.PERSON_ENTITY_ID = subquery.PERSON_ENTITY_ID AND t3.DESCRIPTION = subquery.DESCRIPTION
    WHERE t.OPA_DISCLOSURE_ID = AV_OPA_DISCLOSURE_ID
    GROUP BY t.OPA_DISCL_PERSON_ENTITY_ID, t0.PERSON_ID, t0.ENTITY_NUMBER, t5.ENTITY_NAME;
END
//
