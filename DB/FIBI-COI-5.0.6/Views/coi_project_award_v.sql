DELIMITER //
CREATE VIEW `coi_project_award_v` AS
        SELECT
        NULL AS `ID`,
        `t1`.`AWARD_ID` AS `EXTERNAL_SYSTEM_REF_ID`,
        '1' AS `COI_PROJECT_TYPE_CODE`,
        `t1`.`AWARD_NUMBER` AS `AWARD_NUMBER`,
        `t1`.`ACCOUNT_NUMBER` AS `ACCOUNT_NUMBER`,
        `t1`.`SPONSOR_AWARD_NUMBER` AS `SPONSOR_AWARD_NUMBER`,
        `t1`.`TITLE` AS `TITLE`,
        (SELECT
                `award_persons`.`PERSON_ID`
            FROM
                `award_persons`
            WHERE
                ((`award_persons`.`PI_FLAG` = 'Y')
                    AND (`award_persons`.`AWARD_ID` = `t1`.`AWARD_ID`)
                    AND ((`t1`.`AWARD_SEQUENCE_STATUS` = 'ACTIVE')
                    OR ((`t1`.`AWARD_SEQUENCE_STATUS` = 'PENDING')
                    AND (`t1`.`AWARD_DOCUMENT_TYPE_CODE` = 1))))
            LIMIT 1) AS `PI_PERSON_ID`,
        (SELECT
                `award_persons`.`FULL_NAME`
            FROM
                `award_persons`
            WHERE
                (`award_persons`.`PERSON_ID` = `PI_PERSON_ID`)
            LIMIT 1) AS `PI_NAME`,
        `t5`.`PERSON_ID` AS `KEY_PERSON_ID`,
        `t7`.`FULL_NAME` AS `KEY_PERSON_NAME`,
        `t5`.`PERSON_ROLE_ID` AS `KEY_PERSON_ROLE_CODE`,
		`t5`.`PROJECT_ROLE` AS `KEY_PERSON_ROLE_NAME`,
        `t6`.`DESCRIPTION` AS `AWARD_STATUS`,
		`t6`.`STATUS_CODE` AS `AWARD_STATUS_CODE`,
        `t4`.`SPONSOR_NAME` AS `SPONSOR_NAME`,
		`t4`.`SPONSOR_CODE` AS `SPONSOR_CODE`,
        `t40`.`SPONSOR_NAME` AS `PRIME_SPONSOR_NAME`,
		`t1`.`PRIME_SPONSOR_CODE` AS `PRIME_SPONSOR_CODE`,
        `t1`.`BEGIN_DATE` AS `AWARD_START_DATE`,
        `t1`.`FINAL_EXPIRATION_DATE` AS `AWARD_END_DATE`,
        `t1`.`LEAD_UNIT_NUMBER` AS `LEAD_UNIT_NUMBER`,
        `t3`.`unit_name` AS `LEAD_UNIT_NAME`,
		null AS `PROPOSAL_TYPE_CODE`,
        null AS `UPDATE_TIMESTAMP`,
        null AS `PCK`

    FROM
        ((((((`award` `t1`
        JOIN `unit` `t3` ON ((`t3`.`UNIT_NUMBER` = `t1`.`LEAD_UNIT_NUMBER`)))
        JOIN `sponsor` `t4` ON ((`t4`.`SPONSOR_CODE` = `t1`.`SPONSOR_CODE`)))
        LEFT JOIN `sponsor` `t40` ON ((`t40`.`SPONSOR_CODE` = `t1`.`PRIME_SPONSOR_CODE`)))
        JOIN `award_persons` `t5` ON ((`t5`.`AWARD_ID` = `t1`.`AWARD_ID`)))
        JOIN `award_status` `t6` ON ((`t6`.`STATUS_CODE` = `t1`.`STATUS_CODE`)))
        JOIN `person` `t7` ON ((`t5`.`PERSON_ID` = `t7`.`PERSON_ID`)))
    WHERE
        ((`t1`.`AWARD_SEQUENCE_STATUS` = 'ACTIVE')
            OR ((`t1`.`AWARD_SEQUENCE_STATUS` = 'PENDING')
            AND (`t1`.`AWARD_DOCUMENT_TYPE_CODE` = 1)))
    UNION ALL

	SELECT
        `coi_int_stage_award`.`STAGE_AWARD_ID` AS `ID`,
        `coi_int_stage_award`.`PROJECT_ID` AS `EXTERNAL_SYSTEM_REF_ID`,
        '1' AS `COI_PROJECT_TYPE_CODE`,
        `coi_int_stage_award`.`PROJECT_NUMBER` AS `AWARD_NUMBER`,
		`coi_int_stage_award`.`ACCOUNT_NUMBER` AS `ACCOUNT_NUMBER`,
		`coi_int_stage_award`.`SPONSOR_GRANT_NUMBER` AS `SPONSOR_AWARD_NUMBER`,
        `coi_int_stage_award`.`TITLE` AS `TITLE`,
        (SELECT
                `coi_int_stage_award_person`.`KEY_PERSON_ID`
            FROM
                `coi_int_stage_award_person`
            WHERE
                ((`coi_int_stage_award_person`.`KEY_PERSON_ROLE_NAME` = 'Principal Investigator')
                    AND (`coi_int_stage_award_person`.`PROJECT_NUMBER` = `coi_int_stage_award`.`PROJECT_NUMBER`))
            LIMIT 1) AS `PI_PERSON_ID`,
        (SELECT
                `coi_int_stage_award_person`.`KEY_PERSON_NAME`
            FROM
                `coi_int_stage_award_person`
            WHERE
                (`coi_int_stage_award_person`.`KEY_PERSON_ID` = `PI_PERSON_ID`)
            LIMIT 1) AS `PI_NAME`,
        `coi_int_stage_award_person`.`KEY_PERSON_ID` AS `KEY_PERSON_ID`,
        `coi_int_stage_award_person`.`KEY_PERSON_NAME` AS `KEY_PERSON_NAME`,
        `coi_int_stage_award_person`.`KEY_PERSON_ROLE_CODE` AS `KEY_PERSON_ROLE_CODE`,
        `coi_int_stage_award_person`.`KEY_PERSON_ROLE_NAME` AS `KEY_PERSON_ROLE_NAME`,
        `coi_int_stage_award`.`PROJECT_STATUS` AS `AWARD_STATUS`,
        `coi_int_stage_award`.`PROJECT_STATUS_CODE` AS `AWARD_STATUS_CODE`,
        `coi_int_stage_award`.`SPONSOR_NAME` AS `SPONSOR_NAME`,
        `coi_int_stage_award`.`SPONSOR_CODE` AS `SPONSOR_CODE`,
        `coi_int_stage_award`.`PRIME_SPONSOR_NAME` AS `PRIME_SPONSOR_NAME`,
        `coi_int_stage_award`.`PRIME_SPONSOR_CODE` AS `PRIME_SPONSOR_CODE`,
        `coi_int_stage_award`.`PROJECT_START_DATE` AS `AWARD_START_DATE`,
        `coi_int_stage_award`.`PROJECT_END_DATE` AS `AWARD_END_DATE`,
        `coi_int_stage_award`.`LEAD_UNIT_NUMBER` AS `LEAD_UNIT_NUMBER`,
        `coi_int_stage_award`.`LEAD_UNIT_NAME` AS `LEAD_UNIT_NAME`,
        `coi_int_stage_award`.`PROJECT_TYPE_CODE` AS `PROJECT_TYPE_CODE`,
        `coi_int_stage_award`.`SRC_SYS_UPDATE_TIMESTAMP` AS `UPDATE_TIMESTAMP`,
        `coi_int_stage_award`.`ATTRIBUTE_1_VALUE` AS `PCK`
    FROM
        (`coi_int_stage_award`
        JOIN `coi_int_stage_award_person` ON ((`coi_int_stage_award`.`PROJECT_NUMBER` = `coi_int_stage_award_person`.`PROJECT_NUMBER`)))
//
