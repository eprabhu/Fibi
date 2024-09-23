DELIMITER //
CREATE PROCEDURE `COI_SYNC_REMOVE_DEACTIVATED_PROJECTS`(
    IN AV_MODULE_CODE INT,
    IN AV_MODULE_ITEM_KEY VARCHAR(100)
)
BEGIN
    DECLARE LS_COUNT INT DEFAULT 0;

    -- Determine the count of deactivated projects based on module code
    IF AV_MODULE_CODE = 1 THEN
        SELECT COUNT(*) INTO LS_COUNT
        FROM COI_INT_STAGE_AWARD
        WHERE PROJECT_STATUS_CODE = 4;
    ELSEIF AV_MODULE_CODE = 2 THEN
        SELECT COUNT(*) INTO LS_COUNT
        FROM COI_INT_STAGE_PROPOSAL
        WHERE PROJECT_STATUS_CODE = 4;
    END IF;

    -- Delete records if there are deactivated projects
   /* IF LS_COUNT > 0 THEN
        DELETE T4, T3, T2, T1
        FROM COI_DISCLOSURE T0
        INNER JOIN COI_DISCL_PROJECTS T1 ON T1.DISCLOSURE_ID = T0.DISCLOSURE_ID
        LEFT JOIN COI_DISCL_PROJECT_ENTITY_REL T2
            ON T2.COI_DISCL_PROJECTS_ID = T1.COI_DISCL_PROJECTS_ID
        LEFT JOIN DISCL_COMMENT T3
            ON T3.SUB_MODULE_ITEM_KEY = T2.COI_DISCL_PROJECT_ENTITY_REL_ID
            AND T3.COMPONENT_TYPE_CODE IN ('1', '6')
        LEFT JOIN COI_CONFLICT_HISTORY T4
            ON T4.COI_DISCL_PROJECT_ENTITY_REL_ID = T2.COI_DISCL_PROJECT_ENTITY_REL_ID
        WHERE T1.MODULE_ITEM_KEY = AV_MODULE_ITEM_KEY
        AND T1.MODULE_CODE = AV_MODULE_CODE AND T0.REVIEW_STATUS_CODE != 4;
    END IF; */

END

//
