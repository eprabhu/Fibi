
DROP PROCEDURE IF EXISTS COI_INT_KC_SPON_ORG_RESPONSE;
DROP PROCEDURE IF EXISTS CLENSEMATCH_ENT_INFO_BY_DUNS;
DROP PROCEDURE IF EXISTS GENERATE_ATTACHMENT_NUMBER;
DROP PROCEDURE IF EXISTS GET_ENTITY_DASHBOARD_DYNAMIC_DATA;
DROP PROCEDURE IF EXISTS GET_ENTITY_DASHBOARD_DYNAMIC_COUNT;
DROP PROCEDURE IF EXISTS GET_ALL_SYSTEM_ENTITY_LIST;
DROP PROCEDURE IF EXISTS GET_ALL_SYSTEM_ENTITY_LIST_COUNT;

\. ./Procedures/COI_INT_KC_SPON_ORG_RESPONSE.sql
\. ./Procedures/CLENSEMATCH_ENT_INFO_BY_DUNS.sql
\. ./Procedures/GENERATE_ATTACHMENT_NUMBER.sql
\. ./Procedures/GET_ENTITY_DASHBOARD_DYNAMIC_DATA.sql
\. ./Procedures/GET_ENTITY_DASHBOARD_DYNAMIC_COUNT.sql