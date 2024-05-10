DROP PROCEDURE IF EXISTS EVALUATE_FORM_MANDATORY_VALIDATION;
DROP PROCEDURE IF EXISTS GET_COMPONENT_RULE_ID;
DROP PROCEDURE IF EXISTS EVALUATE_FORM_VALIDATION;
DROP PROCEDURE IF EXISTS COI_EVALUATE_VALIDATION;
DROP FUNCTION IF EXISTS FN_EVAL_DISCLOSURE_QUESTIONNAIRE;


\. ./Procedures/EVALUATE_FORM_MANDATORY_VALIDATION.sql
\. ./Procedures/GET_COMPONENT_RULE_ID.sql
\. ./Procedures/EVALUATE_FORM_VALIDATION.sql
\. ./Procedures/COI_EVALUATE_VALIDATION.sql
\. ./Functions/FN_EVAL_DISCLOSURE_QUESTIONNAIRE.sql
