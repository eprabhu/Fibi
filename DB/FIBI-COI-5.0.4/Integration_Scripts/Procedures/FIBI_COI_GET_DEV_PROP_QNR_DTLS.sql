create or replace PROCEDURE FIBI_COI_GET_DEV_PROP_QNR_DTLS(
AV_PROPOSAL_NUMBER IN EPS_PROPOSAL.PROPOSAL_NUMBER%TYPE,
AV_QUESTIONNAIRE_ID IN QUEST_ANSWER_HEADER.QUESTIONNAIRE_ID%TYPE,
AV_PERSON_ID IN PERSON.PERSON_ID%TYPE,
cur_generic        OUT SYS_REFCURSOR)
IS
BEGIN
    OPEN cur_generic FOR
    SELECT DISTINCT
                    QAH.module_sub_item_key AS PERSON_ID,
                    QA.question_id,
                    QAH.questionnaire_id,
                    QAH.module_item_key     AS PROJECT_NUMBER,
                    QA.ANSWER,
                    '1'ATTRIBUTE_1_LABEL,
                    '2'ATTRIBUTE_1_VALUE,
                    '3'ATTRIBUTE_2_LABEL,
                    '4'ATTRIBUTE_2_VALUE,
                    '5'ATTRIBUTE_3_LABEL,
                    '6'ATTRIBUTE_3_VALUE,
                    P.HOME_UNIT AS HOME_UNIT,
                    '3' as COI_PROJECT_TYPE_CODE

FROM   quest_answer_header QAH
       LEFT JOIN quest_answer QA
              ON QA.questionnaire_ans_header_id =
                 QAH.questionnaire_ans_header_id
        LEFT JOIN PERSON P ON P.PERSON_ID = qah.module_sub_item_key
WHERE  QAH.module_item_key = AV_PROPOSAL_NUMBER
       AND QAH.QUESTIONNAIRE_ID = AV_QUESTIONNAIRE_ID      
       AND QAH.module_sub_item_key = AV_PERSON_ID ; 

END;
