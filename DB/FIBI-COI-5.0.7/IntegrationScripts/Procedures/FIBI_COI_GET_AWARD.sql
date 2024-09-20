create or replace PROCEDURE FIBI_COI_GET_AWARD(
    AV_PROJECT_NUMBER IN AWARD.AWARD_NUMBER%TYPE,
    cur_generic        OUT SYS_REFCURSOR)
IS
LI_LINKED_INST_PROPOSAL_IDS VARCHAR2(4000);
LS_ROOT_AWARD_NUMBER AWARD.AWARD_NUMBER%TYPE;
LS_PARENT_AWARD_NUMBER AWARD.AWARD_NUMBER%TYPE;
LS_PCK_VALUE VARCHAR2(10);
BEGIN

	BEGIN

		SELECT LISTAGG(P.PROPOSAL_NUMBER, ',') WITHIN GROUP (ORDER BY P.PROPOSAL_NUMBER)
		INTO LI_LINKED_INST_PROPOSAL_IDS
		FROM AWARD A
		LEFT JOIN AWARD_FUNDING_PROPOSALS AFP ON AFP.AWARD_ID = A.AWARD_ID
		LEFT JOIN PROPOSAL P ON P.PROPOSAL_ID = AFP.PROPOSAL_ID
		WHERE A.AWARD_NUMBER = AV_PROJECT_NUMBER;

	EXCEPTION    
		WHEN OTHERS THEN
			LI_LINKED_INST_PROPOSAL_IDS := NULL;
	END;	

	BEGIN 
		SELECT ROOT_AWARD_NUMBER,PARENT_AWARD_NUMBER
		INTO LS_ROOT_AWARD_NUMBER,LS_PARENT_AWARD_NUMBER
		FROM AWARD_HIERARCHY 
		WHERE AWARD_NUMBER = AV_PROJECT_NUMBER ;  

	EXCEPTION    
		WHEN OTHERS THEN
			LS_ROOT_AWARD_NUMBER := NULL;
			LS_PARENT_AWARD_NUMBER := NULL;
	END;	

	BEGIN 
		SELECT NVL(T2.VALUE,'N/A') INTO LS_PCK_VALUE
		FROM AWARD T1
		INNER JOIN AWARD_CUSTOM_DATA T2 ON T1.AWARD_ID = T2.AWARD_ID
		WHERE T1.AWARD_NUMBER = AV_PROJECT_NUMBER
		AND T2.CUSTOM_ATTRIBUTE_ID = 31
		AND T1.AWARD_SEQUENCE_STATUS = 'ACTIVE'; 

	EXCEPTION    
		WHEN OTHERS THEN
			LS_PCK_VALUE := 'N/A';
	END;	


    OPEN cur_generic FOR
    SELECT 
        A.AWARD_ID AS PROJECT_ID,
        A.AWARD_NUMBER AS PROJECT_NUMBER,
        A.TITLE AS TITLE,
        A.LEAD_UNIT_NUMBER AS LEAD_UNIT_NUMBER,
        U.UNIT_NAME AS LEAD_UNIT_NAME,
        A.SPONSOR_CODE AS SPONSOR_CODE,
        S.SPONSOR_NAME AS SPONSOR_NAME,
        A.PRIME_SPONSOR_CODE AS PRIME_SPONSOR_CODE,
        SP.SPONSOR_NAME AS PRIME_SPONSOR_NAME,
        A.AWARD_EFFECTIVE_DATE AS PROJECT_START_DATE,
        AMI.FINAL_EXPIRATION_DATE AS PROJECT_END_DATE,
        A.ACCOUNT_NUMBER AS ACCOUNT_NUMBER,
        A.STATUS_CODE AS PROJECT_STATUS_CODE,
        ASS.DESCRIPTION AS PROJECT_STATUS,
        A.SEQUENCE_NUMBER AS VERSION_NUMBER,
        A.AWARD_TYPE_CODE AS PROJECT_TYPE_CODE,
        AT.DESCRIPTION AS PROJECT_TYPE,
        A.DOCUMENT_NUMBER AS DOCUMENT_NUMBER,
        A.AWARD_SEQUENCE_STATUS AS AWARD_SEQUENCE_STATUS,
        A.SPONSOR_AWARD_NUMBER AS SPONSOR_GRANT_NUMBER,
        NULL AS DOCUMENT_URL,
        A.UPDATE_TIMESTAMP AS SRC_SYS_UPDATE_TIMESTAMP,
        A.UPDATE_USER AS SRC_SYS_UPDATE_USER_NAME,
        AMI.ANTICIPATED_TOTAL_AMOUNT AS ANTICIPATED_TOTAL,
        AMI.AMOUNT_OBLIGATED_TO_DATE AS OBLIGATED_TOTAL,
        LI_LINKED_INST_PROPOSAL_IDS AS LINKED_INST_PROPOSAL_IDS,
        LS_ROOT_AWARD_NUMBER AS ROOT_PROJECT_NUMBER,
        LS_PARENT_AWARD_NUMBER AS PARENT_PROJECT_NUMBER,
        'PCK_FLAG' AS ATTRIBUTE_1_LABEL,
		LS_PCK_VALUE AS ATTRIBUTE_1_VALUE,
		NULL AS ATTRIBUTE_2_LABEL,
		NULL AS ATTRIBUTE_2_VALUE,
		NULL AS ATTRIBUTE_3_LABEL,
		NULL AS ATTRIBUTE_3_VALUE,
		NULL AS ATTRIBUTE_4_LABEL,
		NULL AS ATTRIBUTE_4_VALUE,
		NULL AS ATTRIBUTE_5_LABEL,
		NULL AS ATTRIBUTE_5_VALUE
		FROM AWARD A
		INNER JOIN UNIT U ON U.UNIT_NUMBER = A.LEAD_UNIT_NUMBER
		INNER JOIN SPONSOR S ON S.SPONSOR_CODE = A.SPONSOR_CODE
		LEFT JOIN SPONSOR SP ON SP.SPONSOR_CODE = A.PRIME_SPONSOR_CODE
		INNER JOIN AWARD_AMOUNT_INFO AMI ON AMI.AWARD_ID = A.AWARD_ID
		INNER JOIN AWARD_STATUS ASS ON ASS.STATUS_CODE = A.STATUS_CODE
		INNER JOIN AWARD_TYPE AT ON AT.AWARD_TYPE_CODE = A.AWARD_TYPE_CODE		
		WHERE A.AWARD_NUMBER = AV_PROJECT_NUMBER
		AND A.AWARD_SEQUENCE_STATUS = 'ACTIVE'
		AND AMI.AWARD_AMOUNT_INFO_ID = (
			SELECT MAX(T6.AWARD_AMOUNT_INFO_ID)
			FROM AWARD_AMOUNT_INFO T6
			WHERE T6.AWARD_ID = AMI.AWARD_ID);
END;
