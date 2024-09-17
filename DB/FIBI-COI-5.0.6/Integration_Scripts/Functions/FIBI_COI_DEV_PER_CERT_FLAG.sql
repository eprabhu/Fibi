CREATE OR REPLACE FUNCTION FIBI_COI_DEV_PER_CERT_FLAG (
  AV_PROPOSAL_NUMBER IN EPS_PROP_PERSON.PROPOSAL_NUMBER%TYPE,
  AV_PERSON_ID IN EPS_PROP_PERSON.PERSON_ID%TYPE,
  AV_PROP_PERSON_ROLE_ID IN EPS_PROP_PERSON.PROP_PERSON_ROLE_ID%TYPE,
  AV_PERSON_PROJ_ROLE IN EPS_PROP_PERSON.PROJECT_ROLE%TYPE
) RETURN VARCHAR2
IS
  
  IS_CERTIFICATION_REQUIRED NUMBER := 0;
  LI_COUNT NUMBER; 
  LS_SUB_ITEM_CODE VARCHAR2(20);
  
BEGIN
-- Possbile return values are NOT_REQUIRED, COMPLETED, INCOMPLETE
	
	IS_CERTIFICATION_REQUIRED := FIBI_COI_DEV_PER_IS_CERT_REQ(AV_PROPOSAL_NUMBER,AV_PERSON_ID,AV_PROP_PERSON_ROLE_ID,AV_PERSON_PROJ_ROLE);
	
	IF IS_CERTIFICATION_REQUIRED = 0 THEN
	
		RETURN 'NOT_REQUIRED';
		
	END IF;
		
	
		
	IF AV_PROP_PERSON_ROLE_ID = 'PI' THEN
			LS_SUB_ITEM_CODE := '4';
			
	ELSIF  AV_PROP_PERSON_ROLE_ID IN ('COI','MPI') THEN
			LS_SUB_ITEM_CODE := '5';
			
	ELSIF  AV_PROP_PERSON_ROLE_ID = 'KP' THEN
			LS_SUB_ITEM_CODE := '6';
			
	ELSE
			LS_SUB_ITEM_CODE := '0';
			
	END IF;
	
		select COUNT(QUESTIONNAIRE_ANSWER_HEADER_ID) INTO LI_COUNT
		FROM QUESTIONNAIRE_ANSWER_HEADER QAH
		INNER JOIN EPS_PROP_PERSON_CERT_DETAILS EPPCD ON EPPCD.PROPOSAL_NUMBER = substr(QAH.MODULE_ITEM_KEY,1,instr(QAH.MODULE_ITEM_KEY,'|')-1)
		AND EPPCD.CERTIFIED_BY = QAH.MODULE_SUB_ITEM_KEY
		WHERE MODULE_ITEM_CODE = '3' 
		AND MODULE_SUB_ITEM_CODE = LS_SUB_ITEM_CODE
		and substr(QAH.module_item_key,1,(instr(QAH.module_item_key,'|')-1)) = AV_PROPOSAL_NUMBER
		AND QAH.module_sub_item_key = AV_PERSON_ID 
		AND QUESTIONNAIRE_REF_ID_FK IN  (
										SELECT T1.QUESTIONNAIRE_REF_ID_FK
										FROM QUESTIONNAIRE_USAGE T1
										WHERE T1.MODULE_ITEM_CODE = '3'
										AND T1.MODULE_SUB_ITEM_CODE = LS_SUB_ITEM_CODE
								 );
	
		
		IF LI_COUNT > 0 THEN		
			RETURN 'COMPLETED';
			
			
		ELSE		
			RETURN 'INCOMPLETE';
			
		END  IF;
	

	
	
END FIBI_COI_DEV_PER_CERT_FLAG;
