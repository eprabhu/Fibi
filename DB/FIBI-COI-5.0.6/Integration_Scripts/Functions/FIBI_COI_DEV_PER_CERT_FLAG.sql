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
			
	ELSIF  AV_PROP_PERSON_ROLE_ID = 'COI' THEN
			LS_SUB_ITEM_CODE := '5';
			
	ELSIF  AV_PROP_PERSON_ROLE_ID = 'KP' THEN
			LS_SUB_ITEM_CODE := '6';
			
	ELSE
			LS_SUB_ITEM_CODE := '0';
			
	END IF;
	
	select COUNT(questionnaire_answer_header_id) INTO LI_COUNT		
	from questionnaire_answer_header 
	where module_Item_code = '3'
	and module_sub_item_code = LS_SUB_ITEM_CODE
	and  substr(module_item_key,1,(instr(module_item_key,'|')-1)) = AV_PROPOSAL_NUMBER
	and module_sub_item_key = AV_PERSON_ID
	and questionnaire_completed_flag = 'Y'
	and questionnaire_ref_id_fk IN  (
										select t1.questionnaire_ref_id_fk
										from questionnaire_usage t1
										where t1.module_Item_code = '3'
										and t1.module_sub_item_code = LS_SUB_ITEM_CODE
								 );
	
		
		IF LI_COUNT > 0 THEN		
			RETURN 'COMPLETED';
			
			
		ELSE		
			RETURN 'INCOMPLETE';
			
		END  IF;
	

	
	
END FIBI_COI_DEV_PER_CERT_FLAG;
