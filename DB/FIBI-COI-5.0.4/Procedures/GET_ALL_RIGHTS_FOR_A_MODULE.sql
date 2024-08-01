DELIMITER //
CREATE PROCEDURE GET_ALL_RIGHTS_FOR_A_MODULE(
    AV_MODULE_CODE INT(4),
    AV_PERSON_ID VARCHAR(40),
    AV_LEAD_UNIT VARCHAR(8),
    AV_PROJECT_ID DECIMAL(22)
)
BEGIN
DECLARE LI_COUNT INT;
DECLARE LI_ACTIVE_PROJECT_ID DECIMAL(22, 0);
DECLARE LI_FLAG INT;
DECLARE GC_LEAD_UNIT VARCHAR(8);

IF AV_MODULE_CODE = 1 THEN
    SELECT COUNT(1) INTO LI_FLAG
    FROM AWARD
    WHERE AWARD_ID = AV_PROJECT_ID;
	
	 SELECT G1.HOME_UNIT_NUMBER INTO GC_LEAD_UNIT
        FROM AWARD AWD1
        INNER JOIN GRANT_CALL_HEADER G1 ON AWD1.GRANT_HEADER_ID = G1.GRANT_HEADER_ID
        WHERE AWD1.AWARD_ID = AV_PROJECT_ID;
		
    IF LI_FLAG > 0 THEN
        SELECT AWARD_ID INTO LI_ACTIVE_PROJECT_ID
        FROM AWARD
        WHERE AWARD_NUMBER IN (
            SELECT AWARD_NUMBER
            FROM AWARD
            WHERE AWARD_ID = AV_PROJECT_ID
        )
        AND AWARD_SEQUENCE_STATUS = 'ACTIVE';
    END IF;

    SELECT COUNT(S1.MODULE_ITEM_ID) into LI_COUNT
    FROM WORKFLOW S1
    INNER JOIN WORKFLOW_DETAIL S2 ON S1.WORKFLOW_ID = S2.WORKFLOW_ID
    WHERE S1.IS_WORKFLOW_ACTIVE = 'Y'
    AND S2.APPROVER_PERSON_ID = AV_PERSON_ID
    AND S1.MODULE_CODE = 1
    AND S2.APPROVAL_STATUS = 'W'
    AND S1.MODULE_ITEM_ID = AV_PROJECT_ID;
    SELECT COUNT(1) INTO LI_FLAG
    FROM AWARD
    WHERE AWARD_ID = AV_PROJECT_ID
    AND GRANT_HEADER_ID IS NOT NULL;
    IF LI_FLAG > 0 THEN
        SELECT COUNT(1) INTO LI_FLAG
        FROM GRANT_CALL_HEADER T1
        INNER JOIN AWARD T2 ON T1.GRANT_HEADER_ID = T2.GRANT_HEADER_ID
        WHERE T2.AWARD_ID = AV_PROJECT_ID
        AND T1.HOME_UNIT_NUMBER <> '000001';
    END IF;

    IF LI_FLAG = 0 THEN
        SELECT DISTINCT RLE.RIGHT_NAME
        FROM
        PERSON_ROLES PR JOIN  
        (SELECT ROLE_ID,RT.RIGHT_NAME 
           FROM ROLE_RIGHTS RR,
                RIGHTS RT
            WHERE RR.RIGHT_ID = RT.RIGHT_ID
              AND RT.RIGHT_NAME IN (
         'CREATE_AWARD',
        'MODIFY_AWARD',
        'VIEW_AWARD',
        'CREATE_AWARD_BUDGET',
        'MODIFY_AWARD_BUDGET',
        'VIEW_AWARD_BUDGET',
        'CREATE_SERVICE_REQUEST',
        'MODIFY_SERVICE_REQUEST',
        'VIEW_SERVICE_REQUEST',
        'ASSIGN_SERVICE_REQUEST',
        'DELETE_SERVICE_REQUEST',
        'RESOLVE_SERVICE_REQUEST',
        'MAINTAIN_AWARD_BUDGET',
        'MAINTAIN_ATTACHMENTS',
        'MAINTAIN_REPORTING_REQUIREMENTS',
        'MAINTAIN_DATES_AMOUNTS',
        'ADD_APPROVER_WORKFLOW',
        'PASS_WORKFLOW',
        'BYPASS_WORKFLOW',
        'DELEGATE_WORKFLOW',
        'WRITE_REVIEW',
        'MAINTAIN_PRIVATE_COMMENTS',
        'INITIATE_PROJECT_CLOSURE',
        'CREATE_VARIATION_REQUEST',
        'SUBMIT_AWARD',
        'MAINTAIN_AWARD_EXPENSES',
        'WITHDRAW_AWARD',
        'DISAPPROVE_VARIATION_REQUEST',
        'MAINTAIN_TASK',
        'MODIFY_AWARD_OUTCOME',
        'VIEW_EOM_EXPENDITURE_TO_DATE',
        'VIEW_AWARD_PURCHASE_DETAILS',
        'SUBMIT_VARIATION_REQUEST',
        'VIEW_EXPENSE_TRANSACTION',
        'MODIFY_DOCUMENT_PERMISSION',
        'VIEW_PRIVATE_COMMENTS',
        'RESOLVE_COMMENT',
        'REVIEW_COMMENTS_RIGHT',
        'VIEW_CONFIDENTIAL_AWARD_ATTACHMENTS',
        'MAINTAIN_MANPOWER',
        'MANPOWER_VIEW_STAFF_PLAN',
        'MANPOWER_VIEW_STUDENT_PLAN',
        'MANPOWER_VIEW_STAFF_PAYROLL',
        'MANPOWER_MODIFY_STAFF_COMMITTED_AMOUNT',
        'MANPOWER_MAINTAIN_OTHERS',
        'MANPOWER_MAINTAIN_STAFF',
        'MANPOWER_MAINTAIN_STUDENT',
        'MANPOWER_VIEW_OTHERS_PLAN',
        'VIEW_AWARD_PURCHASE_TRANSACTION',
        'VIEW_AWARD_CLAIM_TRACKING',
        'CREATE_PROGRESS_REPORT',
        'VIEW_PROGRESS_REPORT',
        'MODIFY_AWARD_BUDGET_IN_ROUTING',
        'MANPOWER_MODIFY_STAFF_CHARGE_END_DATE',
        'VIEW_ANTICIPATED_FUNDING_DISTRIBUTION',
        'MODIFY_ANTICIPATED_FUNDING_DISTRIBUTION',
		'VIEW_AWARD_REVENUE_DETAILS'
    )
        ) RLE ON RLE.ROLE_ID = PR.ROLE_ID 
		
        WHERE PR.PERSON_ID = AV_PERSON_ID
          AND (( PR. DESCEND_FLAG = 'Y' AND  EXISTS (SELECT 1
                                                                 FROM UNIT_WITH_CHILDREN 
                                                                                  WHERE UNIT_NUMBER = PR.UNIT_NUMBER
																				  AND CHILD_UNIT_NUMBER = AV_LEAD_UNIT
             ))OR ( PR. DESCEND_FLAG = 'N' AND PR.UNIT_NUMBER = AV_LEAD_UNIT )
            )
         
UNION
    SELECT DISTINCT T3.RIGHT_NAME
    FROM AWARD_PERSON_ROLES T1
    INNER JOIN ROLE_RIGHTS T2 ON T1.ROLE_ID = T2.ROLE_ID
    INNER JOIN RIGHTS T3 ON T2.RIGHT_ID = T3.RIGHT_ID
    WHERE T1.AWARD_ID = LI_ACTIVE_PROJECT_ID
    AND T1.PERSON_ID = AV_PERSON_ID
UNION
    SELECT T3.RIGHT_NAME
    FROM AWARD_PERSON_ROLES T1
    INNER JOIN ROLE_RIGHTS T2 ON T1.ROLE_ID = T2.ROLE_ID
    INNER JOIN RIGHTS T3 ON T2.RIGHT_ID = T3.RIGHT_ID
    INNER JOIN AWARD T4 ON T4.AWARD_ID = T1.AWARD_ID
    WHERE T1.AWARD_ID = AV_PROJECT_ID
    AND T4.AWARD_SEQUENCE_STATUS = 'PENDING'
    AND T4.AWARD_DOCUMENT_TYPE_CODE = 1
    AND T1.PERSON_ID = AV_PERSON_ID
UNION
    SELECT t.RIGHT_NAME
    FROM (
        SELECT 'VIEW_EOM_EXPENDITURE_TO_DATE' AS RIGHT_NAME
        FROM dual
        UNION
        SELECT 'VIEW_AWARD_PURCHASE_DETAILS' AS RIGHT_NAME
        FROM dual
        UNION
        SELECT 'VIEW_AWARD' AS RIGHT_NAME
        FROM dual
        UNION
        SELECT 'VIEW_EXPENSE_TRANSACTION' AS RIGHT_NAME
        FROM dual
        UNION
        SELECT 'VIEW_AWARD_PURCHASE_TRANSACTION' AS RIGHT_NAME
        FROM dual
		UNION
		SELECT 'VIEW_AWARD_REVENUE_DETAILS' AS RIGHT_NAME
		FROM DUAL
    ) t
    where 0 < LI_COUNT;
ELSE
	SELECT DISTINCT RLE.RIGHT_NAME
        FROM
        PERSON_ROLES PR JOIN 
        (SELECT ROLE_ID,RT.RIGHT_NAME 
           FROM ROLE_RIGHTS RR,
                RIGHTS RT
            WHERE RR.RIGHT_ID = RT.RIGHT_ID
              AND RT.RIGHT_NAME IN (
         'CREATE_AWARD',
        'MODIFY_AWARD',
        'VIEW_AWARD',
        'CREATE_AWARD_BUDGET',
        'MODIFY_AWARD_BUDGET',
        'VIEW_AWARD_BUDGET',
        'CREATE_SERVICE_REQUEST',
        'MODIFY_SERVICE_REQUEST',
        'VIEW_SERVICE_REQUEST',
        'ASSIGN_SERVICE_REQUEST',
        'DELETE_SERVICE_REQUEST',
        'RESOLVE_SERVICE_REQUEST',
        'MAINTAIN_AWARD_BUDGET',
        'MAINTAIN_ATTACHMENTS',
        'MAINTAIN_REPORTING_REQUIREMENTS',
        'MAINTAIN_DATES_AMOUNTS',
        'ADD_APPROVER_WORKFLOW',
        'PASS_WORKFLOW',
        'BYPASS_WORKFLOW',
        'DELEGATE_WORKFLOW',
        'WRITE_REVIEW',
        'MAINTAIN_PRIVATE_COMMENTS',
        'INITIATE_PROJECT_CLOSURE',
        'CREATE_VARIATION_REQUEST',
        'SUBMIT_AWARD',
        'MAINTAIN_AWARD_EXPENSES',
        'WITHDRAW_AWARD',
        'DISAPPROVE_VARIATION_REQUEST',
        'MAINTAIN_TASK',
        'MODIFY_AWARD_OUTCOME',
        'VIEW_EOM_EXPENDITURE_TO_DATE',
        'VIEW_AWARD_PURCHASE_DETAILS',
        'SUBMIT_VARIATION_REQUEST',
        'VIEW_EXPENSE_TRANSACTION',
        'MODIFY_DOCUMENT_PERMISSION',
        'VIEW_PRIVATE_COMMENTS',
        'RESOLVE_COMMENT',
        'REVIEW_COMMENTS_RIGHT',
        'VIEW_CONFIDENTIAL_AWARD_ATTACHMENTS',
        'MAINTAIN_MANPOWER',
        'MANPOWER_VIEW_STAFF_PLAN',
        'MANPOWER_VIEW_STUDENT_PLAN',
        'MANPOWER_VIEW_STAFF_PAYROLL',
        'MANPOWER_MODIFY_STAFF_COMMITTED_AMOUNT',
        'MANPOWER_MAINTAIN_OTHERS',
        'MANPOWER_MAINTAIN_STAFF',
        'MANPOWER_MAINTAIN_STUDENT',
        'MANPOWER_VIEW_OTHERS_PLAN',
        'MANPOWER_MODIFY_STAFF_CHARGE_START_DATE',
        'VIEW_AWARD_PURCHASE_TRANSACTION',
        'MANPOWER_ENABLE_TRIGGER_POSITION',
        'VIEW_AWARD_CLAIM_TRACKING',
        'CREATE_PROGRESS_REPORT',
        'VIEW_PROGRESS_REPORT',
        'MODIFY_AWARD_BUDGET_IN_ROUTING',
        'MANPOWER_MODIFY_STAFF_CHARGE_END_DATE',
        'VIEW_ANTICIPATED_FUNDING_DISTRIBUTION',
        'MODIFY_ANTICIPATED_FUNDING_DISTRIBUTION',
		'VIEW_AWARD_REVENUE_DETAILS'
    )
        ) RLE ON RLE.ROLE_ID = PR.ROLE_ID 
		
        WHERE PR.PERSON_ID = AV_PERSON_ID
          AND (( PR. DESCEND_FLAG = 'Y' AND  EXISTS (SELECT 1
                                                                 FROM UNIT_WITH_CHILDREN 
                                                                                  WHERE UNIT_NUMBER = PR.UNIT_NUMBER
																				  AND CHILD_UNIT_NUMBER =  AV_LEAD_UNIT
             ))OR ( PR. DESCEND_FLAG = 'N' AND PR.UNIT_NUMBER = AV_LEAD_UNIT )
            )
         
UNION
    SELECT DISTINCT T3.RIGHT_NAME
    FROM AWARD_PERSON_ROLES T1
    INNER JOIN ROLE_RIGHTS T2 ON T1.ROLE_ID = T2.ROLE_ID
    INNER JOIN RIGHTS T3 ON T2.RIGHT_ID = T3.RIGHT_ID
    WHERE T1.AWARD_ID = LI_ACTIVE_PROJECT_ID
    AND T1.PERSON_ID = AV_PERSON_ID
UNION
    SELECT T3.RIGHT_NAME
    FROM AWARD_PERSON_ROLES T1
    INNER JOIN ROLE_RIGHTS T2 ON T1.ROLE_ID = T2.ROLE_ID
    INNER JOIN RIGHTS T3 ON T2.RIGHT_ID = T3.RIGHT_ID
    INNER JOIN AWARD T4 ON T4.AWARD_ID = T1.AWARD_ID
    WHERE T1.AWARD_ID = AV_PROJECT_ID
    AND T4.AWARD_DOCUMENT_TYPE_CODE = 1
    AND T4.AWARD_SEQUENCE_STATUS = 'PENDING'
    AND T1.PERSON_ID = AV_PERSON_ID
UNION
    SELECT t.RIGHT_NAME
    FROM (
        SELECT 'VIEW_EOM_EXPENDITURE_TO_DATE' AS RIGHT_NAME
        FROM dual
        UNION
        SELECT 'VIEW_AWARD_PURCHASE_DETAILS' AS RIGHT_NAME
        FROM dual
        UNION
        SELECT 'VIEW_AWARD' AS RIGHT_NAME
        FROM dual
        UNION
        SELECT 'VIEW_EXPENSE_TRANSACTION' AS RIGHT_NAME
        FROM dual
        UNION
        SELECT 'VIEW_AWARD_PURCHASE_TRANSACTION' AS RIGHT_NAME
        FROM dual
		UNION
		SELECT 'VIEW_AWARD_REVENUE_DETAILS' AS RIGHT_NAME
		FROM DUAL
    ) t
    where 0 < LI_COUNT
UNION
   SELECT DISTINCT RLE.RIGHT_NAME
        FROM 
        PERSON_ROLES PR JOIN                                                           
        (SELECT ROLE_ID,RT.RIGHT_NAME 
           FROM ROLE_RIGHTS RR,
                RIGHTS RT
            WHERE RR.RIGHT_ID = RT.RIGHT_ID
        ) RLE ON RLE.ROLE_ID = PR.ROLE_ID AND RLE.ROLE_ID = 100     
        WHERE PR.PERSON_ID = AV_PERSON_ID
          AND (( PR. DESCEND_FLAG = 'Y' AND  EXISTS (SELECT 1
                                                                 FROM UNIT_WITH_CHILDREN 
                                                                                  WHERE UNIT_NUMBER = PR.UNIT_NUMBER
																				  AND CHILD_UNIT_NUMBER = GC_LEAD_UNIT
             ))OR ( PR. DESCEND_FLAG = 'N' AND PR.UNIT_NUMBER = GC_LEAD_UNIT )
            )
         

UNION

   SELECT DISTINCT RLE.RIGHT_NAME  FROM  PERSON_ROLES PR JOIN        
										(SELECT ROLE_ID,RT.RIGHT_NAME FROM ROLE_RIGHTS RR, RIGHTS RT
										WHERE RR.RIGHT_ID = RT.RIGHT_ID) RLE ON RLE.ROLE_ID = PR.ROLE_ID  
										WHERE PR.PERSON_ID = (SELECT DISTINCT PERSON_ID
										FROM GRANT_CALL_FUNDING_SCHEME_MANAGERS
										WHERE FUNDING_SCHEME_CODE IN (SELECT TS3.FUNDING_SCHEME_CODE FROM GRANT_CALL_HEADER TS1
												INNER JOIN AWARD TS2 ON TS1.GRANT_HEADER_ID = TS2.GRANT_HEADER_ID
												INNER JOIN SPONSOR_FUNDING_SCHEME TS3 ON TS3.FUNDING_SCHEME_ID = TS1.FUNDING_SCHEME_ID
												WHERE TS2.AWARD_ID = AV_PROJECT_ID)
										AND PERSON_ID = AV_PERSON_ID);

END IF;

ELSEIF AV_MODULE_CODE = 2 THEN
    SELECT DISTINCT RLE.RIGHT_NAME
        FROM
        PERSON_ROLES PR JOIN   
        (SELECT ROLE_ID,RT.RIGHT_NAME 
           FROM ROLE_RIGHTS RR,
                RIGHTS RT
            WHERE RR.RIGHT_ID = RT.RIGHT_ID
              AND RT.RIGHT_NAME IN (
        'MODIFY_INST_PROPOSAL',
        'VIEW_INST_PROPOSAL',
        'CREATE_AWARD'
    )
        ) RLE ON RLE.ROLE_ID = PR.ROLE_ID 
        WHERE PR.PERSON_ID = AV_PERSON_ID
          AND (( PR. DESCEND_FLAG = 'Y' AND  EXISTS (SELECT 1
                                                                 FROM UNIT_WITH_CHILDREN 
                                                                                  WHERE UNIT_NUMBER = PR.UNIT_NUMBER
																				  AND CHILD_UNIT_NUMBER = AV_LEAD_UNIT
             ))OR ( PR. DESCEND_FLAG = 'N' AND PR.UNIT_NUMBER = AV_LEAD_UNIT )
            );

ELSEIF AV_MODULE_CODE = 3 THEN
    SELECT COUNT(1) INTO LI_FLAG
    FROM EPS_PROPOSAL
    WHERE PROPOSAL_ID = AV_PROJECT_ID
    AND GRANT_HEADER_ID IS NOT NULL;
	
	 SELECT G1.HOME_UNIT_NUMBER INTO GC_LEAD_UNIT
        FROM EPS_PROPOSAL EPS1
        INNER JOIN GRANT_CALL_HEADER G1 ON EPS1.GRANT_HEADER_ID = G1.GRANT_HEADER_ID
        WHERE EPS1.PROPOSAL_ID = AV_PROJECT_ID;
		
    IF LI_FLAG > 0 THEN
        SELECT COUNT(1) INTO LI_FLAG
        FROM GRANT_CALL_HEADER T1
        INNER JOIN EPS_PROPOSAL T2 ON T1.GRANT_HEADER_ID = T2.GRANT_HEADER_ID
        WHERE T2.PROPOSAL_ID = AV_PROJECT_ID
        AND T1.HOME_UNIT_NUMBER <> '000001';
    END IF;
    IF LI_FLAG = 0 THEN
		SELECT DISTINCT RLE.RIGHT_NAME
        FROM
        PERSON_ROLES PR JOIN    
        (SELECT ROLE_ID,RT.RIGHT_NAME 
           FROM ROLE_RIGHTS RR,
                RIGHTS RT
            WHERE RR.RIGHT_ID = RT.RIGHT_ID
              AND RT.RIGHT_NAME IN (
       'CREATE_PROPOSAL',
        'MODIFY_PROPOSAL',
        'DELETE_PROPOSAL',
        'SUBMIT_PROPOSAL',
        'MAINTAIN_PROPOSAL_BUDGET',
        'VIEW_PROPOSAL',
        'WITHDRAW_PROPOSAL',
        'VIEW_PROPOSAL_ATTACHMENTS',
        'MODIFY_PROPOSAL_ATTACHEMNTS',
        'ASSIGN_REVIEW',
        'ENDORSE_PROPOSAL',
        'MAINTAIN_RANK',
        'DEFINE_APPROVED_BUDGET',
        'MAINTAIN_PRIVATE_COMMENTS',
        'WRITE_REVIEW',
        'VIEW_REVIEW_COMPLETED_TAB',
        'VIEW_REVIEW_IN_PROGRESS_TAB',
        'BYPASS_APPROVER',
        'ALTER_PROPOSAL_DATA',
        'MAINTAIN_PROPOSAL_ACCESS',
        'APPROVE_PROPOSAL_EVALUATION',
        'VIEW_ANY_PROPOSAL',
        'MAINTAIN_APPLICATIONS',
        'SHORTLIST_APPLICATIONS',
        'DEACTIVATE_PROPOSAL',
        'MAINTAIN_EVALUATION_FORM',
        'BYPASS_WORKFLOW',
        'VIEW_EVALUATION_ROUTE_LOG',
        'MODIFY_DOCUMENT_PERMISSION',
        'START_EVALUATION',
        'ADD_APPROVER_WORKFLOW',
        'PD_ADMIN_CORRECTION',
        'CREATE_INST_PROPOSAL',
        'MODIFY_PROPOSAL_ATTACHMENTS_IN_WORKFLOW',
        'PROXY_PROPOSAL_PERSON_CERTIFICATION',
        'CREATE_EXT_REVIEW',
        'VIEW_EXT_REVIEW',
        'MODIFY_EXT_REVIEW',
        'MODIFY_PROPOSAL_QUESTIONNAIRE',
        'VIEW_CONFIDENTIAL_PROPOSAL_ATTACHMENTS'
    )
        ) RLE ON RLE.ROLE_ID = PR.ROLE_ID  
		
        WHERE PR.PERSON_ID = AV_PERSON_ID
          AND (( PR. DESCEND_FLAG = 'Y' AND EXISTS (SELECT 1
                                                                 FROM UNIT_WITH_CHILDREN 
                                                                                  WHERE UNIT_NUMBER = PR.UNIT_NUMBER
																				  AND CHILD_UNIT_NUMBER = AV_LEAD_UNIT
             ))OR ( PR. DESCEND_FLAG = 'N' AND PR.UNIT_NUMBER = AV_LEAD_UNIT )
            )
         
UNION
    SELECT T3.RIGHT_NAME
    FROM EPS_PROPOSAL_PERSON_ROLES T1
    INNER JOIN ROLE_RIGHTS T2 ON T1.ROLE_ID = T2.ROLE_ID
    INNER JOIN RIGHTS T3 ON T2.RIGHT_ID = T3.RIGHT_ID
    WHERE T1.PROPOSAL_ID = AV_PROJECT_ID
    AND T1.PERSON_ID = AV_PERSON_ID;
ELSE 
    SELECT DISTINCT RLE.RIGHT_NAME
        FROM
        PERSON_ROLES PR  JOIN   
        (SELECT ROLE_ID,RT.RIGHT_NAME 
           FROM ROLE_RIGHTS RR,
                RIGHTS RT
            WHERE RR.RIGHT_ID = RT.RIGHT_ID
              AND RT.RIGHT_NAME IN (
        'CREATE_PROPOSAL',
        'MODIFY_PROPOSAL',
        'DELETE_PROPOSAL',
        'SUBMIT_PROPOSAL',
        'MAINTAIN_PROPOSAL_BUDGET',
        'VIEW_PROPOSAL',
        'WITHDRAW_PROPOSAL',
        'VIEW_PROPOSAL_ATTACHMENTS',
        'MODIFY_PROPOSAL_ATTACHEMNTS',
        'ASSIGN_REVIEW',
        'ENDORSE_PROPOSAL',
        'MAINTAIN_RANK',
        'DEFINE_APPROVED_BUDGET',
        'MAINTAIN_PRIVATE_COMMENTS',
        'WRITE_REVIEW',
        'VIEW_REVIEW_COMPLETED_TAB',
        'VIEW_REVIEW_IN_PROGRESS_TAB',
        'BYPASS_APPROVER',
        'ALTER_PROPOSAL_DATA',
        'MAINTAIN_PROPOSAL_ACCESS',
        'APPROVE_PROPOSAL_EVALUATION',
        'VIEW_ANY_PROPOSAL',
        'MAINTAIN_APPLICATIONS',
        'SHORTLIST_APPLICATIONS',
        'DEACTIVATE_PROPOSAL',
        'MAINTAIN_EVALUATION_FORM',
        'BYPASS_WORKFLOW',
        'VIEW_EVALUATION_ROUTE_LOG',
        'MODIFY_DOCUMENT_PERMISSION',
        'START_EVALUATION',
        'ADD_APPROVER_WORKFLOW',
        'PD_ADMIN_CORRECTION',
        'CREATE_INST_PROPOSAL',
        'MODIFY_PROPOSAL_ATTACHMENTS_IN_WORKFLOW',
        'PROXY_PROPOSAL_PERSON_CERTIFICATION',
        'CREATE_EXT_REVIEW',
        'VIEW_EXT_REVIEW',
        'MODIFY_EXT_REVIEW',
        'MODIFY_PROPOSAL_QUESTIONNAIRE',
        'VIEW_CONFIDENTIAL_PROPOSAL_ATTACHMENTS'
    )
        ) RLE     ON RLE.ROLE_ID = PR.ROLE_ID    
        WHERE PR.PERSON_ID = AV_PERSON_ID
          AND (( PR. DESCEND_FLAG = 'Y' AND EXISTS  (SELECT 1
                                                                 FROM UNIT_WITH_CHILDREN 
                                                                                  WHERE UNIT_NUMBER = PR.UNIT_NUMBER
																				  AND CHILD_UNIT_NUMBER = AV_LEAD_UNIT
             ))OR ( PR. DESCEND_FLAG = 'N' AND PR.UNIT_NUMBER = AV_LEAD_UNIT )
            )
           
           
         
UNION
    SELECT T3.RIGHT_NAME
    FROM EPS_PROPOSAL_PERSON_ROLES T1
    INNER JOIN ROLE_RIGHTS T2 ON T1.ROLE_ID = T2.ROLE_ID
    INNER JOIN RIGHTS T3 ON T2.RIGHT_ID = T3.RIGHT_ID
    WHERE T1.PROPOSAL_ID = AV_PROJECT_ID
    AND T1.PERSON_ID = AV_PERSON_ID
UNION
  SELECT DISTINCT RLE.RIGHT_NAME
        FROM
        PERSON_ROLES PR JOIN                   
        (SELECT ROLE_ID,RT.RIGHT_NAME 
           FROM ROLE_RIGHTS RR,
                RIGHTS RT
            WHERE RR.RIGHT_ID = RT.RIGHT_ID
        ) RLE ON RLE.ROLE_ID = PR.ROLE_ID AND RLE.ROLE_ID = 100   
        WHERE PR.PERSON_ID = AV_PERSON_ID
          AND (( PR. DESCEND_FLAG = 'Y' AND  EXISTS (SELECT 1
                                                                 FROM UNIT_WITH_CHILDREN 
                                                                                  WHERE UNIT_NUMBER = PR.UNIT_NUMBER
																				  AND CHILD_UNIT_NUMBER = GC_LEAD_UNIT
             ))OR ( PR. DESCEND_FLAG = 'N' AND PR.UNIT_NUMBER = GC_LEAD_UNIT )
            );
END IF;

ELSEIF AV_MODULE_CODE = 5 THEN
    SELECT DISTINCT RLE.RIGHT_NAME
        FROM
        PERSON_ROLES PR JOIN                  
        (SELECT ROLE_ID,RT.RIGHT_NAME 
           FROM ROLE_RIGHTS RR,
                RIGHTS RT
            WHERE RR.RIGHT_ID = RT.RIGHT_ID
              AND RT.RIGHT_NAME IN (
			   'CREATE_NEGOTIATIONS',
        'MODIFY_NEGOTIATIONS',
        'VIEW_NEGOTIATIONS'
    )
        ) RLE ON RLE.ROLE_ID = PR.ROLE_ID        
        WHERE PR.PERSON_ID = AV_PERSON_ID
          AND (( PR. DESCEND_FLAG = 'Y' AND  EXISTS (SELECT 1
                                                                 FROM UNIT_WITH_CHILDREN 
                                                                                  WHERE UNIT_NUMBER = PR.UNIT_NUMBER
																				  AND CHILD_UNIT_NUMBER = AV_LEAD_UNIT
             ))OR ( PR. DESCEND_FLAG = 'N' AND PR.UNIT_NUMBER = AV_LEAD_UNIT )
            );

ELSEIF AV_MODULE_CODE = 7 THEN

	SELECT DISTINCT RLE.RIGHT_NAME
        FROM
        PERSON_ROLES PR JOIN                     
        (SELECT ROLE_ID,RT.RIGHT_NAME 
           FROM ROLE_RIGHTS RR,
                RIGHTS RT
            WHERE RR.RIGHT_ID = RT.RIGHT_ID
              AND RT.RIGHT_NAME IN (
			    'CREATE_IRB_PROTOCOL',
        'MODIFY_IRB_PROTOCOL',
        'VIEW_IRB_PROTOCOL',
        'IRB_ADMINISTRATOR',
        'IRB_DEPT_ADMINISTATOR'
    )
        ) RLE ON RLE.ROLE_ID = PR.ROLE_ID             
        WHERE PR.PERSON_ID = AV_PERSON_ID
          AND (( PR. DESCEND_FLAG = 'Y' AND EXISTS (SELECT 1
                                                                 FROM UNIT_WITH_CHILDREN 
                                                                                  WHERE UNIT_NUMBER = PR.UNIT_NUMBER
																				  AND CHILD_UNIT_NUMBER = AV_LEAD_UNIT
             ))OR ( PR. DESCEND_FLAG = 'N' AND PR.UNIT_NUMBER = AV_LEAD_UNIT )
            );

ELSEIF AV_MODULE_CODE = 13 THEN

	SELECT DISTINCT RLE.RIGHT_NAME
        FROM
        PERSON_ROLES PR JOIN                       
        (SELECT ROLE_ID,RT.RIGHT_NAME 
           FROM ROLE_RIGHTS RR,
                RIGHTS RT
            WHERE RR.RIGHT_ID = RT.RIGHT_ID
              AND RT.RIGHT_NAME IN (
			     'AGREEMENT_ADMINISTRATOR',
        'CREATE_AGREEMENT',
        'VIEW_ALL_AGREEMENT',
        'SUBMIT_AGREEMENT',
        'GENERATE_DOCUMENT',
        'MODIFY_CLAUSES',
        'ADD_APPROVER_WORKFLOW',
        'BYPASS_WORKFLOW',
        'MODIFY_ALL_AGREEMENT',
        'VIEW_ADMIN_GROUP_AGREEMENT',
        'DELETE_AGREEMENT'
    )
        ) RLE  ON RLE.ROLE_ID = PR.ROLE_ID          
        WHERE PR.PERSON_ID = AV_PERSON_ID
          AND (( PR. DESCEND_FLAG = 'Y' AND EXISTS (SELECT 1
                                                                 FROM UNIT_WITH_CHILDREN 
                                                                                  WHERE UNIT_NUMBER = PR.UNIT_NUMBER
																				  AND CHILD_UNIT_NUMBER = AV_LEAD_UNIT
             ))OR ( PR. DESCEND_FLAG = 'N' AND PR.UNIT_NUMBER = AV_LEAD_UNIT )
            );

ELSEIF AV_MODULE_CODE = 15 THEN

	SELECT DISTINCT RLE.RIGHT_NAME
        FROM
        PERSON_ROLES PR JOIN                    
        (SELECT ROLE_ID,RT.RIGHT_NAME 
           FROM ROLE_RIGHTS RR,
                RIGHTS RT
            WHERE RR.RIGHT_ID = RT.RIGHT_ID
              AND RT.RIGHT_NAME IN (
			    'CREATE_GRANT_CALL',
        'MODIFY_GRANT_CALL',
        'VIEW_GRANT_CALL',
        'PUBLISH_GRANT_CALL',
        'DELETE_GRANT_CALL',
        'ARCHIVE_GRANT_CALL',
        'APPLY_PROPOSAL',
        'VIEW_GRANT_CALL_APPLICATIONS',
        'MAINTAIN_APPLICATIONS',
        'SHORTLIST_APPLICATIONS',
        'VIEW_IOI'
    )
        ) RLE ON RLE.ROLE_ID = PR.ROLE_ID            
        WHERE PR.PERSON_ID = AV_PERSON_ID
          AND (( PR. DESCEND_FLAG = 'Y' AND  EXISTS (SELECT 1
                                                                 FROM UNIT_WITH_CHILDREN 
                                                                                  WHERE UNIT_NUMBER = PR.UNIT_NUMBER
																				  AND CHILD_UNIT_NUMBER = AV_LEAD_UNIT 
             ))OR ( PR. DESCEND_FLAG = 'N' AND PR.UNIT_NUMBER = AV_LEAD_UNIT )
            );

ELSEIF AV_MODULE_CODE = 20 THEN
    
    	SELECT DISTINCT RLE.RIGHT_NAME
        FROM
        PERSON_ROLES PR JOIN                                
        (SELECT ROLE_ID,RT.RIGHT_NAME 
           FROM ROLE_RIGHTS RR,
                RIGHTS RT
            WHERE RR.RIGHT_ID = RT.RIGHT_ID
              AND RT.RIGHT_NAME IN (
			    'VIEW_PRIVATE_COMMENTS',
        'MAINTAIN_PRIVATE_COMMENTS',
        'ADD_APPROVER_WORKFLOW',
        'BYPASS_WORKFLOW',
        'MAINTAIN_SERVICE_REQUEST',
        'CREATE_SERVICE_REQUEST',
        'VIEW_SERVICE_REQUEST',
        'SUBMIT_SERVICE_REQUEST',
        'RESOLVE_SERVICE_REQUEST',
        'ASSIGN_SERVICE_REQUEST',
        'MAINTAIN_WATCHER',
        'VIEW_ASSIGN_GROUP_SERVICE_REQUEST',
        'RETURN_SERVICE_REQUEST',
        'WITHDRAW_SERVICE_REQUEST',
        'DEACTIVATE_SERVICE_REQUEST',
        'DELETE_SERVICE_REQUEST',
        'REOPEN_SERVICE_REQUEST',
        'REJECT_SERVICE_REQUEST',
        'CLOSE_SERVICE_REQUEST',
        'MODIFY_SERVICE_REQUEST_ATTACHMENTS',
		'VIEW_CONFIDENTIAL_SERVICE_REQUEST_ATTACHMENTS',
        'MODIFY_SERVICE_REQUEST_REPORTER'
   )
        ) RLE ON RLE.ROLE_ID = PR.ROLE_ID                    
        WHERE PR.PERSON_ID = AV_PERSON_ID
          AND (( PR. DESCEND_FLAG = 'Y' AND  EXISTS (SELECT 1
                                                                 FROM UNIT_WITH_CHILDREN 
                                                                                  WHERE UNIT_NUMBER = PR.UNIT_NUMBER
																				  AND CHILD_UNIT_NUMBER = AV_LEAD_UNIT 
             ))OR ( PR. DESCEND_FLAG = 'N' AND PR.UNIT_NUMBER = AV_LEAD_UNIT )
            )
         
UNION
    SELECT right_name
    FROM rights
    where right_name in (
        'ASSIGN_SERVICE_REQUEST',
        'RESOLVE_SERVICE_REQUEST',
        'RETURN_SERVICE_REQUEST',
        'REJECT_SERVICE_REQUEST'
   )
    and exists (
        SELECT 1
        FROM sr_header
        where ASSIGNEE_PERSON_ID = AV_PERSON_ID
        AND SR_HEADER_ID = AV_PROJECT_ID
    );

ELSEIF AV_MODULE_CODE = 14 THEN
    SELECT COUNT(1) INTO LI_FLAG
    FROM CLAIM
    WHERE CLAIM_ID = AV_PROJECT_ID;
	
		SELECT G1.HOME_UNIT_NUMBER INTO GC_LEAD_UNIT
        FROM AWARD AWD1
        INNER JOIN GRANT_CALL_HEADER G1 ON AWD1.GRANT_HEADER_ID = G1.GRANT_HEADER_ID
        WHERE AWD1.AWARD_ID = (
                SELECT AWARD_ID
                FROM CLAIM
                where CLAIM_ID = AV_PROJECT_ID
            );
			
    IF LI_FLAG > 0 THEN
        SELECT AWARD_ID INTO LI_ACTIVE_PROJECT_ID
        FROM CLAIM
        WHERE CLAIM_ID = AV_PROJECT_ID;
    END IF;
	
	SELECT DISTINCT RLE.RIGHT_NAME
        FROM
        PERSON_ROLES PR JOIN                          
        (SELECT ROLE_ID,RT.RIGHT_NAME 
           FROM ROLE_RIGHTS RR,
                RIGHTS RT
            WHERE RR.RIGHT_ID = RT.RIGHT_ID
              AND RT.RIGHT_NAME IN (
			    'ADD_APPROVER_WORKFLOW',
        'BYPASS_WORKFLOW',
        'UPLOAD_CLAIM_FORECAST_REPORT',
        'VIEW_CLAIMS',
        'MODIFY_CLAIMS',
        'VIEW_AWARD_CLAIM_TRACKING',
        'CLAIM_MAINTAIN_DETAILED_BREAKDOWN',
        'CLAIM_MODIFY_INVOICE',
        'CLAIM_TRIGGER_INVOICE',
        'CLAIM_PREPARER',
        'VIEW_CONFIDENTIAL_CLAIM_ATTACHMENTS'
    )
        ) RLE ON RLE.ROLE_ID = PR.ROLE_ID              
        WHERE PR.PERSON_ID = AV_PERSON_ID
          AND (( PR. DESCEND_FLAG = 'Y' AND  EXISTS (SELECT 1
                                                                 FROM UNIT_WITH_CHILDREN 
                                                                                  WHERE UNIT_NUMBER = PR.UNIT_NUMBER
																				  AND CHILD_UNIT_NUMBER = AV_LEAD_UNIT
             ))OR ( PR. DESCEND_FLAG = 'N' AND PR.UNIT_NUMBER = AV_LEAD_UNIT )
            )
         
UNION
    SELECT T4.RIGHT_NAME
    FROM AWARD_PERSON_ROLES T1
    INNER JOIN ROLE_RIGHTS T3 ON T1.ROLE_ID = T3.ROLE_ID
    INNER JOIN RIGHTS T4 ON T3.RIGHT_ID = T4.RIGHT_ID
    WHERE PERSON_ID = AV_PERSON_ID
    AND AWARD_ID = LI_ACTIVE_PROJECT_ID
    AND T4.RIGHT_NAME IN (
        'VIEW_CLAIMS',
        'VIEW_AWARD_CLAIM_TRACKING',
        'UPLOAD_CLAIM_FORECAST_REPORT',
        'CLAIM_MAINTAIN_DETAILED_BREAKDOWN',
        'CLAIM_MODIFY_INVOICE',
        'CLAIM_TRIGGER_INVOICE',
        'VIEW_CONFIDENTIAL_CLAIM_ATTACHMENTS'
    )
UNION
       SELECT DISTINCT RLE.RIGHT_NAME
        FROM
        PERSON_ROLES PR JOIN                                    
        (SELECT ROLE_ID,RT.RIGHT_NAME 
           FROM ROLE_RIGHTS RR,
                RIGHTS RT
            WHERE RR.RIGHT_ID = RT.RIGHT_ID
        ) RLE ON  RLE.ROLE_ID = PR.ROLE_ID AND RLE.ROLE_ID = 100   
        WHERE PR.PERSON_ID = AV_PERSON_ID
          AND (( PR. DESCEND_FLAG = 'Y' AND  EXISTS (SELECT 1
                                                                 FROM UNIT_WITH_CHILDREN 
                                                                                  WHERE UNIT_NUMBER = PR.UNIT_NUMBER
																				  AND CHILD_UNIT_NUMBER = GC_LEAD_UNIT
             ))OR ( PR. DESCEND_FLAG = 'N' AND PR.UNIT_NUMBER = GC_LEAD_UNIT )
            )
         

UNION
	
	   SELECT DISTINCT RLE.RIGHT_NAME  FROM  PERSON_ROLES PR JOIN          
										(SELECT ROLE_ID,RT.RIGHT_NAME FROM ROLE_RIGHTS RR, RIGHTS RT
										WHERE RR.RIGHT_ID = RT.RIGHT_ID) RLE ON RLE.ROLE_ID = PR.ROLE_ID      
										WHERE PR.PERSON_ID = (SELECT DISTINCT PERSON_ID
									FROM GRANT_CALL_FUNDING_SCHEME_MANAGERS
									WHERE FUNDING_SCHEME_CODE IN (SELECT TS3.FUNDING_SCHEME_CODE FROM GRANT_CALL_HEADER TS1
											INNER JOIN AWARD TS2 ON TS1.GRANT_HEADER_ID = TS2.GRANT_HEADER_ID
											INNER JOIN SPONSOR_FUNDING_SCHEME TS3 ON TS3.FUNDING_SCHEME_ID = TS1.FUNDING_SCHEME_ID
											WHERE TS2.AWARD_ID = LI_ACTIVE_PROJECT_ID)
									AND PERSON_ID = AV_PERSON_ID);
	
ELSEIF AV_MODULE_CODE = 16 THEN
    SELECT COUNT(1) INTO LI_FLAG
    FROM AWARD_PROGRESS_REPORT
    WHERE PROGRESS_REPORT_ID = AV_PROJECT_ID;
	
	SELECT G1.HOME_UNIT_NUMBER INTO GC_LEAD_UNIT
        FROM AWARD AWD1
            INNER JOIN GRANT_CALL_HEADER G1 ON AWD1.GRANT_HEADER_ID = G1.GRANT_HEADER_ID
        WHERE AWD1.AWARD_ID = (
                SELECT AWARD_ID
                FROM AWARD_PROGRESS_REPORT
                where PROGRESS_REPORT_ID = AV_PROJECT_ID
            );

    IF LI_FLAG > 0 THEN
        SELECT AWARD_ID INTO LI_ACTIVE_PROJECT_ID
        FROM AWARD_PROGRESS_REPORT
        WHERE PROGRESS_REPORT_ID = AV_PROJECT_ID;
    END IF;
	SELECT DISTINCT RLE.RIGHT_NAME
        FROM
        PERSON_ROLES PR JOIN                                           
        (SELECT ROLE_ID,RT.RIGHT_NAME 
           FROM ROLE_RIGHTS RR,
                RIGHTS RT
            WHERE RR.RIGHT_ID = RT.RIGHT_ID
              AND RT.RIGHT_NAME IN (
			   'CREATE_PROGRESS_REPORT',
				'VIEW_PROGRESS_REPORT',
				'MODIFY_PROGRESS_REPORT',
				'ADD_APPROVER_WORKFLOW',
				'BYPASS_WORKFLOW',
				'PROGRESS_REPORT_FLIP_STATUS',
				'VIEW_CONFIDENTIAL_PROGRESS_REPORT_ATTACHMENTS'
    )
        ) RLE ON RLE.ROLE_ID = PR.ROLE_ID                             
        WHERE PR.PERSON_ID = AV_PERSON_ID
          AND (( PR. DESCEND_FLAG = 'Y' AND  EXISTS (SELECT 1
                                                                 FROM UNIT_WITH_CHILDREN 
                                                                                  WHERE UNIT_NUMBER = PR.UNIT_NUMBER
																				  AND CHILD_UNIT_NUMBER = AV_LEAD_UNIT
             ))OR ( PR. DESCEND_FLAG = 'N' AND PR.UNIT_NUMBER = AV_LEAD_UNIT )
            )
         
UNION
    SELECT T4.RIGHT_NAME
    FROM AWARD_PERSON_ROLES T1
    INNER JOIN ROLE_RIGHTS T3 ON T1.ROLE_ID = T3.ROLE_ID
    INNER JOIN RIGHTS T4 ON T3.RIGHT_ID = T4.RIGHT_ID
    WHERE PERSON_ID = AV_PERSON_ID
    AND AWARD_ID = LI_ACTIVE_PROJECT_ID
    AND T4.RIGHT_NAME IN (
        'CREATE_PROGRESS_REPORT',
        'VIEW_PROGRESS_REPORT',
        'MODIFY_PROGRESS_REPORT',
        'VIEW_CONFIDENTIAL_PROGRESS_REPORT_ATTACHMENTS'
    )
UNION
          SELECT DISTINCT RLE.RIGHT_NAME
        FROM
        PERSON_ROLES PR JOIN                                         
        (SELECT ROLE_ID,RT.RIGHT_NAME 
           FROM ROLE_RIGHTS RR,
                RIGHTS RT
            WHERE RR.RIGHT_ID = RT.RIGHT_ID
        ) RLE ON RLE.ROLE_ID = PR.ROLE_ID AND RLE.ROLE_ID = 100     
        WHERE PR.PERSON_ID = AV_PERSON_ID
          AND (( PR. DESCEND_FLAG = 'Y' AND  EXISTS (SELECT 1
                                                                 FROM UNIT_WITH_CHILDREN 
                                                                                  WHERE UNIT_NUMBER = PR.UNIT_NUMBER
																				  AND CHILD_UNIT_NUMBER = GC_LEAD_UNIT
             ))OR ( PR. DESCEND_FLAG = 'N' AND PR.UNIT_NUMBER = GC_LEAD_UNIT )
            )
         

UNION

	
		   SELECT DISTINCT RLE.RIGHT_NAME  FROM  PERSON_ROLES PR JOIN      
										(SELECT ROLE_ID,RT.RIGHT_NAME FROM ROLE_RIGHTS RR, RIGHTS RT
										WHERE RR.RIGHT_ID = RT.RIGHT_ID) RLE ON RLE.ROLE_ID = PR.ROLE_ID   -- 
										WHERE PR.PERSON_ID = (SELECT DISTINCT PERSON_ID
										FROM GRANT_CALL_FUNDING_SCHEME_MANAGERS
										WHERE FUNDING_SCHEME_CODE IN (SELECT TS3.FUNDING_SCHEME_CODE FROM GRANT_CALL_HEADER TS1
												INNER JOIN AWARD TS2 ON TS1.GRANT_HEADER_ID = TS2.GRANT_HEADER_ID
												INNER JOIN SPONSOR_FUNDING_SCHEME TS3 ON TS3.FUNDING_SCHEME_ID = TS1.FUNDING_SCHEME_ID
												WHERE TS2.AWARD_ID = LI_ACTIVE_PROJECT_ID)
										AND PERSON_ID = AV_PERSON_ID);
	
ELSEIF AV_MODULE_CODE = 8 THEN

	SELECT DISTINCT RLE.RIGHT_NAME
        FROM
        PERSON_ROLES PR,
        (SELECT ROLE_ID,RT.RIGHT_NAME 
           FROM ROLE_RIGHTS RR,
                RIGHTS RT
            WHERE RR.RIGHT_ID = RT.RIGHT_ID
              AND RT.RIGHT_NAME IN ( 'MANAGE_FCOI_DISCLOSURE', 'VIEW_FCOI_DISCLOSURE', 'MANAGE_PROJECT_DISCLOSURE', 'VIEW_PROJECT_DISCLOSURE',
		'MANAGE_ENTITY', 'VIEW_ENTITY', 'VIEW_FCOI_DISCLOSURE_PRIVATE_ATTACHMENTS', 'MAINTAIN_FCOI_PRIVATE_COMMENTS',
        'VIEW_FCOI_PRIVATE_COMMENTS', 'MAINTAIN_PRIVATE_NOTES', 'VIEW_PRIVATE_NOTES')) RLE
        WHERE PR.PERSON_ID = AV_PERSON_ID
          AND (( PR. DESCEND_FLAG = 'Y' AND AV_LEAD_UNIT IN (SELECT CHILD_UNIT_NUMBER
                                                                 FROM UNIT_WITH_CHILDREN 
                                                                                  WHERE UNIT_NUMBER = PR.UNIT_NUMBER
             ))OR ( PR. DESCEND_FLAG = 'N' AND PR.UNIT_NUMBER = AV_LEAD_UNIT )
            )
         AND RLE.ROLE_ID = PR.ROLE_ID;

ELSEIF AV_MODULE_CODE = 23 THEN

	SELECT DISTINCT RLE.RIGHT_NAME
        FROM
        PERSON_ROLES PR,
        (SELECT ROLE_ID,RT.RIGHT_NAME 
           FROM ROLE_RIGHTS RR,
                RIGHTS RT
            WHERE RR.RIGHT_ID = RT.RIGHT_ID
              AND RT.RIGHT_NAME IN ( 'MANAGE_OPA_DISCLOSURE', 'VIEW_OPA_DISCLOSURE', 'MAINTAIN_OPA_PRIVATE_COMMENTS', 'VIEW_OPA_PRIVATE_COMMENTS', 'VIEW_OPA_DISCLOSURE_PRIVATE_ATTACHMENTS')) RLE
        WHERE PR.PERSON_ID = AV_PERSON_ID
          AND (( PR. DESCEND_FLAG = 'Y' AND AV_LEAD_UNIT IN (SELECT CHILD_UNIT_NUMBER
                                                                 FROM UNIT_WITH_CHILDREN 
                                                                                  WHERE UNIT_NUMBER = PR.UNIT_NUMBER
             ))OR ( PR. DESCEND_FLAG = 'N' AND PR.UNIT_NUMBER = AV_LEAD_UNIT )
            )
         AND RLE.ROLE_ID = PR.ROLE_ID;

ELSEIF AV_MODULE_CODE = 24 THEN

	SELECT DISTINCT RLE.RIGHT_NAME
        FROM
        PERSON_ROLES PR,
        (SELECT ROLE_ID,RT.RIGHT_NAME 
           FROM ROLE_RIGHTS RR,
                RIGHTS RT
            WHERE RR.RIGHT_ID = RT.RIGHT_ID
              AND RT.RIGHT_NAME IN ('VIEW_TRAVEL_DISCLOSURE', 'MANAGE_TRAVEL_DISCLOSURE')) RLE
        WHERE PR.PERSON_ID = AV_PERSON_ID
          AND (( PR. DESCEND_FLAG = 'Y' AND AV_LEAD_UNIT IN (SELECT CHILD_UNIT_NUMBER
                                                                 FROM UNIT_WITH_CHILDREN 
                                                                                  WHERE UNIT_NUMBER = PR.UNIT_NUMBER
             ))OR ( PR. DESCEND_FLAG = 'N' AND PR.UNIT_NUMBER = AV_LEAD_UNIT )
            )
         AND RLE.ROLE_ID = PR.ROLE_ID;

ELSEIF AV_MODULE_CODE = 27 THEN

SELECT DISTINCT RLE.RIGHT_NAME
        FROM
        PERSON_ROLES PR,
        (SELECT ROLE_ID,RT.RIGHT_NAME 
           FROM ROLE_RIGHTS RR,
                RIGHTS RT
            WHERE RR.RIGHT_ID = RT.RIGHT_ID
              AND RT.RIGHT_NAME IN ('VIEW_CONSULTING_DISCLOSURE', 'MANAGE_CONSULTING_DISCLOSURE')) RLE
        WHERE PR.PERSON_ID = AV_PERSON_ID
          AND (( PR. DESCEND_FLAG = 'Y' AND AV_LEAD_UNIT IN (SELECT CHILD_UNIT_NUMBER
                                                                 FROM UNIT_WITH_CHILDREN 
                                                                                  WHERE UNIT_NUMBER = PR.UNIT_NUMBER
             ))OR ( PR. DESCEND_FLAG = 'N' AND PR.UNIT_NUMBER = AV_LEAD_UNIT )
            )
         AND RLE.ROLE_ID = PR.ROLE_ID;
END IF;
END;
//
