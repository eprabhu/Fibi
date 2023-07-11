DELIMITER //
CREATE PROCEDURE `COI_UPD_ANS_TO_NEW_QUEST_VERSN`(
AV_QUESTIONNAIRE_ID	   INT(12),
AV_QNR_ANS_HEADER_ID  INT(12),
AV_UPDATE_USER       varchar(60)
)
BEGIN
DECLARE LS_QST_ANS_HEADER_ID INT(12);
DECLARE LI_QUEST_ANSWER_ID INT(12);
DECLARE LI_QUEST_ATT_ID INT(12);
DECLARE LS_QUESTION_ID int(12); 
DECLARE LS_QUESTION_NUMBER int(12) ;
DECLARE LS_QUESTION_VERSION_NUMBER int(3); 
DECLARE LS_QUESTIONNAIRE_ID int(12); 
DECLARE LS_SORT_ORDER int(3); 
DECLARE LS_QUESTION varchar(1000); 
DECLARE LS_DESCRIPTION varchar(4000); 
DECLARE LS_PARENT_QUESTION_ID int(12) ;
DECLARE LS_HELP_LINK varchar(500); 
DECLARE LS_ANSWER_TYPE varchar(90) ;
DECLARE LS_ANSWER_LENGTH int(4); 
DECLARE LS_NO_OF_ANSWERS int(3) ;
DECLARE LS_LOOKUP_TYPE varchar(100); 
DECLARE LS_LOOKUP_NAME varchar(100) ;
DECLARE LS_LOOKUP_FIELD varchar(30);
DECLARE LS_GROUP_NAME varchar(60); 
DECLARE LS_GROUP_LABEL varchar(300); 
DECLARE LS_HAS_CONDITION varchar(1) ;
DECLARE LS_UPDATE_TIMESTAMP date ;
DECLARE LS_UPDATE_USER varchar(60);
DECLARE	LI_OPTION_NUMBER int(3) ;
DECLARE	LI_QUESTIONNAIRE_ANSWER_ID int(12);
DECLARE	LI_QUESTION_ID int(12) ;
DECLARE	LI_ANSWER_NUMBER int(3) ;
DECLARE	LI_ANSWER varchar(2000) ;
DECLARE	LI_ANSWER_LOOKUP_CODE varchar(100);
DECLARE	LI_EXPLANATION varchar(4000);
DECLARE	LI_QUESTIONNAIRE_ANS_HEADER_ID int(12) ;
DECLARE	LV_ATTACHMENT_ID int(12);
DECLARE	LV_QUESTIONNAIRE_ANSWER_ID int(12)  ;
DECLARE	LV_ATTACHMENT longblob;
DECLARE	LV_FILE_NAME varchar(300)  ;
DECLARE	LV_CONTENT_TYPE varchar(100) ;
DECLARE	LV_UPDATE_TIMESTAMP datetime  ;
DECLARE	LV_UPDATE_USER varchar(60);
DECLARE	LC_QUESTION_OPTION_ID int(12);
DECLARE	LC_QUESTION_ID int(12);
DECLARE	LC_OPTION_NUMBER int(3);
DECLARE	LC_OPTION_LABEL varchar(600) ;
DECLARE	LC_REQUIRE_EXPLANATION varchar(1);
DECLARE	LC_EXPLANTION_LABEL varchar(2000) ;
DECLARE	LC_UPDATE_TIMESTAMP date ;
DECLARE	LC_UPDATE_USER varchar(60);
DECLARE	LS_COLUMN_1 varchar(200);
DECLARE	LS_COLUMN_2 varchar(200);
DECLARE	LS_COLUMN_3 varchar(200);
DECLARE	LS_COLUMN_4 varchar(200);
DECLARE	LS_COLUMN_5 varchar(200);
DECLARE	LS_COLUMN_6 varchar(200);
DECLARE	LS_COLUMN_7 varchar(200);
DECLARE	LS_COLUMN_8 varchar(200);
DECLARE	LS_COLUMN_9 varchar(200);
DECLARE	LS_COLUMN_10 varchar(200);
DECLARE LI_ORDER_NUMBER INT(12);
DECLARE LS_RULE_ID int(11);


	SELECT IFNULL(MAX(QUESTIONNAIRE_ANS_HEADER_ID),0)+1 INTO LS_QST_ANS_HEADER_ID FROM QUEST_ANSWER_HEADER;
	
	INSERT INTO QUEST_ANSWER_HEADER
	(QUESTIONNAIRE_ANS_HEADER_ID, QUESTIONNAIRE_ID, MODULE_ITEM_CODE, MODULE_SUB_ITEM_CODE, MODULE_ITEM_KEY, MODULE_SUB_ITEM_KEY, QUESTIONNAIRE_COMPLETED_FLAG, UPDATE_TIMESTAMP, UPDATE_USER)
	SELECT 
	LS_QST_ANS_HEADER_ID,AV_QUESTIONNAIRE_ID, MODULE_ITEM_CODE, MODULE_SUB_ITEM_CODE, MODULE_ITEM_KEY, MODULE_SUB_ITEM_KEY, 'N', UTC_TIMESTAMP(),AV_UPDATE_USER
	FROM QUEST_ANSWER_HEADER
	WHERE  
	QUESTIONNAIRE_ANS_HEADER_ID = AV_QNR_ANS_HEADER_ID;
	
UPDATE QUEST_COLUMN_NEXTVALUE SET QUESTIONNAIRE_ANS_HEADER_ID = LS_QST_ANS_HEADER_ID;     


BLOCK1:BEGIN
DECLARE DONE1 INT DEFAULT FALSE;
DECLARE SEL_QST_QUESTION CURSOR FOR SELECT  *  FROM QUEST_QUESTION WHERE QUESTIONNAIRE_ID = AV_QUESTIONNAIRE_ID ORDER BY SORT_ORDER;
  DECLARE CONTINUE HANDLER FOR NOT FOUND SET DONE1 = TRUE;
  OPEN SEL_QST_QUESTION;
  
  SEL_QST_QUESTION_LOOP: LOOP 
  FETCH SEL_QST_QUESTION INTO LS_QUESTION_ID , LS_QUESTION_NUMBER , LS_QUESTION_VERSION_NUMBER ,LS_QUESTIONNAIRE_ID,  LS_SORT_ORDER  ,LS_QUESTION,  LS_DESCRIPTION, LS_PARENT_QUESTION_ID  ,LS_HELP_LINK , LS_ANSWER_TYPE , LS_ANSWER_LENGTH ,LS_NO_OF_ANSWERS, LS_LOOKUP_TYPE , LS_LOOKUP_NAME , LS_LOOKUP_FIELD , LS_GROUP_NAME , LS_GROUP_LABEL ,LS_HAS_CONDITION,LS_UPDATE_TIMESTAMP, LS_UPDATE_USER, LS_RULE_ID ; 
  
     IF DONE1 THEN
			LEAVE SEL_QST_QUESTION_LOOP;
		END IF;
        
        BLOCK2:BEGIN
		
		DECLARE DONE2 INT DEFAULT FALSE;
		
		DECLARE SEL_QUEST_ANSWER CURSOR FOR SELECT T1.OPTION_NUMBER,T1.QUESTIONNAIRE_ANSWER_ID,T4.QUESTION_NUMBER,T4.ANSWER_TYPE,T1.QUESTION_ID,T1.ANSWER_NUMBER,T1.ANSWER,
			T1.ANSWER_LOOKUP_CODE,T1.EXPLANATION,T3.QUESTIONNAIRE_ANSWER_ATT_ID AS ATTACHMENT_ID,T2.QUESTIONNAIRE_ANS_HEADER_ID  
			FROM  COI_QUEST_ANSWER T1  
			INNER JOIN QUEST_ANSWER_HEADER T2 ON T1.QUESTIONNAIRE_ANS_HEADER_ID = T2.QUESTIONNAIRE_ANS_HEADER_ID
			INNER JOIN QUEST_QUESTION T4 ON T1.QUESTION_ID = T4.QUESTION_ID						
			LEFT OUTER JOIN COI_QUEST_ANSWER_ATTACHMENT T3 ON T3.QUESTIONNAIRE_ANSWER_ID = T1.QUESTIONNAIRE_ANSWER_ID  
			WHERE T2.QUESTIONNAIRE_ANS_HEADER_ID = AV_QNR_ANS_HEADER_ID AND T4.QUESTION_NUMBER = LS_QUESTION_NUMBER
      AND T4.ANSWER_TYPE = LS_ANSWER_TYPE
			ORDER BY T1.QUESTIONNAIRE_ANSWER_ID,T1.QUESTION_ID;
			
		DECLARE CONTINUE HANDLER FOR NOT FOUND SET DONE2 = TRUE;
		
		OPEN SEL_QUEST_ANSWER;
		
		SEL_QUEST_ANSWER_LOOP: LOOP 
		
		FETCH SEL_QUEST_ANSWER INTO LI_OPTION_NUMBER,LI_QUESTIONNAIRE_ANSWER_ID,LS_QUESTION_NUMBER,LS_ANSWER_TYPE,LI_QUESTION_ID,LI_ANSWER_NUMBER,LI_ANSWER,LI_ANSWER_LOOKUP_CODE,LI_EXPLANATION,LV_ATTACHMENT_ID,LI_QUESTIONNAIRE_ANS_HEADER_ID ;
		
			IF DONE2 THEN
			LEAVE SEL_QUEST_ANSWER_LOOP;
			END IF;
			
				IF (LS_ANSWER_TYPE <> 'Checkbox' AND LS_ANSWER_TYPE <> 'Y/N' AND LS_ANSWER_TYPE <> 'Y/N/NA' 
					AND LS_ANSWER_TYPE <> 'Radio') THEN
					
					SELECT IFNULL(MAX(QUESTIONNAIRE_ANSWER_ID),0)+1 INTO LI_QUEST_ANSWER_ID FROM COI_QUEST_ANSWER;
          
            
          
            INSERT INTO COI_QUEST_ANSWER(QUESTIONNAIRE_ANSWER_ID, QUESTIONNAIRE_ANS_HEADER_ID, QUESTION_ID, OPTION_NUMBER, ANSWER_NUMBER, ANSWER, ANSWER_LOOKUP_CODE, EXPLANATION, UPDATE_TIMESTAMP, UPDATE_USER)
            VALUES(LI_QUEST_ANSWER_ID,LS_QST_ANS_HEADER_ID,LS_QUESTION_ID, LI_OPTION_NUMBER, LI_ANSWER_NUMBER, LI_ANSWER, LI_ANSWER_LOOKUP_CODE, LI_EXPLANATION, UTC_TIMESTAMP(),AV_UPDATE_USER);                  
			
           
            BLOCK3:BEGIN
			
			DECLARE DONE3 INT DEFAULT FALSE;
			
            DECLARE QUEST_ATT CURSOR FOR SELECT * FROM COI_QUEST_ANSWER_ATTACHMENT
									  WHERE QUESTIONNAIRE_ANSWER_ID = LI_QUESTIONNAIRE_ANSWER_ID;
									  
			DECLARE CONTINUE HANDLER FOR NOT FOUND SET DONE3=TRUE;						  
									  
            OPEN QUEST_ATT;
			 
			 QUEST_ATT_LOOP: LOOP 
			
            FETCH QUEST_ATT INTO LV_ATTACHMENT_ID,LV_QUESTIONNAIRE_ANSWER_ID,LV_ATTACHMENT,LV_FILE_NAME,LV_CONTENT_TYPE,LV_UPDATE_TIMESTAMP,LV_UPDATE_USER;
           
			IF DONE3 THEN
			LEAVE QUEST_ATT_LOOP;
			END IF;
              
              SELECT IFNULL(MAX(QUESTIONNAIRE_ANSWER_ATT_ID),0)+1 INTO LI_QUEST_ATT_ID FROM COI_QUEST_ANSWER_ATTACHMENT;
						
              INSERT INTO COI_QUEST_ANSWER_ATTACHMENT
              (QUESTIONNAIRE_ANSWER_ATT_ID, QUESTIONNAIRE_ANSWER_ID, ATTACHMENT, FILE_NAME, CONTENT_TYPE, UPDATE_TIMESTAMP, UPDATE_USER)
              VALUES(LI_QUEST_ATT_ID, LI_QUEST_ANSWER_ID, LV_ATTACHMENT, LV_FILE_NAME, LV_CONTENT_TYPE, LV_UPDATE_TIMESTAMP ,LV_UPDATE_USER);
              
              UPDATE QUEST_COLUMN_NEXTVALUE SET QUESTIONNAIRE_ANSWER_ATT_ID = LI_QUEST_ATT_ID;			
              
            
			END LOOP QUEST_ATT_LOOP;
             
            CLOSE QUEST_ATT; 
			END BLOCK3;
           
            UPDATE QUEST_COLUMN_NEXTVALUE SET QUESTIONNAIRE_ANSWER_ID = LI_QUEST_ANSWER_ID;
            
				ELSE
                
				
				BLOCK4:BEGIN
				
				DECLARE DONE4 INT DEFAULT FALSE;
				
				DECLARE SEL_QUEST_OPTION CURSOR FOR SELECT * FROM QUEST_QUESTION_OPTION WHERE QUESTION_ID = LS_QUESTION_ID ORDER BY QUESTION_OPTION_ID;
				
				DECLARE CONTINUE HANDLER FOR NOT FOUND SET DONE4=TRUE;
				
				OPEN SEL_QUEST_OPTION;
				
				SEL_QUEST_OPTION_LOOP :LOOP
				
				FETCH SEL_QUEST_OPTION INTO LC_QUESTION_OPTION_ID,LC_QUESTION_ID ,LC_OPTION_NUMBER ,LC_OPTION_LABEL ,LC_REQUIRE_EXPLANATION ,LC_EXPLANTION_LABEL ,LC_UPDATE_TIMESTAMP,LC_UPDATE_USER;
				
				
				    IF DONE4 THEN
					LEAVE SEL_QUEST_OPTION_LOOP;
					END IF;
					
					IF (LC_OPTION_LABEL = LI_ANSWER) THEN
						
						SELECT IFNULL(MAX(QUESTIONNAIRE_ANSWER_ID),0)+1 INTO LI_QUEST_ANSWER_ID FROM COI_QUEST_ANSWER;
						
					INSERT INTO COI_QUEST_ANSWER(QUESTIONNAIRE_ANSWER_ID, QUESTIONNAIRE_ANS_HEADER_ID, QUESTION_ID, OPTION_NUMBER, ANSWER_NUMBER, ANSWER, ANSWER_LOOKUP_CODE, EXPLANATION, UPDATE_TIMESTAMP, UPDATE_USER)
						VALUES(LI_QUEST_ANSWER_ID,LS_QST_ANS_HEADER_ID,LS_QUESTION_ID, LI_OPTION_NUMBER, LI_ANSWER_NUMBER, LI_ANSWER, LI_ANSWER_LOOKUP_CODE, LI_EXPLANATION, UTC_TIMESTAMP(),AV_UPDATE_USER);                  
						
						UPDATE QUEST_COLUMN_NEXTVALUE SET QUESTIONNAIRE_ANSWER_ID = LI_QUEST_ANSWER_ID;
                        
					END IF;
				
				END LOOP SEL_QUEST_OPTION_LOOP;
				CLOSE SEL_QUEST_OPTION;
				END BLOCK4;
                 
                
        END IF;
		    IF(LS_ANSWER_TYPE = 'Table') THEN
                    BLOCK5:BEGIN
        			    DECLARE DONE5 INT DEFAULT FALSE;

        				DECLARE SEL_QUEST_TABLE_ANSWER CURSOR FOR SELECT t1.ORDER_NUMBER, t1.COLUMN_1, t1.COLUMN_2, t1.COLUMN_3, t1.COLUMN_4, t1.COLUMN_5, t1.COLUMN_6,
        				t1.COLUMN_7, t1.COLUMN_8, t1.COLUMN_9, t1.COLUMN_10 FROM COI_QUEST_TABLE_ANSWER t1 INNER JOIN QUEST_QUESTION t2 ON t1.QUESTION_ID = t2.QUESTION_ID
        				WHERE t1.QUESTIONNAIRE_ANS_HEADER_ID = AV_QNR_ANS_HEADER_ID AND t2.QUESTION_NUMBER = LS_QUESTION_NUMBER ORDER BY t1.ORDER_NUMBER;

        				DECLARE CONTINUE HANDLER FOR NOT FOUND SET DONE5 = TRUE;

        				OPEN SEL_QUEST_TABLE_ANSWER;

        				SEL_QUEST_TABLE_ANSWER_LOOP: LOOP

        				FETCH SEL_QUEST_TABLE_ANSWER INTO LI_ORDER_NUMBER, LS_COLUMN_1, LS_COLUMN_2, LS_COLUMN_3, LS_COLUMN_4, LS_COLUMN_5, LS_COLUMN_6, LS_COLUMN_7,
        					LS_COLUMN_8, LS_COLUMN_9, LS_COLUMN_10;

        					IF DONE5 THEN
        					LEAVE SEL_QUEST_TABLE_ANSWER_LOOP;
        					END IF;

        					SELECT IFNULL(MAX(QUEST_TABLE_ANSWER_ID),0)+1 INTO LI_QUEST_ANSWER_ID FROM COI_QUEST_TABLE_ANSWER;

        					INSERT INTO COI_QUEST_TABLE_ANSWER(QUEST_TABLE_ANSWER_ID, QUESTIONNAIRE_ANS_HEADER_ID, QUESTION_ID, ORDER_NUMBER, COLUMN_1, COLUMN_2, COLUMN_3,
        					COLUMN_4, COLUMN_5, COLUMN_6, COLUMN_7, COLUMN_8, COLUMN_9, COLUMN_10, UPDATE_TIMESTAMP, UPDATE_USER) VALUES(LI_QUEST_ANSWER_ID, LS_QST_ANS_HEADER_ID,
        					LS_QUESTION_ID, LI_ORDER_NUMBER, LS_COLUMN_1, LS_COLUMN_2, LS_COLUMN_3, LS_COLUMN_4, LS_COLUMN_5, LS_COLUMN_6, LS_COLUMN_7,
        					LS_COLUMN_8, LS_COLUMN_9, LS_COLUMN_10, NOW(), AV_UPDATE_USER);

        					UPDATE QUEST_COLUMN_NEXTVALUE SET QUESTIONNAIRE_ANSWER_TABLE_ID = LI_QUEST_ANSWER_ID;

        				END LOOP SEL_QUEST_TABLE_ANSWER_LOOP;
        				CLOSE SEL_QUEST_TABLE_ANSWER;
        			END BLOCK5;
        	END IF;
		
	    END LOOP SEL_QUEST_ANSWER_LOOP;
		CLOSE SEL_QUEST_ANSWER;
		END BLOCK2;
	    
	 
    END LOOP SEL_QST_QUESTION_LOOP;
	CLOSE SEL_QST_QUESTION;
  END BLOCK1;

    
    SELECT 
        LS_QST_ANS_HEADER_ID AS QUEST_ANS_HEADER_ID
    FROM DUAL; 
   
END
//
