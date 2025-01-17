CREATE TRIGGER PER_ROLES_POST_UPD AFTER UPDATE ON PERSON_ROLES FOR EACH ROW BEGIN
	IF OLD.PERSON_ID <> NEW.PERSON_ID OR OLD.ROLE_ID <> NEW.ROLE_ID OR IFNULL(OLD.UNIT_NUMBER,'X') <> IFNULL(NEW.UNIT_NUMBER,'X') OR OLD.DESCEND_FLAG <> NEW.DESCEND_FLAG THEN
	   CALL ROLE_RIGHTS_AUDIT_PROC
								('PER_ROLES',			
								'UPDATE',				
								NULL,					
								NULL,					
								OLD.PERSON_ROLES_ID,	
								NEW.PERSON_ROLES_ID,	
								OLD.PERSON_ID,			
								NEW.PERSON_ID,			
								OLD.ROLE_ID,			
								NEW.ROLE_ID,			
								NULL,					
								NULL,					
								OLD.UNIT_NUMBER,		
								NEW.UNIT_NUMBER,		
								OLD.DESCEND_FLAG,		
								NEW.DESCEND_FLAG,
								NEW.UPDATE_USER);		
    END IF;

END