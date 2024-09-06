DELIMITER //
CREATE PROCEDURE ENRICH_FOREIGN_NAME (
    IN AV_ENTITY_ID INT,
    IN AV_FOREIGN_NAME VARCHAR(200),
    IN AV_PERSON_ID VARCHAR(40)
)
BEGIN
    IF NOT EXISTS (
        SELECT 1 
        FROM entity_foreign_name 
        WHERE ENTITY_ID = AV_ENTITY_ID 
          AND FOREIGN_NAME = AV_FOREIGN_NAME
    ) THEN	
				
        INSERT INTO entity_foreign_name (ENTITY_ID, FOREIGN_NAME, UPDATED_BY, UPDATE_TIMESTAMP)
        VALUES (AV_ENTITY_ID, AV_FOREIGN_NAME, AV_PERSON_ID, NOW());
		
    END IF;
END
//