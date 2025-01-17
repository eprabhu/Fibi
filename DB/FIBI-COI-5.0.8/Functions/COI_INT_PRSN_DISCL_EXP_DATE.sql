DELIMITER //
CREATE FUNCTION COI_INT_PRSN_DISCL_EXP_DATE(
AV_PERSON_ID  VARCHAR(40),
AV_TYPE_CODE VARCHAR(3)
) RETURNS varchar(20) CHARSET utf8mb4
    DETERMINISTIC
BEGIN
	DECLARE LS_EXP_DATE VARCHAR(20);
		
SELECT DATE_FORMAT(MAX(CD.EXPIRATION_DATE), '%Y-%m-%d') INTO  LS_EXP_DATE 
		FROM COI_DISCLOSURE CD
		WHERE CD.PERSON_ID = AV_PERSON_ID 
		  AND CD.DISPOSITION_STATUS_CODE = '3' 
		  AND CD.FCOI_TYPE_CODE = AV_TYPE_CODE
		  AND CD.DISCLOSURE_ID = (SELECT MAX(DISCLOSURE_ID) 
								   FROM COI_DISCLOSURE 
								   WHERE PERSON_ID = CD.PERSON_ID 
								   AND DISPOSITION_STATUS_CODE = CD.DISPOSITION_STATUS_CODE
                                   AND FCOI_TYPE_CODE = CD.FCOI_TYPE_CODE);

RETURN LS_EXP_DATE;	
END
//
