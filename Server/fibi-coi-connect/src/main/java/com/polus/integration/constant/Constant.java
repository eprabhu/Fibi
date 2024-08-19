package com.polus.integration.constant;

import java.util.Collections;
import java.util.HashMap;
import java.util.Map;

public interface Constant {

	// Security Constant
    String SECRET = "q3t6w9z$C&F)J@NcRfUjWnZr4u7x!A%D";
    String TOKEN_PREFIX = "Bearer ";
    String HEADER_STRING = "Authorization";
    long EXPIRATION_TIME = 43_200_000; // 12 hour
    String SIGN_UP_URL = "/authenticate";
    
    String LOGIN_USER_FULL_NAME = "fullName";
    String LOGIN_PERSON_ID = "personId";
    String LOGIN_PERSON_UNIT = "unitNumber";
    String IS_EXTERNAL_USER = "isExternalUser";
    String HASH_ALGORITHM = "SHA";
    String CHARSET = "UTF-8";
    String ERROR_CODE = "ER004";
    // Module Code
    Integer AWARD_MODULE_CODE = 1;
    Integer DEV_PROPOSAL_MODULE_CODE = 3;
    Integer COI_MODULE_CODE = 8;
    //SubModuleCode
    Integer SUB_MODULE_CODE= 0;
    Integer COI_INTEGRATION_SUB_MODULE_CODE= 802;
    String SUB_MODULE_ITEM_KEY= "0";
    //Queue Action type
    String PROPOSAL_INTEGRATION_ACTION_TYPE = "PROPOSAL_INTEGRATION";
    String QUESTIONNAIRE_INTEGRATION_ACTION_TYPE = "PROPOSAL_QUESTIONNAIRE_INTEGRATION";
	String COI_PROJECT_TYPE_PROPOSAL = "3";
	String PENDING_PROJECT = "pendingProject";
	String FIBI_DIRECT_EXCHANGE = "FIBI.DIRECT.EXCHANGE";
	String AC_TYPE_UPDATE = "U";
	String AC_TYPE_INSERT = "I";
	
	String UPDATE_BY = "10000000001";
	
	String DEFAULT_IS_ACTIVE_VALUE = "Y";	
	
    Map<String, String> REQUIRED_DnB_INDUSTRY_TYPE = Map.of(
            "24659", "International Standard Industrial Classification Revision 4",
            "35912", "D&B Hoovers Industry Classification",
            "3599", "D&B Standard Industry Code",
            "37788", "North American Industry Classification System 2022",
            "700", "North American Industry Classification System"
        );
	
	String PRIMARY_DnB_INDUSTRY_TYPE = "37788";
	
	String IS_PRIMARY_YES = "Y";
	String IS_PRIMARY_NO = "N";

}
