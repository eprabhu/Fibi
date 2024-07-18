package com.polus.integration.constant;

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
    
}
