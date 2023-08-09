package com.polus.fibi.graphconnect.exceptions;

public class CustomGraphException extends RuntimeException {

    /**
	 * 
	 */
	private static final long serialVersionUID = -8937243986813113686L;
	private String message;
    
    public CustomGraphException(String message) {
        super(message);
        this.message = message;
    }

    public String getMessage() {
        return message;
    }

} 
