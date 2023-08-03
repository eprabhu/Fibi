package com.polus.fibi.graphconnect.exceptions;

public class COIEntityGraphProcessingException extends CustomGraphException {

	/**
	 * 
	 */
	private static final long serialVersionUID = -5372330445465943955L;
	private String userRequest;
	
	public COIEntityGraphProcessingException(String userRequest) {
		super(userRequest);
		this.userRequest = userRequest;
	}

	public String getUserRequest() {
        return userRequest;
    }
}
