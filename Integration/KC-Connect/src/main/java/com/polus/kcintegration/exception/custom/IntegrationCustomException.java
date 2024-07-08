package com.polus.kcintegration.exception.custom;

public class IntegrationCustomException extends RuntimeException {

	private static final long serialVersionUID = 1L;

	public IntegrationCustomException(String message, Throwable cause) {
		super(message, cause);
	}

}
