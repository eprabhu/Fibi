package com.polus.fibicomp.applicationexception.dto;

import java.io.Serializable;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.polus.fibicomp.applicationexception.pojo.ApplicationErrorDetails;

public class ApplicationException extends RuntimeException implements Serializable {

	private static final long serialVersionUID = 1L;

	@JsonIgnore
	private final Exception e;

	private final ApplicationErrorDetails applicationErrorDetails;

	public ApplicationException(String message, Exception e, String errorCode) {
		super();
		this.e = e;
		this.applicationErrorDetails = new ApplicationErrorDetails(errorCode, message);
	}

	public ApplicationException(String message, String errorCode) {
		super();
		this.e = new Exception();
		this.applicationErrorDetails = new ApplicationErrorDetails(errorCode, message);
	}

	public Exception getE() {
		return e;
	}

	public ApplicationErrorDetails getApplicationErrorDetails() {
		return applicationErrorDetails;
	}

}
