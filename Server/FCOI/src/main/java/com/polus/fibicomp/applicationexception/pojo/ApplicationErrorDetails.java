package com.polus.fibicomp.applicationexception.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.fasterxml.jackson.annotation.JsonIgnore;

@Entity
@Table(name = "EXCEPTION_LOG")
@EntityListeners(AuditingEntityListener.class)
public class ApplicationErrorDetails implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "ERROR_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer id;

	@JsonIgnore
	@Column(name = "ERROR_CODE")
	private String errorCode;

	@Column(name = "MESSAGE")
	private String errorMessage;

	@Column(name = "API_REQUEST")
	private String apiRequest;

	@Column(name = "METHOD")
	private String method;

	@Column(name = "REQUEST_BODY")
	private String requestBody;

	@Column(name = "REQUESTER_PERSON")
	private String requesterPersonId;

	@Column(name = "DEBUG_MESSSAGE")
	private String debugMessage;

	@JsonIgnore
	@Column(name = "STACKTRACE")
	private String stackTrace;

	@CreatedBy
	@Column(name = "CREATE_USER")
	private String createUser;

	@CreatedDate
	@Column(name = "CREATE_TIMESTAMP")
	private Timestamp createTimestamp;

	public ApplicationErrorDetails() {
		super();
	}

	public ApplicationErrorDetails(String errorCode, String errorMessage) {
		super();
		this.errorMessage = errorMessage;
		this.errorCode = errorCode;
	}

	public Integer getId() {
		return id;
	}

	public void setId(Integer id) {
		this.id = id;
	}

	public String getErrorCode() {
		return errorCode;
	}

	public void setErrorCode(String errorCode) {
		this.errorCode = errorCode;
	}

	public String getErrorMessage() {
		return errorMessage;
	}

	public void setErrorMessage(String errorMessage) {
		this.errorMessage = errorMessage;
	}

	public String getDebugMessage() {
		return debugMessage;
	}

	public void setDebugMessage(String debugMessage) {
		this.debugMessage = debugMessage;
	}

	public String getApiRequest() {
		return apiRequest;
	}

	public void setApiRequest(String apiRequest) {
		this.apiRequest = apiRequest;
	}

	public String getMethod() {
		return method;
	}

	public void setMethod(String method) {
		this.method = method;
	}

	public String getRequestBody() {
		return requestBody;
	}

	public void setRequestBody(String requestBody) {
		this.requestBody = requestBody;
	}

	public String getStackTrace() {
		return stackTrace;
	}

	public void setStackTrace(String stackTrace) {
		this.stackTrace = stackTrace;
	}

	public String getRequesterPersonId() {
		return requesterPersonId;
	}

	public void setRequesterPersonId(String requesterPersonId) {
		this.requesterPersonId = requesterPersonId;
	}

}
