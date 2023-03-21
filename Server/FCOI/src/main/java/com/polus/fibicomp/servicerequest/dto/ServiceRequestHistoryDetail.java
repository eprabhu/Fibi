package com.polus.fibicomp.servicerequest.dto;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

public class ServiceRequestHistoryDetail {

	private Integer actionLogId;
	private String updateTimeStampInAMPM;
	private String fullName;
	private String comments;
	private String message;
	private Integer serviceRequestId;
	private List<ServiceRequestEditField> serviceRequestEditFields;
	private Timestamp updateTimestamp;

	public ServiceRequestHistoryDetail() {
		serviceRequestEditFields = new ArrayList<>();
	}

	public Integer getActionLogId() {
		return actionLogId;
	}

	public void setActionLogId(Integer actionLogId) {
		this.actionLogId = actionLogId;
	}

	public String getUpdateTimeStampInAMPM() {
		return updateTimeStampInAMPM;
	}

	public void setUpdateTimeStampInAMPM(String updateTimeStampInAMPM) {
		this.updateTimeStampInAMPM = updateTimeStampInAMPM;
	}

	public String getFullName() {
		return fullName;
	}

	public void setFullName(String fullName) {
		this.fullName = fullName;
	}

	public String getComments() {
		return comments;
	}

	public void setComments(String comments) {
		this.comments = comments;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public Integer getServiceRequestId() {
		return serviceRequestId;
	}

	public void setServiceRequestId(Integer serviceRequestId) {
		this.serviceRequestId = serviceRequestId;
	}

	public List<ServiceRequestEditField> getServiceRequestEditFields() {
		return serviceRequestEditFields;
	}

	public void setServiceRequestEditFields(List<ServiceRequestEditField> serviceRequestEditFields) {
		this.serviceRequestEditFields = serviceRequestEditFields;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

}
