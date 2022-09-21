package com.polus.fibicomp.servicerequest.dto;

public class StatusHistory {

	private String action;
	private String fullName;
	private String holdInterval;
	private String serviceStatus;
	private String updateTimeStampInAMPM;

	public String getAction() {
		return action;
	}

	public void setAction(String action) {
		this.action = action;
	}

	public String getFullName() {
		return fullName;
	}

	public void setFullName(String fullName) {
		this.fullName = fullName;
	}

	public String getHoldInterval() {
		return holdInterval;
	}

	public void setHoldInterval(String holdInterval) {
		this.holdInterval = holdInterval;
	}

	public String getServiceStatus() {
		return serviceStatus;
	}

	public void setServiceStatus(String serviceStatus) {
		this.serviceStatus = serviceStatus;
	}

	public String getUpdateTimeStampInAMPM() {
		return updateTimeStampInAMPM;
	}

	public void setUpdateTimeStampInAMPM(String updateTimeStampInAMPM) {
		this.updateTimeStampInAMPM = updateTimeStampInAMPM;
	}

}
