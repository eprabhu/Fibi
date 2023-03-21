package com.polus.fibicomp.correspondence.dto;

public class IRBCorrespondenceDto {
	private String protocolNumber;
	private String studyTitle;
	private String expirationDate;
	private String toUserName;
	private String actionDate;
	private String committeAction;

	public String getStudyTitle() {
		return studyTitle;
	}

	public void setStudyTitle(String studyTitle) {
		this.studyTitle = studyTitle;
	}

	public String getExpirationDate() {
		return expirationDate;
	}

	public void setExpirationDate(String expirationDate) {
		this.expirationDate = expirationDate;
	}

	public String getToUserName() {
		return toUserName;
	}

	public void setToUserName(String toUserName) {
		this.toUserName = toUserName;
	}

	public String getActionDate() {
		return actionDate;
	}

	public void setActionDate(String actionDate) {
		this.actionDate = actionDate;
	}

	public String getCommitteAction() {
		return committeAction;
	}

	public void setCommitteAction(String committeAction) {
		this.committeAction = committeAction;
	}

	public String getProtocolNumber() {
		return protocolNumber;
	}

	public void setProtocolNumber(String protocolNumber) {
		this.protocolNumber = protocolNumber;
	}
}
