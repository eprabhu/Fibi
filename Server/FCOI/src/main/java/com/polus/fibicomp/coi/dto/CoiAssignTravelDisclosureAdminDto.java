package com.polus.fibicomp.coi.dto;

import java.sql.Timestamp;

public class CoiAssignTravelDisclosureAdminDto {

	private Integer travelDisclosureId;
	private String adminPersonId;
	private Integer adminGroupId;
	private String adminPersonName;
	private String adminGroupName;
	private String travelDisclosureStatus;
	private String travelDisclosureStatusCode;
	private String reviewStatus;
	private String reviewStatusCode;
	private String dispositionStatus;
	private String dispositionStatusCode;
	private String documentStatusCode;
	private String documentStatus;
	private String versionStatus;
	private Timestamp updateTimestamp;

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public String getDocumentStatusCode() {
		return documentStatusCode;
	}

	public void setDocumentStatusCode(String documentStatusCode) {
		this.documentStatusCode = documentStatusCode;
	}

	public String getDocumentStatus() {
		return documentStatus;
	}

	public void setDocumentStatus(String documentStatus) {
		this.documentStatus = documentStatus;
	}

	public String getVersionStatus() {
		return versionStatus;
	}

	public void setVersionStatus(String versionStatus) {
		this.versionStatus = versionStatus;
	}

	public Integer getTravelDisclosureId() {
		return travelDisclosureId;
	}

	public void setTravelDisclosureId(Integer travelDisclosureId) {
		this.travelDisclosureId = travelDisclosureId;
	}

	public String getAdminPersonId() {
		return adminPersonId;
	}

	public void setAdminPersonId(String adminPersonId) {
		this.adminPersonId = adminPersonId;
	}

	public Integer getAdminGroupId() {
		return adminGroupId;
	}

	public void setAdminGroupId(Integer adminGroupId) {
		this.adminGroupId = adminGroupId;
	}

	public String getAdminPersonName() {
		return adminPersonName;
	}

	public void setAdminPersonName(String adminPersonName) {
		this.adminPersonName = adminPersonName;
	}

	public String getAdminGroupName() {
		return adminGroupName;
	}

	public void setAdminGroupName(String adminGroupName) {
		this.adminGroupName = adminGroupName;
	}

	public String getTravelDisclosureStatus() {
		return travelDisclosureStatus;
	}

	public void setTravelDisclosureStatus(String travelDisclosureStatus) {
		this.travelDisclosureStatus = travelDisclosureStatus;
	}

	public String getTravelDisclosureStatusCode() {
		return travelDisclosureStatusCode;
	}

	public void setTravelDisclosureStatusCode(String travelDisclosureStatusCode) {
		this.travelDisclosureStatusCode = travelDisclosureStatusCode;
	}

	public String getReviewStatus() {
		return reviewStatus;
	}

	public void setReviewStatus(String reviewStatus) {
		this.reviewStatus = reviewStatus;
	}

	public String getReviewStatusCode() {
		return reviewStatusCode;
	}

	public void setReviewStatusCode(String reviewStatusCode) {
		this.reviewStatusCode = reviewStatusCode;
	}

	public String getDispositionStatus() {
		return dispositionStatus;
	}

	public void setDispositionStatus(String dispositionStatus) {
		this.dispositionStatus = dispositionStatus;
	}

	public String getDispositionStatusCode() {
		return dispositionStatusCode;
	}

	public void setDispositionStatusCode(String dispositionStatusCode) {
		this.dispositionStatusCode = dispositionStatusCode;
	}

}
