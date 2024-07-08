package com.polus.fibicomp.coi.dto;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.Date;

import com.polus.core.pojo.Unit;

public class CoiTravelDashboardDto {

	private Integer travelDisclosureId;
	private String travellerName;
	private String travellerTypeDescription;
	private String travelDisclosureStatusCode;
	private String travelDisclosureStatusDescription;
	private String travelEntityName;
	private String travelCity;
	private String travelCountry;
	private String travelState;
	private BigDecimal travelAmount;
	private String documentStatusCode;
	private String documentStatusDescription;
	private Unit unitDetails;
	private Timestamp certifiedAt;
	private Date expirationDate;
	private String reviewStatusCode;
	private String reviewDescription;
	private String travelPurpose;
	private Date travelStartDate;
	private Date travelEndDate;
	private Date travelSubmissionDate;
	private Timestamp acknowledgeAt;
	private String adminPersonId;
	private Integer adminGroupId;
	private String versionStatus;
	private String createUser;
	private String updateUser;
	private Timestamp createTimestamp;
	private Timestamp updateTimestamp;

	public Integer getTravelDisclosureId() {
		return travelDisclosureId;
	}

	public void setTravelDisclosureId(Integer travelDisclosureId) {
		this.travelDisclosureId = travelDisclosureId;
	}

	public String getTravellerName() {
		return travellerName;
	}

	public void setTravellerName(String travellerName) {
		this.travellerName = travellerName;
	}

	public String getTravellerTypeDescription() {
		return travellerTypeDescription;
	}

	public void setTravellerTypeDescription(String travellerTypeDescription) {
		this.travellerTypeDescription = travellerTypeDescription;
	}

	public String getTravelDisclosureStatusCode() {
		return travelDisclosureStatusCode;
	}

	public void setTravelDisclosureStatusCode(String travelDisclosureStatusCode) {
		this.travelDisclosureStatusCode = travelDisclosureStatusCode;
	}

	public String getTravelDisclosureStatusDescription() {
		return travelDisclosureStatusDescription;
	}

	public void setTravelDisclosureStatusDescription(String travelDisclosureStatusDescription) {
		this.travelDisclosureStatusDescription = travelDisclosureStatusDescription;
	}

	public String getTravelEntityName() {
		return travelEntityName;
	}

	public void setTravelEntityName(String travelEntityName) {
		this.travelEntityName = travelEntityName;
	}

	public String getTravelCity() {
		return travelCity;
	}

	public void setTravelCity(String travelCity) {
		this.travelCity = travelCity;
	}

	public String getTravelCountry() {
		return travelCountry;
	}

	public void setTravelCountry(String travelCountry) {
		this.travelCountry = travelCountry;
	}

	public String getTravelState() {
		return travelState;
	}

	public void setTravelState(String travelState) {
		this.travelState = travelState;
	}

	public BigDecimal getTravelAmount() {
		return travelAmount;
	}

	public void setTravelAmount(BigDecimal travelAmount) {
		this.travelAmount = travelAmount;
	}

	public String getDocumentStatusCode() {
		return documentStatusCode;
	}

	public void setDocumentStatusCode(String documentStatusCode) {
		this.documentStatusCode = documentStatusCode;
	}

	public String getDocumentStatusDescription() {
		return documentStatusDescription;
	}

	public void setDocumentStatusDescription(String documentStatusDescription) {
		this.documentStatusDescription = documentStatusDescription;
	}

	public Unit getUnitDetails() {
		return unitDetails;
	}

	public void setUnitDetails(Unit unitDetails) {
		this.unitDetails = unitDetails;
	}

	public Timestamp getCertifiedAt() {
		return certifiedAt;
	}

	public void setCertifiedAt(Timestamp certifiedAt) {
		this.certifiedAt = certifiedAt;
	}

	public Date getExpirationDate() {
		return expirationDate;
	}

	public void setExpirationDate(Date expirationDate) {
		this.expirationDate = expirationDate;
	}

	public String getReviewStatusCode() {
		return reviewStatusCode;
	}

	public void setReviewStatusCode(String reviewStatusCode) {
		this.reviewStatusCode = reviewStatusCode;
	}

	public String getReviewDescription() {
		return reviewDescription;
	}

	public void setReviewDescription(String reviewDescription) {
		this.reviewDescription = reviewDescription;
	}

	public String getTravelPurpose() {
		return travelPurpose;
	}

	public void setTravelPurpose(String travelPurpose) {
		this.travelPurpose = travelPurpose;
	}

	public Date getTravelStartDate() {
		return travelStartDate;
	}

	public void setTravelStartDate(Date travelStartDate) {
		this.travelStartDate = travelStartDate;
	}

	public Date getTravelEndDate() {
		return travelEndDate;
	}

	public void setTravelEndDate(Date travelEndDate) {
		this.travelEndDate = travelEndDate;
	}

	public Date getTravelSubmissionDate() {
		return travelSubmissionDate;
	}

	public void setTravelSubmissionDate(Date travelSubmissionDate) {
		this.travelSubmissionDate = travelSubmissionDate;
	}

	public Timestamp getAcknowledgeAt() {
		return acknowledgeAt;
	}

	public void setAcknowledgeAt(Timestamp acknowledgeAt) {
		this.acknowledgeAt = acknowledgeAt;
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

	public String getVersionStatus() {
		return versionStatus;
	}

	public void setVersionStatus(String versionStatus) {
		this.versionStatus = versionStatus;
	}

	public String getCreateUser() {
		return createUser;
	}

	public void setCreateUser(String createUser) {
		this.createUser = createUser;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Timestamp getCreateTimestamp() {
		return createTimestamp;
	}

	public void setCreateTimestamp(Timestamp createTimestamp) {
		this.createTimestamp = createTimestamp;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

}
