package com.polus.fibicomp.coi.dto;


import javax.persistence.Column;
import java.sql.Timestamp;
import java.util.Date;

public class CoiDisclosureDto {

	private Integer disclosureId;
	private String personId;
	private String homeUnit;
	private String homeUnitName;
	private Integer disclosureNumber;
	private Integer versionNumber;
	private String versionStatus;
	private String conflictStatusCode;
	private String conflictStatus;
	private String dispositionStatusCode;
	private String dispositionStatus;
	private String reviewStatusCode;
	private String reviewStatus;
	private Date certifiedAt;
	private Date expirationDate;
	private Timestamp updateTimestamp;
	private Timestamp createTimestamp;
	private String updateUserFullName;
	private String createUserFullName;
	private Integer adminGroupId;
	private String adminPersonId;
	private String disclosurePersonFullName;

	public Integer getDisclosureId() {
		return disclosureId;
	}

	public void setDisclosureId(Integer disclosureId) {
		this.disclosureId = disclosureId;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getHomeUnit() {
		return homeUnit;
	}

	public void setHomeUnit(String homeUnit) {
		this.homeUnit = homeUnit;
	}

	public Integer getDisclosureNumber() {
		return disclosureNumber;
	}

	public void setDisclosureNumber(Integer disclosureNumber) {
		this.disclosureNumber = disclosureNumber;
	}

	public Integer getVersionNumber() {
		return versionNumber;
	}

	public void setVersionNumber(Integer versionNumber) {
		this.versionNumber = versionNumber;
	}

	public String getVersionStatus() {
		return versionStatus;
	}

	public void setVersionStatus(String versionStatus) {
		this.versionStatus = versionStatus;
	}

	public Date getExpirationDate() {
		return expirationDate;
	}

	public void setExpirationDate(Date expirationDate) {
		this.expirationDate = expirationDate;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public Timestamp getCreateTimestamp() {
		return createTimestamp;
	}

	public void setCreateTimestamp(Timestamp createTimestamp) {
		this.createTimestamp = createTimestamp;
	}

	public String getUpdateUserFullName() {
		return updateUserFullName;
	}

	public void setUpdateUserFullName(String updateUserFullName) {
		this.updateUserFullName = updateUserFullName;
	}

	public String getCreateUserFullName() {
		return createUserFullName;
	}

	public void setCreateUserFullName(String createUserFullName) {
		this.createUserFullName = createUserFullName;
	}

	public String getHomeUnitName() {
		return homeUnitName;
	}

	public void setHomeUnitName(String homeUnitName) {
		this.homeUnitName = homeUnitName;
	}

	public String getConflictStatusCode() {
		return conflictStatusCode;
	}

	public void setConflictStatusCode(String conflictStatusCode) {
		this.conflictStatusCode = conflictStatusCode;
	}

	public String getConflictStatus() {
		return conflictStatus;
	}

	public void setConflictStatus(String conflictStatus) {
		this.conflictStatus = conflictStatus;
	}

	public String getDispositionStatusCode() {
		return dispositionStatusCode;
	}

	public void setDispositionStatusCode(String dispositionStatusCode) {
		this.dispositionStatusCode = dispositionStatusCode;
	}

	public String getDispositionStatus() {
		return dispositionStatus;
	}

	public void setDispositionStatus(String dispositionStatus) {
		this.dispositionStatus = dispositionStatus;
	}

	public String getReviewStatusCode() {
		return reviewStatusCode;
	}

	public void setReviewStatusCode(String reviewStatusCode) {
		this.reviewStatusCode = reviewStatusCode;
	}

	public String getReviewStatus() {
		return reviewStatus;
	}

	public void setReviewStatus(String reviewStatus) {
		this.reviewStatus = reviewStatus;
	}

	public Date getCertifiedAt() {
		return certifiedAt;
	}

	public void setCertifiedAt(Date certifiedAt) {
		this.certifiedAt = certifiedAt;
	}

	public Integer getAdminGroupId() {
		return adminGroupId;
	}

	public void setAdminGroupId(Integer adminGroupId) {
		this.adminGroupId = adminGroupId;
	}

	public String getAdminPersonId() {
		return adminPersonId;
	}

	public void setAdminPersonId(String adminPersonId) {
		this.adminPersonId = adminPersonId;
	}

	public String getDisclosurePersonFullName() {
		return disclosurePersonFullName;
	}

	public void setDisclosurePersonFullName(String disclosurePersonFullName) {
		this.disclosurePersonFullName = disclosurePersonFullName;
	}
}
