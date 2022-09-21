package com.polus.fibicomp.fastintegration.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "SAP_FUND_CENTER")
public class SapFundCenter implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "FUND_CENTER_CODE")
	private String fundCenterCode;

	@Column(name = "FUND_CENTER_NAME")
	private String fundCenterName;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "VALID_FROM")
	private Timestamp validFrom;

	@Column(name = "VALID_TO")
	private Timestamp validTo;

	@Column(name = "FM_AREA")
	private String fmArea;

	@Column(name = "COMPANY_CODE")
	private String companyCode;

	@Column(name = "BUSINESS_AREA")
	private String businessArea;

	@Column(name = "RESPONSIBLE_PERSON")
	private String responsiblePerson;

	@Column(name = "IS_ACTIVE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isActive;

	@Column(name = "CAMPUS")
	private String campus;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;	
	
	@Column(name = "DATE_WHEN_FEED_INACTIVE")
	private Timestamp dateWhenFeedInactive;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Transient
	private String fundCenterDetails;

	public Timestamp getDateWhenFeedInactive() {
		return dateWhenFeedInactive;
	}

	public void setDateWhenFeedInactive(Timestamp dateWhenFeedInactive) {
		this.dateWhenFeedInactive = dateWhenFeedInactive;
	}

	public String getFundCenterCode() {
		return fundCenterCode;
	}

	public void setFundCenterCode(String fundCenterCode) {
		this.fundCenterCode = fundCenterCode;
	}

	public String getFundCenterName() {
		return fundCenterName;
	}

	public void setFundCenterName(String fundCenterName) {
		this.fundCenterName = fundCenterName;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public Timestamp getValidFrom() {
		return validFrom;
	}

	public void setValidFrom(Timestamp validFrom) {
		this.validFrom = validFrom;
	}

	public Timestamp getValidTo() {
		return validTo;
	}

	public void setValidTo(Timestamp validTo) {
		this.validTo = validTo;
	}

	public String getFmArea() {
		return fmArea;
	}

	public void setFmArea(String fmArea) {
		this.fmArea = fmArea;
	}

	public String getCompanyCode() {
		return companyCode;
	}

	public void setCompanyCode(String companyCode) {
		this.companyCode = companyCode;
	}

	public String getBusinessArea() {
		return businessArea;
	}

	public void setBusinessArea(String businessArea) {
		this.businessArea = businessArea;
	}

	public String getResponsiblePerson() {
		return responsiblePerson;
	}

	public void setResponsiblePerson(String responsiblePerson) {
		this.responsiblePerson = responsiblePerson;
	}

	public Boolean getIsActive() {
		return isActive;
	}

	public void setIsActive(Boolean isActive) {
		this.isActive = isActive;
	}

	public String getCampus() {
		return campus;
	}

	public void setCampus(String campus) {
		this.campus = campus;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public String getFundCenterDetails() {
		StringBuilder fundCenter = new StringBuilder(fundCenterCode);
		fundCenterDetails = fundCenter.append(" - ").append(fundCenterName).toString();
		return fundCenterDetails;
	}

	public void setFundCenterDetails(String fundCenterDetails) {
		this.fundCenterDetails = fundCenterDetails;
	}
	
}
