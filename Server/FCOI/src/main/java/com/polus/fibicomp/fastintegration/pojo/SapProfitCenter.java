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
@Table(name = "SAP_PROFIT_CENTER")
public class SapProfitCenter implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "PROFIT_CENTER_CODE")
	private String profitCenterCode;

	@Column(name = "PROFIT_CENTER_NAME")
	private String profitCenterName;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "CONTROLLING_AREA")
	private String controllingArea;

	@Column(name = "VALID_FROM")
	private Timestamp validFrom;

	@Column(name = "VALID_TO")
	private Timestamp validTo;

	@Column(name = "RESPONSIBLE_PERSON")
	private String responsiblePerson;

	@Column(name = "RESPONSIBLE_USER")
	private String responsibleUser;

	@Column(name = "PROFIT_CENTER_GROUP")
	private String profitCenterGroup;

	@Column(name = "SEGMENT")
	private String segment;

	@Column(name = "IS_ACTIVE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isActive;

	@Column(name = "CAMPUS")
	private String campus;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;
	
	@Column(name = "ENTERED_ON")
	private String enteredOn;
	
	@Column(name = "ENTERED_BY")
	private String enteredBy;

	@Column(name = "COMPANY_CODE")
	private String companyCode;
	
	@Column(name = "SEARCH_TERM")
	private String searchTerm;
	
	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Transient
	private String profitCenterDetails;

	@Column(name = "DATE_WHEN_FEED_INACTIVE")
	private Timestamp dateWhenFeedInactive;
	
	public Timestamp getDateWhenFeedInactive() {
		return dateWhenFeedInactive;
	}

	public void setDateWhenFeedInactive(Timestamp dateWhenFeedInactive) {
		this.dateWhenFeedInactive = dateWhenFeedInactive;
	}

	public String getEnteredOn() {
		return enteredOn;
	}

	public void setEnteredOn(String enteredOn) {
		this.enteredOn = enteredOn;
	}

	public String getEnteredBy() {
		return enteredBy;
	}

	public void setEnteredBy(String enteredBy) {
		this.enteredBy = enteredBy;
	}

	public String getCompanyCode() {
		return companyCode;
	}

	public void setCompanyCode(String companyCode) {
		this.companyCode = companyCode;
	}

	public String getSearchTerm() {
		return searchTerm;
	}

	public void setSearchTerm(String searchTerm) {
		this.searchTerm = searchTerm;
	}

	public String getProfitCenterCode() {
		return profitCenterCode;
	}

	public void setProfitCenterCode(String profitCenterCode) {
		this.profitCenterCode = profitCenterCode;
	}

	public String getProfitCenterName() {
		return profitCenterName;
	}

	public void setProfitCenterName(String profitCenterName) {
		this.profitCenterName = profitCenterName;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getControllingArea() {
		return controllingArea;
	}

	public void setControllingArea(String controllingArea) {
		this.controllingArea = controllingArea;
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

	public String getResponsiblePerson() {
		return responsiblePerson;
	}

	public void setResponsiblePerson(String responsiblePerson) {
		this.responsiblePerson = responsiblePerson;
	}

	public String getResponsibleUser() {
		return responsibleUser;
	}

	public void setResponsibleUser(String responsibleUser) {
		this.responsibleUser = responsibleUser;
	}

	public String getProfitCenterGroup() {
		return profitCenterGroup;
	}

	public void setProfitCenterGroup(String profitCenterGroup) {
		this.profitCenterGroup = profitCenterGroup;
	}

	public String getSegment() {
		return segment;
	}

	public void setSegment(String segment) {
		this.segment = segment;
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

	public String getProfitCenterDetails() {
		StringBuilder profitCenter = new StringBuilder(profitCenterCode);
		profitCenterDetails = profitCenter.append(" - ").append(profitCenterName).toString();
		return profitCenterDetails;
	}

	public void setProfitCenterDetails(String profitCenterDetails) {
		this.profitCenterDetails = profitCenterDetails;
	}
	
}
