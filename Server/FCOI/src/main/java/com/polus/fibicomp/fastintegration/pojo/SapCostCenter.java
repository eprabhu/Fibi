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
@Table(name = "SAP_COST_CENTER")
public class SapCostCenter implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "COST_CENTER_CODE")
	private String costCenterCode;

	@Column(name = "COST_CENTER_NAME")
	private String costCenterName;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "CONTROLLING_AREA")
	private String controllingArea;

	@Column(name = "VALID_FROM")
	private Timestamp validFrom;

	@Column(name = "VALID_TO")
	private Timestamp validTo;

	@Column(name = "RESPONSIBLE_USER")
	private String responsibleUser;

	@Column(name = "RESPONSIBLE_PERSON")
	private String responsiblePerson;

	@Column(name = "COST_CENTER_CATEGORY")
	private String costCenterCategory;

	@Column(name = "HIERARCHY_AREA")
	private String hierarchyArea;

	@Column(name = "COMPANY_CODE")
	private String companyCode;

	@Column(name = "CURRENCY")
	private String currency;
	
	@Column(name = "COST_CENTER_TYPE")
	private String costCenterType;
	
	@Column(name = "PROFIT_CENTER")
	private String profitCenter;

	@Column(name = "IS_ACTIVE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isActive;

	@Column(name = "CAMPUS")
	private String campus;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "DATE_WHEN_FEED_INACTIVE")
	private Timestamp dateWhenFeedInactive;
	
	@Transient
	private String costCenterDetails;

	public Timestamp getDateWhenFeedInactive() {
		return dateWhenFeedInactive;
	}

	public void setDateWhenFeedInactive(Timestamp dateWhenFeedInactive) {
		this.dateWhenFeedInactive = dateWhenFeedInactive;
	}

	public String getCostCenterType() {
		return costCenterType;
	}

	public void setCostCenterType(String costCenterType) {
		this.costCenterType = costCenterType;
	}

	public String getProfitCenter() {
		return profitCenter;
	}

	public void setProfitCenter(String profitCenter) {
		this.profitCenter = profitCenter;
	}

	public String getCostCenterCode() {
		return costCenterCode;
	}

	public void setCostCenterCode(String costCenterCode) {
		this.costCenterCode = costCenterCode;
	}

	public String getCostCenterName() {
		return costCenterName;
	}

	public void setCostCenterName(String costCenterName) {
		this.costCenterName = costCenterName;
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

	public String getResponsibleUser() {
		return responsibleUser;
	}

	public void setResponsibleUser(String responsibleUser) {
		this.responsibleUser = responsibleUser;
	}

	public String getResponsiblePerson() {
		return responsiblePerson;
	}

	public void setResponsiblePerson(String responsiblePerson) {
		this.responsiblePerson = responsiblePerson;
	}

	public String getCostCenterCategory() {
		return costCenterCategory;
	}

	public void setCostCenterCategory(String costCenterCategory) {
		this.costCenterCategory = costCenterCategory;
	}

	public String getHierarchyArea() {
		return hierarchyArea;
	}

	public void setHierarchyArea(String hierarchyArea) {
		this.hierarchyArea = hierarchyArea;
	}

	public String getCompanyCode() {
		return companyCode;
	}

	public void setCompanyCode(String companyCode) {
		this.companyCode = companyCode;
	}

	public String getCurrency() {
		return currency;
	}

	public void setCurrency(String currency) {
		this.currency = currency;
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

	public String getCostCenterDetails() {
		StringBuilder costCenter = new StringBuilder(costCenterCode);
		costCenterDetails = costCenter.append(" - ").append(costCenterName).toString();
		return costCenterDetails;
	}

	public void setCostCenterDetails(String costCenterDetails) {
		this.costCenterDetails = costCenterDetails;
	}
}
