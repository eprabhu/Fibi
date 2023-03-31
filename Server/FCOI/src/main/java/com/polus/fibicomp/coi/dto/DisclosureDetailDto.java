package com.polus.fibicomp.coi.dto;

import java.sql.Timestamp;
import java.util.List;
import java.util.Map;

public class DisclosureDetailDto {

	private Integer moduleCode;

	private Integer moduleItemId;

	private String moduleItemKey;

	private String title;

	private String principalInvestigator;

	private String sponsor;

	private Timestamp startDate;

	private Timestamp endDate;

	private Boolean sfiCompleted;

	private String moduleStatus;

	private String unitName;

	private String unitNumber;

	private String primeSponsor;

	private List<Map<Object, Object>> disclosureStatusCount;

	public Integer getModuleCode() {
		return moduleCode;
	}

	public void setModuleCode(Integer moduleCode) {
		this.moduleCode = moduleCode;
	}

	public Integer getModuleItemId() {
		return moduleItemId;
	}

	public void setModuleItemId(Integer moduleItemId) {
		this.moduleItemId = moduleItemId;
	}

	public String getModuleItemKey() {
		return moduleItemKey;
	}

	public void setModuleItemKey(String moduleItemKey) {
		this.moduleItemKey = moduleItemKey;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getPrincipalInvestigator() {
		return principalInvestigator;
	}

	public void setPrincipalInvestigator(String principalInvestigator) {
		this.principalInvestigator = principalInvestigator;
	}

	public String getSponsor() {
		return sponsor;
	}

	public void setSponsor(String sponsor) {
		this.sponsor = sponsor;
	}

	public Timestamp getStartDate() {
		return startDate;
	}

	public void setStartDate(Timestamp startDate) {
		this.startDate = startDate;
	}

	public Timestamp getEndDate() {
		return endDate;
	}

	public void setEndDate(Timestamp endDate) {
		this.endDate = endDate;
	}

	public Boolean getSfiCompleted() {
		return sfiCompleted;
	}

	public void setSfiCompleted(Boolean sfiCompleted) {
		this.sfiCompleted = sfiCompleted;
	}

	public String getModuleStatus() {
		return moduleStatus;
	}

	public void setModuleStatus(String moduleStatus) {
		this.moduleStatus = moduleStatus;
	}

	public String getUnitName() {
		return unitName;
	}

	public void setUnitName(String unitName) {
		this.unitName = unitName;
	}

	public String getPrimeSponsor() {
		return primeSponsor;
	}

	public void setPrimeSponsor(String primeSponsor) {
		this.primeSponsor = primeSponsor;
	}

	public String getUnitNumber() {
		return unitNumber;
	}

	public void setUnitNumber(String unitNumber) {
		this.unitNumber = unitNumber;
	}

	public List<Map<Object, Object>> getDisclosureStatusCount() {
		return disclosureStatusCount;
	}

	public void setDisclosureStatusCount(List<Map<Object, Object>> disclosureStatusCount) {
		this.disclosureStatusCount = disclosureStatusCount;
	}
}
