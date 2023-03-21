package com.polus.fibicomp.currentandpending.dto;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import com.polus.fibicomp.currentandpending.pojo.CPReportHeader;

public class CurrentAndPendingPersonDTO {
	
	private String personId;

	private Boolean isGenerated;

	private Boolean nonEmployeeFlag;

	private String createUser;

	private String lastUpdatedUserFullName;

	private String roleName;

	private String personName;

	private Timestamp lastUpdatedTimestamp;

	private List<CurrentAndPendingModuleDTO> currentAwards;
	
	private List<CurrentAndPendingModuleDTO> pendingProposals;

	private List<CurrentAndPendingModulePrintDTO> currentPrintAwards;
	
	private List<CurrentAndPendingModulePrintDTO> pendingPrintProposals;

	private CPReportHeader cpReportHeader;

	private Integer moduleCode;

	private String moduleItemId;

	public CurrentAndPendingPersonDTO() {
		currentPrintAwards = new ArrayList<>();
		pendingPrintProposals = new ArrayList<>();
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public Boolean getIsGenerated() {
		return isGenerated;
	}

	public void setIsGenerated(Boolean isGenerated) {
		this.isGenerated = isGenerated;
	}

	public Boolean getNonEmployeeFlag() {
		return nonEmployeeFlag;
	}

	public void setNonEmployeeFlag(Boolean nonEmployeeFlag) {
		this.nonEmployeeFlag = nonEmployeeFlag;
	}

	public String getCreateUser() {
		return createUser;
	}

	public void setCreateUser(String createUser) {
		this.createUser = createUser;
	}

	public String getLastUpdatedUserFullName() {
		return lastUpdatedUserFullName;
	}

	public void setLastUpdatedUserFullName(String lastUpdatedUserFullName) {
		this.lastUpdatedUserFullName = lastUpdatedUserFullName;
	}

	public String getRoleName() {
		return roleName;
	}

	public void setRoleName(String roleName) {
		this.roleName = roleName;
	}

	public String getPersonName() {
		return personName;
	}

	public void setPersonName(String personName) {
		this.personName = personName;
	}

	public Timestamp getLastUpdatedTimestamp() {
		return lastUpdatedTimestamp;
	}

	public void setLastUpdatedTimestamp(Timestamp lastUpdatedTimestamp) {
		this.lastUpdatedTimestamp = lastUpdatedTimestamp;
	}

	public List<CurrentAndPendingModuleDTO> getCurrentAwards() {
		return currentAwards;
	}

	public void setCurrentAwards(List<CurrentAndPendingModuleDTO> currentAwards) {
		this.currentAwards = currentAwards;
	}

	public List<CurrentAndPendingModuleDTO> getPendingProposals() {
		return pendingProposals;
	}

	public void setPendingProposals(List<CurrentAndPendingModuleDTO> pendingProposals) {
		this.pendingProposals = pendingProposals;
	}

	public CPReportHeader getCpReportHeader() {
		return cpReportHeader;
	}

	public void setCpReportHeader(CPReportHeader cpReportHeader) {
		this.cpReportHeader = cpReportHeader;
	}

	public Integer getModuleCode() {
		return moduleCode;
	}

	public void setModuleCode(Integer moduleCode) {
		this.moduleCode = moduleCode;
	}

	public String getModuleItemId() {
		return moduleItemId;
	}

	public void setModuleItemId(String moduleItemId) {
		this.moduleItemId = moduleItemId;
	}

	public List<CurrentAndPendingModulePrintDTO> getCurrentPrintAwards() {
		return currentPrintAwards;
	}

	public void setCurrentPrintAwards(List<CurrentAndPendingModulePrintDTO> currentPrintAwards) {
		this.currentPrintAwards = currentPrintAwards;
	}

	public List<CurrentAndPendingModulePrintDTO> getPendingPrintProposals() {
		return pendingPrintProposals;
	}

	public void setPendingPrintProposals(List<CurrentAndPendingModulePrintDTO> pendingPrintProposals) {
		this.pendingPrintProposals = pendingPrintProposals;
	}

}
