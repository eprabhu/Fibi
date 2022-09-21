package com.polus.fibicomp.currentandpending.dto;

import com.polus.fibicomp.pojo.ProposalPersonRole;

public class CurrentAndPendingModulePrintDTO {

	private String moduleItemId;

	private String moduleItemKey;

	private String title;

	private String sponsorAwardNumber;

	private ProposalPersonRole proposalPersonRole;

	private String department;

	private String leadPrincipalInvestigator;

	private String startDate;

	private String endDate;

	private String sponsorCode;

	private String sponsorName;

	private String percentageEffort;

	private String totalAwardAmount;

	private Boolean isExcluded = false;

	private Integer cpReportHeaderId;

	private String leadPIPersonId;

	private Boolean leadPiNonEmployeeFlag;

	private Integer cpReportProjectDetailId;

	private Integer personRoleId;

	private String annualDirectCost;

	private CPReportProjectDetailDTO cPReportProjectDetailDTO;

	private String applicationId;

	private String moduleStatus;

	private String grantCallName;

	private String fundingType;

	private String fundingSource;

	private Boolean isManuallyAdded;

	public String getModuleItemId() {
		return moduleItemId;
	}

	public void setModuleItemId(String moduleItemId) {
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

	public String getSponsorAwardNumber() {
		return sponsorAwardNumber;
	}

	public void setSponsorAwardNumber(String sponsorAwardNumber) {
		this.sponsorAwardNumber = sponsorAwardNumber;
	}

	public ProposalPersonRole getProposalPersonRole() {
		return proposalPersonRole;
	}

	public void setProposalPersonRole(ProposalPersonRole proposalPersonRole) {
		this.proposalPersonRole = proposalPersonRole;
	}

	public String getDepartment() {
		return department;
	}

	public void setDepartment(String department) {
		this.department = department;
	}

	public String getLeadPrincipalInvestigator() {
		return leadPrincipalInvestigator;
	}

	public void setLeadPrincipalInvestigator(String leadPrincipalInvestigator) {
		this.leadPrincipalInvestigator = leadPrincipalInvestigator;
	}

	public String getSponsorCode() {
		return sponsorCode;
	}

	public void setSponsorCode(String sponsorCode) {
		this.sponsorCode = sponsorCode;
	}

	public Boolean getIsExcluded() {
		return isExcluded;
	}

	public void setIsExcluded(Boolean isExcluded) {
		this.isExcluded = isExcluded;
	}

	public Integer getCpReportHeaderId() {
		return cpReportHeaderId;
	}

	public void setCpReportHeaderId(Integer cpReportHeaderId) {
		this.cpReportHeaderId = cpReportHeaderId;
	}

	public String getLeadPIPersonId() {
		return leadPIPersonId;
	}

	public void setLeadPIPersonId(String leadPIPersonId) {
		this.leadPIPersonId = leadPIPersonId;
	}

	public Boolean getLeadPiNonEmployeeFlag() {
		return leadPiNonEmployeeFlag;
	}

	public void setLeadPiNonEmployeeFlag(Boolean leadPiNonEmployeeFlag) {
		this.leadPiNonEmployeeFlag = leadPiNonEmployeeFlag;
	}

	public Integer getCpReportProjectDetailId() {
		return cpReportProjectDetailId;
	}

	public void setCpReportProjectDetailId(Integer cpReportProjectDetailId) {
		this.cpReportProjectDetailId = cpReportProjectDetailId;
	}

	public Integer getPersonRoleId() {
		return personRoleId;
	}

	public void setPersonRoleId(Integer personRoleId) {
		this.personRoleId = personRoleId;
	}

	public CPReportProjectDetailDTO getcPReportProjectDetailDTO() {
		return cPReportProjectDetailDTO;
	}

	public void setcPReportProjectDetailDTO(CPReportProjectDetailDTO cPReportProjectDetailDTO) {
		this.cPReportProjectDetailDTO = cPReportProjectDetailDTO;
	}

	public String getStartDate() {
		return startDate;
	}

	public void setStartDate(String startDate) {
		this.startDate = startDate;
	}

	public String getEndDate() {
		return endDate;
	}

	public void setEndDate(String endDate) {
		this.endDate = endDate;
	}

	public String getSponsorName() {
		return sponsorName;
	}

	public void setSponsorName(String sponsorName) {
		this.sponsorName = sponsorName;
	}

	public String getPercentageEffort() {
		return percentageEffort;
	}

	public void setPercentageEffort(String percentageEffort) {
		this.percentageEffort = percentageEffort;
	}

	public String getTotalAwardAmount() {
		return totalAwardAmount;
	}

	public void setTotalAwardAmount(String totalAwardAmount) {
		this.totalAwardAmount = totalAwardAmount;
	}

	public String getAnnualDirectCost() {
		return annualDirectCost;
	}

	public void setAnnualDirectCost(String annualDirectCost) {
		this.annualDirectCost = annualDirectCost;
	}

	public String getApplicationId() {
		return applicationId;
	}

	public void setApplicationId(String applicationId) {
		this.applicationId = applicationId;
	}

	public String getGrantCallName() {
		return grantCallName;
	}

	public void setGrantCallName(String grantCallName) {
		this.grantCallName = grantCallName;
	}

	public String getFundingType() {
		return fundingType;
	}

	public void setFundingType(String fundingType) {
		this.fundingType = fundingType;
	}

	public String getFundingSource() {
		return fundingSource;
	}

	public void setFundingSource(String fundingSource) {
		this.fundingSource = fundingSource;
	}

	public String getModuleStatus() {
		return moduleStatus;
	}

	public void setModuleStatus(String moduleStatus) {
		this.moduleStatus = moduleStatus;
	}

	public Boolean getIsManuallyAdded() {
		return isManuallyAdded;
	}

	public void setIsManuallyAdded(Boolean isManuallyAdded) {
		this.isManuallyAdded = isManuallyAdded;
	}

}
