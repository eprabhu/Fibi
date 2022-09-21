package com.polus.fibicomp.currentandpending.dto;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.List;

import com.polus.fibicomp.currentandpending.pojo.CPReportProjectDetail;
import com.polus.fibicomp.currentandpending.pojo.CPReportProjectDetailExt;
import com.polus.fibicomp.pojo.Currency;
import com.polus.fibicomp.pojo.ProposalPersonRole;
import com.polus.fibicomp.pojo.Sponsor;
import com.polus.fibicomp.pojo.SponsorType;
import com.polus.fibicomp.proposal.pojo.ProposalFundingStatus;

public class CurrentAndPendingModuleDTO {

	private Integer moduleItemId;

	private String moduleItemKey;

	private String title;

	private String sponsorAwardNumber;

	private ProposalPersonRole proposalPersonRole;

	private String department;

	private String leadPrincipalInvestigator;

	private Timestamp startDate;

	private Timestamp endDate;

	private String sponsorCode;

	private Sponsor sponsor;

	private BigDecimal percentageEffort;

	private BigDecimal totalAwardAmount;

	private Boolean isExcluded = false;

	private CPReportProjectDetailExt cpReportProjectDetailExt;

	private Integer cpReportHeaderId;

	private String leadPIPersonId;

	private Boolean leadPiNonEmployeeFlag;

	private Integer cpReportProjectDetailId;

	private Integer personRoleId;

	private BigDecimal annualDirectCost;

	private CPReportProjectDetailDTO cPReportProjectDetailDTO;

	private String applicationId;

	private String moduleStatus;

	private Boolean isManuallyAdded;

	private List<CPReportProjectDetail> cpReportProjectDetails;

	private SponsorType sponsorType;

	private ProposalFundingStatus proposalFundingStatus;

	private Currency currency;

	private String grantCallName;

	private String currencyCode;

	private String sponsorTypeCode;

	private Integer fundingStatusCode;

	private Integer linkedModuleCode;

	public String getGrantCallName() {
		return grantCallName;
	}

	public void setGrantCallName(String grantCallName) {
		this.grantCallName = grantCallName;
	}

	public Integer getModuleItemId() {
		return moduleItemId;
	}

	public void setModuleItemId(Integer moduleItemId) {
		this.moduleItemId = moduleItemId;
	}

	public Boolean getIsExcluded() {
		return isExcluded;
	}

	public void setIsExcluded(Boolean isExcluded) {
		this.isExcluded = isExcluded;
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

	public String getSponsorCode() {
		return sponsorCode;
	}

	public void setSponsorCode(String sponsorCode) {
		this.sponsorCode = sponsorCode;
	}

	public Sponsor getSponsor() {
		return sponsor;
	}

	public void setSponsor(Sponsor sponsor) {
		this.sponsor = sponsor;
	}

	public BigDecimal getPercentageEffort() {
		return percentageEffort;
	}

	public void setPercentageEffort(BigDecimal percentageEffort) {
		this.percentageEffort = percentageEffort;
	}

	public BigDecimal getTotalAwardAmount() {
		return totalAwardAmount;
	}

	public void setTotalAwardAmount(BigDecimal totalAwardAmount) {
		this.totalAwardAmount = totalAwardAmount;
	}

	public CPReportProjectDetailExt getCpReportProjectDetailExt() {
		return cpReportProjectDetailExt;
	}

	public void setCpReportProjectDetailExt(CPReportProjectDetailExt cpReportProjectDetailExt) {
		this.cpReportProjectDetailExt = cpReportProjectDetailExt;
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

	public BigDecimal getAnnualDirectCost() {
		return annualDirectCost;
	}

	public void setAnnualDirectCost(BigDecimal annualDirectCost) {
		this.annualDirectCost = annualDirectCost;
	}

	public CPReportProjectDetailDTO getcPReportProjectDetailDTO() {
		return cPReportProjectDetailDTO;
	}

	public void setcPReportProjectDetailDTO(CPReportProjectDetailDTO cPReportProjectDetailDTO) {
		this.cPReportProjectDetailDTO = cPReportProjectDetailDTO;
	}

	public String getApplicationId() {
		return applicationId;
	}

	public void setApplicationId(String applicationId) {
		this.applicationId = applicationId;
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

	public List<CPReportProjectDetail> getCpReportProjectDetails() {
		return cpReportProjectDetails;
	}

	public void setCpReportProjectDetails(List<CPReportProjectDetail> cpReportProjectDetails) {
		this.cpReportProjectDetails = cpReportProjectDetails;
	}

	public SponsorType getSponsorType() {
		return sponsorType;
	}

	public void setSponsorType(SponsorType sponsorType) {
		this.sponsorType = sponsorType;
	}

	public ProposalFundingStatus getProposalFundingStatus() {
		return proposalFundingStatus;
	}

	public void setProposalFundingStatus(ProposalFundingStatus proposalFundingStatus) {
		this.proposalFundingStatus = proposalFundingStatus;
	}

	public Currency getCurrency() {
		return currency;
	}

	public void setCurrency(Currency currency) {
		this.currency = currency;
	}

	public String getCurrencyCode() {
		return currencyCode;
	}

	public void setCurrencyCode(String currencyCode) {
		this.currencyCode = currencyCode;
	}

	public String getSponsorTypeCode() {
		return sponsorTypeCode;
	}

	public void setSponsorTypeCode(String sponsorTypeCode) {
		this.sponsorTypeCode = sponsorTypeCode;
	}

	public Integer getFundingStatusCode() {
		return fundingStatusCode;
	}

	public void setFundingStatusCode(Integer fundingStatusCode) {
		this.fundingStatusCode = fundingStatusCode;
	}

	public Integer getLinkedModuleCode() {
		return linkedModuleCode;
	}

	public void setLinkedModuleCode(Integer linkedModuleCode) {
		this.linkedModuleCode = linkedModuleCode;
	}

}
