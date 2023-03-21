package com.polus.fibicomp.award.awardprojectoutcome.dto;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;

import com.polus.fibicomp.pojo.FundingScheme;
import com.polus.fibicomp.pojo.Sponsor;

public class ModuleDetails implements Serializable {

	private static final long serialVersionUID = 1L;

	private Integer moduleId;

	private String moduleItemKey;

	private String moduleStatus;

	private String sponsorCode;

	private String sponsorName;

	private String fundingSchemeCode;

	private String fundingSchemeName;

	private String title;

	private BigDecimal totalProjectValue = BigDecimal.ZERO;

	private String applicationId;

	private String piName;

	private String piPersonId;

	private Integer piRolodexId;

	private Sponsor sponsor;

	private FundingScheme fundingScheme;

	private String leadUnitName;

	private String sponsorAwardNumber;

	private String accountNumber;

	private String leadUnitNumber;

	private String status;

	private Integer moduleCode;

	private Integer moduleItemId;

	private String personId;

	private Timestamp startDate;

	private Timestamp endDate;

	private String primeSponsorCode;

	private String rolodexId;

	private String totalCost;

	private String anticipatedTotal;

	private String obligatedTotal;

	private String agreementType;

	private String agreementReqName;

	public String getPiPersonId() {
		return piPersonId;
	}

	public void setPiPersonId(String piPersonId) {
		this.piPersonId = piPersonId;
	}

	public Integer getPiRolodexId() {
		return piRolodexId;
	}

	public void setPiRolodexId(Integer piRolodexId) {
		this.piRolodexId = piRolodexId;
	}

	public String getModuleItemKey() {
		return moduleItemKey;
	}

	public void setModuleItemKey(String moduleItemKey) {
		this.moduleItemKey = moduleItemKey;
	}

	public String getModuleStatus() {
		return moduleStatus;
	}

	public void setModuleStatus(String moduleStatus) {
		this.moduleStatus = moduleStatus;
	}

	public String getSponsorName() {
		return sponsorName;
	}

	public void setSponsorName(String sponsorName) {
		this.sponsorName = sponsorName;
	}

	public String getSponsorCode() {
		return sponsorCode;
	}

	public void setSponsorCode(String sponsorCode) {
		this.sponsorCode = sponsorCode;
	}

	public String getFundingSchemeCode() {
		return fundingSchemeCode;
	}

	public void setFundingSchemeCode(String fundingSchemeCode) {
		this.fundingSchemeCode = fundingSchemeCode;
	}

	public String getFundingSchemeName() {
		return fundingSchemeName;
	}

	public void setFundingSchemeName(String fundingSchemeName) {
		this.fundingSchemeName = fundingSchemeName;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public BigDecimal getTotalProjectValue() {
		return totalProjectValue;
	}

	public void setTotalProjectValue(BigDecimal totalProjectValue) {
		this.totalProjectValue = totalProjectValue;
	}

	public String getApplicationId() {
		return applicationId;
	}

	public void setApplicationId(String applicationId) {
		this.applicationId = applicationId;
	}

	public String getPiName() {
		return piName;
	}

	public void setPiName(String piName) {
		this.piName = piName;
	}

	public Integer getModuleId() {
		return moduleId;
	}

	public void setModuleId(Integer moduleId) {
		this.moduleId = moduleId;
	}

	public Sponsor getSponsor() {
		return sponsor;
	}

	public void setSponsor(Sponsor sponsor) {
		this.sponsor = sponsor;
	}

	public FundingScheme getFundingScheme() {
		return fundingScheme;
	}

	public void setFundingScheme(FundingScheme fundingScheme) {
		this.fundingScheme = fundingScheme;
	}

	public String getLeadUnitName() {
		return leadUnitName;
	}

	public void setLeadUnitName(String leadUnitName) {
		this.leadUnitName = leadUnitName;
	}

	public String getSponsorAwardNumber() {
		return sponsorAwardNumber;
	}

	public void setSponsorAwardNumber(String sponsorAwardNumber) {
		this.sponsorAwardNumber = sponsorAwardNumber;
	}

	public String getAccountNumber() {
		return accountNumber;
	}

	public void setAccountNumber(String accountNumber) {
		this.accountNumber = accountNumber;
	}

	public String getLeadUnitNumber() {
		return leadUnitNumber;
	}

	public void setLeadUnitNumber(String leadUnitNumber) {
		this.leadUnitNumber = leadUnitNumber;
	}

	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
	}

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

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
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

	public String getPrimeSponsorCode() {
		return primeSponsorCode;
	}

	public void setPrimeSponsorCode(String primeSponsorCode) {
		this.primeSponsorCode = primeSponsorCode;
	}

	public String getRolodexId() {
		return rolodexId;
	}

	public void setRolodexId(String rolodexId) {
		this.rolodexId = rolodexId;
	}

	public String getTotalCost() {
		return totalCost;
	}

	public void setTotalCost(String totalCost) {
		this.totalCost = totalCost;
	}

	public String getAnticipatedTotal() {
		return anticipatedTotal;
	}

	public void setAnticipatedTotal(String anticipatedTotal) {
		this.anticipatedTotal = anticipatedTotal;
	}

	public String getObligatedTotal() {
		return obligatedTotal;
	}

	public void setObligatedTotal(String obligatedTotal) {
		this.obligatedTotal = obligatedTotal;
	}

	public String getAgreementType() {
		return agreementType;
	}

	public void setAgreementType(String agreementType) {
		this.agreementType = agreementType;
	}

	public String getAgreementReqName() {
		return agreementReqName;
	}

	public void setAgreementReqName(String agreementReqName) {
		this.agreementReqName = agreementReqName;
	}

}
