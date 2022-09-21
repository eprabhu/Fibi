package com.polus.fibicomp.budget.vo;

import java.util.List;
import java.util.Set;

import com.polus.fibicomp.adminportal.pojo.RateType;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardAmountInfo;
import com.polus.fibicomp.award.pojo.AwardPerson;
import com.polus.fibicomp.award.vo.AwardBudgetImportInfo;
import com.polus.fibicomp.budget.pojo.AppointmentType;
import com.polus.fibicomp.budget.pojo.AwardBudgetDetail;
import com.polus.fibicomp.budget.pojo.AwardBudgetHeader;
import com.polus.fibicomp.budget.pojo.AwardBudgetPerson;
import com.polus.fibicomp.budget.pojo.AwardRates;
import com.polus.fibicomp.budget.pojo.BudgetHeader;
import com.polus.fibicomp.budget.pojo.BudgetTemplateType;
import com.polus.fibicomp.budget.pojo.CostElement;
import com.polus.fibicomp.budget.pojo.JobCode;
import com.polus.fibicomp.budget.pojo.TbnPerson;
import com.polus.fibicomp.proposal.pojo.CostSharingType;
import com.polus.fibicomp.budget.pojo.FundDisbursementBasisType;

public class AwardBudgetVO {

	private Integer awardId;

	private AwardAmountInfo awardAmountInfo;

	private AwardBudgetHeader awardBudgetHeader;

	private String userName;

	private String userFullName;

	private String activityTypeCode;

	private List<CostElement> sysGeneratedCostElements;

	private String awardNumber;

	private String acType;

	private Integer budgetDetailId;

	private AwardBudgetDetail awardBudgetDetail;

	private List<AwardBudgetImportInfo> awardBudgetImportInfos;

	private Integer devPropBudgetHeaderId;

	private Integer devPropBudgetPeriodId;

	private String ipNumber;

	private Integer pdNumber;

	private Integer pdBudgetId;

	private List<AwardBudgetHeader> awardBudgetHeaders;

	private Integer awardBudgetId;

	private Integer devPropBudgetPeriodNumber;

	private Boolean isCreated = false;

	private Award award;

	private List<AwardBudgetDetail> awardBudgetDetailList;

	private Boolean isAllPeriod;

	private List<AwardBudgetHeaderDetail> awardBudgetList;

	private BudgetHeader budgetHeader;

	private Integer copyPeriodId;

	private Integer currentPeriodId;

	private Integer budgetHeaderId;

	private List<AwardBudgetPerson> awardBudgetPersonList;

	private List<AppointmentType> appointmentType;

	private List<TbnPerson> tbnPersons;

	private List<JobCode> jobCode;

	private List<AwardPerson> awardPersons;

	private Integer budgetPersonId;

	private Integer budgetPeriodId;

	private Integer budgetPersonDetailId;

	private boolean status;

	private String message;

	private Boolean isEnabledAwardBudgetDetail;

	private Boolean isSinglePeriodBudgetEnabled = false;

	private Boolean isAutoCalculationEnabled = false;

	private Set<String> rateClassTypes;

	private List<AwardRates> awardRates;

	private String defaultAbPersonItemToBeTBN;

	private Boolean enableAbPersonAppliedSalary = false;
	
	private Boolean canIncludeInBudget = false;

	private String availableFundType;

	private List<RateType> rateTypes;

	private AwardBudgetPerson awardBudgetPerson;

	private Boolean isNonPersonalLineItemEnabled = false;

	private Integer budgetNonPersonDtlId;

	private String internalOrderCode;
	
	private Boolean modifyABInRouting = false;	

	private List<BudgetTemplateType> budgetTemplateTypes;

	private Integer budgetTemplateTypeId;

	private Boolean enabledCampusFlagAward = false;

	private String onCampusRates;

	private String offCampusRates;

	private Boolean isShowBudgetOHRatePercentage;
	
	private Integer costSharingTypeCode;
	
	private Boolean enableCostShareStatus;
	
	private List<CostSharingType> costSharingTypes;

	private List<FundDisbursementBasisType> fundDisbursementBasisTypes;

	private Boolean showAwardBudgetFieldForSap;

	public Boolean getShowAwardBudgetFieldForSap() {
		return showAwardBudgetFieldForSap;
	}

	public void setShowAwardBudgetFieldForSap(Boolean showAwardBudgetFieldForSap) {
		this.showAwardBudgetFieldForSap = showAwardBudgetFieldForSap;
	}


	public List<FundDisbursementBasisType> getFundDisbursementBasisTypes() {
		return fundDisbursementBasisTypes;
	}

	public void setFundDisbursementBasisTypes(List<FundDisbursementBasisType> fundDisbursementBasisTypes) {
		this.fundDisbursementBasisTypes = fundDisbursementBasisTypes;
	}
	
	public Integer getCostSharingTypeCode() {
		return costSharingTypeCode;
	}

	public void setCostSharingTypeCode(Integer costSharingTypeCode) {
		this.costSharingTypeCode = costSharingTypeCode;
	}

	public Boolean getModifyABInRouting() {
		return modifyABInRouting;
	}

	public void setModifyABInRouting(Boolean modifyABInRouting) {
		this.modifyABInRouting = modifyABInRouting;
	}

	public Integer getBudgetPeriodId() {
		return budgetPeriodId;
	}

	public List<RateType> getRateTypes() {
		return rateTypes;
	}

	public void setRateTypes(List<RateType> rateTypes) {
		this.rateTypes = rateTypes;
	}

	public void setBudgetPeriodId(Integer budgetPeriodId) {
		this.budgetPeriodId = budgetPeriodId;
	}

	public Integer getAwardBudgetId() {
		return awardBudgetId;
	}

	public void setAwardBudgetId(Integer awardBudgetId) {
		this.awardBudgetId = awardBudgetId;
	}

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public AwardAmountInfo getAwardAmountInfo() {
		return awardAmountInfo;
	}

	public void setAwardAmountInfo(AwardAmountInfo awardAmountInfo) {
		this.awardAmountInfo = awardAmountInfo;
	}

	public AwardBudgetHeader getAwardBudgetHeader() {
		return awardBudgetHeader;
	}

	public void setAwardBudgetHeader(AwardBudgetHeader awardBudgetHeader) {
		this.awardBudgetHeader = awardBudgetHeader;
	}

	public String getUserName() {
		return userName;
	}

	public void setUserName(String userName) {
		this.userName = userName;
	}

	public String getUserFullName() {
		return userFullName;
	}

	public void setUserFullName(String userFullName) {
		this.userFullName = userFullName;
	}

	public String getActivityTypeCode() {
		return activityTypeCode;
	}

	public void setActivityTypeCode(String activityTypeCode) {
		this.activityTypeCode = activityTypeCode;
	}

	public List<CostElement> getSysGeneratedCostElements() {
		return sysGeneratedCostElements;
	}

	public void setSysGeneratedCostElements(List<CostElement> sysGeneratedCostElements) {
		this.sysGeneratedCostElements = sysGeneratedCostElements;
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public String getAcType() {
		return acType;
	}

	public void setAcType(String acType) {
		this.acType = acType;
	}

	public Integer getBudgetDetailId() {
		return budgetDetailId;
	}

	public void setBudgetDetailId(Integer budgetDetailId) {
		this.budgetDetailId = budgetDetailId;
	}

	public AwardBudgetDetail getAwardBudgetDetail() {
		return awardBudgetDetail;
	}

	public void setAwardBudgetDetail(AwardBudgetDetail awardBudgetDetail) {
		this.awardBudgetDetail = awardBudgetDetail;
	}

	public Integer getDevPropBudgetHeaderId() {
		return devPropBudgetHeaderId;
	}

	public void setDevPropBudgetHeaderId(Integer devPropBudgetHeaderId) {
		this.devPropBudgetHeaderId = devPropBudgetHeaderId;
	}

	public Integer getDevPropBudgetPeriodId() {
		return devPropBudgetPeriodId;
	}

	public void setDevPropBudgetPeriodId(Integer devPropBudgetPeriodId) {
		this.devPropBudgetPeriodId = devPropBudgetPeriodId;
	}

	public String getIpNumber() {
		return ipNumber;
	}

	public void setIpNumber(String ipNumber) {
		this.ipNumber = ipNumber;
	}

	public Integer getPdNumber() {
		return pdNumber;
	}

	public void setPdNumber(Integer pdNumber) {
		this.pdNumber = pdNumber;
	}

	public Integer getPdBudgetId() {
		return pdBudgetId;
	}

	public void setPdBudgetId(Integer pdBudgetId) {
		this.pdBudgetId = pdBudgetId;
	}

	public Integer getDevPropBudgetPeriodNumber() {
		return devPropBudgetPeriodNumber;
	}

	public void setDevPropBudgetPeriodNumber(Integer devPropBudgetPeriodNumber) {
		this.devPropBudgetPeriodNumber = devPropBudgetPeriodNumber;
	}

	public Boolean getIsCreated() {
		return isCreated;
	}

	public void setIsCreated(Boolean isCreated) {
		this.isCreated = isCreated;
	}

	public List<AwardBudgetDetail> getAwardBudgetDetailList() {
		return awardBudgetDetailList;
	}

	public void setAwardBudgetDetailList(List<AwardBudgetDetail> awardBudgetDetailList) {
		this.awardBudgetDetailList = awardBudgetDetailList;
	}

	public Award getAward() {
		return award;
	}

	public void setAward(Award award) {
		this.award = award;
	}

	public Boolean getIsAllPeriod() {
		return isAllPeriod;
	}

	public void setIsAllPeriod(Boolean isAllPeriod) {
		this.isAllPeriod = isAllPeriod;
	}

	public List<AwardBudgetHeader> getAwardBudgetHeaders() {
		return awardBudgetHeaders;
	}

	public void setAwardBudgetHeaders(List<AwardBudgetHeader> awardBudgetHeaders) {
		this.awardBudgetHeaders = awardBudgetHeaders;
	}

	public List<AwardBudgetHeaderDetail> getAwardBudgetList() {
		return awardBudgetList;
	}

	public void setAwardBudgetList(List<AwardBudgetHeaderDetail> awardBudgetList) {
		this.awardBudgetList = awardBudgetList;
	}

	public BudgetHeader getBudgetHeader() {
		return budgetHeader;
	}

	public void setBudgetHeader(BudgetHeader budgetHeader) {
		this.budgetHeader = budgetHeader;
	}

	public Integer getCopyPeriodId() {
		return copyPeriodId;
	}

	public void setCopyPeriodId(Integer copyPeriodId) {
		this.copyPeriodId = copyPeriodId;
	}

	public Integer getCurrentPeriodId() {
		return currentPeriodId;
	}

	public void setCurrentPeriodId(Integer currentPeriodId) {
		this.currentPeriodId = currentPeriodId;
	}

	public Integer getBudgetHeaderId() {
		return budgetHeaderId;
	}

	public void setBudgetHeaderId(Integer budgetHeaderId) {
		this.budgetHeaderId = budgetHeaderId;
	}

	public List<AwardBudgetPerson> getAwardBudgetPersonList() {
		return awardBudgetPersonList;
	}

	public void setAwardBudgetPersonList(List<AwardBudgetPerson> awardBudgetPersonList) {
		this.awardBudgetPersonList = awardBudgetPersonList;
	}

	public List<AppointmentType> getAppointmentType() {
		return appointmentType;
	}

	public void setAppointmentType(List<AppointmentType> appointmentType) {
		this.appointmentType = appointmentType;
	}

	public List<TbnPerson> getTbnPersons() {
		return tbnPersons;
	}

	public void setTbnPersons(List<TbnPerson> tbnPersons) {
		this.tbnPersons = tbnPersons;
	}

	public List<JobCode> getJobCode() {
		return jobCode;
	}

	public void setJobCode(List<JobCode> jobCode) {
		this.jobCode = jobCode;
	}

	public List<AwardPerson> getAwardPersons() {
		return awardPersons;
	}

	public void setAwardPersons(List<AwardPerson> awardPersons) {
		this.awardPersons = awardPersons;
	}

	public Integer getBudgetPersonId() {
		return budgetPersonId;
	}

	public void setBudgetPersonId(Integer budgetPersonId) {
		this.budgetPersonId = budgetPersonId;
	}

	public Integer getBudgetPersonDetailId() {
		return budgetPersonDetailId;
	}

	public void setBudgetPersonDetailId(Integer budgetPersonDetailId) {
		this.budgetPersonDetailId = budgetPersonDetailId;
	}

	public boolean isStatus() {
		return status;
	}

	public void setStatus(boolean status) {
		this.status = status;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public Boolean getIsSinglePeriodBudgetEnabled() {
		return isSinglePeriodBudgetEnabled;
	}

	public void setIsSinglePeriodBudgetEnabled(Boolean isSinglePeriodBudgetEnabled) {
		this.isSinglePeriodBudgetEnabled = isSinglePeriodBudgetEnabled;
	}

	public Boolean getIsEnabledAwardBudgetDetail() {
		return isEnabledAwardBudgetDetail;
	}

	public void setIsEnabledAwardBudgetDetail(Boolean isEnabledAwardBudgetDetail) {
		this.isEnabledAwardBudgetDetail = isEnabledAwardBudgetDetail;
	}

	public Boolean getIsAutoCalculationEnabled() {
		return isAutoCalculationEnabled;
	}

	public void setIsAutoCalculationEnabled(Boolean isAutoCalculationEnabled) {
		this.isAutoCalculationEnabled = isAutoCalculationEnabled;
	}

	public Set<String> getRateClassTypes() {
		return rateClassTypes;
	}

	public void setRateClassTypes(Set<String> rateClassTypes) {
		this.rateClassTypes = rateClassTypes;
	}

	public List<AwardRates> getAwardRates() {
		return awardRates;
	}

	public void setAwardRates(List<AwardRates> awardRates) {
		this.awardRates = awardRates;
	}

	public String getDefaultAbPersonItemToBeTBN() {
		return defaultAbPersonItemToBeTBN;
	}

	public void setDefaultAbPersonItemToBeTBN(String defaultAbPersonItemToBeTBN) {
		this.defaultAbPersonItemToBeTBN = defaultAbPersonItemToBeTBN;
	}

	public Boolean getEnableAbPersonAppliedSalary() {
		return enableAbPersonAppliedSalary;
	}

	public void setEnableAbPersonAppliedSalary(Boolean enableAbPersonAppliedSalary) {
		this.enableAbPersonAppliedSalary = enableAbPersonAppliedSalary;
	}

	public List<AwardBudgetImportInfo> getAwardBudgetImportInfos() {
		return awardBudgetImportInfos;
	}

	public void setAwardBudgetImportInfos(List<AwardBudgetImportInfo> awardBudgetImportInfos) {
		this.awardBudgetImportInfos = awardBudgetImportInfos;
	}

	public Boolean getCanIncludeInBudget() {
		return canIncludeInBudget;
	}

	public void setCanIncludeInBudget(Boolean canIncludeInBudget) {
		this.canIncludeInBudget = canIncludeInBudget;
	}

	public String getAvailableFundType() {
		return availableFundType;
	}

	public void setAvailableFundType(String availableFundType) {
		this.availableFundType = availableFundType;
	}

	public AwardBudgetPerson getAwardBudgetPerson() {
		return awardBudgetPerson;
	}

	public void setAwardBudgetPerson(AwardBudgetPerson awardBudgetPerson) {
		this.awardBudgetPerson = awardBudgetPerson;
	}

	public Boolean getIsNonPersonalLineItemEnabled() {
		return isNonPersonalLineItemEnabled;
	}

	public void setIsNonPersonalLineItemEnabled(Boolean isNonPersonalLineItemEnabled) {
		this.isNonPersonalLineItemEnabled = isNonPersonalLineItemEnabled;
	}

	public Integer getBudgetNonPersonDtlId() {
		return budgetNonPersonDtlId;
	}

	public void setBudgetNonPersonDtlId(Integer budgetNonPersonDtlId) {
		this.budgetNonPersonDtlId = budgetNonPersonDtlId;
	}

	public String getInternalOrderCode() {
		return internalOrderCode;
	}

	public void setInternalOrderCode(String internalOrderCode) {
		this.internalOrderCode = internalOrderCode;
	}

	public List<BudgetTemplateType> getBudgetTemplateTypes() {
		return budgetTemplateTypes;
	}

	public void setBudgetTemplateTypes(List<BudgetTemplateType> budgetTemplateTypes) {
		this.budgetTemplateTypes = budgetTemplateTypes;
	}

	public Integer getBudgetTemplateTypeId() {
		return budgetTemplateTypeId;
	}

	public void setBudgetTemplateTypeId(Integer budgetTemplateTypeId) {
		this.budgetTemplateTypeId = budgetTemplateTypeId;
	}

	public Boolean getEnabledCampusFlagAward() {
		return enabledCampusFlagAward;
	}

	public void setEnabledCampusFlagAward(Boolean enabledCampusFlagAward) {
		this.enabledCampusFlagAward = enabledCampusFlagAward;
	}

	public String getOnCampusRates() {
		return onCampusRates;
	}

	public void setOnCampusRates(String onCampusRates) {
		this.onCampusRates = onCampusRates;
	}

	public String getOffCampusRates() {
		return offCampusRates;
	}

	public void setOffCampusRates(String offCampusRates) {
		this.offCampusRates = offCampusRates;
	}

	public Boolean getShowBudgetOHRatePercentage() {
		return isShowBudgetOHRatePercentage;
	}

	public void setShowBudgetOHRatePercentage(Boolean showBudgetOHRatePercentage) {
		isShowBudgetOHRatePercentage = showBudgetOHRatePercentage;
	}
	
	public Boolean getEnableCostShareStatus() {
		return enableCostShareStatus;
	}

	public void setEnableCostShareStatus(Boolean enableCostShareStatus) {
		this.enableCostShareStatus = enableCostShareStatus;
	}

	public List<CostSharingType> getCostSharingTypes() {
		return costSharingTypes;
	}

	public void setCostSharingTypes(List<CostSharingType> costSharingTypes) {
		this.costSharingTypes = costSharingTypes;
	}
	
}
