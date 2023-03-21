package com.polus.fibicomp.budget.vo;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Set;

import com.polus.fibicomp.adminportal.pojo.RateType;
import com.polus.fibicomp.budget.pojo.AppointmentType;
import com.polus.fibicomp.budget.pojo.BudgetDetail;
import com.polus.fibicomp.budget.pojo.BudgetHeader;
import com.polus.fibicomp.budget.pojo.BudgetPeriod;
import com.polus.fibicomp.budget.pojo.BudgetPerson;
import com.polus.fibicomp.budget.pojo.BudgetStatus;
import com.polus.fibicomp.budget.pojo.BudgetTemplateType;
import com.polus.fibicomp.budget.pojo.CostElement;
import com.polus.fibicomp.budget.pojo.JobCode;
import com.polus.fibicomp.budget.pojo.TbnPerson;
import com.polus.fibicomp.proposal.comparator.SimpleBudgetDetailComparatorByBudgetCategoryCode;
import com.polus.fibicomp.proposal.comparator.SimpleBudgetDetailComparatorBySystemGenerated;
import com.polus.fibicomp.proposal.pojo.CostSharingType;
import com.polus.fibicomp.proposal.pojo.ProposalPerson;

public class BudgetVO {

	private Integer budgetId;

	private BudgetPerson budgetPerson;

	private List<BudgetPerson> budgetPersonList;

	private List<AppointmentType> appointmentType;

	private List<JobCode> jobCode;

	private BudgetHeader budgetHeader;

	private Integer proposalId;

	private Set<String> rateClassTypes;

	private List<TbnPerson> tbnPersons;

	private List<CostElement> sysGeneratedCostElements;

	private List<BudgetStatus> budgetStatus;

	private String activityTypeCode;

	private List<RateType> rateTypes;

	private String budgetDescription;

	private Timestamp proposalStartDate;

	private Timestamp proposalEndDate;

	private String userName;

	private String userFullName;

	private List<BudgetHeader> budgetHeaders;

	private List<BudgetPeriod> budgetPeriods;

	private List<BudgetDetail> budgetDetails;

	private List<BudgetHeaderDetail> budgetHeaderDetails;

	private Integer budgetPeriod;

	private List<SimpleBudgetVO> simpleBudgetVo;

	private SimpleBudgetVO simpleBudget;

	private Boolean isAutoCalc = false;

	private List<ProposalPerson> proposalPersons;

	private Integer lineItemNumber;

	private BigDecimal totalDirectCost = BigDecimal.ZERO;

	private BigDecimal totalCost = BigDecimal.ZERO;

	private BigDecimal totalIndirectCost = BigDecimal.ZERO;

	private Integer previousFinalBudgetId;

	private String budgetTabName; // DETAILED, SIMPLE, CATEGORY_TOTAL, BUDGET_SUMMARY

	private Integer maxLineItemNumber;

	private Integer grantTypeCode;

	private Boolean isBudgetHeaderFound = false;

	private Integer budgetPeriodId;

	private Integer budgetDetailId;

	private String updateUser;

	private Integer copyPeriodId;

	private Integer currentPeriodId;

	private Integer budgetPersonDetailId;

	private Boolean status;

	private String message;

	private Boolean isModularBudgetEnabled = false;

	private Boolean isSimpleBudgetEnabled = false;

	private Boolean isDetailedBudgetEnabled = false;

	private Boolean isBudgetCategoryTotalEnabled = false;

	private Boolean isFirstVersion = false;

	private Boolean isAutoCalculateEnabled = false;

	private Boolean isSysGeneratedCostElementEnabled = false;

	private Boolean isSinglePeriodBudgetEnabled = false;
	
	private Boolean isCalculationWithPredefinedSalary = false;
	
	private Boolean isShowInKind = false;
	
	private Boolean isShowCostShareAndUnderrecovery = false;
	
	private Boolean isShowModifiedDirectCost = false;
	
	private String proposalType;

	private Boolean isApprovedBudget = false;

	private Integer categoryCode;
	
	private String copyType;
	
	private Integer copiedProposalId;

	private List<BudgetTemplateType> budgetTemplateTypes;
	
	private List<CostSharingType> costSharingType;

	private Integer budgetTemplateTypeId;

	private Boolean isBudgetVersionEnabled = Boolean.FALSE;

	private Boolean isPeriodTotalEnabled = Boolean.FALSE;

	private Boolean isGeneratePeriodsEnabled = Boolean.FALSE;

	private Boolean isBudgetSummaryEnabled = Boolean.FALSE;

	private Boolean isCampusFlagEnabled = Boolean.FALSE;

	private Boolean isOverHeadRateTypeEnabled = Boolean.FALSE;

	private Boolean isProposalComparison = false;

	private BudgetSummary budgetSummary;

	private BudgetModularVO budgetModularVO;

	private Boolean isShowBudgetOHRatePercentage;
	
	private Integer costSharingTypeCode;
	
	private Boolean enableCostShareStatus;

	public BudgetVO() {
		simpleBudgetVo = new ArrayList<>();
	}

	public Integer getBudgetId() {
		return budgetId;
	}

	public void setBudgetId(Integer budgetId) {
		this.budgetId = budgetId;
	}

	public List<BudgetPerson> getBudgetPersonList() {
		return budgetPersonList;
	}

	public void setBudgetPersonList(List<BudgetPerson> budgetPersonList) {
		this.budgetPersonList = budgetPersonList;
	}

	public List<AppointmentType> getAppointmentType() {
		return appointmentType;
	}

	public void setAppointmentType(List<AppointmentType> appointmentType) {
		this.appointmentType = appointmentType;
	}

	public List<BudgetPeriod> getBudgetPeriods() {
		return budgetPeriods;
	}
	
	public void setBudgetPeriods(List<BudgetPeriod> budgetPeriods) {
		this.budgetPeriods = budgetPeriods;
	}

	public List<BudgetDetail> getBudgetDetails() {
		return budgetDetails;
	}

	public void setBudgetDetails(List<BudgetDetail> budgetDetails) {
		this.budgetDetails = budgetDetails;
	}

	public List<JobCode> getJobCode() {
		return jobCode;
	}

	public void setJobCode(List<JobCode> jobCode) {
		this.jobCode = jobCode;
	}

	public BudgetPerson getBudgetPerson() {
		return budgetPerson;
	}

	public void setBudgetPerson(BudgetPerson budgetPerson) {
		this.budgetPerson = budgetPerson;
	}

	public BudgetHeader getBudgetHeader() {
		return budgetHeader;
	}

	public void setBudgetHeader(BudgetHeader budgetHeader) {
		this.budgetHeader = budgetHeader;
	}

	public Integer getProposalId() {
		return proposalId;
	}

	public void setProposalId(Integer proposalId) {
		this.proposalId = proposalId;
	}

	public Set<String> getRateClassTypes() {
		return rateClassTypes;
	}

	public void setRateClassTypes(Set<String> rateClassTypes) {
		this.rateClassTypes = rateClassTypes;
	}

	public List<TbnPerson> getTbnPersons() {
		return tbnPersons;
	}

	public void setTbnPersons(List<TbnPerson> tbnPersons) {
		this.tbnPersons = tbnPersons;
	}

	public List<CostElement> getSysGeneratedCostElements() {
		return sysGeneratedCostElements;
	}

	public void setSysGeneratedCostElements(List<CostElement> sysGeneratedCostElements) {
		this.sysGeneratedCostElements = sysGeneratedCostElements;
	}

	public List<BudgetStatus> getBudgetStatus() {
		return budgetStatus;
	}

	public void setBudgetStatus(List<BudgetStatus> budgetStatus) {
		this.budgetStatus = budgetStatus;
	}

	public String getActivityTypeCode() {
		return activityTypeCode;
	}

	public void setActivityTypeCode(String activityTypeCode) {
		this.activityTypeCode = activityTypeCode;
	}

	public List<RateType> getRateTypes() {
		return rateTypes;
	}

	public void setRateTypes(List<RateType> rateTypes) {
		this.rateTypes = rateTypes;
	}

	public String getBudgetDescription() {
		return budgetDescription;
	}

	public void setBudgetDescription(String budgetDescription) {
		this.budgetDescription = budgetDescription;
	}

	public Timestamp getProposalStartDate() {
		return proposalStartDate;
	}

	public void setProposalStartDate(Timestamp proposalStartDate) {
		this.proposalStartDate = proposalStartDate;
	}

	public Timestamp getProposalEndDate() {
		return proposalEndDate;
	}

	public void setProposalEndDate(Timestamp proposalEndDate) {
		this.proposalEndDate = proposalEndDate;
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

	public List<BudgetHeader> getBudgetHeaders() {
		return budgetHeaders;
	}

	public void setBudgetHeaders(List<BudgetHeader> budgetHeaders) {
		this.budgetHeaders = budgetHeaders;
	}

	public Integer getBudgetPeriodId() {
		return budgetPeriodId;
	}

	public void setBudgetPeriodId(Integer budgetPeriodId) {
		this.budgetPeriodId = budgetPeriodId;
	}

	public Integer getBudgetDetailId() {
		return budgetDetailId;
	}

	public void setBudgetDetailId(Integer budgetDetailId) {
		this.budgetDetailId = budgetDetailId;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
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

	public Integer getBudgetPersonDetailId() {
		return budgetPersonDetailId;
	}

	public void setBudgetPersonDetailId(Integer budgetPersonDetailId) {
		this.budgetPersonDetailId = budgetPersonDetailId;
	}

	public Boolean getStatus() {
		return status;
	}

	public void setStatus(Boolean status) {
		this.status = status;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public Boolean getIsModularBudgetEnabled() {
		return isModularBudgetEnabled;
	}

	public void setIsModularBudgetEnabled(Boolean isModularBudgetEnabled) {
		this.isModularBudgetEnabled = isModularBudgetEnabled;
	}

	public Boolean getIsSimpleBudgetEnabled() {
		return isSimpleBudgetEnabled;
	}

	public void setIsSimpleBudgetEnabled(Boolean isSimpleBudgetEnabled) {
		this.isSimpleBudgetEnabled = isSimpleBudgetEnabled;
	}

	public Boolean getIsDetailedBudgetEnabled() {
		return isDetailedBudgetEnabled;
	}

	public void setIsDetailedBudgetEnabled(Boolean isDetailedBudgetEnabled) {
		this.isDetailedBudgetEnabled = isDetailedBudgetEnabled;
	}

	public Boolean getIsBudgetCategoryTotalEnabled() {
		return isBudgetCategoryTotalEnabled;
	}

	public void setIsBudgetCategoryTotalEnabled(Boolean isBudgetCategoryTotalEnabled) {
		this.isBudgetCategoryTotalEnabled = isBudgetCategoryTotalEnabled;
	}

	public Boolean getIsFirstVersion() {
		return isFirstVersion;
	}

	public void setIsFirstVersion(Boolean isFirstVersion) {
		this.isFirstVersion = isFirstVersion;
	}

	public List<BudgetHeaderDetail> getBudgetHeaderDetails() {
		return budgetHeaderDetails;
	}

	public void setBudgetHeaderDetails(List<BudgetHeaderDetail> budgetHeaderDetails) {
		this.budgetHeaderDetails = budgetHeaderDetails;
	}

	public Integer getBudgetPeriod() {
		return budgetPeriod;
	}

	public void setBudgetPeriod(Integer budgetPeriod) {
		this.budgetPeriod = budgetPeriod;
	}

	public List<SimpleBudgetVO> getSimpleBudgetVo() {
		if (simpleBudgetVo != null && !simpleBudgetVo.isEmpty()) {
			Collections.sort(simpleBudgetVo, new SimpleBudgetDetailComparatorByBudgetCategoryCode());
			Collections.sort(simpleBudgetVo, new SimpleBudgetDetailComparatorBySystemGenerated());
		}
		return simpleBudgetVo;
	}

	public void setSimpleBudgetVo(List<SimpleBudgetVO> simpleBudgetVo) {
		this.simpleBudgetVo = simpleBudgetVo;
	}

	public SimpleBudgetVO getSimpleBudget() {
		return simpleBudget;
	}

	public void setSimpleBudget(SimpleBudgetVO simpleBudget) {
		this.simpleBudget = simpleBudget;
	}

	public Boolean getIsAutoCalc() {
		return isAutoCalc;
	}

	public void setIsAutoCalc(Boolean isAutoCalc) {
		this.isAutoCalc = isAutoCalc;
	}

	public List<ProposalPerson> getProposalPersons() {
		return proposalPersons;
	}

	public void setProposalPersons(List<ProposalPerson> proposalPersons) {
		this.proposalPersons = proposalPersons;
	}

	public Integer getLineItemNumber() {
		return lineItemNumber;
	}

	public void setLineItemNumber(Integer lineItemNumber) {
		this.lineItemNumber = lineItemNumber;
	}

	public BigDecimal getTotalDirectCost() {
		return totalDirectCost;
	}

	public void setTotalDirectCost(BigDecimal totalDirectCost) {
		this.totalDirectCost = totalDirectCost;
	}

	public BigDecimal getTotalCost() {
		return totalCost;
	}

	public void setTotalCost(BigDecimal totalCost) {
		this.totalCost = totalCost;
	}

	public BigDecimal getTotalIndirectCost() {
		return totalIndirectCost;
	}

	public void setTotalIndirectCost(BigDecimal totalIndirectCost) {
		this.totalIndirectCost = totalIndirectCost;
	}

	public Integer getPreviousFinalBudgetId() {
		return previousFinalBudgetId;
	}

	public void setPreviousFinalBudgetId(Integer previousFinalBudgetId) {
		this.previousFinalBudgetId = previousFinalBudgetId;
	}

	public Boolean getIsAutoCalculateEnabled() {
		return isAutoCalculateEnabled;
	}

	public void setIsAutoCalculateEnabled(Boolean isAutoCalculateEnabled) {
		this.isAutoCalculateEnabled = isAutoCalculateEnabled;
	}

	public String getBudgetTabName() {
		return budgetTabName;
	}

	public void setBudgetTabName(String budgetTabName) {
		this.budgetTabName = budgetTabName;
	}

	public Integer getMaxLineItemNumber() {
		return maxLineItemNumber;
	}

	public void setMaxLineItemNumber(Integer maxLineItemNumber) {
		this.maxLineItemNumber = maxLineItemNumber;
	}

	public Integer getGrantTypeCode() {
		return grantTypeCode;
	}

	public void setGrantTypeCode(Integer grantTypeCode) {
		this.grantTypeCode = grantTypeCode;
	}

	public Boolean getIsBudgetHeaderFound() {
		return isBudgetHeaderFound;
	}

	public void setIsBudgetHeaderFound(Boolean isBudgetHeaderFound) {
		this.isBudgetHeaderFound = isBudgetHeaderFound;
	}

	public Boolean getIsSysGeneratedCostElementEnabled() {
		return isSysGeneratedCostElementEnabled;
	}

	public void setIsSysGeneratedCostElementEnabled(Boolean isSysGeneratedCostElementEnabled) {
		this.isSysGeneratedCostElementEnabled = isSysGeneratedCostElementEnabled;
	}

	public String getProposalType() {
		return proposalType;
	}

	public void setProposalType(String proposalType) {
		this.proposalType = proposalType;
	}

	public Boolean getIsApprovedBudget() {
		return isApprovedBudget;
	}

	public void setIsApprovedBudget(Boolean isApprovedBudget) {
		this.isApprovedBudget = isApprovedBudget;
	}

	public Integer getCategoryCode() {
		return categoryCode;
	}

	public void setCategoryCode(Integer categoryCode) {
		this.categoryCode = categoryCode;
	}

	public Boolean getIsSinglePeriodBudgetEnabled() {
		return isSinglePeriodBudgetEnabled;
	}

	public void setIsSinglePeriodBudgetEnabled(Boolean isSinglePeriodBudgetEnabled) {
		this.isSinglePeriodBudgetEnabled = isSinglePeriodBudgetEnabled;
	}

	public Boolean getIsCalculationWithPredefinedSalary() {
		return isCalculationWithPredefinedSalary;
	}

	public void setIsCalculationWithPredefinedSalary(Boolean isCalculationWithPredefinedSalary) {
		this.isCalculationWithPredefinedSalary = isCalculationWithPredefinedSalary;
	}

	public Boolean getIsShowInKind() {
		return isShowInKind;
	}

	public void setIsShowInKind(Boolean isShowInKind) {
		this.isShowInKind = isShowInKind;
	}

	public Boolean getIsShowCostShareAndUnderrecovery() {
		return isShowCostShareAndUnderrecovery;
	}

	public void setIsShowCostShareAndUnderrecovery(Boolean isShowCostShareAndUnderrecovery) {
		this.isShowCostShareAndUnderrecovery = isShowCostShareAndUnderrecovery;
	}

	public String getCopyType() {
		return copyType;
	}

	public void setCopyType(String copyType) {
		this.copyType = copyType;
	}

	public Integer getCopiedProposalId() {
		return copiedProposalId;
	}

	public void setCopiedProposalId(Integer copiedProposalId) {
		this.copiedProposalId = copiedProposalId;
	}

	public Boolean getIsShowModifiedDirectCost() {
		return isShowModifiedDirectCost;
	}

	public void setIsShowModifiedDirectCost(Boolean isShowModifiedDirectCost) {
		this.isShowModifiedDirectCost = isShowModifiedDirectCost;
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

	public Boolean getIsBudgetVersionEnabled() {
		return isBudgetVersionEnabled;
	}

	public void setIsBudgetVersionEnabled(Boolean isBudgetVersionEnabled) {
		this.isBudgetVersionEnabled = isBudgetVersionEnabled;
	}

	public Boolean getIsPeriodTotalEnabled() {
		return isPeriodTotalEnabled;
	}

	public void setIsPeriodTotalEnabled(Boolean isPeriodTotalEnabled) {
		this.isPeriodTotalEnabled = isPeriodTotalEnabled;
	}

	public Boolean getIsGeneratePeriodsEnabled() {
		return isGeneratePeriodsEnabled;
	}

	public void setIsGeneratePeriodsEnabled(Boolean isGeneratePeriodsEnabled) {
		this.isGeneratePeriodsEnabled = isGeneratePeriodsEnabled;
	}

	public Boolean getIsBudgetSummaryEnabled() {
		return isBudgetSummaryEnabled;
	}

	public void setIsBudgetSummaryEnabled(Boolean isBudgetSummaryEnabled) {
		this.isBudgetSummaryEnabled = isBudgetSummaryEnabled;
	}

	public Boolean getIsCampusFlagEnabled() {
		return isCampusFlagEnabled;
	}

	public void setIsCampusFlagEnabled(Boolean isCampusFlagEnabled) {
		this.isCampusFlagEnabled = isCampusFlagEnabled;
	}

	public Boolean getIsOverHeadRateTypeEnabled() {
		return isOverHeadRateTypeEnabled;
	}

	public void setIsOverHeadRateTypeEnabled(Boolean isOverHeadRateTypeEnabled) {
		this.isOverHeadRateTypeEnabled = isOverHeadRateTypeEnabled;
  }
  
	public Boolean getIsProposalComparison() {
		return isProposalComparison;
	}

	public void setIsProposalComparison(Boolean isProposalComparison) {
		this.isProposalComparison = isProposalComparison;
	}

	public BudgetSummary getBudgetSummary() {
		return budgetSummary;
	}

	public void setBudgetSummary(BudgetSummary budgetSummary) {
		this.budgetSummary = budgetSummary;
	}

	public BudgetModularVO getBudgetModularVO() {
		return budgetModularVO;
	}

	public void setBudgetModularVO(BudgetModularVO budgetModularVO) {
		this.budgetModularVO = budgetModularVO;
	}

	public Boolean getShowBudgetOHRatePercentage() {
		return isShowBudgetOHRatePercentage;
	}

	public void setShowBudgetOHRatePercentage(Boolean showBudgetOHRatePercentage) {
		isShowBudgetOHRatePercentage = showBudgetOHRatePercentage;
	}
	
	public List<CostSharingType> getCostSharingType() {
		return costSharingType;
	}
	
	public void setCostSharingType(List<CostSharingType> costSharingType) {
		this.costSharingType = costSharingType;
	}
	
	public Integer getCostSharingTypeCode() {
		return costSharingTypeCode;
	}

	public void setCostSharingTypeCode(Integer costSharingTypeCode) {
		this.costSharingTypeCode = costSharingTypeCode;
	}
	
	public Boolean getEnableCostShareStatus() {
		return enableCostShareStatus;
	}

	public void setEnableCostShareStatus(Boolean enableCostShareStatus) {
		this.enableCostShareStatus = enableCostShareStatus;
	}


}
