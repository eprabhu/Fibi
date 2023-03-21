package com.polus.fibicomp.vo;

import java.util.List;

import com.polus.fibicomp.pojo.FileType;

public class ParameterVO {

	private Boolean isOrcidWOrkEnabled = false;

	private Boolean isGrantCallStatusAutomated = false;

	private Boolean isEnableSpecialReview = false;

	private Boolean isAwardManpowerActive = false;

	private List<FileType> fileTypes;

	private Boolean canUserAddOrganization = Boolean.FALSE;

	private Boolean isWafEnabled = Boolean.FALSE;

	private Boolean isEvaluation = Boolean.FALSE;

	private Boolean isMapRouting = Boolean.FALSE;

	private Boolean isEvaluationAndMapRouting = Boolean.FALSE;

	private Boolean isEnableAddtoAddressBook = Boolean.FALSE;

	private Boolean isDevProposalVersioningEnabled = Boolean.FALSE;

	private Boolean isProposalOrganizationEnabled = Boolean.FALSE;

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
	
	private Boolean isApprovedBudget = false;

	private Boolean isBudgetVersionEnabled = Boolean.FALSE;

	private Boolean isPeriodTotalEnabled = Boolean.FALSE;

	private Boolean isGeneratePeriodsEnabled = Boolean.FALSE;

	private Boolean isBudgetSummaryEnabled = Boolean.FALSE;

	private Boolean isCampusFlagEnabled = Boolean.FALSE;

	private Boolean triageQuestionnaireRequired = Boolean.FALSE;
	
	private Boolean isShowCreateAgreement  = Boolean.FALSE;
	
	private Boolean isShowAgreementSupport = Boolean.FALSE;

	private Boolean isShowAgreementNotifyAction = Boolean.FALSE;

	private Boolean isEnableLock;

	private Boolean isEnableSocket;

	private Boolean isShowBudgetOHRatePercentage;
	
	public Boolean getIsOrcidWOrkEnabled() {
		return isOrcidWOrkEnabled;
	}

	public void setIsOrcidWOrkEnabled(Boolean isOrcidWOrkEnabled) {
		this.isOrcidWOrkEnabled = isOrcidWOrkEnabled;
	}

	public Boolean getIsGrantCallStatusAutomated() {
		return isGrantCallStatusAutomated;
	}

	public void setIsGrantCallStatusAutomated(Boolean isGrantCallStatusAutomated) {
		this.isGrantCallStatusAutomated = isGrantCallStatusAutomated;
	}

	public Boolean getIsEnableSpecialReview() {
		return isEnableSpecialReview;
	}

	public void setIsEnableSpecialReview(Boolean isEnableSpecialReview) {
		this.isEnableSpecialReview = isEnableSpecialReview;
	}

	public Boolean getIsAwardManpowerActive() {
		return isAwardManpowerActive;
	}

	public void setIsAwardManpowerActive(Boolean isAwardManpowerActive) {
		this.isAwardManpowerActive = isAwardManpowerActive;
	}

	public List<FileType> getFileTypes() {
		return fileTypes;
	}

	public void setFileTypes(List<FileType> fileTypes) {
		this.fileTypes = fileTypes;
	}

	public Boolean getCanUserAddOrganization() {
		return canUserAddOrganization;
	}

	public void setCanUserAddOrganization(Boolean canUserAddOrganization) {
		this.canUserAddOrganization = canUserAddOrganization;
	}

	public Boolean getIsWafEnabled() {
		return isWafEnabled;
	}

	public void setIsWafEnabled(Boolean isWafEnabled) {
		this.isWafEnabled = isWafEnabled;
	}

	public Boolean getIsEvaluation() {
		return isEvaluation;
	}

	public void setIsEvaluation(Boolean isEvaluation) {
		this.isEvaluation = isEvaluation;
	}

	public Boolean getIsMapRouting() {
		return isMapRouting;
	}

	public void setIsMapRouting(Boolean isMapRouting) {
		this.isMapRouting = isMapRouting;
	}

	public Boolean getIsEvaluationAndMapRouting() {
		return isEvaluationAndMapRouting;
	}

	public void setIsEvaluationAndMapRouting(Boolean isEvaluationAndMapRouting) {
		this.isEvaluationAndMapRouting = isEvaluationAndMapRouting;
	}

	public Boolean getIsEnableAddtoAddressBook() {
		return isEnableAddtoAddressBook;
	}

	public void setIsEnableAddtoAddressBook(Boolean isEnableAddtoAddressBook) {
		this.isEnableAddtoAddressBook = isEnableAddtoAddressBook;
	}

	public Boolean getIsDevProposalVersioningEnabled() {
		return isDevProposalVersioningEnabled;
	}

	public void setIsDevProposalVersioningEnabled(Boolean isDevProposalVersioningEnabled) {
		this.isDevProposalVersioningEnabled = isDevProposalVersioningEnabled;
	}

	public Boolean getIsProposalOrganizationEnabled() {
		return isProposalOrganizationEnabled;
	}

	public void setIsProposalOrganizationEnabled(Boolean isProposalOrganizationEnabled) {
		this.isProposalOrganizationEnabled = isProposalOrganizationEnabled;
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

	public Boolean getIsAutoCalculateEnabled() {
		return isAutoCalculateEnabled;
	}

	public void setIsAutoCalculateEnabled(Boolean isAutoCalculateEnabled) {
		this.isAutoCalculateEnabled = isAutoCalculateEnabled;
	}

	public Boolean getIsSysGeneratedCostElementEnabled() {
		return isSysGeneratedCostElementEnabled;
	}

	public void setIsSysGeneratedCostElementEnabled(Boolean isSysGeneratedCostElementEnabled) {
		this.isSysGeneratedCostElementEnabled = isSysGeneratedCostElementEnabled;
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

	public Boolean getIsShowModifiedDirectCost() {
		return isShowModifiedDirectCost;
	}

	public void setIsShowModifiedDirectCost(Boolean isShowModifiedDirectCost) {
		this.isShowModifiedDirectCost = isShowModifiedDirectCost;
	}

	public Boolean getIsApprovedBudget() {
		return isApprovedBudget;
	}

	public void setIsApprovedBudget(Boolean isApprovedBudget) {
		this.isApprovedBudget = isApprovedBudget;
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

	public Boolean getTriageQuestionnaireRequired() {
		return triageQuestionnaireRequired;
	}

	public void setTriageQuestionnaireRequired(Boolean triageQuestionnaireRequired) {
		this.triageQuestionnaireRequired = triageQuestionnaireRequired;
	}

	public Boolean getIsShowCreateAgreement() {
		return isShowCreateAgreement;
	}

	public void setIsShowCreateAgreement(Boolean isShowCreateAgreement) {
		this.isShowCreateAgreement = isShowCreateAgreement;
	}

	public Boolean getIsShowAgreementSupport() {
		return isShowAgreementSupport;
	}

	public void setIsShowAgreementSupport(Boolean isShowAgreementSupport) {
		this.isShowAgreementSupport = isShowAgreementSupport;
	}

	public Boolean getIsShowAgreementNotifyAction() {
		return isShowAgreementNotifyAction;
	}

	public void setIsShowAgreementNotifyAction(Boolean isShowAgreementNotifyAction) {
		this.isShowAgreementNotifyAction = isShowAgreementNotifyAction;
	}

	public Boolean getIsEnableLock() {
		return isEnableLock;
	}

	public void setIsEnableLock(Boolean isEnableLock) {
		this.isEnableLock = isEnableLock;
	}

	public Boolean getIsEnableSocket() {
		return isEnableSocket;
	}

	public void setIsEnableSocket(Boolean isEnableSocket) {
		this.isEnableSocket = isEnableSocket;
	}

	public Boolean getShowBudgetOHRatePercentage() {
		return isShowBudgetOHRatePercentage;
	}

	public void setShowBudgetOHRatePercentage(Boolean showBudgetOHRatePercentage) {
		isShowBudgetOHRatePercentage = showBudgetOHRatePercentage;
	}
}
