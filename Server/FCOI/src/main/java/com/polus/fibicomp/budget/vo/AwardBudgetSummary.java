package com.polus.fibicomp.budget.vo;

import java.math.BigDecimal;
import java.util.Collections;
import java.util.List;

import com.polus.fibicomp.award.comparator.AwardBudgetSummaryComparatorByBudgetCategoryName;
import com.polus.fibicomp.budget.pojo.AwardBudgetHeader;

public class AwardBudgetSummary {

	private List<AwardBudgetPeriodSummary> budgetPeriodSummaries;

	private BigDecimal periodTotalSum = BigDecimal.ZERO;

	private List<AwardBudgetSummaryVO> budgetSummaryVOs;

	private AwardBudgetHeader budgetHeader;
	
	private BigDecimal periodTotalFundRequestedSum = BigDecimal.ZERO;

	private Boolean isBudgetVersionEnabled = Boolean.FALSE;

	private Boolean isBudgetSummaryEnabled = Boolean.FALSE;
	
	private Boolean isShowBudgetOHRatePercentage;
	
	private Boolean enableCostShareStatus;

	private Boolean showAwardBudgetFieldForSap;

	private  Boolean enabledCampusFlagAward;

	public Boolean getEnabledCampusFlagAward() {
		return enabledCampusFlagAward;
	}

	public void setEnabledCampusFlagAward(Boolean enabledCampusFlagAward) {
		this.enabledCampusFlagAward = enabledCampusFlagAward;
	}

	public Boolean getShowAwardBudgetFieldForSap() {
		return showAwardBudgetFieldForSap;
	}

	public void setShowAwardBudgetFieldForSap(Boolean showAwardBudgetFieldForSap) {
		this.showAwardBudgetFieldForSap = showAwardBudgetFieldForSap;
	}

	public List<AwardBudgetPeriodSummary> getBudgetPeriodSummaries() {
		if (budgetPeriodSummaries != null && !budgetPeriodSummaries.isEmpty()) {
			Collections.sort(budgetPeriodSummaries, new AwardBudgetSummaryComparatorByBudgetCategoryName());
		}
		return budgetPeriodSummaries;
	}

	public void setBudgetPeriodSummaries(List<AwardBudgetPeriodSummary> budgetPeriodSummaries) {
		this.budgetPeriodSummaries = budgetPeriodSummaries;
	}

	public List<AwardBudgetSummaryVO> getBudgetSummaryVOs() {
		return budgetSummaryVOs;
	}

	public void setBudgetSummaryVOs(List<AwardBudgetSummaryVO> budgetSummaryVOs) {
		this.budgetSummaryVOs = budgetSummaryVOs;
	}

	public BigDecimal getPeriodTotalSum() {
		return periodTotalSum;
	}

	public void setPeriodTotalSum(BigDecimal periodTotalSum) {
		this.periodTotalSum = periodTotalSum;
	}

	public AwardBudgetHeader getBudgetHeader() {
		return budgetHeader;
	}

	public void setBudgetHeader(AwardBudgetHeader budgetHeader) {
		this.budgetHeader = budgetHeader;
	}

	public BigDecimal getPeriodTotalFundRequestedSum() {
		return periodTotalFundRequestedSum;
	}

	public void setPeriodTotalFundRequestedSum(BigDecimal periodTotalFundRequestedSum) {
		this.periodTotalFundRequestedSum = periodTotalFundRequestedSum;
	}

	public Boolean getIsBudgetVersionEnabled() {
		return isBudgetVersionEnabled;
	}

	public void setIsBudgetVersionEnabled(Boolean isBudgetVersionEnabled) {
		this.isBudgetVersionEnabled = isBudgetVersionEnabled;
	}

	public Boolean getIsBudgetSummaryEnabled() {
		return isBudgetSummaryEnabled;
	}

	public void setIsBudgetSummaryEnabled(Boolean isBudgetSummaryEnabled) {
		this.isBudgetSummaryEnabled = isBudgetSummaryEnabled;
	}
	
	public Boolean getEnableCostShareStatus() {
		return enableCostShareStatus;
	}

	public void setEnableCostShareStatus(Boolean enableCostShareStatus) {
		this.enableCostShareStatus = enableCostShareStatus;
	}

	public Boolean getIsShowBudgetOHRatePercentage() {
		return isShowBudgetOHRatePercentage;
	}

	public void setIsShowBudgetOHRatePercentage(Boolean isShowBudgetOHRatePercentage) {
		this.isShowBudgetOHRatePercentage = isShowBudgetOHRatePercentage;
	}

}
