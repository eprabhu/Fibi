package com.polus.fibicomp.budget.vo;

import java.math.BigDecimal;
import java.util.Collections;
import java.util.List;

import com.polus.fibicomp.budget.pojo.BudgetHeader;
import com.polus.fibicomp.proposal.comparator.BudgetSummaryComparatorByBudgetCategoryName;

public class BudgetSummary {

	private List<BudgetPeriodSummary> budgetPeriodSummaries;

	private BigDecimal periodTotalSum = BigDecimal.ZERO;

	private List<BudgetSummaryVO> budgetSummaryVOs;

	private BudgetHeader budgetHeader;
	
	private BigDecimal periodTotalFundRequestedSum = BigDecimal.ZERO;

	private BigDecimal periodTotalCostShareSum = BigDecimal.ZERO;

	public List<BudgetPeriodSummary> getBudgetPeriodSummaries() {
		if (budgetPeriodSummaries != null && !budgetPeriodSummaries.isEmpty()) {
			Collections.sort(budgetPeriodSummaries, new BudgetSummaryComparatorByBudgetCategoryName());
		}
		return budgetPeriodSummaries;
	}

	public void setBudgetPeriodSummaries(List<BudgetPeriodSummary> budgetPeriodSummaries) {
		this.budgetPeriodSummaries = budgetPeriodSummaries;
	}

	public List<BudgetSummaryVO> getBudgetSummaryVOs() {
		return budgetSummaryVOs;
	}

	public void setBudgetSummaryVOs(List<BudgetSummaryVO> budgetSummaryVOs) {
		this.budgetSummaryVOs = budgetSummaryVOs;
	}

	public BigDecimal getPeriodTotalSum() {
		return periodTotalSum;
	}

	public void setPeriodTotalSum(BigDecimal periodTotalSum) {
		this.periodTotalSum = periodTotalSum;
	}

	public BudgetHeader getBudgetHeader() {
		return budgetHeader;
	}

	public void setBudgetHeader(BudgetHeader budgetHeader) {
		this.budgetHeader = budgetHeader;
	}

	public BigDecimal getPeriodTotalFundRequestedSum() {
		return periodTotalFundRequestedSum;
	}

	public void setPeriodTotalFundRequestedSum(BigDecimal periodTotalFundRequestedSum) {
		this.periodTotalFundRequestedSum = periodTotalFundRequestedSum;
	}

	public BigDecimal getPeriodTotalCostShareSum() {
		return periodTotalCostShareSum;
	}

	public void setPeriodTotalCostShareSum(BigDecimal periodTotalCostShareSum) {
		this.periodTotalCostShareSum = periodTotalCostShareSum;
	}

}
