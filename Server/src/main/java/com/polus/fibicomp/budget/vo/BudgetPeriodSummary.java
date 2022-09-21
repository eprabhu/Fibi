package com.polus.fibicomp.budget.vo;

import java.math.BigDecimal;
import java.util.List;

public class BudgetPeriodSummary {

	private List<BudgetSummaryVO> budgetSummaryVOs;

	private String budgetCategory;

	private BigDecimal totalLineItemCostSum = BigDecimal.ZERO;
	
	private BigDecimal totalFundRequestedCostSum = BigDecimal.ZERO;
	
	private Integer sortOrder;

	private String totalFundRequestedCost;

	private String totalLineItemCostSums;

	private BigDecimal totalCostShareAmountSum = BigDecimal.ZERO;

	private String budgetCategoryCode;

	public String getBudgetCategory() {
		return budgetCategory;
	}

	public void setBudgetCategory(String budgetCategory) {
		this.budgetCategory = budgetCategory;
	}

	public BigDecimal getTotalLineItemCostSum() {
		return totalLineItemCostSum;
	}

	public void setTotalLineItemCostSum(BigDecimal totalLineItemCostSum) {
		this.totalLineItemCostSum = totalLineItemCostSum;
	}

	public List<BudgetSummaryVO> getBudgetSummaryVOs() {
		return budgetSummaryVOs;
	}

	public void setBudgetSummaryVOs(List<BudgetSummaryVO> budgetSummaryVOs) {
		this.budgetSummaryVOs = budgetSummaryVOs;
	}

	public BigDecimal getTotalFundRequestedCostSum() {
		return totalFundRequestedCostSum;
	}

	public void setTotalFundRequestedCostSum(BigDecimal totalFundRequestedCostSum) {
		this.totalFundRequestedCostSum = totalFundRequestedCostSum;
	}

	public Integer getSortOrder() {
		return sortOrder;
	}

	public void setSortOrder(Integer sortOrder) {
		this.sortOrder = sortOrder;
	}

	public String getTotalFundRequestedCost() {
		return totalFundRequestedCost;
	}

	public void setTotalFundRequestedCost(String totalFundRequestedCost) {
		this.totalFundRequestedCost = totalFundRequestedCost;
	}

	public String getTotalLineItemCostSums() {
		return totalLineItemCostSums;
	}

	public void setTotalLineItemCostSums(String totalLineItemCostSums) {
		this.totalLineItemCostSums = totalLineItemCostSums;
	}

	public BigDecimal getTotalCostShareAmountSum() {
		return totalCostShareAmountSum;
	}

	public void setTotalCostShareAmountSum(BigDecimal totalCostShareAmountSum) {
		this.totalCostShareAmountSum = totalCostShareAmountSum;
	}

	public String getBudgetCategoryCode() {
		return budgetCategoryCode;
	}

	public void setBudgetCategoryCode(String budgetCategoryCode) {
		this.budgetCategoryCode = budgetCategoryCode;
	}
}
