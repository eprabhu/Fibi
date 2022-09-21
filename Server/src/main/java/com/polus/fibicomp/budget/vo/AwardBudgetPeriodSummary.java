package com.polus.fibicomp.budget.vo;

import java.math.BigDecimal;
import java.util.List;

public class AwardBudgetPeriodSummary {

	private List<AwardBudgetSummaryVO> budgetSummaryVOs;

	private String budgetCategory;

	private BigDecimal totalLineItemCostSum = BigDecimal.ZERO;

	private BigDecimal totalFundRequestedCostSum = BigDecimal.ZERO;

	private Integer sortOrder;

	private String totalLineItemCost;

	private String totalFundRequestedCost;

	private String totalLineItemCostSums;

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

	public List<AwardBudgetSummaryVO> getBudgetSummaryVOs() {
		return budgetSummaryVOs;
	}

	public void setBudgetSummaryVOs(List<AwardBudgetSummaryVO> budgetSummaryVOs) {
		this.budgetSummaryVOs = budgetSummaryVOs;
	}

	public String getTotalLineItemCost() {
		return totalLineItemCost;
	}

	public void setTotalLineItemCost(String totalLineItemCost) {
		this.totalLineItemCost = totalLineItemCost;
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

	public String getBudgetCategoryCode() {
		return budgetCategoryCode;
	}

	public void setBudgetCategoryCode(String budgetCategoryCode) {
		this.budgetCategoryCode = budgetCategoryCode;
	}

}
