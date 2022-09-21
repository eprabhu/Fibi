package com.polus.fibicomp.proposal.print.dto;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import com.polus.fibicomp.budget.pojo.BudgetDetail;

public class LineItem {

	private BudgetDetail budgetDetail;
	private List<PeriodCost> periodCosts;
	private String lineItemDescription;
	private BigDecimal totalLineItemCost;

	public LineItem() {
		periodCosts = new ArrayList<>();
	}

	public BudgetDetail getBudgetDetail() {
		return budgetDetail;
	}

	public void setBudgetDetail(BudgetDetail budgetDetail) {
		this.budgetDetail = budgetDetail;
	}

	public String getLineItemDescription() {
		return lineItemDescription;
	}

	public void setLineItemDescription(String lineItemDescription) {
		this.lineItemDescription = lineItemDescription;
	}

	public BigDecimal getTotalLineItemCost() {
		return totalLineItemCost;
	}

	public void setTotalLineItemCost(BigDecimal totalLineItemCost) {
		this.totalLineItemCost = totalLineItemCost;
	}

	public List<PeriodCost> getPeriodCosts() {
		return periodCosts;
	}

	public void setPeriodCosts(List<PeriodCost> periodCosts) {
		this.periodCosts = periodCosts;
	}

}
