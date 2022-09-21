package com.polus.fibicomp.budget.vo;

import java.math.BigDecimal;

public class AwardBudgetSummaryVO {

	private Integer periodNumber;

	private BigDecimal lineItemCost = BigDecimal.ZERO;

	private String lineItemCostValue;

	private String lineItemCosts;

	public BigDecimal getLineItemCost() {
		return lineItemCost;
	}

	public void setLineItemCost(BigDecimal lineItemCost) {
		this.lineItemCost = lineItemCost;
	}

	public Integer getPeriodNumber() {
		return periodNumber;
	}

	public void setPeriodNumber(Integer periodNumber) {
		this.periodNumber = periodNumber;
	}

	public String getLineItemCostValue() {
		return lineItemCostValue;
	}

	public void setLineItemCostValue(String lineItemCostValue) {
		this.lineItemCostValue = lineItemCostValue;
	}

	public String getLineItemCosts() {
		return lineItemCosts;
	}

	public void setLineItemCosts(String lineItemCosts) {
		this.lineItemCosts = lineItemCosts;
	}

}
