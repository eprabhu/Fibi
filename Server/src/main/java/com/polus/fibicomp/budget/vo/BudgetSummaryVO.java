package com.polus.fibicomp.budget.vo;

import java.math.BigDecimal;

public class BudgetSummaryVO {

	private Integer periodNumber;

	private BigDecimal lineItemCost = BigDecimal.ZERO;
	
	private BigDecimal totalFundRequested = BigDecimal.ZERO;
	
	private BigDecimal totalModifiedDirectCost = BigDecimal.ZERO;

	private String totalFundRequestedAmount;

	private String lineItemCosts;

	private BigDecimal totalCostSharingAmount = BigDecimal.ZERO;
	
	private BigDecimal periodTotalSum = BigDecimal.ZERO;
	
	private BigDecimal periodCostShareTotalSum = BigDecimal.ZERO;
	
	private BigDecimal periodTotalFundRequestedSum  = BigDecimal.ZERO;

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

	public BigDecimal getTotalFundRequested() {
		return totalFundRequested;
	}

	public void setTotalFundRequested(BigDecimal totalFundRequested) {
		this.totalFundRequested = totalFundRequested;
	}

	public BigDecimal getTotalModifiedDirectCost() {
		return totalModifiedDirectCost;
	}

	public void setTotalModifiedDirectCost(BigDecimal totalModifiedDirectCost) {
		this.totalModifiedDirectCost = totalModifiedDirectCost;
	}

	public String getTotalFundRequestedAmount() {
		return totalFundRequestedAmount;
	}

	public void setTotalFundRequestedAmount(String totalFundRequestedAmount) {
		this.totalFundRequestedAmount = totalFundRequestedAmount;
	}

	public String getLineItemCosts() {
		return lineItemCosts;
	}

	public void setLineItemCosts(String lineItemCosts) {
		this.lineItemCosts = lineItemCosts;
	}

	public BigDecimal getTotalCostSharingAmount() {
		return totalCostSharingAmount;
	}

	public void setTotalCostSharingAmount(BigDecimal totalCostSharingAmount) {
		this.totalCostSharingAmount = totalCostSharingAmount;
	}

	public BigDecimal getPeriodTotalSum() {
		return periodTotalSum;
	}

	public void setPeriodTotalSum(BigDecimal periodTotalSum) {
		this.periodTotalSum = periodTotalSum;
	}

	public BigDecimal getPeriodCostShareTotalSum() {
		return periodCostShareTotalSum;
	}

	public void setPeriodCostShareTotalSum(BigDecimal periodCostShareTotalSum) {
		this.periodCostShareTotalSum = periodCostShareTotalSum;
	}

	public BigDecimal getPeriodTotalFundRequestedSum() {
		return periodTotalFundRequestedSum;
	}

	public void setPeriodTotalFundRequestedSum(BigDecimal periodTotalFundRequestedSum) {
		this.periodTotalFundRequestedSum = periodTotalFundRequestedSum;
	}

}
