package com.polus.fibicomp.award.vo;

import java.util.Date;

public class BudgetImportPeriodsInfo {

	private Integer budgetPeriodId;

	private Integer budgetId;

	private Integer periodNumber;

	private Date periodStartDate;

	private Date periodEndDate;

	private String concatenatedString;
	
	private String totalCost;

	public String getTotalCost() {
		return totalCost;
	}

	public void setTotalCost(String totalCost) {
		this.totalCost = totalCost;
	}

	public Integer getBudgetPeriodId() {
		return budgetPeriodId;
	}

	public void setBudgetPeriodId(Integer budgetPeriodId) {
		this.budgetPeriodId = budgetPeriodId;
	}

	public Integer getBudgetId() {
		return budgetId;
	}

	public void setBudgetId(Integer budgetId) {
		this.budgetId = budgetId;
	}

	public Integer getPeriodNumber() {
		return periodNumber;
	}

	public void setPeriodNumber(Integer periodNumber) {
		this.periodNumber = periodNumber;
	}

	public Date getPeriodStartDate() {
		return periodStartDate;
	}

	public void setPeriodStartDate(Date periodStartDate) {
		this.periodStartDate = periodStartDate;
	}

	public Date getPeriodEndDate() {
		return periodEndDate;
	}

	public void setPeriodEndDate(Date periodEndDate) {
		this.periodEndDate = periodEndDate;
	}

	public String getConcatenatedString() {
		return concatenatedString;
	}

	public void setConcatenatedString(String concatenatedString) {
		this.concatenatedString = concatenatedString;
	}

}
