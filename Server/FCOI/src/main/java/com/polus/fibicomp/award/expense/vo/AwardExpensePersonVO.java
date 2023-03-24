package com.polus.fibicomp.award.expense.vo;

import java.math.BigDecimal;
import java.sql.Timestamp;

public class AwardExpensePersonVO {

	private Integer budgetPersonDetailId;

	private Integer budgetDetailId;

	private Integer budgetPersonId;

	private Integer lineItemNumber;

	private BigDecimal underRecoveryAmount;

	private BigDecimal percentageCharged;

	private BigDecimal percentageEffort;

	private BigDecimal costSharingAmount;

	private BigDecimal costSharingPercentage;

	private BigDecimal salaryRequested;

	private BigDecimal totalSalary;

	private BigDecimal noOfMonths;

	private String internalOrderCode;

	private Timestamp updateTimeStamp;

	private String updateUser;

	private String budgetPersonName;

	private Timestamp endDate;

	private Timestamp startDate;

	public Integer getBudgetPersonDetailId() {
		return budgetPersonDetailId;
	}

	public void setBudgetPersonDetailId(Integer budgetPersonDetailId) {
		this.budgetPersonDetailId = budgetPersonDetailId;
	}

	public Integer getBudgetDetailId() {
		return budgetDetailId;
	}

	public void setBudgetDetailId(Integer budgetDetailId) {
		this.budgetDetailId = budgetDetailId;
	}

	public Integer getBudgetPersonId() {
		return budgetPersonId;
	}

	public void setBudgetPersonId(Integer budgetPersonId) {
		this.budgetPersonId = budgetPersonId;
	}

	public Integer getLineItemNumber() {
		return lineItemNumber;
	}

	public void setLineItemNumber(Integer lineItemNumber) {
		this.lineItemNumber = lineItemNumber;
	}

	public BigDecimal getUnderRecoveryAmount() {
		return underRecoveryAmount;
	}

	public void setUnderRecoveryAmount(BigDecimal underRecoveryAmount) {
		this.underRecoveryAmount = underRecoveryAmount;
	}

	public BigDecimal getPercentageCharged() {
		return percentageCharged;
	}

	public void setPercentageCharged(BigDecimal percentageCharged) {
		this.percentageCharged = percentageCharged;
	}

	public BigDecimal getPercentageEffort() {
		return percentageEffort;
	}

	public void setPercentageEffort(BigDecimal percentageEffort) {
		this.percentageEffort = percentageEffort;
	}

	public BigDecimal getCostSharingAmount() {
		return costSharingAmount;
	}

	public void setCostSharingAmount(BigDecimal costSharingAmount) {
		this.costSharingAmount = costSharingAmount;
	}

	public BigDecimal getCostSharingPercentage() {
		return costSharingPercentage;
	}

	public void setCostSharingPercentage(BigDecimal costSharingPercentage) {
		this.costSharingPercentage = costSharingPercentage;
	}

	public BigDecimal getSalaryRequested() {
		return salaryRequested;
	}

	public void setSalaryRequested(BigDecimal salaryRequested) {
		this.salaryRequested = salaryRequested;
	}

	public BigDecimal getTotalSalary() {
		return totalSalary;
	}

	public void setTotalSalary(BigDecimal totalSalary) {
		this.totalSalary = totalSalary;
	}

	public BigDecimal getNoOfMonths() {
		return noOfMonths;
	}

	public void setNoOfMonths(BigDecimal noOfMonths) {
		this.noOfMonths = noOfMonths;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public String getInternalOrderCode() {
		return internalOrderCode;
	}

	public void setInternalOrderCode(String internalOrderCode) {
		this.internalOrderCode = internalOrderCode;
	}

	public String getBudgetPersonName() {
		return budgetPersonName;
	}

	public void setBudgetPersonName(String budgetPersonName) {
		this.budgetPersonName = budgetPersonName;
	}

	public Timestamp getEndDate() {
		return endDate;
	}

	public void setEndDate(Timestamp endDate) {
		this.endDate = endDate;
	}

	public Timestamp getStartDate() {
		return startDate;
	}

	public void setStartDate(Timestamp startDate) {
		this.startDate = startDate;
	}
}
