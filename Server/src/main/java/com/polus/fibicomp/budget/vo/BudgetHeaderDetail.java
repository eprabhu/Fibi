package com.polus.fibicomp.budget.vo;

import java.math.BigDecimal;
import java.sql.Timestamp;

public class BudgetHeaderDetail {

	private Integer budgetId;

	private Integer proposalId;

	private Timestamp startDate;

	private Timestamp endDate;

	private BigDecimal totalCost = BigDecimal.ZERO;

	private BigDecimal totalDirectCost = BigDecimal.ZERO;

	private BigDecimal totalIndirectCost = BigDecimal.ZERO;

	private Boolean isFinalBudget = false;

	private BigDecimal costSharingAmount = BigDecimal.ZERO;

	private BigDecimal underrecoveryAmount = BigDecimal.ZERO;

	private Boolean isSelected = false;
	
	private BigDecimal totalFundRequested = BigDecimal.ZERO;
	
	private BigDecimal totalModifiedDirectCost = BigDecimal.ZERO;
	
	private BigDecimal totalInKind = BigDecimal.ZERO;
	
	private BigDecimal totalOfTotalCost = BigDecimal.ZERO;

	private Integer versionNumber;

	private Boolean isApprovedBudget = false;

	public Integer getBudgetId() {
		return budgetId;
	}

	public void setBudgetId(Integer budgetId) {
		this.budgetId = budgetId;
	}

	public Integer getProposalId() {
		return proposalId;
	}

	public void setProposalId(Integer proposalId) {
		this.proposalId = proposalId;
	}

	public Timestamp getStartDate() {
		return startDate;
	}

	public void setStartDate(Timestamp startDate) {
		this.startDate = startDate;
	}

	public Timestamp getEndDate() {
		return endDate;
	}

	public void setEndDate(Timestamp endDate) {
		this.endDate = endDate;
	}

	public BigDecimal getTotalCost() {
		return totalCost;
	}

	public void setTotalCost(BigDecimal totalCost) {
		this.totalCost = totalCost;
	}

	public BigDecimal getTotalDirectCost() {
		return totalDirectCost;
	}

	public void setTotalDirectCost(BigDecimal totalDirectCost) {
		this.totalDirectCost = totalDirectCost;
	}

	public BigDecimal getTotalIndirectCost() {
		return totalIndirectCost;
	}

	public void setTotalIndirectCost(BigDecimal totalIndirectCost) {
		this.totalIndirectCost = totalIndirectCost;
	}

	public Boolean getIsFinalBudget() {
		return isFinalBudget;
	}

	public void setIsFinalBudget(Boolean isFinalBudget) {
		this.isFinalBudget = isFinalBudget;
	}

	public BigDecimal getCostSharingAmount() {
		return costSharingAmount;
	}

	public void setCostSharingAmount(BigDecimal costSharingAmount) {
		this.costSharingAmount = costSharingAmount;
	}

	public BigDecimal getUnderrecoveryAmount() {
		return underrecoveryAmount;
	}

	public void setUnderrecoveryAmount(BigDecimal underrecoveryAmount) {
		this.underrecoveryAmount = underrecoveryAmount;
	}

	public Boolean getIsSelected() {
		return isSelected;
	}

	public void setIsSelected(Boolean isSelected) {
		this.isSelected = isSelected;
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

	public Integer getVersionNumber() {
		return versionNumber;
	}

	public void setVersionNumber(Integer versionNumber) {
		this.versionNumber = versionNumber;
	}

	public Boolean getIsApprovedBudget() {
		return isApprovedBudget;
	}

	public void setIsApprovedBudget(Boolean isApprovedBudget) {
		this.isApprovedBudget = isApprovedBudget;
	}

	public BigDecimal getTotalInKind() {
		return totalInKind;
	}

	public void setTotalInKind(BigDecimal totalInKind) {
		this.totalInKind = totalInKind;
	}

	public BigDecimal getTotalOfTotalCost() {
		return totalOfTotalCost;
	}

	public void setTotalOfTotalCost(BigDecimal totalOfTotalCost) {
		this.totalOfTotalCost = totalOfTotalCost;
	}
	
}
