package com.polus.fibicomp.award.revenue.vo;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.List;

import com.polus.fibicomp.award.revenue.pojo.AwardRevenueTransactions;

public class AwardRevenueVO {

	private List<AwardRevenueTransactions> awardRevenueTransactions;
	
	private String budgetCategory; 

	private String budgetCategoryCode;

	private String awardNumber;

	private String accountNumber;

	private String internalOrderCode;

	private BigDecimal totalRevenueAmount;

	private String costElement;

	private Timestamp fiPostingStartDate;

	private Timestamp fiPostingEndDate;

	private List<String> internalOrderCodes;

	public List<AwardRevenueTransactions> getAwardRevenueTransactions() {
		return awardRevenueTransactions;
	}

	public void setAwardRevenueTransactions(List<AwardRevenueTransactions> awardRevenueTransactions) {
		this.awardRevenueTransactions = awardRevenueTransactions;
	}

	public String getBudgetCategory() {
		return budgetCategory;
	}

	public void setBudgetCategory(String budgetCategory) {
		this.budgetCategory = budgetCategory;
	}

	public String getBudgetCategoryCode() {
		return budgetCategoryCode;
	}

	public void setBudgetCategoryCode(String budgetCategoryCode) {
		this.budgetCategoryCode = budgetCategoryCode;
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public String getAccountNumber() {
		return accountNumber;
	}

	public void setAccountNumber(String accountNumber) {
		this.accountNumber = accountNumber;
	}

	public String getInternalOrderCode() {
		return internalOrderCode;
	}

	public void setInternalOrderCode(String internalOrderCode) {
		this.internalOrderCode = internalOrderCode;
	}

	public BigDecimal getTotalRevenueAmount() {
		return totalRevenueAmount;
	}

	public void setTotalRevenueAmount(BigDecimal totalRevenueAmount) {
		this.totalRevenueAmount = totalRevenueAmount;
	}

	public String getCostElement() {
		return costElement;
	}

	public void setCostElement(String costElement) {
		this.costElement = costElement;
	}

	public Timestamp getFiPostingStartDate() {
		return fiPostingStartDate;
	}

	public void setFiPostingStartDate(Timestamp fiPostingStartDate) {
		this.fiPostingStartDate = fiPostingStartDate;
	}

	public Timestamp getFiPostingEndDate() {
		return fiPostingEndDate;
	}

	public void setFiPostingEndDate(Timestamp fiPostingEndDate) {
		this.fiPostingEndDate = fiPostingEndDate;
	}

	public List<String> getInternalOrderCodes() {
		return internalOrderCodes;
	}

	public void setInternalOrderCodes(List<String> internalOrderCodes) {
		this.internalOrderCodes = internalOrderCodes;
	}

}
