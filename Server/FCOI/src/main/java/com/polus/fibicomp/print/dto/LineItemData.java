package com.polus.fibicomp.print.dto;

import java.util.ArrayList;
import java.util.List;

public class LineItemData {

	private String costElement;

	private String lineItemDescription;

	private String quantity;

	private String lineItemCost;

	private String costSharingPercentage;

	private String costSharingAmount;

	private String sponsorRequestedAmount;

	private String budgetCategory;

	private List<BudgetPersonalDetail> budgetPersonalDetails;

	private Boolean hasBudgetPerson;

	private String internalOrderCode;

	private String balanceToDate;

	private String previousLineItemCost;

	private Boolean isBudgetCategoryNotSystemGenerated;

	private List<BudgetNonPersonDetail> nonPersonsDetails;

	private Boolean hasNonBudgetPerson;

	public LineItemData() {
		super();
		budgetPersonalDetails = new ArrayList<>();
		nonPersonsDetails = new ArrayList<>();
	}

	public String getCostElement() {
		return costElement;
	}

	public void setCostElement(String costElement) {
		this.costElement = costElement;
	}

	public String getLineItemDescription() {
		return lineItemDescription;
	}

	public void setLineItemDescription(String lineItemDescription) {
		this.lineItemDescription = lineItemDescription;
	}

	public String getQuantity() {
		return quantity;
	}

	public void setQuantity(String quantity) {
		this.quantity = quantity;
	}

	public String getLineItemCost() {
		return lineItemCost;
	}

	public void setLineItemCost(String lineItemCost) {
		this.lineItemCost = lineItemCost;
	}	

	public String getCostSharingPercentage() {
		return costSharingPercentage;
	}

	public void setCostSharingPercentage(String costSharingPercentage) {
		this.costSharingPercentage = costSharingPercentage;
	}

	public String getCostSharingAmount() {
		return costSharingAmount;
	}

	public void setCostSharingAmount(String costSharingAmount) {
		this.costSharingAmount = costSharingAmount;
	}

	public String getSponsorRequestedAmount() {
		return sponsorRequestedAmount;
	}

	public void setSponsorRequestedAmount(String sponsorRequestedAmount) {
		this.sponsorRequestedAmount = sponsorRequestedAmount;
	}

	public List<BudgetPersonalDetail> getBudgetPersonalDetails() {
		return budgetPersonalDetails;
	}

	public void setBudgetPersonalDetails(List<BudgetPersonalDetail> budgetPersonalDetails) {
		this.budgetPersonalDetails = budgetPersonalDetails;
	}

	public String getBudgetCategory() {
		return budgetCategory;
	}

	public void setBudgetCategory(String budgetCategory) {
		this.budgetCategory = budgetCategory;
	}

	public Boolean getHasBudgetPerson() {
		return hasBudgetPerson;
	}

	public void setHasBudgetPerson(Boolean hasBudgetPerson) {
		this.hasBudgetPerson = hasBudgetPerson;
	}

	public String getInternalOrderCode() {
		return internalOrderCode;
	}

	public void setInternalOrderCode(String internalOrderCode) {
		this.internalOrderCode = internalOrderCode;
	}

	public String getBalanceToDate() {
		return balanceToDate;
	}

	public void setBalanceToDate(String balanceToDate) {
		this.balanceToDate = balanceToDate;
	}

	public String getPreviousLineItemCost() {
		return previousLineItemCost;
	}

	public void setPreviousLineItemCost(String previousLineItemCost) {
		this.previousLineItemCost = previousLineItemCost;
	}

	public Boolean getIsBudgetCategoryNotSysBooleantemGenerated() {
		return isBudgetCategoryNotSystemGenerated;
	}

	public void setIsBudgetCategoryNotSystemGenerated(Boolean isBudgetCategoryNotSystemGenerated) {
		this.isBudgetCategoryNotSystemGenerated = isBudgetCategoryNotSystemGenerated;
	}

	public List<BudgetNonPersonDetail> getNonPersonsDetails() {
		return nonPersonsDetails;
	}

	public void setNonPersonsDetails(List<BudgetNonPersonDetail> nonPersonsDetails) {
		this.nonPersonsDetails = nonPersonsDetails;
	}

	public Boolean getHasNonBudgetPerson() {
		return hasNonBudgetPerson;
	}

	public void setHasNonBudgetPerson(Boolean hasNonBudgetPerson) {
		this.hasNonBudgetPerson = hasNonBudgetPerson;
	}
}
