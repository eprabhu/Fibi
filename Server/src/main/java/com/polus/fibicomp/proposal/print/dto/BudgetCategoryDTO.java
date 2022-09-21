package com.polus.fibicomp.proposal.print.dto;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

public class BudgetCategoryDTO {

	private String category;
	private String categoryCode;
	private BigDecimal totalCategoryCost;
	private List<LineItem> lineItemList;

	public BudgetCategoryDTO() {
		lineItemList = new ArrayList<>();
	}

	public String getCategory() {
		return category;
	}
	public void setCategory(String category) {
		this.category = category;
	}
	public List<LineItem> getLineItemList() {
		return lineItemList;
	}
	public void setLineItemList(List<LineItem> lineItemList) {
		this.lineItemList = lineItemList;
	}

	public String getCategoryCode() {
		return categoryCode;
	}

	public void setCategoryCode(String categoryCode) {
		this.categoryCode = categoryCode;
	}

	public BigDecimal getTotalCategoryCost() {
		return totalCategoryCost;
	}

	public void setTotalCategoryCost(BigDecimal totalCategoryCost) {
		this.totalCategoryCost = totalCategoryCost;
	}

}
