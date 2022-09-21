package com.polus.fibicomp.budget.vo;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

public class SimpleBudgetVO {

	private List<SimpleBudgetLineItemVO> lineItemList;

	private String categoryCode;

	private String categoryName;

	private BigDecimal totalCategoryCost;

	private Integer budgetId;

	private String updateUser;

	private Boolean isSystemGeneratedCostElement = false;

	private Integer sortOrder;

	public SimpleBudgetVO() {
		lineItemList = new ArrayList<>();
	}

	public List<SimpleBudgetLineItemVO> getLineItemList() {
		return lineItemList;
	}

	public void setLineItemList(List<SimpleBudgetLineItemVO> lineItemList) {
		this.lineItemList = lineItemList;
	}

	public String getCategoryCode() {
		return categoryCode;
	}

	public void setCategoryCode(String categoryCode) {
		this.categoryCode = categoryCode;
	}

	public String getCategoryName() {
		return categoryName;
	}

	public void setCategoryName(String categoryName) {
		this.categoryName = categoryName;
	}

	public BigDecimal getTotalCategoryCost() {
		return totalCategoryCost;
	}

	public void setTotalCategoryCost(BigDecimal totalCategoryCost) {
		this.totalCategoryCost = totalCategoryCost;
	}

	public Integer getBudgetId() {
		return budgetId;
	}

	public void setBudgetId(Integer budgetId) {
		this.budgetId = budgetId;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Boolean getIsSystemGeneratedCostElement() {
		return isSystemGeneratedCostElement;
	}

	public void setIsSystemGeneratedCostElement(Boolean isSystemGeneratedCostElement) {
		this.isSystemGeneratedCostElement = isSystemGeneratedCostElement;
	}

	public Integer getSortOrder() {
		return sortOrder;
	}

	public void setSortOrder(Integer sortOrder) {
		this.sortOrder = sortOrder;
	}

}
