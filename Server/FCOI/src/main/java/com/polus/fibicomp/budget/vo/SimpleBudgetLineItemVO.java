package com.polus.fibicomp.budget.vo;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.Collections;
import java.util.List;
import com.polus.fibicomp.budget.pojo.BudgetCategory;
import com.polus.fibicomp.budget.pojo.CostElement;
import com.polus.fibicomp.proposal.comparator.SimpleBudgetDetailComparatorByBudgetPeriod;

public class SimpleBudgetLineItemVO {

	private static final long serialVersionUID = 1L;

	private BigDecimal totalLineItemCost;

	private List<PeriodCost> periodCostsList;

	private BigDecimal quantity;

	private String costElementCode;

	private CostElement costElement;

	private String budgetCategoryCode;

	private BudgetCategory budgetCategory;

	private Integer lineItemNumber;

	private String lineItemDescription;

	private Boolean isSystemGeneratedCostElement = false;

	private String systemGeneratedCEType;

	private Boolean isApplyInflationRate = true;

	private Boolean applyInRateFlag = false;

	private Boolean submitCostSharingFlag = false;

	private Timestamp updateTimeStamp;

	private Boolean emptyEntry = false;

	public List<PeriodCost> getPeriodCostsList() {
		if (periodCostsList != null && !periodCostsList.isEmpty()) {
		Collections.sort(periodCostsList, new SimpleBudgetDetailComparatorByBudgetPeriod());
	}
		return periodCostsList;
	}

	public void setPeriodCostsList(List<PeriodCost> periodCostsList) {
		this.periodCostsList = periodCostsList;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public BigDecimal getQuantity() {
		return quantity;
	}

	public void setQuantity(BigDecimal quantity) {
		this.quantity = quantity;
	}

	public BigDecimal getTotalLineItemCost() {
		return totalLineItemCost;
	}

	public void setTotalLineItemCost(BigDecimal totalLineItemCost) {
		this.totalLineItemCost = totalLineItemCost;
	}

	public String getCostElementCode() {
		return costElementCode;
	}

	public void setCostElementCode(String costElementCode) {
		this.costElementCode = costElementCode;
	}

	public CostElement getCostElement() {
		return costElement;
	}

	public void setCostElement(CostElement costElement) {
		this.costElement = costElement;
	}

	public BudgetCategory getBudgetCategory() {
		return budgetCategory;
	}

	public void setBudgetCategory(BudgetCategory budgetCategory) {
		this.budgetCategory = budgetCategory;
	}

	public Integer getLineItemNumber() {
		return lineItemNumber;
	}

	public void setLineItemNumber(Integer lineItemNumber) {
		this.lineItemNumber = lineItemNumber;
	}

	public String getLineItemDescription() {
		return lineItemDescription;
	}

	public void setLineItemDescription(String lineItemDescription) {
		this.lineItemDescription = lineItemDescription;
	}

	public Boolean getIsSystemGeneratedCostElement() {
		return isSystemGeneratedCostElement;
	}

	public void setIsSystemGeneratedCostElement(Boolean isSystemGeneratedCostElement) {
		this.isSystemGeneratedCostElement = isSystemGeneratedCostElement;
	}

	public String getSystemGeneratedCEType() {
		return systemGeneratedCEType;
	}

	public void setSystemGeneratedCEType(String systemGeneratedCEType) {
		this.systemGeneratedCEType = systemGeneratedCEType;
	}

	public Boolean getIsApplyInflationRate() {
		return isApplyInflationRate;
	}

	public void setIsApplyInflationRate(Boolean isApplyInflationRate) {
		this.isApplyInflationRate = isApplyInflationRate;
	}

	public Boolean getApplyInRateFlag() {
		return applyInRateFlag;
	}

	public void setApplyInRateFlag(Boolean applyInRateFlag) {
		this.applyInRateFlag = applyInRateFlag;
	}

	public Boolean getSubmitCostSharingFlag() {
		return submitCostSharingFlag;
	}

	public void setSubmitCostSharingFlag(Boolean submitCostSharingFlag) {
		this.submitCostSharingFlag = submitCostSharingFlag;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}

	public String getBudgetCategoryCode() {
		return budgetCategoryCode;
	}

	public void setBudgetCategoryCode(String budgetCategoryCode) {
		this.budgetCategoryCode = budgetCategoryCode;
	}

	public Boolean getEmptyEntry() {
		return emptyEntry;
	}

	public void setEmptyEntry(Boolean emptyEntry) {
		this.emptyEntry = emptyEntry;
	}

}
