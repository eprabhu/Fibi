package com.polus.fibicomp.budget.vo;

import com.polus.fibicomp.budget.pojo.AwardBudgetDetail;

public class AwardDetailBudgetVO {
	
	private AwardBudgetDetail awardBudgetDetail;

	private String awardId;

	private Integer lineItemCount;

	private Integer budgetPeriodId;

	private String updateUser;

	private Boolean isNonPersonalLineItemEnabled = false;

	private Boolean costElementChanged;

	public AwardBudgetDetail getAwardBudgetDetail() {
		return awardBudgetDetail;
	}

	public void setAwardBudgetDetail(AwardBudgetDetail awardBudgetDetail) {
		this.awardBudgetDetail = awardBudgetDetail;
	}

	public String getAwardId() {
		return awardId;
	}

	public void setAwardId(String awardId) {
		this.awardId = awardId;
	}

	public Integer getLineItemCount() {
		return lineItemCount;
	}

	public void setLineItemCount(Integer lineItemCount) {
		this.lineItemCount = lineItemCount;
	}

	public Integer getBudgetPeriodId() {
		return budgetPeriodId;
	}

	public void setBudgetPeriodId(Integer budgetPeriodId) {
		this.budgetPeriodId = budgetPeriodId;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Boolean getIsNonPersonalLineItemEnabled() {
		return isNonPersonalLineItemEnabled;
	}

	public void setIsNonPersonalLineItemEnabled(Boolean isNonPersonalLineItemEnabled) {
		this.isNonPersonalLineItemEnabled = isNonPersonalLineItemEnabled;
	}

	public Boolean getCostElementChanged() {
		return costElementChanged;
	}

	public void setCostElementChanged(Boolean costElementChanged) {
		this.costElementChanged = costElementChanged;
	}

}
