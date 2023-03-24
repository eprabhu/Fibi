package com.polus.fibicomp.budget.vo;

import java.util.List;

import com.polus.fibicomp.budget.pojo.AwardBudgetPeriod;

public class AwardBudgetPeriodVO {

	private List<AwardBudgetPeriod> period;
	
	private Integer budgetId;

	private Integer budgetPeriodId;

	private String updateUser;

	public List<AwardBudgetPeriod> getPeriod() {
		return period;
	}

	public void setPeriod(List<AwardBudgetPeriod> period) {
		this.period = period;
	}

	public Integer getBudgetId() {
		return budgetId;
	}

	public void setBudgetId(Integer budgetId) {
		this.budgetId = budgetId;
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

}
