package com.polus.fibicomp.proposal.print.dto;

import java.math.BigDecimal;

public class PeriodCost {

	private Integer budgetPeriod;
	private BigDecimal cost;

	public Integer getBudgetPeriod() {
		return budgetPeriod;
	}
	public void setBudgetPeriod(Integer budgetPeriod) {
		this.budgetPeriod = budgetPeriod;
	}
	public BigDecimal getCost() {
		return cost;
	}
	public void setCost(BigDecimal cost) {
		this.cost = cost;
	}

}
