package com.polus.fibicomp.budget.vo;

import java.math.BigDecimal;
import java.util.List;

import com.polus.fibicomp.budget.pojo.BudgetModular;

public class BudgetModularVO {

	private Integer budgetId;
	
	private Integer budgetModularIDCId;
	
	private List<BudgetModular> modularBudget;
	
	private BigDecimal totalDirectCostLessConsorFnaforAllPeriod;
	
	private BigDecimal totalConsortiumFnaforAllPeriod;
	
	private BigDecimal totalDirectCostforAllPeriod;
	
	private BigDecimal totalIndirectDirectCostforAllPeriod;
	
	private BigDecimal totalDirectAndInDirectCostforAllPeriod;

	public Integer getBudgetId() {
		return budgetId;
	}

	public void setBudgetId(Integer budgetId) {
		this.budgetId = budgetId;
	}	
	
	public Integer getBudgetModularIDCId() {
		return budgetModularIDCId;
	}

	public void setBudgetModularIDCId(Integer budgetModularIDCId) {
		this.budgetModularIDCId = budgetModularIDCId;
	}

	public List<BudgetModular> getModularBudget() {
		return modularBudget;
	}

	public void setModularBudget(List<BudgetModular> modularBudget) {
		this.modularBudget = modularBudget;
	}

	public BigDecimal getTotalDirectCostLessConsorFnaforAllPeriod() {
		return totalDirectCostLessConsorFnaforAllPeriod;
	}

	public void setTotalDirectCostLessConsorFnaforAllPeriod(BigDecimal totalDirectCostLessConsorFnaforAllPeriod) {
		this.totalDirectCostLessConsorFnaforAllPeriod = totalDirectCostLessConsorFnaforAllPeriod;
	}

	public BigDecimal getTotalConsortiumFnaforAllPeriod() {
		return totalConsortiumFnaforAllPeriod;
	}

	public void setTotalConsortiumFnaforAllPeriod(BigDecimal totalConsortiumFnaforAllPeriod) {
		this.totalConsortiumFnaforAllPeriod = totalConsortiumFnaforAllPeriod;
	}

	public BigDecimal getTotalDirectCostforAllPeriod() {
		return totalDirectCostforAllPeriod;
	}

	public void setTotalDirectCostforAllPeriod(BigDecimal totalDirectCostforAllPeriod) {
		this.totalDirectCostforAllPeriod = totalDirectCostforAllPeriod;
	}

	public BigDecimal getTotalIndirectDirectCostforAllPeriod() {
		return totalIndirectDirectCostforAllPeriod;
	}

	public void setTotalIndirectDirectCostforAllPeriod(BigDecimal totalIndirectDirectCostforAllPeriod) {
		this.totalIndirectDirectCostforAllPeriod = totalIndirectDirectCostforAllPeriod;
	}

	public BigDecimal getTotalDirectAndInDirectCostforAllPeriod() {
		return totalDirectAndInDirectCostforAllPeriod;
	}

	public void setTotalDirectAndInDirectCostforAllPeriod(BigDecimal totalDirectAndInDirectCostforAllPeriod) {
		this.totalDirectAndInDirectCostforAllPeriod = totalDirectAndInDirectCostforAllPeriod;
	}
	
	
	
}
