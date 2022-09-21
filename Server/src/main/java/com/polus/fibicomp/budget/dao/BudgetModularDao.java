package com.polus.fibicomp.budget.dao;

import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.budget.pojo.BudgetModular;
import com.polus.fibicomp.budget.pojo.BudgetModularIDC;

@Transactional
@Service
public interface BudgetModularDao {

	public List<BudgetModular> fetchBudgetModular(Integer budgetId);
	
	public BudgetModular insertBudgetModular(BudgetModular budgetModular);
	
	public BudgetModularIDC insertBudgetModularIDC(BudgetModularIDC budgetModularIDC);
	
	public BudgetModular saveBudgetModular(BudgetModular budgetModular);
	
	public void deleteBudgetModularIDCLine(Integer budgetModularIDCId);
	
}
