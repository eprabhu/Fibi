package com.polus.fibicomp.budget.service;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.budget.vo.BudgetModularVO;

@Transactional
@Service
public interface BudgetModularService {

	/**
	 * @param budgetId
	 * @return
	 */
	public String proposalModularBudget(Integer budgetId);
	
	/**
	 * @param budgetModularIDCId
	 * @return
	 */
	public String deleteModularBudgetInDirectLine(Integer budgetModularIDCId);
	
	/**
	 * @param budgetModularVO
	 * @return
	 */
	public String saveProposalModularBudget(BudgetModularVO budgetModularVO);

	/**
	 * @param budgetId
	 * @return
	 */
	public BudgetModularVO prepareModularBudget(Integer budgetId);	
	
}
