package com.polus.fibicomp.budget.service;

import com.polus.fibicomp.budget.pojo.BudgetPerson;
import com.polus.fibicomp.budget.vo.AwardBudgetVO;

public interface BudgetPersonService {

	/**
	 * This method is used to fetch all proposal budget person in a particular
	 * budget
	 * 
	 * @param budgetId
	 * @param proposalId
	 * @return
	 */
	public String getBudgetPersons(Integer budgetId, Integer proposalId);

	/**
	 * This method is used to add new proposal budget person
	 * 
	 * @param budgetPerson
	 * @return
	 */
	public String saveOrUpdateProposalBudgetPerson(BudgetPerson budgetPerson);

	/**
	 * This method is used for remove proposal budget person
	 * 
	 * @param budgetPersonId
	 * @param budgetId
	 * @return
	 */
	public String deleteBudgetPerson(Integer budgetPersonId, Integer budgetId);

	/**
	 * This method is used to check the person is already in a budget
	 * 
	 * @param budgetPersonId
	 * @return
	 */
	public Boolean checkBudgetPersonAddedInBudget(Integer budgetPersonId);

	/**
	 * this method is used to save persons involved in award budget.
	 * 
	 * @param vo - AwardBudgetVO object
	 * @return
	 */
	public String saveOrUpdateAwardBudgetPerson(AwardBudgetVO vo);

	/**
	 * This method is used to delete the Award budget persons
	 * 
	 * @param budgetPersonId
	 * @param budgetId
	 * @param awardId
	 * @param userName
	 * @return
	 */
	public String deleteAwardBudgetPerson(Integer budgetPersonId, Integer budgetId, Integer awardId, String userName);

	/**
	 * This method is used for get all award persons
	 * 
	 * @param budgetHeaderId
	 * @param awardId
	 * @return
	 */
	public String getAwardBudgetPersons(Integer budgetHeaderId, String awardId);

}
