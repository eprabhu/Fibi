package com.polus.fibicomp.proposal.lookup.service;

import java.util.HashMap;
import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.budget.pojo.BudgetCategory;
import com.polus.fibicomp.budget.pojo.CostElement;
import com.polus.fibicomp.grantcall.pojo.GrantCall;
import com.polus.fibicomp.pojo.ScienceKeyword;
import com.polus.fibicomp.pojo.Unit;
import com.polus.fibicomp.vo.SponsorSearchResult;

@Transactional
@Service(value = "proposalLookUpService")
public interface ProposalLookUpService {

	/**
	 * This method is used to fetch filtered sponsors based on input string.
	 * @param searchString - input string.
	 * @return a list of sponsors.
	 */
	public List<SponsorSearchResult> findSponsor(String searchString);

	/**
	 * This method is used to get grant call details based on search string.
	 * @param searchString - input string.
	 * @param moduleCode 
	 * @param includeClosedGrantCall 
	 * @return a list of grant calls.
	 */
	public List<GrantCall> getGrantCallsBasedOnSearchString(String searchString, Integer moduleCode, Boolean includeClosedGrantCall);

	/**
	 * This method is used to get department details based on search string.
	 * @param searchString - input string.
	 * @param list 
	 * @param check 
	 * @param personId 
	 * @return a list of departments.
	 */
	public List<Unit> getUnitNumberForDepartment(String searchString, String check, List<String> rightName, String personId);

	/**
	 * This method is used to get cost elements based on search string.
	 * @param searchString - input string.
	 * @param budgetCategoryCodes - budget category Code.
	 * @return a list of cost elements.
	 */
	public List<CostElement> findCostElementByParams(String searchString, List<String> budgetCategoryCodes);

	/**
	 * This method is used to get key words based on search string.
	 * @param searchString - input string.
	 * @return a list of key words.
	 */
	public List<ScienceKeyword> findKeyWordsList(String searchString);

	/**
	 * This method is used to get lead units based on search string.
	 * @param searchString - input string.
	 * @return a list of units.
	 */
	public List<Unit> findLeadUnitsList(String searchString);

	/**
	 * This method is used to get budget categories based on search string.
	 * @param searchString - input string.
	 * @return a list of budget categories.
	 */
	public List<BudgetCategory> findBudgetCategoryList(String searchString);

}
