package com.polus.fibicomp.proposal.lookup.dao;

import java.util.List;
import java.util.Set;

import com.polus.fibicomp.proposal.pojo.*;
import org.springframework.stereotype.Service;

import com.polus.fibicomp.budget.pojo.BudgetCategory;
import com.polus.fibicomp.budget.pojo.CostElement;
import com.polus.fibicomp.grantcall.pojo.GrantCall;
import com.polus.fibicomp.pojo.ActivityType;
import com.polus.fibicomp.pojo.ProposalPersonRole;
import com.polus.fibicomp.pojo.ScienceKeyword;
import com.polus.fibicomp.pojo.Unit;
import com.polus.fibicomp.roles.pojo.Role;
import com.polus.fibicomp.vo.SponsorSearchResult;

@Service
public interface ProposalLookUpDao {

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
	 * @param personId 
	 * @param isExternalUser
	 * @return a list of grant calls.
	 */
	public List<GrantCall> getGrantCallsBasedOnSearchString(String searchString, Integer moduleCode, Boolean includeClosedGrantCall, Boolean isExternalUser);

	/**
	 * This method is used to get department details based on search string.
	 * @param searchString - input string.
	 * @return a list of departments.
	 */
	public List<Unit> getDepartmentList(String searchString);

	/**
	 * This method is used to get cost elements based on search string.
	 * @param searchString - input string.
	 * @param budgetCategoryCodes - budget category code.
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
	 * This method is used to get departments based on search string.
	 * @param unitNumbers - set of unit numbers.
	 * @param searchString - input string.
	 * @return a list of departments.
	 */
	public List<Unit> fetchLeadUnitsByUnitNumbers(Set<String> unitNumbers, String searchString);

	/**
	 * This method is used to get budget categories based on search string.
	 * @param searchString - input string.
	 * @return a list of budget categories.
	 */
	public List<BudgetCategory> findBudgetCategoryList(String searchString);

	/**
	 * This method is used to fetch all proposal attachment types.
	 * @return A list of proposal attachment types.
	 */
	public List<ProposalAttachmentType> fetchAllProposalAttachmentTypes();

	/**
	 * This method is used to fetch all proposal person roles.
	 * @return A list of roles of proposal person.
	 */
	public List<ProposalPersonRole> fetchAllProposalPersonRoles();

	/**
	 * This method is used to fetch status based on status code.
	 * @param statusCode - status code of the proposal.
	 * @return An object of proposal status.
	 */
	public ProposalStatus fetchProposalStatusByStatusCode(Integer statusCode);

	/**
	 * This method is used to fetch all proposal types.
	 * @return A list of proposal types.
	 */
	public List<ProposalType> fetchAllProposalTypes();

	/**
	 * This method is used to fetch proposal person roles based on proposalId.
	 * @param proposalId - Id of the proposal.
	 * @param roleId - Id of the Role.
	 * @return Proposal person roles list.
	 */
	public List<ProposalPersonRoles> fetchProposalPersonRoles(Integer proposalId, Integer roleId);

	/**
	 * This method is used to fetch proposal roles.
	 * @return list of roles.
	 */
	public List<Role> fetchProposalRoles();

	/**
	 * This method is used to fetch all DisciplineCluster.
	 * @return List of DisciplineCluster
	 */
	public List<DisciplineCluster> fetchAllDisciplineCluster();

	/**
	 * This method is used to fetch all activity types based on activity type code .
	 * @param grantTypeCode
	 * @return
	 */
	public List<ActivityType> getAllActivityForGrantType();

	/**
	 * This method is used to fetch Proposal Funding Status.
	 * @return List of ProposalFundingStatus
	 */
	public List<ProposalFundingStatus> fetchAllProposalFundingStatus();

	/**
	 * This method is used to get the proposal status of given status codes
	 * @param statusCodes
	 * @return list of proposal statuses
	 */
	public List<ProposalStatus> fetAllProposalStatus(List<Integer> statusCodes);

	/**
	 * This method is used to get the proposal organization type
	 * @return List<OrganizationType>
	 */
	public List<OrganizationType> loadOrganizationType();

	/**
	 * This method used to fetch all key personnel attachment type
	 * @return
	 */
	List<ProposalKeyPersonnelAttachmentType> fetchAllKeyPersonalProposalAttachmentTypes();
    
	/**
	 * This method used to getAllOrganizationType
	 * @return
	 */
	List<OrganizationType> loadAllOrganizationType();
}
