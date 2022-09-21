package com.polus.fibicomp.budget.dao;

import java.sql.Timestamp;
import java.util.List;
import java.util.Set;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.adminportal.pojo.InstituteRate;
import com.polus.fibicomp.adminportal.pojo.RateType;
import com.polus.fibicomp.budget.pojo.AppointmentType;
import com.polus.fibicomp.budget.pojo.AwardBudgetDetail;
import com.polus.fibicomp.budget.pojo.AwardBudgetPeriod;
import com.polus.fibicomp.budget.pojo.BudgetCategory;
import com.polus.fibicomp.budget.pojo.BudgetDetail;
import com.polus.fibicomp.budget.pojo.BudgetDetailCalcAmount;
import com.polus.fibicomp.budget.pojo.BudgetHeader;
import com.polus.fibicomp.budget.pojo.BudgetPeriod;
import com.polus.fibicomp.budget.pojo.BudgetPerson;
import com.polus.fibicomp.budget.pojo.BudgetPersonalDetails;
import com.polus.fibicomp.budget.pojo.BudgetStatus;
import com.polus.fibicomp.budget.pojo.BudgetTemplate;
import com.polus.fibicomp.budget.pojo.BudgetTemplateType;
import com.polus.fibicomp.budget.pojo.CostElement;
import com.polus.fibicomp.budget.pojo.FibiProposalRate;
import com.polus.fibicomp.budget.pojo.JobCode;
import com.polus.fibicomp.budget.pojo.TbnPerson;
import com.polus.fibicomp.proposal.pojo.CostSharingType;
import com.polus.fibicomp.budget.pojo.FundDisbursementBasisType;

@Transactional
@Service
public interface BudgetDao {

	/**
	 * Used to fetch the Institute rates with the budget start date & end date
	 * @param startDate
	 * @param endDate
	 * @param activityTypeCode
	 * @param campusFlag
	 * @return InstituteRate list.
	 */
	public List<InstituteRate> filterInstituteRateByDateRange(Timestamp startDate, Timestamp endDate,
			String activityTypeCode, String campusFlag);
	/**
	 * Used to fetch all cost elements in budget
	 * @return CostElement list
	 */
	public List<CostElement> getAllCostElements();
	/**
	 * Used to fetch overhead rate type parameters
	 * @param rateClassCode
	 * @param rateTypeCode
	 * @return RateType object.
	 */
	public RateType getOHRateTypeByParams(String rateClassCode, String rateTypeCode);
	/**
	 * Used to fetch fibi proposal rates.
	 * @param budgetId
	 * @param budgetStartDate
	 * @param rateClassCode
	 * @param rateTypeCode
	 * @param activityTypeCode
	 * @return FibiProposalRate object.
	 */
	public FibiProposalRate fetchApplicableProposalRate(Integer budgetId, Timestamp budgetStartDate,
			String rateClassCode, String rateTypeCode, String activityTypeCode);
	/**
	 * Used to fetch BudgetHeader by budget id.
	 * @param budgetId
	 * @return BudgetHeader object.
	 */
	public BudgetHeader fetchBudgetByBudgetId(Integer budgetId);
	/**
	 * Used to save or update budget header.
	 * @param budgetHeader
	 * @return 
	 */
	public void saveOrUpdateBudget(BudgetHeader budgetHeader);
	/**
	 * Used to fetch Institute Rate by date less than maximum.
	 * @param startDate
	 * @param activityTypeCode
	 * @param rateClassCode
	 * @param rateTypeCode
	 * @return InstituteRate object
	 */
	public InstituteRate fetchInstituteRateByDateLessthanMax(Timestamp startDate, String activityTypeCode,
			String rateClassCode, String rateTypeCode);
	/**
	 * Used to fetch cost element by id.
	 * @param costElement
	 * @return CostElement object.
	 */
	public CostElement fetchCostElementsById(String costElement);
	/**
	 * Used to fetch maximum budget period by budget id.
	 * @param budgetId
	 * @return BudgetPeriod object
	 */
	public BudgetPeriod getMaxBudgetPeriodByBudgetId(Integer budgetId);
	/**
	 * Used to save budget period.
	 * @param budgetPeriod
	 * @return BudgetPeriod object
	 */
	public BudgetPeriod saveBudgetPeriod(BudgetPeriod budgetPeriod);
	/**
	 * Used to fetch budget period by id.
	 * @param periodId
	 * @return BudgetPeriod object
	 */
	public BudgetPeriod getPeriodById(Integer periodId);
	/**
	 * Used to save budget detail.
	 * @param budgetDetail
	 * @return BudgetDetail object
	 */
	public BudgetDetail saveBudgetDetail(BudgetDetail budgetDetail);
	/**
	 * Used to delete budget period.
	 * @param budgetPeriod
	 * @return BudgetPeriod object
	 */
	public BudgetPeriod deleteBudgetPeriod(BudgetPeriod budgetPeriod);
	/**
	 * Used to delete budget detail.
	 * @param budgetDetail
	 * @return BudgetDetail object
	 */
	public BudgetDetail deleteBudgetDetail(BudgetDetail budgetDetail);
	/**
	 * Used to fetch all tbn persons.
	 * @param 
	 * @return TbnPerson list
	 */
	public List<TbnPerson> fetchAllTbnPerson();
	/**
	 * Used to save budget header.
	 * @param budgetHeader
	 * @return BudgetHeader object
	 */
	public BudgetHeader saveBudgetHeader(BudgetHeader budgetHeader);
	/**
	 * Used to delete Budget detail calculation amount 
	 * @param budgetDetailCalcAmount
	 * @return BudgetDetailCalcAmount object
	 */
	public BudgetDetailCalcAmount deleteBudgetDetailCalcAmount(BudgetDetailCalcAmount budgetDetailCalcAmount);
	/**
	 * Used to delete budget person detail 
	 * @param budgetPersonDetailId
	 * @return 
	 */
	public void deleteBudgetPersonDetail(Integer budgetPersonDetailId);
	/**
	 * Used to fetch all budget status
	 * @param 
	 * @return Budget status list.
	 */
	public List<BudgetStatus> fetchAllBudgetStatus();
	/**
	 * Used to fetch budget status by status code. 
	 * @param budgetStatusCode
	 * @return Budget status object
	 */
	public BudgetStatus getBudgetStatusById(String budgetStatusCode);
	/**
	 * Used to fetch budget person list.
	 * @param budgetId
	 * @return BudgetPerson list
	 */
	public List<BudgetPerson> getBudgetPersons(Integer budgetId);
	/**
	 * Used to save or update budget person. 
	 * @param budgetPerson
	 * @return BudgetPerson object
	 */
	public BudgetPerson saveOrUpdateProposalBudgetPerson(BudgetPerson budgetPerson);
	/**
	 * Used to delete budget person. 
	 * @param budgetPersonId
	 * @return
	 */
	public void deleteBudgetPerson(Integer budgetPersonId);
	/**
	 * Used to fetch Rate types by params.
	 * @param rateTypeCode
	 * @param rateClassCodes
	 * @return RateType list
	 */
	public List<RateType> fetchRateTypeByParams(String rateTypeCode, List<String> rateClassCodes);
	/**
	 * Used to fetch rate class code  by rate type
	 * @param budgetPeriodId
	 * @return rateclass code list
	 */
	public List<String> fetchRateClassCodesByType(String rateClassType);
	/**
	 * Used to fetch budget category name by id.
	 * @param budgetCategoryId
	 * @return BudgetCategoryName
	 */
	public String fetchBudgetCategoryName(String budgetCategoryId);
	/**
	 * USed to fetch budget headers based on proposal id.
	 * @param proposalId
	 * @return Budget header list
	 */
	public List<BudgetHeader> fetchBudgetsByProposalId(Integer proposalId);
	/**
	 * Used to fetch maximum budget version in proposal
	 * @param proposalId
	 * @return integer
	 */
	public Integer maxBudgetVersionNumberByProposalId(Integer proposalId);
	/**
	 * Used to fetch budget period based on period id.
	 * @param budgetPeriodId
	 * @return Budget Period object
	 */
	public BudgetPeriod fetchBudgetPeriodBasedOnPeriodId(Integer budgetPeriodId);
	/**
	 * USed to fetch the budget detail based on budget detail id.
	 * @param budgetDetailId
	 * @return Budget Detail object
	 */
	public BudgetDetail fetchBudgetDetailBasedOnBudgetDetailId(Integer budgetDetailId);
	/**
	 * Used to delete the budget header
	 * @param budgetHeader
	 * @return BudgetHeader object
	 */
	public BudgetHeader deleteBudgetHeader(BudgetHeader budgetHeader);
	/**
	 * Used to fetch the budget of version number maximum.
	 * @param proposalId
	 * @return Budgetheader object
	 */
	public BudgetHeader getMaxBudgetVersionOfBudget(Integer proposalId);
	/**
	 * Used to fetch the maximum line item number
	 * @param budgetId
	 * @return Integer
	 */
	public Integer maxBudgetLineItemNumberByBudgetHeader(Integer budgetId);
	/**
	 * Used to fetch budget detail based on line item number
	 * @param lineItemNumber
	 * @return Budgetdetail list
	 */
	public List<BudgetDetail> fetchBudgetDetailBasedOnLineItemNumber(Integer lineItemNumber);
	/**
	 * Used to delete  all budget details
	 * @param budgetDetails
	 * @return BudgetDetail list
	 */
	public List<BudgetDetail> deleteAllBudgetDetail(List<BudgetDetail> budgetDetails);
	/**
	 * Used to fetch active budget for a proposal 
	 * @param proposalId
	 * @return BudgetHeader object
	 */
	public BudgetHeader fetchActiveBudgetByProposalId(Integer proposalId);
	/**
	 * Used to fetch all appointment types
	 * @param 
	 * @return appointment type list
	 */
	public List<AppointmentType> loadAllAppointmentTypes();
	/**
	 * Used to fetch all job codes
	 * @param 
	 * @return Jobcode list
	 */
	public List<JobCode> loadAllJobCodes();
	/**
	 * Used to check whether person is in budget or not
	 * @param budgetPersonId
	 * @return true/false
	 */
	public Boolean checkBudgetPersonAddedInBudget(Integer budgetPersonId);
	/**
	 * Used to fetch the rate class codes
	 * @param rateClasses
	 * @return Rate class code list
	 */
	public List<String> fetchRateClassCodesNotInParams(Set<String> rateClasses);
	/**
	 * Used to fetch the rate type codes based on rate class codes
	 * @param rateClassCode
	 * @return RateType list
	 */
	public List<String> fetchRateTypeCodesByRateClassCode(String rateClassCode);
	/**
	 * Used to fetch the cost element name based on cost element id
	 * @param costElementId
	 * @return cost element name
	 */
	public String fetchCostElementName(String costElementId);
	/**
	 * Used to save budget detail calculation amount details.
	 * @param budgetDetailCalcAmount
	 * @return 
	 */
	public void saveOrUpdateBudgetCalAmount(BudgetDetailCalcAmount budgetDetailCalcAmount);
	/**
	 * Used to fetch budget person details list.
	 * @param budgetPersonId
	 * @return BudgetPersonDetails list
	 */
	public List<BudgetPersonalDetails> getBudgetPersonDetailsList(Integer budgetPersonId);
	/**
	 * Used to fetch Budget category based on code.
	 * @param budgetCategoryCode
	 * @return BudgetCategory object
	 */
	public BudgetCategory fetchBudgetCategoryBasedOnCode(String budgetCategoryCode);
	/**
	 * used to fetch the budget person details by budget person id
	 * @param budgetPersonId
	 * @return BudgetPerson object
	 */
	public BudgetPerson getBudgetPersonByPersonId(Integer budgetPersonId);
	/**
	 * Used to fetch the award budget periods.
	 * @param awardbudgetId
	 * @return AwardBudgetPeriod list
	 */
	public List<AwardBudgetPeriod> getAwardBudgetPeriodsByBudgetId(Integer awardbudgetId);
	/**
	 * Used to fetch the award budget detail by period
	 * @param budgetPeriodId
	 * @return AwardBudget detail list
	 */
	public List<AwardBudgetDetail> fetchAwardBudgetDetailByPeriodId(Integer budgetPeriodId);

	/**
	 * This method is used to fetch finalBudgetDetails
	 * @param proposalId
	 * @return budgetheader object
	 */
	public BudgetHeader fetchFinalBudget(Integer proposalId);

	/**
	 * Used to fetch the cost element list based on budge category code
	 * @param categoryCode
	 * @return list of cost elements
	 */
	public List<CostElement> fetchCostElementNotInBudgetCategoryCode(String categoryCode);

	/**
	 * Used to fetch the budget personal details by budgetpersondetailId
	 * @param budgetPersonDetailId
	 * @return BudgetPersonalDetails object
	 */
	public BudgetPersonalDetails getBudgetPersonalDetailsById(Integer budgetPersonDetailId);

	/**
	 * @param budgetId
	 * @param tbnId
	 * @param jobCodeType
	 * @param personType
	 * @param personId
	 * @param rolodexId
	 * @return true/false
	 */
	public boolean checkBudgetPersonInBudget(Integer budgetId, String tbnId, String jobCodeType, String personType, String personId, Integer rolodexId);

	/**
	 * Used to fetch the budget person using tbnId.
	 * @param budgetId
	 * @param personType
	 * @param appointmentTypeCode 
	 * @param jobCodeType 
	 * @return budget person object
	 */
	public BudgetPerson getBugetTbnPersonByTbnId(Integer budgetId, String personType, String tbnId, String appointmentTypeCode, String jobCodeType);

	/**
	 * Used to fetch the budget person using person id.
	 * @param budgetId
	 * @param personType
	 * @param jobCodeType 
	 * @param appointmentTypeCode 
	 * @param tbnId
	 * @return Budget Person
	 */
	public BudgetPerson getBugetPersonByPersonId(Integer budgetId, String personType, String personId, String appointmentTypeCode, String jobCodeType);

	/**
	 * Used to fetch the rolodex person in Budget
	 * @param budgetId
	 * @param personType
	 * @param rolodexId
	 * @param appointmentTypeCode 
	 * @param jobCodeType 
	 * @return Budget Person
	 */
	public BudgetPerson getBugetRolodexPersonByRolodexId(Integer budgetId, String personType, Integer rolodexId, String appointmentTypeCode, String jobCodeType);

	/**
	 * This method is used to get all the budget template types by module code
	 * @param moduleCode
	 * @return list of budget template type
	 */
	public List<BudgetTemplateType> getBudgetTemplateTypesByModuleCode(Integer moduleCode);

	/**
	 * This method is used to fetch the budget template based on budget template Id
	 * @param budgetTemplateTypeId
	 * @return list of budget templates
	 */
	public List<BudgetTemplate> fetchBudgetTemplatesByTemplateType(Integer budgetTemplateTypeId);

	/**
	 * @param proposalId
	 * @param updateUser
	 * @param currentTimestamp
	 */
	public void updateProposalBudgetStatus(Integer proposalId, String budgetStatusCode, String updateUser);

	/**
	 * @param budgetTemplateTypeId
	 */
	public BudgetTemplateType fetchBudgetTemplateTypeById(Integer budgetTemplateTypeId);

	public BudgetHeader fetchLinkedBudgetsByProposalNumber(String proposalNumber);
	
	/**
	 * @param proposalId
	 * @param budgetVersionNumber
	 */
	public void updateBudgetLatestVersionFlag(Integer proposalId, Integer budgetVersionNumber);

	/**
	 * Used to fetch all cost sharing type
	 * @param 
	 * @return cost sharing type
	 */
	public List<CostSharingType> getCostSharingType();

	/**
	 * Used to fetch all Fund disbursement basis type
	 * @param
	 * @return fund disbursement basis type
	 */
	public List<FundDisbursementBasisType> getFundDisbursementBasisType();

}
