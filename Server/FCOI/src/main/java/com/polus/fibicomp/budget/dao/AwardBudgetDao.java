package com.polus.fibicomp.budget.dao;

import java.sql.Timestamp;
import java.util.List;
import java.util.Set;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.adminportal.pojo.InstituteRate;
import com.polus.fibicomp.adminportal.pojo.RateType;
import com.polus.fibicomp.award.vo.AwardBudgetImportInfo;
import com.polus.fibicomp.award.vo.BudgetImportPeriodsInfo;
import com.polus.fibicomp.budget.pojo.AwardBudgetDetail;
import com.polus.fibicomp.budget.pojo.AwardBudgetDetailCalcAmount;
import com.polus.fibicomp.budget.pojo.AwardBudgetFundType;
import com.polus.fibicomp.budget.pojo.AwardBudgetHeader;
import com.polus.fibicomp.budget.pojo.AwardBudgetNonPersonDetail;
import com.polus.fibicomp.budget.pojo.AwardBudgetPeriod;
import com.polus.fibicomp.budget.pojo.AwardBudgetPerson;
import com.polus.fibicomp.budget.pojo.AwardBudgetPersonalDetail;
import com.polus.fibicomp.budget.pojo.AwardBudgetStatus;
import com.polus.fibicomp.budget.pojo.AwardRates;
import com.polus.fibicomp.budget.pojo.BudgetCategory;
import com.polus.fibicomp.budget.pojo.BudgetType;
import com.polus.fibicomp.budget.pojo.CostElement;
import com.polus.fibicomp.budget.pojo.TbnPerson;

@Transactional
@Service
public interface AwardBudgetDao {

	public List<InstituteRate> filterInstituteRateByDateRange(Timestamp startDate, Timestamp endDate,
			String activityTypeCode);

	public List<CostElement> getAllCostElements();

	public RateType getOHRateTypeByParams(String rateClassCode, String rateTypeCode);

	public AwardRates fetchApplicableAwardRate(Integer budgetId, Timestamp budgetStartDate, String rateClassCode,
			String rateTypeCode, String activityTypeCode);

	public AwardBudgetHeader fetchBudgetByBudgetId(Integer budgetId);

	public InstituteRate fetchInstituteRateByDateLessthanMax(Timestamp startDate, String activityTypeCode,
			String rateClassCode, String rateTypeCode);

	public List<CostElement> fetchCostElementsByIds(List<String> costElements);

	public CostElement fetchCostElementsById(String costElement);

	public AwardBudgetPeriod getMaxBudgetPeriodByBudgetId(Integer budgetId);

	public AwardBudgetPeriod saveBudgetPeriod(AwardBudgetPeriod budgetPeriod);

	public List<BudgetCategory> fetchAllBudgetCategory();

	public List<CostElement> fetchCostElementByBudgetCategory(String budgetCategoryCode);

	public AwardBudgetPeriod getPeriodById(Integer periodId);

	public AwardBudgetDetail saveBudgetDetail(AwardBudgetDetail budgetDetail);

	public AwardBudgetPeriod deleteBudgetPeriod(AwardBudgetPeriod budgetPeriod);

	public AwardBudgetDetail deleteBudgetDetail(AwardBudgetDetail budgetDetail);

	public List<TbnPerson> fetchAllTbnPerson();

	public AwardBudgetHeader saveBudgetHeader(AwardBudgetHeader budgetHeader);

	public AwardBudgetDetailCalcAmount deleteBudgetDetailCalcAmount(AwardBudgetDetailCalcAmount budgetDetailCalcAmount);

	public void deleteBudgetPersonDetail(Integer budgetPersonDetailId);

	public BudgetType getBudgetTypeById(String budgetTypeCode);

	public List<AwardBudgetPeriod> getAwardBudgetPeriodsByBudgetId(Integer awardbudgetId);

	public List<AwardBudgetImportInfo> getDevProposalBudgets(Integer awardId);

	public List<BudgetImportPeriodsInfo> getDevProposalBudgetPeriods(Integer awardbudgetId);

	public List<AwardBudgetHeader> getAwardBudgetVersionsByAwardId(Integer awardId);

	public AwardBudgetStatus getAwardBudgetStatusById(String awardBudgetStatusCode);

	public AwardBudgetHeader getAwardBudgetHeaderByAwardId(Integer awardId);

	public Integer maxAwardBudgetVersionNumberByAwardId(Integer awardId);

	/**
	 * This method is used to save or update the award persons.
	 * 
	 * @param awardBudgetPerson
	 * @return
	 */
	public AwardBudgetPerson saveOrUpdateAwardBudgetPerson(AwardBudgetPerson awardBudgetPerson);

	/**
	 * This method is used to fetch all award budget person data as list by the
	 * award budgetId
	 * 
	 * @param budgetHeaderId
	 * @return
	 */
	public List<AwardBudgetPerson> getBudgetPersons(Integer budgetHeaderId);

	/**
	 * this method is for delete Award budget based on the person id
	 * 
	 * @param budgetPersonId
	 */
	public void deleteAwardBudgetPerson(Integer budgetPersonId);

	/**
	 * 
	 * @param awardId
	 * @return
	 */
	public List<AwardBudgetHeader> fetchAwardBudgetHeaderByAwardId(Integer awardId);

	/**
	 * 
	 * @param budgetHeaderId
	 * @param tbnId
	 * @param jobCode
	 * @param personId
	 * @param rolodexId
	 * @return
	 */
	public boolean checkBudgetPersonInBudget(Integer budgetHeaderId, String tbnId, String jobCode, String personId,
			Integer rolodexId);

	/**
	 * 
	 * @param budgetHeaderId
	 * @param tbnId
	 * @return
	 */
	public AwardBudgetPerson getBugetTbnPersonByTbnId(Integer budgetHeaderId, String tbnId);

	/**
	 * 
	 * @param budgetHeaderId
	 * @param personId
	 * @return
	 */
	public AwardBudgetPerson getBugetPersonByPersonId(Integer budgetHeaderId, String personId);

	/**
	 * 
	 * @param budgetHeaderId
	 * @param rolodexId
	 * @return
	 */
	public AwardBudgetPerson getBugetRolodexPersonByRolodexId(Integer budgetHeaderId, Integer rolodexId);

	/**
	 * This method is used for save or update award budget header
	 * 
	 * @param budgetHeader
	 * @return
	 */
	public AwardBudgetHeader saveOrUpdateAwardBudgetOverView(AwardBudgetHeader budgetHeader);

	/**
	 * This method is used for save or update award budget period
	 * 
	 * @param budgetPeriod
	 * @return
	 */
	public AwardBudgetPeriod saveOrUpdateAwardBudgetPeriod(AwardBudgetPeriod budgetPeriod);

	/**
	 * this method is used for save or update award budget line item.
	 * 
	 * @param awardBudgetDetail
	 * @return
	 */
	public AwardBudgetDetail saveOrUpdateAwardBudgetLineItem(AwardBudgetDetail awardBudgetDetail);

	/**
	 * 
	 * @param awardBudgetHeader
	 * @return
	 */
	public AwardBudgetHeader saveAwardBudgetHeader(AwardBudgetHeader awardBudgetHeader);

	public List<AwardBudgetDetail> deleteAllBudgetDetail(List<AwardBudgetDetail> budgetDetail);

	/**
	 * 
	 * @param budgetPersonId
	 * @return
	 */
	public AwardBudgetPerson getAwardBudgetPersonBypersonId(Integer budgetPersonId);

	/**
	 * 
	 * @param budgetPeriodId
	 * @return
	 */
	public List<AwardBudgetDetail> fetchAwardBudgetDetailByPeriodId(Integer budgetPeriodId);

	/**
	 * 
	 * @param budgetDetailId
	 * @return
	 */
	public List<AwardBudgetPersonalDetail> fetchAwardPersonDetailsBydetailedId(Integer budgetDetailId);

	/**
	 * 
	 * @param persons
	 */
	public void saveOrUpdateBudgetPersonDetails(AwardBudgetPersonalDetail persons);

	/**
	 * 
	 * @param persons
	 */
	public void deleteAwardBudgetPersonDetail(AwardBudgetPersonalDetail persons);

	/**
	 * 
	 * @param budgetPeriodId
	 * @return
	 */
	public List<AwardBudgetDetail> fetchSysGeneratedCE(Integer budgetPeriodId);

	/**
	 * this method is used for delete all award budget line item
	 * @param awardBudgetDetail
	 */
	public void deleteAllAwardBudgetLineItem(List<AwardBudgetDetail> awardBudgetDetail);

	/**
	 * This method is used to fetch all award budget person details data as list by the award budget detail id
	 * @param budgetDetailId
	 * @return AwardBudgetPersonalDetail
	 */
	public List<AwardBudgetPersonalDetail> getAwardBudgetPersonalDetailsByDetailId(Integer budgetDetailId);

	/**
	 * this method is used to check budget person is present in the budget
	 * 
	 * @param budgetId
	 * @param personId
	 * @param rolodexId
	 * @param personType
	 * @param tbnId
	 * @return
	 */
	public Boolean checkBudgetPerson(Integer budgetId, String personId, Integer rolodexId, String personType,
			String tbnId);

	/**
	 * This method is used for fetch budget persons by person id and budget id
	 * 
	 * @param budgetId
	 * @param personId
	 * @param rolodexId
	 * @param personType
	 * @param tbnId
	 * @return
	 */
	public AwardBudgetPerson getBudgetPersonByPersonId(Integer budgetId, String personId, Integer rolodexId,
			String personType, String tbnId);

	/**
	 * This method is used for get maximum line item number by budget period id
	 * @param budgetPersonId
	 * @return
	 */
	public Integer getMaxLineItemNumberByPeriodId(Integer budgetPeriodId);

	/**
	 * This method is used to get Award Budget Details By Params
	 * @param budgetHeaderId
	 * @param costElement
	 * @return AwardBudgetDetail
	 */
	public List<AwardBudgetDetail> getAwardBudgetDetailsByParams(Integer budgetHeaderId , String costElement);

	/**
	 * This method is used to save Award budget rates
	 * @param awardRate
	 * @return
	 */
	public AwardRates saveOrUpdateAwardBudgetRate(AwardRates awardRate);

	/**
	 * This method is used to fetch all Award budget rates based on the budget id
	 * @param budgetId
	 * @return
	 */
	public List<AwardRates> fetchAwardRatesByBudgetId(Integer budgetId);

	/**
	 * This method is used to delete Award budget rates
	 * @param awardRate
	 */
	public void deleteAwardBudgetRate(List<AwardRates> awardRate);

	/**
	 * This method is used for save or update the AwardBudet detail calculated amount
	 * @param awardBudgetDetailCalcAmount
	 * @return
	 */
	public AwardBudgetDetailCalcAmount saveOrUpdateAwardBudgetDetailCalcAmount(AwardBudgetDetailCalcAmount awardBudgetDetailCalcAmount);

	/**
	 * This method is used for fetch award budget rates by budget detail id
	 * @param detailId
	 * @return
	 */
	public List<AwardBudgetDetailCalcAmount> getAwardBudgetCalcAmountByAwdBudgetDetailId(Integer detailId);

	/**
	 * This method is used for delete award budget detail calculated amount
	 * @param awardBudgetDetailCalcAmount
	 */
	public void deleteAwardBudgetCalcAmountByAwdBudgetDetailId(AwardBudgetDetailCalcAmount awardBudgetDetailCalcAmount);
	
	/**
	 * This method is used for delete all award budget detail calculated amount
	 * @param awardBudgetDetailCalcAmount
	 */
	public void deleteAllCalcAmount(List<AwardBudgetDetailCalcAmount> awardBudgetDetailCalcAmount);

	/**
	 * This method is used for check person is added in the budget
	 * @param budgetPersonId
	 * @return
	 */
	public Boolean checkAwardBudgetPersonAddedInBudget(Integer budgetPersonId);

	/**
	 * This method is used to get the IO Code based on budgetDetailId
	 * @param budgetDetailId
	 * @return InternalOrderCode
	 */
	public String getInternalOrderCodeByBudgetDetailId(Integer budgetDetailId);

	/**
	 * This method is used for fetch all award budgets based on award number
	 * @param awardNumber
	 * @return
	 */
	public List<AwardBudgetHeader> fetchAwardBudgetHeaderByAwardId(String awardNumber);
	
	/**
	 * 
	 * @param seqNumber
	 * @param awardNumber
	 * @return
	 */
	public AwardBudgetHeader getAwardIdrBySeqNumberAndAwardNumber (Integer seqNumber, String awardNumber);

	public String getIOCodeByBudgetDetailId(Integer budgetDetailId);

	/**
	 * This method is used for fetch InsituteRates based on startDate,activityTypeCode,rateClassCode,rateTypeCode and unitNumber
	 * @param startDate
	 * @param activityTypeCode
	 * @param rateClassCode
	 * @param rateTypeCode
	 * @param unitNumber
	 * @return InstituteRate
	 */
	public InstituteRate fetchInstituteRateByUnitAndDateLessthanMax(Timestamp startDate, String activityTypeCode, String rateClassCode, String rateTypeCode, String unitNumber);

	/**
	 * This method is used to delete Award budget non person details
	 * @param  budgetNonPersonDtlId  - Id of budget non person detail.
	 */
	public void deleteBudgetNonPersonDetail(Integer budgetNonPersonDtlId);

	/**
	 * This method is used to fetch all budget fund type
	 * @return list of budget fund type
	 */
	public List<AwardBudgetFundType> fetchAllBudgetFundType();

	/**
	 * This method is used to fetch the Award budget fund type by fund type code
	 * @param availableFundType
	 * @return selected budget fund type
	 */
	public AwardBudgetFundType getBudgetFundTypeByCode(String availableFundType);

	/**
	 * This method is used to get latest award budget header if the budget satus is not error in posting
	 * @param awardId
	 * @return award budget header
	 */
	public AwardBudgetHeader getAwardBudgetHeaderWithOutErrorInPosting(Integer awardId);

	/**
	 * This method is used to get latest award budget header
	 * @param awardId
	 * @return award budget header
	 */
	public AwardBudgetHeader getLatestAwardBudgetHeader(String awardNumber);

	/**
	 * @param budgetDetailId
	 * @return
	 */
	public AwardBudgetDetail getAwardBudgetDetailsByDetailId(Integer budgetDetailId);

	/**
	 * 
	 * @param budgetPersonDetailId
	 * @return
	 */
	public AwardBudgetPersonalDetail getAwardBudgetPersonalDetailsByPersonDetailId(Integer budgetPersonDetailId);

	public AwardBudgetNonPersonDetail getAwardBudgetNonPersonalDetailsByPersonDetailId(Integer budgetNonPersonDetailId);

	/**
	 * @param batchId
	 * @param awardNumber
	 * @param budgetStatusCode
	 * @return
	 */
	public List<Integer> getAwardBudgetVersionBasedOnBatchId(Integer batchId, String awardNumber, String currentBudgetStatusCode);

	/**
	 * @param versionNumbers
	 * @param budgetStatus
	 * @param awardNumber
	 * @param currentBudgetStatusCode
	 */
	public void updateAwardBudgetHeadersBasedOnVersionNumbers(List<Integer> versionNumbers, String budgetStatus, String awardNumber, String currentBudgetStatusCode);

	/**
	 * @param awardNumber
	 * @param currentBudgetStatusCode
	 * @return
	 */
	public List<Integer> getMasterAwardBudget(String awardNumber, String currentBudgetStatusCode);

	/**
	 * @param awardIds
	 * @param currentBudgetStatusCode
	 * @return
	 */
	public List<Integer> getAwardBudgetVersionBasedOnAwardIdsAndStatus(Set<Integer> awardIds, String awardNumber, String currentBudgetStatusCode);

	/**
	 * @param budgetPeriodIds
	 * @return all award budget details of given budget periods
	 */
	List<AwardBudgetDetail> fetchAwardBudgetDetailByPeriodIds(Set<Integer> budgetPeriodIds);

	/**
	 * @param budgetDetailIds
	 * @return all award budget calc amount of given budget details
	 */
	List<AwardBudgetDetailCalcAmount> getAwardBudgetCalcAmountByAwdBudgetDetailIds(Set<Integer> budgetDetailIds);

	/**
	 * This method is used to get all the budget person by budget id
	 * @param budgetId
	 * @return list of budget persons
	 */
	public List<AwardBudgetPerson> copyBudgetPersonWithoutLineItem(Integer budgetId);

	/**
	 * This method is used to get next line item sequence number
	 * @param budgetId
	 * @param budgetCategoryCode
	 * @param accountNumber
	 * @return list of budget persons
	 */
	public String getNextLineItemSequenceNumber(Integer budgetId, String budgetCategoryCode, String accountNumber);

}
