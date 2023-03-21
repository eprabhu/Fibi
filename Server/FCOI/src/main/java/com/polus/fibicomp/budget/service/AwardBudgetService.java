package com.polus.fibicomp.budget.service;

import java.math.BigDecimal;
import java.util.List;
import java.util.Set;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardAmountInfo;
import com.polus.fibicomp.award.vo.AwardVO;
import com.polus.fibicomp.budget.pojo.AwardBudgetDetail;
import com.polus.fibicomp.budget.pojo.AwardBudgetDetailCalcAmount;
import com.polus.fibicomp.budget.pojo.AwardBudgetHeader;
import com.polus.fibicomp.budget.pojo.AwardBudgetPeriod;
import com.polus.fibicomp.budget.pojo.AwardRates;
import com.polus.fibicomp.budget.pojo.CostElement;
import com.polus.fibicomp.budget.vo.AwardBudgetVO;
import com.polus.fibicomp.budget.vo.AwardDetailBudgetVO;

@Transactional
@Service
public interface AwardBudgetService {

	/**
	 * This method is to create a new budget for a award.
	 * 
	 * @param vo - Object of AwardVO.
	 * @return set of values to create a budget.
	 */
	public String createAwardBudget(AwardBudgetVO vo);

	/**
	 * This method is to save or update award budget.
	 * 
	 * @param vo - Object of AwardVO.
	 * @return saved String.
	 */
	public String saveOrUpdateAwardBudget(AwardBudgetVO vo);

	/**
	 * This method is to save or update award budget.
	 * 
	 * @param vo - Object of AwardVO.
	 * @return saved award.
	 */
	public Award saveOrUpdateAwardBudget(AwardVO vo);

	/**
	 * This method is generate budget periods.
	 * 
	 * @param vo - Object of AwardVO.
	 * @return list of budget periods.
	 */
	public List<AwardBudgetPeriod> generateBudgetPeriods(AwardBudgetHeader budget);


	/**
	 * This method is to check whether budget line item exists.
	 * 
	 * @param budget       - Details of budget.
	 * @param budgetPeriod - Budget Period Value.
	 * @return flag based on condition checking.
	 */
	public boolean budgetLineItemExists(AwardBudgetHeader budget, Integer budgetPeriod);

	/**
	 * This method is to system generated cost elements.
	 * 
	 * @param activityTypeCode - Activity type code.
	 * @return list of cost elements.
	 */
	public List<CostElement> fetchSysGeneratedCostElements(String activityTypeCode);

	/**
	 * This method is to delete budget personnel line from a Budget line item.
	 * 
	 * @param vo - Object of AwardVO.
	 * @return award after deleting budget line item.
	 */
	public String deletePersonnelLine(AwardBudgetVO vo);

	public String loadBudgetByAwardId(AwardBudgetVO vo);

	public String getDevProposalBudgetByAwardId(AwardBudgetVO vo);

	public String importProposalBudget(AwardBudgetVO vo);

	public String getAwardBudgetVersionsByAwardId(AwardBudgetVO vo);

	/**
	 * This method is to calculate balance to date value of award budget line item.
	 * 
	 * @param awardBudgetDetail - Object of AwardBudgetDetail.
	 * @param fundCode          - fundCode
	 */
	public void calculateBalanceToDateValue(AwardBudgetDetail awardBudgetDetail, String fundCode);

	public AwardBudgetHeader createAwardBudgetHeader(AwardBudgetVO vo, Award award);

	public AwardBudgetHeader generateAwardBudgetFromProposalBudget(AwardBudgetVO vo);

	/**
	 * This method is to covert string json to object .
	 * 
	 * @param formDataJSON - String JSON.
	 * @return AwardBudgetVO.
	 */
	public AwardBudgetVO convertStringJSONToObject(String formDataJSON);

	/**
	 * This method is used for add new period in award budget
	 * 
	 * @param budgetId
	 * @param updateUser
	 * @return
	 */
	public String addAwardBudgetPeriod(Integer budgetId, String updateUser);

	/**
	 * This method is for copy award budget period
	 * 
	 * @param vo
	 * @return
	 */
	public String copyAwardBudgetPeriod(AwardBudgetVO vo);

	/**
	 * This method is used for generate all award budget periods
	 * 
	 * @param vo
	 * @return
	 */
	public String generateAwardBudgetPeriods(AwardBudgetVO vo);

	/**
	 * This method is to get new award budget calculated amount.
	 * 
	 * @param budgetPeriod - Contains budget period.
	 * @param budgetDetail - Contains budget detail.
	 * @param proposalRate - Proposal rate details.
	 * @return budget detail calculated amount.
	 */
	public AwardBudgetDetailCalcAmount getNewAwardBudgetCalculatedAmount(AwardBudgetPeriod budgetPeriod,
			AwardBudgetDetail budgetDetail, AwardRates awardRates);

	/**
	 * This method is used to copy award budget header
	 * 
	 * @param vo - contains award budget vo
	 * @return
	 */
	public String copyAwardBudget(AwardBudgetVO vo);

	/**
	 * this method is used for copy award budget period
	 * 
	 * @param copyBudget
	 * @param orginalBudget
	 * @param activityTypeCode
	 * @param userName
	 * @return
	 */
	public List<AwardBudgetPeriod> copyBudgetPeriods(AwardBudgetHeader copyBudget, AwardBudgetHeader orginalBudget,
			String activityTypeCode, String userName);

	/**
	 * This method is used for save or update award budget header details
	 * 
	 * @param budgetHeader
	 * @return
	 */
	public String saveOrUpdateAwardBudgetOverView(AwardBudgetVO vo);

	/**
	 * this method is used to save or update award budget periods and total section.
	 * 
	 * @param awardBudgetPeriod
	 * @param budgetId
	 * @param updateUser
	 * @return
	 */
	public String saveOrUpdateAwardBudgetPeriod(List<AwardBudgetPeriod> awardBudgetPeriod, Integer budgetId, String updateUser);

	/**
	 * this method is used for save or update award budget line item.
	 * 
	 * @param awardBudgetDetail
	 * @return
	 */
	public String saveOrUpdateAwardBudgetLineItem(AwardDetailBudgetVO vo);

	/**
	 * this method is used to remove award budget period
	 * 
	 * @param budgetPeriodId
	 * @param updateUser
	 * @return
	 */
	public String deleteAwardBudgetPeriod(Integer budgetPeriodId, String updateUser);

	/**
	 * this method is used for get all budget period list based on budgetId
	 * 
	 * @param budgetId
	 * @return
	 */
	public List<AwardBudgetPeriod> getAllBudgetPeriodByBudgetId(Integer budgetId);

	/**
	 * This method is used for delete budget line item
	 * 
	 * @param budgetId
	 * @return
	 */
	public String deleteBudgetLineItem(AwardBudgetVO vo);

	/**
	 * This method is used to set award budget period data
	 * 
	 * @param budgetHeader
	 * @return
	 */
	public List<AwardBudgetPeriod> fetchAwardBudgetPeriods(AwardBudgetHeader budgetHeader);

	/**
	 * this method is for Create award budget summary.
	 * @param awardBudgetId
	 * @return
	 */
	public String fetchBudgetSummaryTable(Integer awardBudgetId);

	/**
	 * This method is used for sync new award budget rates
	 * @param AwardBudgetVO
	 * @return
	 */
	public String getSyncAwardBudgetRates(AwardBudgetVO vo);
	
	/**
	 * This method is used for save or update rates in the table Award_Rates
	 * @param awardRates
	 * @return
	 */
	public List<AwardRates> saveOrUpdateAwardBudgetRates(List<AwardRates> awardRates);
	
	/**
	 * This method is used to fetch all Award budget rates based on the budget id
	 * @param budgetId
	 * @return
	 */
	public List<AwardRates> getAwardRatesByBudgetId(Integer budgetId);
	
	/**
	 * This method is used delete Award budget rates
	 * @param awardRates
	 */
	public void deleteAwardBudgetRate(List<AwardRates> awardRates);
	
	/**
	 * This method is used for fetch rates based on the award budget id
	 * @param budget
	 * @param activityTypeCode
	 * @param rateClassTypes
	 * @return
	 */
	public List<AwardRates> fetchFilteredAwardRates(AwardBudgetHeader budget, String activityTypeCode,Set<String> rateClassTypes);

	/**
	 * this method is used for reset award budget rates
	 * @param budgetId
	 * @param userName
	 * @return
	 */
	public String resetAwardRates(Integer budgetId, String userName);

	/**
	 * this method is used for apply the rate changes
	 * @param budgetId
	 * @param awardRates
	 * @param userName
	 * @return
	 */
	public String applayAwardRates(Integer budgetId, List<AwardRates> awardRates, String userName);

	/**
	 * This method is used for save budget detail calculation amount.
	 * 
	 * @param awardBudgetDetailCalcAmount
	 * @return
	 */
	public String saveOrUpdateAwardBudgetCalcAmount(List<AwardBudgetDetailCalcAmount> awardBudgetDetailCalcAmounts);

	/**
	 * This method is used for getAwardBudgetDetailsBasedOnDetailId
	 * 
	 * @param detailId
	 * @return nothing
	 */
	public String getAwardBudgetCalcAmountByAwdBudgetDetailId(Integer detailId);

	/**
	 * This method is used for delete award budget calculation amount
	 * 
	 * @param detailId
	 * @return
	 */
	public void deleteAwardBudgetCalcAmountByAwdBudgetDetailId(Integer detailId);

	/**
	 * This method is used for the internal calculations for the award budgets
	 * 
	 * @param awardBudgetId
	 * @return
	 */
	public AwardBudgetVO calculateAwardBudget(Integer awardBudgetId);

	/**
	 * This method is used to check the person is already in a budget
	 * 
	 * @param budgetPersonId
	 * @return
	 */
	public Boolean checkAwardBudgetPersonAddedInBudget(Integer budgetPersonId);

	/**
	 * This method is used for calculate the virement amount
	 * 
	 * @param awardNumber
	 * @return
	 */
	public BigDecimal calculateVirement(Integer budgetPeriodId);
	
	/**
	 * this method is used for find the initial available funds
	 * 
	 * @param awardId
	 * @param canIncludeInBudget
	 * @param availableFundType
	 * @return
	 */
	public BigDecimal calculateAvailableFund(Integer awardId, String awardNumber, boolean canIncludeInBudget, String availableFundType);
	
	/**
	 * This method is used to get calculateAnticipatedAmount
	 * 
	 * @param awardId
	 * @param availableFundType
	 * @return
	 */
	public BigDecimal calculateAnticipatedAmount(Integer awardId, String awardNumber);
	
	/**
	 * This method is used to get calculateObligatedTotal
	 * 
	 * @param awardNumber
	 * @return
	 */
	public BigDecimal calculateObligatedTotal(Integer awardId, String awardNumber);

	/**
	 * This method is used for calculate total cost share amount that included in budget
	 * 
	 * @param awardId
	 * @return
	 */
	public BigDecimal calculateCostShareIncludedInBudget(Integer awardId);


	/**
	 * This method is used to set award budget period data
	 * 
	 * @param budgetHeader
	 * @return
	 */
	public List<AwardBudgetPeriod> setPeriodData(AwardBudgetHeader budgetHeader);
	
	/**
	 * This method is used to fetch Budget Details using AwardId
	 * 
	 * @param awardId
	 * @return
	 */
	public String getBudgetDetailsByAwardId(Integer awardId);

	/**
	 * This method is used to delete Award budget non person details
	 * @param  vo  - object of AwardBudgetVO.
	 * @return
	 */
	public String deleteNonPersonnelLine(AwardBudgetVO vo);

	/**
	 * This method is used to fetch all budget fund types
	 * @return list of budget fund types
	 */
	public String fetchAllBudgetFundType();

	/**
	 * @param awardBudgetHeader
	 * @return
	 */
	public AwardBudgetVO calculateAwardBudgetHeader(AwardBudgetHeader awardBudgetHeader);

	/**
	 * @param awardBudgetHeader
	 */
	public void calculateAwardBudgetPeriod(AwardBudgetHeader awardBudgetHeader);

	/**
	 * @param awardId
	 * @param awardNumber
	 * @return
	 */
	public AwardAmountInfo fetchLatestAwardAmountInfo(Integer awardId, String awardNumber);

}
