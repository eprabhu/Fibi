package com.polus.fibicomp.budget.service;

import java.util.List;
import java.util.Set;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.budget.pojo.BudgetDetail;
import com.polus.fibicomp.budget.pojo.BudgetDetailCalcAmount;
import com.polus.fibicomp.budget.pojo.BudgetHeader;
import com.polus.fibicomp.budget.pojo.BudgetPeriod;
import com.polus.fibicomp.budget.pojo.CostElement;
import com.polus.fibicomp.budget.pojo.FibiProposalRate;
import com.polus.fibicomp.budget.vo.BudgetVO;
import com.polus.fibicomp.budget.vo.SimpleBudgetVO;
import com.polus.fibicomp.proposal.pojo.Proposal;

@Transactional
@Service
public interface BudgetService {

	/**
	 * This method is to fetch filtered proposal rates.
	 * @param budgetHeader - Object of BudgetHeader.
	 * @param rateClassTypes - type of rate class.
	 * @return list of FibiProposalRates.
	 */
	public List<FibiProposalRate> fetchFilteredProposalRates(BudgetHeader budgetHeader, String activityTypeCode,
			Set<String> rateClassTypes, Integer grantTypeCode);

	/**
	 * This method is to create a new budget for a proposal.
	 * @param vo - Object of ProposalVO.
	 * @return set of values to create a budget.
	 */
	public String createProposalBudget(BudgetVO vo);

	/**
	 * This method is to save or update proposal budget.
	 * @param vo - Object of BudgetVO.
	 * @return saved budget detail.
	 */
	public String saveOrUpdateProposalBudget(BudgetVO vo);

	/**
	 * This method is to get synched budget rates.
	 * @param vo - Object of ProposalVO.
	 * @return synched budget rates.
	 */
	public String getSyncBudgetRates(BudgetVO budgetVO);

	/**
	 * This method is to calculate budget.
	 * @param vo - Object of ProposalVO.
	 * @return proposal after budget updation.
	 */
	public String autoCalculate(BudgetVO budgetVO);

	/**
	 * This method is generate budget periods.
	 * @param vo - Object of ProposalVO.
	 * @return list of budget periods.
	 */
	public List<BudgetPeriod> generateBudgetPeriods(BudgetHeader budget);

	
	/**
	 * This method is generate budget periods based on Calendar year 1/Jan to 31/Dec.
	 * @param vo - Object of ProposalVO.
	 * @return list of budget periods.
	 */
	public List<BudgetPeriod> generateBudgetPeriodsByCalendarYear(BudgetHeader budget);
	
	/**
	 * This method is add a new budget period.
	 * @param vo - Object of ProposalVO.
	 * @return proposal after budget period creation.
	 */
	public String addBudgetPeriod(BudgetVO vo);

	/**
	 * This method is to check whether budget line item exists.
	 * @param budget - Details of budget.
	 * @param budgetPeriod - Budget Period Value.
	 * @return flag based on condition checking.
	 */
	public boolean budgetLineItemExists(BudgetHeader budget, Integer budgetPeriod);

//	/**
//	 * This method is to generate all periods.
//	 * @param budget - Details of budgetHeader.
//	 */
//	public void generateAllPeriods(BudgetHeader budget);

	/**
	 * This method is to calculate budget.
	 * @param budget - Details of budgetHeader.
	 */
	public void calculateBudget(BudgetHeader budget);

	/**
     * This method is recalculate the budget. For Proposal Budget, recalcuate is same as calculate.
     * For Award Budget, it removes Award Budget Sumamry Calc Amounts before the calculation.
     * @param budget.
     */
    public void recalculateBudget(BudgetHeader budget);

    /**
     * This method is to check whether Budget Summary calculated amounts for a BudgetPeriod.
     * have been modified on AwardBudgetSummary screen.
     * @return true if there is any change.
     */
    public boolean isRateOverridden(BudgetPeriod budgetPeriod);

//    /**
//	 * This method is to check whether there is leap days in budget period.
//	 * @param sDate - Start Date.
//	 * @param eDate - End date.
//	 * @return flag based on condition checking.
//	 */
//    public boolean isLeapDaysInPeriod(Date sDate, Date eDate);

//    /**
//	 * This method is to get new start and end dates of budget.
//	 * @param sDate - Start Date.
//	 * @param eDate - End date.
//	 * @param gap - gap.
//	 * @param duration - duration between start and end date.
//	 * @param prevDate - Previous Date.
//	 * @param leapDayInPeriod - flag that tells whether there is leap day in period.
//	 * @param leapDayInGap - flag that tells whether there is leap day in gap.
//	 * @return list containing start date and end end date.
//	 */
//    public List<Timestamp> getNewStartEndDates(List<Timestamp> startEndDates, int gap, int duration, Timestamp prevDate, boolean leapDayInPeriod, boolean leapDayInGap);

    /**
	 * This method is to system generated cost elements.
	 * @param activityTypeCode - Activity type code.
	 * @return list of cost elements.
	 */
    public List<CostElement> fetchSysGeneratedCostElements(String activityTypeCode);

    /**
	 * This method is to reset proposal rates.
	 * @param vo - Object of ProposalVO.
	 * @return proposal after reseting proposal rates.
	 */
	public String resetProposalRates(BudgetVO vo);

	/**
	 * This method is to delete budget period.
	 * @param vo - Object of BudgetVO.
	 * @return proposal after deleting budget period.
	 */
	public String deleteBudgetPeriod(BudgetVO vo);

	/**
	 * This method is to delete budget line item.
	 * @param vo - Object of ProposalVO.
	 * @return proposal after deleting budget line item.
	 */
	public String deleteBudgetLineItem(BudgetVO vo);

	/**
	 * This method is to copy budget period.
	 * @param vo - Object of ProposalVO.
	 * @return proposal after copying budget period.
	 */
	public String copyBudgetPeriod(BudgetVO vo);

	/**
	 * This method is to get new budget calculated amount.
	 * @param budgetPeriod - Contains budget period.
	 * @param budgetDetail - Contains budget detail.
	 * @param proposalRate - Proposal rate details.
	 * @return budget detail calculated amount.
	 */
	public BudgetDetailCalcAmount getNewBudgetCalculatedAmount(BudgetPeriod budgetPeriod, BudgetDetail budgetDetail, FibiProposalRate proposalRate);

	/**
	 * This method is to get new budget calculated amount.
	 * @param budgetHeader - BudgetHeader.
	 * @return budgetHeader that contains calculated cost.
	 */
	public BudgetHeader calculateCost(BudgetHeader budgetHeader);

	/**
	 * This method is to generate budget periods.
	 * @param vo - Object of ProposalVO.
	 * @return generated budget periods.
	 */
	public String generateBudgetPeriods(BudgetVO vo);

	/**
	 * This method is to copy budget based on budgetId .
	 * @param vo - Object of ProposalVO.
	 * @return copied budget period.
	 */
	public String copyProposalBudget(BudgetVO vo);

	/**
	 * This method is to delete budget line item.
	 * @param vo - Object of ProposalVO.
	 * @return proposal after deleting budget line item.
	 */
	public String deleteSimpleBudgetLineItem(BudgetVO budgetVO);

	/**
	 * This method is add a new budget period.
	 * @param vo - Object of ProposalVO.
	 * @return proposal after budget period creation.
	 */
	public String addSimpleBudgetPeriod(BudgetVO vo);

	/**
	 * This method is to delete budget personnel line from a Budget line item.
	 * @param vo - Object of ProposalVO.
	 * @return proposal after deleting budget line item.
	 */
	public String deletePersonnelLine(BudgetVO proposalVO);

	/**
	 * This method is used to load all budget lookup values.
	 * @param vo - Object of ProposalVO.
	 */
	public void loadBudgetInitialData(BudgetVO vo);

	/**
	 * This method is used to budget summary values.
	 * @param budgetHeaderId - Budget header id.
	 */
	public String fetchBudgetSummaryTable(Integer budgetHeaderId);

	/**
	 * This method is used to load all Proposal Budget Headers based on proposal Id.
	 *@param vo - object of  BudgetVO.
	 *@return proposal Budgets.
	 */
	public String loadBudgetByProposalId(BudgetVO budgetVO);

	/**
	 * This method is used to saveorUpdateBudgetSummary.
	 *@param vo - object of  BudgetVO.
	 *@return proposal Budgets.
	 */
	public String saveorUpdateBudgetSummary(BudgetVO vo);

	/**
	 * This method is used to delete Budget Based On BudgetHeader Id.
	 *@param budgetId - id of  BudgetHeader.
	 *@return string of proposal Budgets.
	 */
	public String deleteBudgetHeader(BudgetVO budgetVO);

	/**
	 * This method is used to load BudgetHeader based on budgetId.
	 *@param vo - object of  BudgetVO.
	 *@return proposal Budget.
	 */
	public String loadBudgetByBudgetId(BudgetVO budgetVO);

	/**
	 * This method is used to load BudgetHeader based on budgetId for simple Budget.
	 *@param vo - object of  BudgetVO.
	 *@return proposal Budget.
	 */
	public String loadSimpleBudgetByBudgetId(BudgetVO vo);

	/**
	 * This method is to save or update proposal simple budget.
	 * @param vo - Object of SimpleBudgetVO.
	 * @return saved budget detail.
	 */
	public String saveOrUpdateSimpleBudget(BudgetVO vo);

	/**
	 * This method is generate simple budget periods.
	 * @param vo - Object of ProposalVO.
	 * @return list of budget periods.
	 */
	public String generateSimpleBudgetPeriods(BudgetVO vo);

	/**
	 * This method is to copy budget based on budgetId .
	 * @param vo - Object of ProposalVO.
	 * @return new budget version.
	 */
	public String createApprovedProposalBudget(BudgetVO vo);

	/**
	 * This method is to covert string json to object .
	 * @param formDataJSON - String JSON.
	 * @return BudgetVO.
	 */
	public BudgetVO convertStringJSONToObject(String formDataJSON);

	/**
	 * This method is to prepare SimpleBudgetVO .
	 * @param budgetId - budgetId.
	 * @return list of SimpleBudgetVO.
	 */
	public List<SimpleBudgetVO> prepareSimpleBudget(Integer budgetId);

		/**
	 * This method is used to load all cost elements values by budget category names.
	 * costElement
	 */
	public String fetchCostElementByCategories();
	/**
	 * 
	 * @param vo
	 * @return
	 */
	public void clearSystemGenCost(BudgetVO vo);
	/**
	 * 
	 * @param vo
	 * @return vo
	 */
	public String calculateAfterDelete(BudgetVO vo);
	/**
	 * This method is to generate budget periods based on proposal end date.
	 * @param proposal - Object of Proposal.
	 * @param userName - update user name.
	 * @return generated budget periods list.
	 */
	public List<BudgetPeriod> generateExtendedBudgetPeriods(Proposal proposal, String userName);

	/**
	 * 
	 * @param copiedBudget - Budget Header object of copied Budget
	 * @param activityTypeCode - activity type code
	 * @return budgetHeader object
	 */
	public BudgetHeader calculateBudgetByPeriod(BudgetHeader copiedBudget, String activityTypeCode);

	/**
	 * @param updateProposalBudgetPeriods - update the budget period based on proposal start and end date
	 * @param BudgetVO - vo
	 * @return budgetHeader details
	 */
	public String updateProposalBudgetPeriods(BudgetVO vo);

}
