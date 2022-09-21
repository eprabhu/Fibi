package com.polus.fibicomp.award.revenue.service;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.award.revenue.dao.AwardRevenueDao;
import com.polus.fibicomp.award.revenue.pojo.AwardRevenueDetails;
import com.polus.fibicomp.award.revenue.vo.AwardRevenueVO;
import com.polus.fibicomp.budget.dao.AwardBudgetDao;
import com.polus.fibicomp.budget.dao.BudgetDao;
import com.polus.fibicomp.budget.pojo.BudgetCategory;
import com.polus.fibicomp.budget.pojo.CostElement;
import com.polus.fibicomp.common.dao.CommonDao;

@Transactional
@Service(value = "awardRevenueService")
public class AwardRevenueServiceImpl implements AwardRevenueService {

	@Autowired
	@Qualifier(value = "awardRevenueDao")
	private AwardRevenueDao awardRevenueDao;

	@Autowired
	private CommonDao commonDao;

	@Autowired
	@Qualifier(value = "awardBudgetDao")
    private AwardBudgetDao awardBudgetDao;

	@Autowired
	private BudgetDao budgetDao;

	@Override
	public String loadRevenueDetailsByParams(String awardNumber, String accountNumber) {
		List<AwardRevenueDetails> awardRevenueDetails = awardRevenueDao.getRevenueDetailsByParams(awardNumber, accountNumber);
		if (awardRevenueDetails != null && !awardRevenueDetails.isEmpty()) {
			List<BigDecimal> totalRevenueAmount = new ArrayList<>();
			BigDecimal totalAmount = BigDecimal.ZERO;
			for (AwardRevenueDetails awardRevenueDetail : awardRevenueDetails) {
				List<AwardRevenueVO> awardRevenueDetailVOs = new ArrayList<>();
				String budgetCategoryCode = awardRevenueDetail.getInternalOrderCode().substring(awardRevenueDetail.getAccountNumber().length(), awardRevenueDetail.getInternalOrderCode().length()).substring(0, 3);
				if (budgetCategoryCode != null) {
					prepareAwardRevenueDetail(awardRevenueDetail, budgetCategoryCode, awardRevenueDetailVOs);
					totalRevenueAmount.add(awardRevenueDetail.getTotalRevenueAmount());
					if (!totalRevenueAmount.isEmpty()) {
						totalAmount = totalRevenueAmount.stream().reduce(BigDecimal.ZERO, BigDecimal::add);
						awardRevenueDetail.setTotalAmount(totalAmount);
					}
				}
			}
		}
		return commonDao.convertObjectToJSON(awardRevenueDetails);
	}

	private AwardRevenueDetails prepareAwardRevenueDetail(AwardRevenueDetails awardRevenueDetail, String budgetCategoryCode, List<AwardRevenueVO> awardRevenueDetailVOs) {
		List<CostElement> costElements = awardBudgetDao.fetchCostElementByBudgetCategory(budgetCategoryCode);
		if (costElements != null && !costElements.isEmpty()) {
			CostElement costElement = costElements.get(0);
			AwardRevenueVO awardRevenueVO = new AwardRevenueVO();
			if (budgetCategoryCode != null && !budgetCategoryCode.isEmpty()) {
				BudgetCategory budgetCategory = budgetDao.fetchBudgetCategoryBasedOnCode(budgetCategoryCode);
				if (budgetCategory != null) {
					awardRevenueVO.setBudgetCategoryCode(budgetCategoryCode);
					awardRevenueVO.setBudgetCategory(budgetCategory.getDescription());
				}
			}
			awardRevenueVO.setAccountNumber(awardRevenueDetail.getAccountNumber());
			awardRevenueVO.setAwardNumber(awardRevenueDetail.getAwardNumber());
			awardRevenueVO.setInternalOrderCode(awardRevenueDetail.getInternalOrderCode());
			awardRevenueVO.setTotalRevenueAmount(awardRevenueDetail.getTotalRevenueAmount());
			awardRevenueVO.setCostElement(costElement.getDescription());
			awardRevenueDetailVOs.add(awardRevenueVO);
			awardRevenueDetail.setAwardRevenueVOs(awardRevenueDetailVOs);
		}
		return awardRevenueDetail;
	}

	@Override
	public String fetchRevenueTransactionsByParams(String awardNumber, String accountNumber, List<String> internalOrderCodes, Timestamp fiPostingStartDate, Timestamp fiPostingEndDate) {
		return commonDao.convertObjectToJSON(awardRevenueDao.fetchRevenueTransactionsByParams(awardNumber, accountNumber, internalOrderCodes, fiPostingStartDate, fiPostingEndDate));
	}

}
