package com.polus.fibicomp.budget.service;

import java.math.BigDecimal;
import java.util.ArrayList;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.budget.dao.BudgetDao;
import com.polus.fibicomp.budget.dao.BudgetModularDao;
import com.polus.fibicomp.budget.pojo.BudgetHeader;
import com.polus.fibicomp.budget.pojo.BudgetModular;
import com.polus.fibicomp.budget.pojo.BudgetModularIDC;
import com.polus.fibicomp.budget.pojo.BudgetPeriod;
import com.polus.fibicomp.budget.vo.BudgetModularVO;
import com.polus.fibicomp.common.dao.CommonDao;

@Transactional
@Service(value = "budgetModularService")
public class BudgetModularServiceImpl implements BudgetModularService {

	protected static Logger logger = LogManager.getLogger(BudgetModularServiceImpl.class.getName());

	private static final String SUCCESS = "Success";

	@Autowired
	private BudgetDao budgetDao;

	@Autowired
	private BudgetModularDao budgetModularDao;

	@Autowired
	public CommonDao commonDao;

	@Override
	public String proposalModularBudget(Integer budgetId) {
		return commonDao.convertObjectToJSON(prepareModularBudget(budgetId));
	}

	@Override
	public BudgetModularVO prepareModularBudget(Integer budgetId) {
		BudgetModularVO budgetModularVO = new BudgetModularVO();
		List<BudgetModular> budgetModular = budgetModularDao.fetchBudgetModular(budgetId);
		if (budgetModular == null || budgetModular.isEmpty()) {
			budgetModular = generateModularBudget(budgetId);
			budgetModularVO.setModularBudget(budgetModular);
		} else {
			budgetModularVO.setModularBudget(budgetModular);
			setPeriodStartEndDates(budgetModular, budgetId);
			calculateCummulative(budgetModularVO, budgetModular);
		}
		return budgetModularVO;
	}

	private List<BudgetModular> setPeriodStartEndDates(List<BudgetModular> budgetModularList, Integer budgetId) {
		List<BudgetPeriod> budgetPeriodlist = getBudgetPeriods(budgetId);
		for (BudgetModular budgetModular : budgetModularList) {
			Integer budgetPeriodId = budgetModular.getBudgetPeriodId();
			setPeriodStartEndDate(budgetPeriodlist, budgetModular, budgetPeriodId);
		}
		return budgetModularList;
	}

	private BudgetModular setPeriodStartEndDate(List<BudgetPeriod> budgetPeriodlist, BudgetModular budgetModular,
			Integer budgetPeriodId) {
		if (budgetPeriodlist != null && !budgetPeriodlist.isEmpty()) {
			for (BudgetPeriod budgetPeriod : budgetPeriodlist) {
				if (budgetPeriod.getBudgetPeriodId().equals(budgetPeriodId)) {
					budgetModular.setStartDate(budgetPeriod.getStartDate());
					budgetModular.setEndDate(budgetPeriod.getEndDate());
				}
			}
		}
		return budgetModular;
	}

	private List<BudgetPeriod> getBudgetPeriods(Integer budgetId) {
		BudgetHeader budgetHeader = budgetDao.fetchBudgetByBudgetId(budgetId);
		List<BudgetPeriod> budgetPeriodlist = budgetHeader.getBudgetPeriods();
		return budgetPeriodlist;
	}

	private BudgetModularVO calculateCummulative(BudgetModularVO budgetModularVO, List<BudgetModular> budgetModularList) {
		BigDecimal totalDirectCostLessConsorFnaforAllPeriod = BigDecimal.valueOf(0);
		BigDecimal totalConsortiumFnaforAllPeriod = BigDecimal.valueOf(0);
		BigDecimal totalDirectCostforAllPeriod = BigDecimal.valueOf(0);
		BigDecimal totalIndirectDirectCostforAllPeriod = BigDecimal.valueOf(0);
		BigDecimal totalDirectAndInDirectCostforAllPeriod = BigDecimal.valueOf(0);
		for (BudgetModular budgetModular : budgetModularList) {
			BigDecimal totalIndirectDirectCost = BigDecimal.valueOf(0);
			for (BudgetModularIDC budgetModularIDC : budgetModular.getIdc()) {
				totalIndirectDirectCost = totalIndirectDirectCost.add(budgetModularIDC.getFundsRequested());
			}
			budgetModular.setTotalIndirectCost(totalIndirectDirectCost);
			budgetModular.setTotalDirectAndInDirectCost(totalIndirectDirectCost.add(budgetModular.getTotalDirectCost()));
			totalDirectCostLessConsorFnaforAllPeriod = totalDirectCostLessConsorFnaforAllPeriod.add(budgetModular.getDirectCostLessConsorFna());
			totalConsortiumFnaforAllPeriod = totalConsortiumFnaforAllPeriod.add(budgetModular.getConsortiumFna());
			totalDirectCostforAllPeriod = totalDirectCostforAllPeriod.add(budgetModular.getTotalDirectCost());
			totalIndirectDirectCostforAllPeriod = totalIndirectDirectCostforAllPeriod.add(budgetModular.getTotalIndirectCost());
			totalDirectAndInDirectCostforAllPeriod = totalDirectAndInDirectCostforAllPeriod.add(budgetModular.getTotalDirectAndInDirectCost());
		}
		budgetModularVO.setTotalConsortiumFnaforAllPeriod(totalConsortiumFnaforAllPeriod);
		budgetModularVO.setTotalDirectAndInDirectCostforAllPeriod(totalDirectAndInDirectCostforAllPeriod);
		budgetModularVO.setTotalDirectCostforAllPeriod(totalDirectCostforAllPeriod);
		budgetModularVO.setTotalDirectCostLessConsorFnaforAllPeriod(totalDirectCostLessConsorFnaforAllPeriod);
		budgetModularVO.setTotalIndirectDirectCostforAllPeriod(totalIndirectDirectCostforAllPeriod);
		return budgetModularVO;
	}

	private List<BudgetModular> generateModularBudget(Integer budgetId) {
		List<BudgetModular> budgetModularList = new ArrayList<>();
		BudgetHeader budgetHeader = budgetDao.fetchBudgetByBudgetId(budgetId);
		if (budgetHeader != null) {
			List<BudgetPeriod> budgetPeriodlist = budgetHeader.getBudgetPeriods();
			if (budgetPeriodlist != null && !budgetPeriodlist.isEmpty()) {
				for (BudgetPeriod budgetPeriod : budgetPeriodlist) {
					BudgetModular budgetModular = setBudgetModular(budgetId, budgetPeriod);
					budgetModularList.add(budgetModular);
				}
			}
		}
		return budgetModularList;
	}

	private BudgetModular setBudgetModular(Integer budgetId, BudgetPeriod budgetPeriod) {
		BudgetModular budgetModular = new BudgetModular();
		ArrayList<BudgetModularIDC> idcList = new ArrayList<BudgetModularIDC>();
		budgetModular.setBudgetPeriodId(budgetPeriod.getBudgetPeriodId());
		budgetModular.setBudgetId(budgetId);
		budgetModular.setStartDate(budgetPeriod.getStartDate());
		budgetModular.setEndDate(budgetPeriod.getEndDate());
		budgetModular.setConsortiumFna(BigDecimal.valueOf(0));
		budgetModular.setDirectCostLessConsorFna(BigDecimal.valueOf(0));
		budgetModular.setTotalDirectAndInDirectCost(BigDecimal.valueOf(0));
		budgetModular.setTotalDirectCost(BigDecimal.valueOf(0));
		budgetModular.setTotalIndirectCost(BigDecimal.valueOf(0));
		budgetModular.setIdc(idcList);
		return budgetModular;
	}

	@Override
	public String deleteModularBudgetInDirectLine(Integer budgetModularIDCId) {
		budgetModularDao.deleteBudgetModularIDCLine(budgetModularIDCId);
		return commonDao.convertObjectToJSON(SUCCESS);
	}

	@Override
	public String saveProposalModularBudget(BudgetModularVO budgetModularVO) {
		saveModularBudget(budgetModularVO);
		return commonDao.convertObjectToJSON(budgetModularVO);
	}

	private void saveModularBudget(BudgetModularVO budgetModularVO) {
		List<BudgetModular> budgetModularList = budgetModularVO.getModularBudget();
		for (BudgetModular budgetModular : budgetModularList) {
			budgetModularDao.saveBudgetModular(budgetModular);
		}
	}

}
