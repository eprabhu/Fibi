package com.polus.fibicomp.awardbudgetapiintegration.scheduler;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import com.polus.fibicomp.awardbudgetapiintegration.service.BudgetAPIService;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;

@Component
public class AwardBudgetAPIScheduler {

	@Autowired
	@Qualifier(value = "budgetAPIService")
	private BudgetAPIService budgetAPIService;

	@Autowired
	private CommonDao commonDao;

	protected static Logger logger = LogManager.getLogger(AwardBudgetAPIScheduler.class.getName());

	@Scheduled(cron = "${budget.api.schedule}", zone = Constants.CRON_JOB_TIMEZONE)
	public void startBudgetAPIFeed() {
		logger.info("Award_budget_integration API execution starts {}", commonDao.getCurrentTimestamp());
		budgetAPIService.fetchBudgetAPIResponse();
	}
}
