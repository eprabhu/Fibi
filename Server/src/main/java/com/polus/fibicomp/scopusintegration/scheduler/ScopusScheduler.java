package com.polus.fibicomp.scopusintegration.scheduler;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.scopusintegration.service.ScopusService;

@Component
public class ScopusScheduler {

	protected static Logger logger = LogManager.getLogger(ScopusScheduler.class.getName());

	@Autowired
	@Qualifier(value = "scopusService")
	private ScopusService scopusService;

	@Scheduled(cron = "${scopus.api.schedule}", zone = Constants.CRON_JOB_TIMEZONE)
	public void startScopusFeed() {
		logger.info("scopus_integration API execution");
		scopusService.fetchScopusAPIResponse();
	}
}
