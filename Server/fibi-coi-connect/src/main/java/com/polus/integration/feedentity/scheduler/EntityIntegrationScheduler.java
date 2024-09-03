package com.polus.integration.feedentity.scheduler;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.stereotype.Component;

import com.polus.integration.feedentity.service.EntityOutboundIntegrationService;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Component
public class EntityIntegrationScheduler {

	@Autowired
	private EntityOutboundIntegrationService integrationService;

	@Scheduled(fixedRate = 1800000) // 30 minutes in milliseconds
	public void scheduleTask() {
		try {
			log.info("Scheduler started - Setting up header token and calling getEntityDetails");

			// Calling the service method with parameters
			integrationService.getEntityDetails(null, null);

			log.info("Scheduler completed successfully");

		} catch (Exception e) {
			log.error("Exception occurred while executing scheduled task", e);
			// You can add more specific exception handling if needed
		}
	}
}
