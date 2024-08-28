package com.polus.kcintegration.award.service;

import java.util.concurrent.CompletableFuture;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.retry.annotation.EnableRetry;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.kcintegration.proposal.service.RetryService;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@EnableAsync
@EnableRetry
@Transactional
@Service
public class AwardIntegrationServiceImpl implements AwardIntegrationService {

	@Autowired
    private RetryService proposalRetryService;

	@Async
	@Override
	public CompletableFuture<ResponseEntity<String>> feedAward(String projectNumber) {
		log.info("Award feed started for projectNumber: {}", projectNumber);
		return CompletableFuture.supplyAsync(() -> {
			try {
				log.info("sleep start....");
				Thread.sleep(10000);
				log.info("sleep end....");
				// Retry the fetching and messaging if the list is null or empty
				log.info("retryFetchAwardAndSendMessage start....");
				proposalRetryService.retryFetchAwardAndSendMessage(projectNumber);
				log.info("retryFetchAwardAndSendMessage end....");
				return new ResponseEntity<>("Message successfully sent to queue", HttpStatus.OK);
			} catch (Exception e) {
				log.error("Error occurred in feedAward: {}", e.getMessage());
				return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body("Error during feedInstituteProposal: " + e.getMessage());
			}
		}).exceptionally(e -> {
	        log.error("Async operation failed: {}", e.getMessage());
	        return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).body("Async operation failed: {}" + e.getMessage());
	    });
	}

}
