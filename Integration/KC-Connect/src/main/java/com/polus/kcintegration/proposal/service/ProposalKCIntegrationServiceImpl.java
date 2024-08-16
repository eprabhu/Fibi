package com.polus.kcintegration.proposal.service;

import java.util.concurrent.CompletableFuture;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.retry.annotation.EnableRetry;
import org.springframework.scheduling.annotation.Async;
import org.springframework.scheduling.annotation.EnableAsync;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.kcintegration.exception.custom.IntegrationCustomException;
import lombok.extern.slf4j.Slf4j;

@Slf4j
@EnableAsync
@EnableRetry
@Transactional
@Service
public class ProposalKCIntegrationServiceImpl implements ProposalKCIntegrationService {

	@Autowired
    private ProposalRetryService proposalRetryService;

	@Async
	@Override
	public void feedProposal(String proposalNumber) {
		log.info("Proposal feed started for proposalNumber: {}", proposalNumber);
		CompletableFuture.runAsync(() -> {
			try {
				log.info("sleep start....");
				Thread.sleep(10000);
				log.info("sleep end....");
				// Retry the fetching and messaging if the list is null or empty
				log.info("retryFetchProposalAndSendMessage start....");
				proposalRetryService.retryFetchProposalAndSendMessage(proposalNumber);
				log.info("retryFetchProposalAndSendMessage end....");
			} catch (Exception e) {
				log.error("Error occurred in feedProposal: {}", e.getMessage());
				throw new IntegrationCustomException("Error during feedProposal: {}", e);
			}
		}).exceptionally(e -> {
	        log.error("Async operation failed: {}", e.getMessage());
	        return null;
	    });
	}

	@Async
	@Override
	public void feedPersonQuestionnaireAndCreateDisclosure(String moduleItemId, Integer questionnaireId, String personId) {
		log.info("feedPersonQuestionnaireAndCreateDisclosure....");
		CompletableFuture.runAsync(() -> {
			try {
				log.info("sleep start....");
				Thread.sleep(10000);
				log.info("sleep end....");
				// Retry the fetching and messaging if the list is null or empty
				log.info("retryFetchAndSendMessage start....");
				proposalRetryService.retryFetchAndSendMessage(moduleItemId, questionnaireId, personId);
				log.info("retryFetchAndSendMessage end....");
			} catch (Exception e) {
				log.error("Error occurred in feedPersonQuestionnaireAndCreateDisclosure: {}", e.getMessage());
				throw new IntegrationCustomException("Error during feedPersonQuestionnaireAndCreateDisclosure: {}", e);
			}
		}).exceptionally(e -> {
	        log.error("Async operation failed: {}", e.getMessage());
	        return null;
	    });
	}

}
