package com.polus.kcintegration.proposal.service;

import java.util.List;

import org.springframework.amqp.core.Message;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.retry.annotation.Backoff;
import org.springframework.retry.annotation.Recover;
import org.springframework.retry.annotation.Retryable;
import org.springframework.stereotype.Service;

import com.polus.kcintegration.constant.Constant;
import com.polus.kcintegration.dao.KCIntegrationDao;
import com.polus.kcintegration.exception.custom.IntegrationCustomException;
import com.polus.kcintegration.message.service.MessagingService;
import com.polus.kcintegration.proposal.dao.ProposalKCIntegrationDao;
import com.polus.kcintegration.proposal.dto.ProposalDTO;
import com.polus.kcintegration.proposal.dto.QuestionnaireDTO;

import lombok.extern.slf4j.Slf4j;

@Slf4j
@Service
public class ProposalRetryService {

	@Autowired
	private ProposalKCIntegrationDao proposalKCDao;

	@Autowired
	private MessagingService messagingService;

	@Autowired
	private KCIntegrationDao kcIntegrationDao;

	@Value("${fibi.messageq.queues.devProposalIntegration}")
	private String devProposalIntegrationQueue;

	@Value("${fibi.messageq.queues.devPropQuesAnsIntegration}")
	private String devPropQuesAnsIntegrationQueue;

	@Retryable(retryFor = { IntegrationCustomException.class }, maxAttempts = 3, backoff = @Backoff(delay = 5000))
	public void retryFetchAndSendMessage(String moduleItemId, Integer questionnaireId, String personId) {
		List<QuestionnaireDTO> questionnaireVOs = proposalKCDao.fetchQuestionnaireDetailsByParams(moduleItemId, questionnaireId, personId);
		if (questionnaireVOs == null || questionnaireVOs.isEmpty()) {
			throw new IntegrationCustomException("Questionnaire details are null or empty", null);
		}
		messagingService.sendMessage(Constant.FIBI_DIRECT_EXCHANGE, devPropQuesAnsIntegrationQueue, new Message(kcIntegrationDao.convertObjectToJSON(questionnaireVOs).getBytes()));
		log.info("Message successfully sent to queue.");
	}

	@Retryable(retryFor = { IntegrationCustomException.class }, maxAttempts = 3, backoff = @Backoff(delay = 5000))
	public void retryFetchProposalAndSendMessage(String proposalNumber) {
		ProposalDTO feedProposal = proposalKCDao.fetchProposalByProposalNumber(proposalNumber);
		if (feedProposal != null) {
			feedProposal.setProposalPersons(proposalKCDao.fetchProposalPersons(proposalNumber));
		}
	    if (feedProposal == null) {
	        log.error("Proposal details are null for proposalNumber: {}", proposalNumber);
	        throw new IntegrationCustomException("Proposal details are null", null);
	    }
		messagingService.sendMessage(Constant.FIBI_DIRECT_EXCHANGE, devProposalIntegrationQueue, new Message(kcIntegrationDao.convertObjectToJSON(feedProposal).getBytes()));
		log.info("Message successfully sent to queue for proposalNumber: {}", proposalNumber);
	}

	@Recover
	public void recover(IntegrationCustomException e, String moduleItemId, Integer questionnaireId, String personId) {
		log.error("Retries exhausted for feedPersonQuestionnaireAndCreateDisclosure with params {}, {}, {}. Error: {}", moduleItemId, questionnaireId, personId, e.getMessage());
		throw new IntegrationCustomException("Retries exhausted for feedPersonQuestionnaireAndCreateDisclosure with params", e);
	}

	@Recover
	public void recoverFeedProposal(IntegrationCustomException e, String proposalNumber) {
		log.error("Retries exhausted for feedProposal with params {}, {}, {}. Error: {}", proposalNumber, e.getMessage());
		throw new IntegrationCustomException("Retries exhausted for feedProposal with params", e);
	}

}
