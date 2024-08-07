package com.polus.kcintegration.proposal.service;

import java.util.List;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.amqp.core.Message;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import com.polus.kcintegration.constant.Constant;
import com.polus.kcintegration.dao.KCIntegrationDao;
import com.polus.kcintegration.exception.custom.IntegrationCustomException;
import com.polus.kcintegration.message.service.MessagingService;
import com.polus.kcintegration.proposal.dao.ProposalKCIntegrationDao;
import com.polus.kcintegration.proposal.dto.ProposalDTO;
import com.polus.kcintegration.proposal.dto.QuestionnaireDTO;

@Transactional
@Service
public class ProposalKCIntegrationServiceImpl implements ProposalKCIntegrationService {

	protected static Logger logger = LogManager.getLogger(ProposalKCIntegrationServiceImpl.class.getName());

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

	@Override
	public void feedProposal(String proposalNumber) {
		try {
			ProposalDTO feedProposal = proposalKCDao.fetchProposalByProposalNumber(proposalNumber);
			if (feedProposal != null) {
				feedProposal.setProposalPersons(proposalKCDao.fetchProposalPersons(proposalNumber));
			}
			messagingService.sendMessage(Constant.FIBI_DIRECT_EXCHANGE, devProposalIntegrationQueue, new Message(kcIntegrationDao.convertObjectToJSON(feedProposal).getBytes()));
		} catch (Exception e) {
			logger.error("Error occurred in feedProposal: {}", e.getMessage());
			throw new IntegrationCustomException("Error during feedProposal :{}", e, proposalNumber);
		}
	}

	@Override
	public void feedPersonQuestionnaireAndCreateDisclosure(String moduleItemId, Integer questionnaireId, String personId) {
		try {
			List<QuestionnaireDTO> questionnaireVOs = proposalKCDao.fetchQuestionnaireDetailsByParams(moduleItemId, questionnaireId, personId);
			messagingService.sendMessage(Constant.FIBI_DIRECT_EXCHANGE, devPropQuesAnsIntegrationQueue, new Message(kcIntegrationDao.convertObjectToJSON(questionnaireVOs).getBytes()));
		} catch (Exception e) {
			logger.error("Error occurred in syncPersonQuestionnaireAndCreateDisclosure: {}", e.getMessage());
			throw new IntegrationCustomException("Error during syncPersonQuestionnaireAndCreateDisclosure :{}", e);
		}
	}
	
}
