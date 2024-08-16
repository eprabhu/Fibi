package com.polus.integration.proposal.controller;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import com.polus.integration.proposal.service.ProposalIntegrationService;
import com.polus.integration.proposal.vo.ProposalIntegrationVO;


@RestController
public class ProposalIntegrationController {

	protected static Logger logger = LogManager.getLogger(ProposalIntegrationController.class.getName());

	@Autowired
	private ProposalIntegrationService proposalIntegrationService;

	@PostMapping("/feedProposal")
	public void feedProposal(@RequestBody ProposalIntegrationVO vo) {
		logger.info("Request for feedProposal");
		proposalIntegrationService.feedProposalDetails(vo.getProposalDTO());
	}

	@PostMapping("/createProposalDisclosure")
	public void feedPersonQuestionnaireAndCreateDisclosure(@RequestBody ProposalIntegrationVO vo) {
		logger.info("Request for createProposalDisclosure");
		logger.info("QuestionnaireVO : {}", vo);
		proposalIntegrationService.feedPersonQuestionnaireAndCreateDisclosure(vo.getQuestionnaireVOs());
	}	

}
