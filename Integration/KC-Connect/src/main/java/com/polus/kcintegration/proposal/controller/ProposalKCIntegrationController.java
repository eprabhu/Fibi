package com.polus.kcintegration.proposal.controller;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;
import com.polus.kcintegration.proposal.service.ProposalKCIntegrationService;
import com.polus.kcintegration.proposal.vo.ProposalKCIntegrationVO;
import com.polus.kcintegration.proposal.vo.QuestionnaireVO;

@RestController
public class ProposalKCIntegrationController {

	protected static Logger logger = LogManager.getLogger(ProposalKCIntegrationController.class.getName());

	@Autowired
	private ProposalKCIntegrationService proposalIntegrationService;

	@PostMapping("/feedProposal")
	public void feedProposal(@RequestBody ProposalKCIntegrationVO vo) {
		logger.info("Request for feedProposal");
		logger.info("ProposalNumber :{}", vo.getProposalNumber());
		proposalIntegrationService.feedProposal(vo.getProposalNumber());
	}

	@PostMapping("/createProposalDisclosure")
	public void syncPersonQuestionnaireAndCreateDisclosure(@RequestBody QuestionnaireVO vo) {
		logger.info("Request for createProposalDisclosure");
		logger.info("QuestionnaireVO : {}", vo);
		proposalIntegrationService.feedPersonQuestionnaireAndCreateDisclosure(vo.getProposalNumber(), vo.getQuestionnaireId(), vo.getPersonId());
	}	

}
