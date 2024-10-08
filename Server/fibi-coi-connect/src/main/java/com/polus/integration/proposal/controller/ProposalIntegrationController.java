package com.polus.integration.proposal.controller;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import com.polus.integration.proposal.dto.DisclosureResponse;
import com.polus.integration.proposal.service.ProposalIntegrationService;
import com.polus.integration.proposal.vo.ProposalIntegrationVO;

import jakarta.servlet.http.HttpServletRequest;


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

	@GetMapping("/coi/disclosure/status/proposals/{proposalNumber}/persons/{personId}")
	public ResponseEntity<DisclosureResponse> feedProposalPersonDisclosureStatus(HttpServletRequest request,
			@PathVariable(value = "proposalNumber", required = true) String proposalNumber,
			@PathVariable(value = "personId", required = true) String personId) {

		String clientIp = request.getRemoteAddr();
		logger.info("Request received for feedProposalPersonDisclosureStatus from IP: {}, Proposal Number: {} and Person ID : {}", clientIp, proposalNumber, personId);
		DisclosureResponse response = null;
		try {
			response = proposalIntegrationService.feedProposalPersonDisclosureStatus(proposalNumber, personId);
		} catch (Exception e) {
			return new ResponseEntity<>(response, HttpStatus.INTERNAL_SERVER_ERROR);
		}

		return new ResponseEntity<>(response, HttpStatus.OK);
	}

	@GetMapping("/coi/disclosure/proposals/{proposalNumber}/validate")
	public ResponseEntity<DisclosureResponse> checkProposalDisclosureStatus(HttpServletRequest request,
			@PathVariable(value = "proposalNumber", required = true) String proposalNumber) {

		String clientIp = request.getRemoteAddr();
		logger.info("Request received for checkProposalDisclosureStatus from IP: {}, Proposal Number: {}", clientIp, proposalNumber);
		DisclosureResponse response = null;

		try {
			response = proposalIntegrationService.checkProposalDisclosureStatus(proposalNumber);
		} catch (Exception e) {
			return new ResponseEntity<>(response, HttpStatus.INTERNAL_SERVER_ERROR);
		}

		return new ResponseEntity<>(response, HttpStatus.OK);
	}

}
