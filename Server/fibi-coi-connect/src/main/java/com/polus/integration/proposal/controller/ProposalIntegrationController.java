package com.polus.integration.proposal.controller;

import org.apache.commons.lang3.StringUtils;
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

import jakarta.persistence.PersistenceException;
import jakarta.servlet.http.HttpServletRequest;
import lombok.extern.slf4j.Slf4j;


@RestController
@Slf4j
public class ProposalIntegrationController {

	@Autowired
	private ProposalIntegrationService proposalIntegrationService;

	@PostMapping("/feedProposal")
	public void feedProposal(@RequestBody ProposalIntegrationVO vo) {
		log.info("Request for feedProposal");
		proposalIntegrationService.feedProposalDetails(vo.getProposalDTO());
	}

	@PostMapping("/createProposalDisclosure")
	public void feedPersonQuestionnaireAndCreateDisclosure(@RequestBody ProposalIntegrationVO vo) {
		log.info("Request for createProposalDisclosure");
		log.info("QuestionnaireVO : {}", vo);
		proposalIntegrationService.feedPersonQuestionnaireAndCreateDisclosure(vo.getQuestionnaireVOs());
	}	

	@GetMapping("/coi/disclosure/status/proposals/{proposalNumber}/persons/{personId}")
	public ResponseEntity<DisclosureResponse> feedProposalPersonDisclosureStatus(HttpServletRequest request,
			@PathVariable("proposalNumber") String proposalNumber, @PathVariable("personId") String personId) {
		String clientIp = request.getRemoteAddr();
		log.info("Request received for feedProposalPersonDisclosureStatus and IP: {}, Proposal Number: {}, Person ID: {}", clientIp, proposalNumber, personId);

		if (StringUtils.isBlank(proposalNumber) || StringUtils.isBlank(personId)) {
			log.warn("Invalid proposal number or person ID provided.");
			return new ResponseEntity<>(DisclosureResponse.builder().error("Invalid proposal number or person ID.").build(), HttpStatus.BAD_REQUEST);
		}

		try {
			DisclosureResponse response = proposalIntegrationService.feedProposalPersonDisclosureStatus(proposalNumber, personId);
			return new ResponseEntity<>(response, HttpStatus.OK);
		} catch (Exception e) {
			log.error("Error fetching disclosure status for proposalNumber: {}, personId: {}: {}", proposalNumber, personId, e.getMessage(), e);
			return new ResponseEntity<>(DisclosureResponse.builder().error("An internal server error occurred.").build(), HttpStatus.INTERNAL_SERVER_ERROR);
		}
	}

	@GetMapping("/coi/disclosure/proposals/{proposalNumber}/validate")
	public ResponseEntity<DisclosureResponse> checkProposalDisclosureStatus(HttpServletRequest request, @PathVariable("proposalNumber") String proposalNumber) {
		String clientIp = request.getRemoteAddr();
		log.info("Request received for checkProposalDisclosureStatus and IP: {}, Proposal Number: {}", clientIp, proposalNumber);

		if (StringUtils.isBlank(proposalNumber)) {
			log.warn("Invalid proposal number provided.");
			return new ResponseEntity<>(DisclosureResponse.builder().error("Invalid proposal number.").build(), HttpStatus.BAD_REQUEST);
		}

		try {
			DisclosureResponse response = proposalIntegrationService.checkProposalDisclosureStatus(proposalNumber);
			return new ResponseEntity<>(response, HttpStatus.OK);
		} catch (PersistenceException e) {
			log.error("Database error for proposalNumber {}: {}", proposalNumber, e.getMessage(), e);
			return new ResponseEntity<>(DisclosureResponse.builder().error("Database error occurred.").build(), HttpStatus.INTERNAL_SERVER_ERROR);
		} catch (Exception e) {
			log.error("Error processing request for proposalNumber {}: {}", proposalNumber, e.getMessage(), e);
			return new ResponseEntity<>(DisclosureResponse.builder().error("An unexpected error occurred.").build(), HttpStatus.INTERNAL_SERVER_ERROR);
		}
	}

	@GetMapping("/coi/disclosure/type/{disclosureType}/person/{personId}/expiration-date")
	public ResponseEntity<DisclosureResponse> feedDisclosureExpirationDate(HttpServletRequest request, @PathVariable("disclosureType") String disclosureType, @PathVariable("personId") String personId) {
		String clientIp = request.getRemoteAddr();
		log.info("Request received for feedDisclosureExpirationDate and IP: {}, Disclosure Type: {}, Person ID: {}", clientIp, disclosureType, personId);

		if (StringUtils.isBlank(personId) || StringUtils.isBlank(disclosureType)) {
			log.warn("Invalid personId or disclosureType provided.");
			return new ResponseEntity<>(DisclosureResponse.builder().error("Invalid personId or disclosureType.").build(), HttpStatus.BAD_REQUEST);
		}

		try {
			DisclosureResponse response = proposalIntegrationService.feedDisclosureExpirationDate(disclosureType, personId);
			return new ResponseEntity<>(response, HttpStatus.OK);
		} catch (PersistenceException e) {
			log.error("Database error for personId {} and disclosureType {}: {}", personId, disclosureType, e.getMessage(), e);
			return new ResponseEntity<>(DisclosureResponse.builder().error("Database error occurred.").build(), HttpStatus.INTERNAL_SERVER_ERROR);
		} catch (Exception e) {
			log.error("Error processing request for personId {} and disclosureType {}: {}", personId, disclosureType, e.getMessage(), e);
			return new ResponseEntity<>(DisclosureResponse.builder().error("An unexpected error occurred.").build(), HttpStatus.INTERNAL_SERVER_ERROR);
		}
	}

}
