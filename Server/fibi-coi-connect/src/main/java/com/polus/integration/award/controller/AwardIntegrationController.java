package com.polus.integration.award.controller;

import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import com.polus.integration.award.service.AwardIntegrationService;
import com.polus.integration.award.vo.AwardIntegrationVO;
import com.polus.integration.proposal.dto.DisclosureResponse;

import jakarta.persistence.PersistenceException;
import jakarta.servlet.http.HttpServletRequest;
import lombok.extern.slf4j.Slf4j;

@RestController
@Slf4j
public class AwardIntegrationController {

	@Autowired
	private AwardIntegrationService awardService;

	@PostMapping("/feedAward")
	public void feedAward(@RequestBody AwardIntegrationVO vo) {
		log.info("Request for feedAward");
		awardService.feedAward(vo.getAward());
	}

	@GetMapping("/coi/disclosure/status/awards/{awardNumber}/persons/{personId}")
	public ResponseEntity<DisclosureResponse> feedAwardPersonDisclosureStatus(HttpServletRequest request, @PathVariable("awardNumber") String awardNumber, @PathVariable("personId") String personId) {
		String clientIp = request.getRemoteAddr();
		log.info("Request received for feedAwardPersonDisclosureStatus and IP: {}, Award Number: {}, Person ID: {}", clientIp, awardNumber, personId);

		if (StringUtils.isBlank(awardNumber) || StringUtils.isBlank(personId)) {
			log.warn("Invalid award number or person ID provided.");
			return new ResponseEntity<>(DisclosureResponse.builder().error("Invalid award number or person ID.").build(), HttpStatus.BAD_REQUEST);
		}

		try {
			DisclosureResponse response = awardService.feedAwardPersonDisclosureStatus(awardNumber, personId);
			return new ResponseEntity<>(response, HttpStatus.OK);
		} catch (Exception e) {
			log.error("Error fetching disclosure status for awardNumber: {}, personId: {}: {}", awardNumber, personId, e.getMessage(), e);
			return new ResponseEntity<>(DisclosureResponse.builder().error("An internal server error occurred.").build(), HttpStatus.INTERNAL_SERVER_ERROR);
		}
	}

	@GetMapping("/coi/disclosure/awards/{awardNumber}/validate")
	public ResponseEntity<DisclosureResponse> checkAwardDisclosureStatus(HttpServletRequest request, @PathVariable("awardNumber") String awardNumber) {
		String clientIp = request.getRemoteAddr();
		log.info("Request received for checkAwardDisclosureStatus and IP: {}, Award Number: {}", clientIp, awardNumber);

		if (StringUtils.isBlank(awardNumber)) {
			log.warn("Invalid award number provided.");
			return new ResponseEntity<>(DisclosureResponse.builder().error("Invalid award number.").build(), HttpStatus.BAD_REQUEST);
		}

		try {
			DisclosureResponse response = awardService.checkAwardDisclosureStatus(awardNumber);
			return new ResponseEntity<>(response, HttpStatus.OK);
		} catch (PersistenceException e) {
			log.error("Database error for awardNumber {}: {}", awardNumber, e.getMessage(), e);
			return new ResponseEntity<>(DisclosureResponse.builder().error("Database error occurred.").build(), HttpStatus.INTERNAL_SERVER_ERROR);
		} catch (Exception e) {
			log.error("Error processing request for awardNumber {}: {}", awardNumber, e.getMessage(), e);
			return new ResponseEntity<>(DisclosureResponse.builder().error("An unexpected error occurred.").build(), HttpStatus.INTERNAL_SERVER_ERROR);
		}
	}

}
