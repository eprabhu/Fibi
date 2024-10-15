package com.polus.integration.award.service;

import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.polus.integration.award.dto.AwardDTO;
import com.polus.integration.proposal.dto.DisclosureResponse;


@Service
public interface AwardIntegrationService {


	/**
	 * @param award
	 */
	public ResponseEntity<AwardDTO> feedAward(AwardDTO award);

	public DisclosureResponse feedAwardPersonDisclosureStatus(String awardNumber, String personId);

	public DisclosureResponse checkAwardDisclosureStatus(String awardNumber);

}
