package com.polus.integration.award.service;

import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.polus.integration.award.dto.AwardDTO;


@Service
public interface AwardIntegrationService {


	/**
	 * @param award
	 */
	public ResponseEntity<AwardDTO> feedAward(AwardDTO award);

}
