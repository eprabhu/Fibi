package com.polus.integration.award.service;

import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.integration.award.dto.AwardDTO;


@Transactional
@Service
public interface AwardIntegrationService {


	/**
	 * @param award
	 */
	public ResponseEntity<AwardDTO> feedAward(AwardDTO award);

}
