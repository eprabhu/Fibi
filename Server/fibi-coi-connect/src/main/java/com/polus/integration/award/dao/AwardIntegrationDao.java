package com.polus.integration.award.dao;


import org.springframework.stereotype.Service;

import com.polus.integration.award.dto.AwardDTO;

import jakarta.transaction.Transactional;

@Transactional
@Service
public interface AwardIntegrationDao {

	/**
	 * @param awardDTO
	 * @param moduleCode 
	 * @return
	 */
	public Boolean canUpdateProjectDisclosureFlag(AwardDTO awardDTO);

}
