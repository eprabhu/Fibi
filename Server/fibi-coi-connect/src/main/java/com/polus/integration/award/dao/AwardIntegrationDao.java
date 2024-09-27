package com.polus.integration.award.dao;


import org.springframework.stereotype.Service;

import com.polus.integration.award.dto.AwardDTO;
import com.polus.integration.award.pojo.COIIntegrationAward;
import com.polus.integration.award.pojo.COIIntegrationAwardPerson;

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

	/**
	 * @param projectNumber
	 */
	public void postIntegrationProcess(String projectNumber);

	/**
	 * @param award
	 */
	public void saveAward(COIIntegrationAward award);

	/**
	 * @param projectPerson
	 */
	public void saveAwardPerson(COIIntegrationAwardPerson projectPerson);

}
