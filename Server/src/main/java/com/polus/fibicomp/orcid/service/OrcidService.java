package com.polus.fibicomp.orcid.service;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.orcid.dto.OrcidVO;

@Transactional
@Service
public interface OrcidService {

	public String getPersonOrcidWorks(String personId);

	/**
	 * This method is used to disConnectWebhook.
	 * @param orcidId - Integer value orcidId.
	 * @return String
	 */
	public String getOrcidWorkById(Integer putCode);

	/**
	 * This method is used to link PersonOrcidWork To an Award.
	 * @param vo - object of OrcidVO.
	 * @return String
	 */
	public String linkPersonOrcidWorkToAward(OrcidVO vo);

	public String unLinkPersonOrcidWorkFromAward(OrcidVO vo);

	public String getLinkedOrcidWorksOfAward(OrcidVO vo);

	public void saveOrcidErrorLog(Integer statusCode, String message, String orcidId, String putCode);

}
