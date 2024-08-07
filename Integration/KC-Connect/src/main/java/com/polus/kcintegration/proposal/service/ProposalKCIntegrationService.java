package com.polus.kcintegration.proposal.service;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;


@Transactional
@Service
public interface ProposalKCIntegrationService {

	/**
	 * @param proposalNumber
	 */
	public void feedProposal(String proposalNumber);

	/**
	 * @param moduleItemId
	 * @param personId 
	 * @param questionnaireId 
	 * @return
	 */
	public void feedPersonQuestionnaireAndCreateDisclosure(String moduleItemId, Integer questionnaireId, String personId);

}
