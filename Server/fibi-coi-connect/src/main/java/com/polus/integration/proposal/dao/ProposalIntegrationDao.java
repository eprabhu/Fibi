package com.polus.integration.proposal.dao;


import org.springframework.stereotype.Service;

import com.polus.integration.proposal.pojo.COIIntegrationPropQuestAns;
import com.polus.integration.proposal.pojo.COIIntegrationProposal;
import com.polus.integration.proposal.pojo.COIIntegrationProposalPerson;
import com.polus.integration.proposal.questionnaire.pojo.FibiCoiQnrMapping;

import jakarta.transaction.Transactional;

@Transactional
@Service
public interface ProposalIntegrationDao {

	/**
	 * @param coiIntegrationProposal
	 * @return
	 */
	public COIIntegrationProposal saveOrUpdateCoiIntegrationProposal(COIIntegrationProposal coiIntegrationProposal);

	/**
	 * @param coiIntegrationProposal
	 * @return
	 */
	public COIIntegrationProposalPerson saveOrUpdateCoiIntegrationProposalPerson(COIIntegrationProposalPerson coiIntegrationProposalPerson);

	/**
	 * @param coiIntegrationProposal
	 * @return
	 */
	public COIIntegrationPropQuestAns saveOrUpdateCoiIntegrationQuestionnaire(COIIntegrationPropQuestAns coiIntegrationPropQuestAns);

	/**
	 * @param questionnaireId
	 * @return
	 */
	public FibiCoiQnrMapping getQuestionnaireMappingInfo(Integer questionnaireId);

	/**
	 * @param fibiQstnId
	 * @param questionnaireId
	 * @param proposalNumber
	 * @param disclosurePersonId
	 * @return
	 */
	public String getQuestionAnswerByParams(Integer fibiQstnId, Integer questionnaireId, Integer proposalNumber, String disclosurePersonId);

}
