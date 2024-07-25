package com.polus.integration.proposal.dao;


import org.springframework.stereotype.Service;

import com.polus.integration.proposal.pojo.COIIntegrationPropQuestAns;
import com.polus.integration.proposal.pojo.COIIntegrationProposal;
import com.polus.integration.proposal.pojo.COIIntegrationProposalPerson;

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

}
