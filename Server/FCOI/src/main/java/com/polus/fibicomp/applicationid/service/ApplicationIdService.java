package com.polus.fibicomp.applicationid.service;

import org.springframework.stereotype.Service;

import com.polus.fibicomp.proposal.pojo.Proposal;

@Service
public interface ApplicationIdService {

	/**
	 * This method is used to generate Application ID for Internal Proposals. 
	 * @param Proposal
	 * @return Application ID.
	 */
	public String generateApplicationIdProposal(Proposal proposal);

}
