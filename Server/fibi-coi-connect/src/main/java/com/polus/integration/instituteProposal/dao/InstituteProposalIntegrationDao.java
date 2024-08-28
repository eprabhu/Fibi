package com.polus.integration.instituteProposal.dao;


import org.springframework.stereotype.Service;

import com.polus.integration.instituteProposal.dto.InstituteProposalDTO;

import jakarta.transaction.Transactional;

@Transactional
@Service
public interface InstituteProposalIntegrationDao {

	/**
	 * @param instituteProposalDTO
	 * @param moduleCode 
	 * @return
	 */
	public Boolean canUpdateProjectDisclosureFlag(InstituteProposalDTO instituteProposalDTO);

}
