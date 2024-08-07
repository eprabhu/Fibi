package com.polus.integration.proposal.repository;



import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.polus.integration.proposal.pojo.COIIntegrationProposal;

@Repository
public interface ProposalIntegrationRepository extends JpaRepository<COIIntegrationProposal, Integer> {

	@Query("SELECT e FROM COIIntegrationProposal e WHERE e.proposalNumber = :proposalNumber")
	COIIntegrationProposal findProposalByProposalNumber(@Param("proposalNumber") String proposalNumber);

}
