package com.polus.integration.proposal.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.polus.integration.proposal.pojo.COIIntPropPersonCompositeKey;
import com.polus.integration.proposal.pojo.COIIntegrationProposalPerson;

@Repository
public interface ProposalPersonIntegrationRepository extends JpaRepository<COIIntegrationProposalPerson, COIIntPropPersonCompositeKey> {


	@Query("SELECT e FROM COIIntegrationProposalPerson e WHERE e.proposalNumber = :proposalNumber")
	List<COIIntegrationProposalPerson> findProposalPersonsByProposalNumber(@Param("proposalNumber") Integer proposalNumber);

}
