package com.polus.integration.entity.cleansematch.entity;

import java.util.List;

import org.springframework.data.domain.Page;
import org.springframework.data.domain.Pageable;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.polus.integration.entity.cleansematch.dto.StageDnBEntityMatchDTO;

@Repository
public interface DnBEntityMatchRepository extends JpaRepository<StageDnBEntityMatch, String> {
	Page<StageDnBEntityMatch> findAll(Pageable pageable);

	@Query("SELECT new com.polus.integration.entity.cleansematch.dto.StageDnBEntityMatchDTO"
			+ "( e.id, e.sourceDataCode, e.sourceDataName, e.integrationStatusCode, e.candidateMatchedQuantity,"
			+ " e.bestMatchResult, e.bestMatchConfidenceCode, e.httpStatusCode, "
			+ " e.externalSysTransactionId, e.errorCode, e.errorMessage, e.errorDetails ) "
			+ " FROM StageDnBEntityMatch e " + " WHERE e.externalSysTransactionId IS NOT NULL")
	List<StageDnBEntityMatchDTO> GetMatchCompleted();

}
