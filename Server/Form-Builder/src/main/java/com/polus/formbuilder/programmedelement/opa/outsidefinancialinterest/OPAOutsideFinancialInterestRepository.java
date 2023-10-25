package com.polus.formbuilder.programmedelement.opa.outsidefinancialinterest;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

@Repository
public interface OPAOutsideFinancialInterestRepository extends JpaRepository<OPAOutsideFinancialInterestEntity, Integer>{

	@Query(value = "SELECT * FROM OPA_OUTSIDE_FIN_INTERESTS WHERE OPA_DISCLOSURE_ID = ?1", nativeQuery = true)
	List<OPAOutsideFinancialInterestEntity> fetchAllByOPADisclosureID(Integer opaDisclosure);
	
}
