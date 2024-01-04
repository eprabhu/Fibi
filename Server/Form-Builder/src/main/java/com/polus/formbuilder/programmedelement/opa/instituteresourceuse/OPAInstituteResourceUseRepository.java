package com.polus.formbuilder.programmedelement.opa.instituteresourceuse;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

@Repository
public interface OPAInstituteResourceUseRepository extends JpaRepository<OPAInstituteResourceUseEntity, Integer>{

	@Query(value = "SELECT * FROM OPA_INSTITUTE_RESOURCE_USE WHERE OPA_DISCLOSURE_ID = ?1", nativeQuery = true)
	List<OPAInstituteResourceUseEntity> fetchAllByOPADisclosureID(Integer opaDisclosureId);

}
