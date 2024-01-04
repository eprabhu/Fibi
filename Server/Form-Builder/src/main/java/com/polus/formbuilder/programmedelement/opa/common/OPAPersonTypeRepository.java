package com.polus.formbuilder.programmedelement.opa.common;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

@Repository
public interface OPAPersonTypeRepository extends JpaRepository<OPAPersonType, Integer>{
	 
	 @Query(value = "SELECT DESCRIPTION FROM OPA_PERSON_TYPE WHERE OPA_PERSON_TYPE_CODE = ?1", nativeQuery = true)
	 String fetchTypeByPersonTypeCode(String opaPersonTypeCode);
		
}
