package com.polus.formbuilder.programmedelement.opa.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.jpa.repository.query.Procedure;
import org.springframework.stereotype.Repository;

import com.polus.formbuilder.programmedelement.opa.compuncomp.OPADisclPersonEntity;

@Repository
public interface OPADisclPersonEntityRepository extends JpaRepository<OPADisclPersonEntity, Integer>{

	 @Procedure(name = "PROC_SYNC_OPA_PER_ENTITY")
	 void syncOPAPerEntity(Integer opaDisclosureId, String updateUser);
	 
	 @Query(value = "SELECT * FROM OPA_DISCL_PERSON_ENTITY WHERE OPA_DISCLOSURE_ID = ?1 AND PERSON_ENTITY_ID = ?2", nativeQuery = true)
	 OPADisclPersonEntity FetchByPersonEntityId(Integer opaDisclosureId, Integer personEntityId);
		
}
