package com.polus.formbuilder.programmedelement.opa.compuncomp;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.polus.formbuilder.entity.FormBuilderHeaderEntity;
import com.polus.formbuilder.programmedelement.opa.entity.OPADiscActivityEntity;

@Repository
public interface OPADiscActivityEntityRepository extends JpaRepository<OPADiscActivityEntity, Integer>{

	@Query(value = "SELECT * FROM OPA_DISCL_ACTIVITIES WHERE OPA_DISCLOSURE_ID = ?1", nativeQuery = true)
	List<OPADiscActivityEntity> fetchAllByOPADisclosureID(Integer opaDisclosure);
	
}
