package com.polus.formbuilder.programmedelement.opa.studentsubordinateinvolvement;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

@Repository
public interface OPAStudentSubordinateInvolvementRepository extends JpaRepository<OPAStudentSubordinateInvolvementEntity, Integer>{

	@Query(value = "SELECT * FROM OPA_STUDENT_SUBORDINATE_INVOLVEMENT WHERE OPA_DISCLOSURE_ID = ?1", nativeQuery = true)
	List<OPAStudentSubordinateInvolvementEntity> fetchAllByOPADisclosureID(Integer opaDisclosureId);

}
