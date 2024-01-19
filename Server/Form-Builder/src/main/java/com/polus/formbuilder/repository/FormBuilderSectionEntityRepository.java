package com.polus.formbuilder.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.polus.formbuilder.entity.FormBuilderSectionEntity;

@Repository
public interface FormBuilderSectionEntityRepository extends JpaRepository<FormBuilderSectionEntity, Integer> {

	@Query(value = "SELECT * FROM FORM_BUILDER_SECTION WHERE FORM_BUILDER_ID = ?1 AND IS_ACTIVE = 'Y' ORDER BY SECTION_ORDER_NUMBER", nativeQuery = true)
	List<FormBuilderSectionEntity> getSectionDetailsByFormId(Integer formId);
	
}
