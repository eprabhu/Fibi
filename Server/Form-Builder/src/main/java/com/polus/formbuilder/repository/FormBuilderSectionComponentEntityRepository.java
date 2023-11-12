package com.polus.formbuilder.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.polus.formbuilder.entity.FormBuilderSectionComponentEntity;
import com.polus.formbuilder.entity.FormBuilderSectionEntity;

@Repository
public interface FormBuilderSectionComponentEntityRepository  extends JpaRepository<FormBuilderSectionComponentEntity, Integer> {

	@Query(value = "SELECT * FROM FORM_BUILDER_SECTION_COMPONENT WHERE FORM_BUILDER_SECTION_ID = ?1", nativeQuery = true)
	List<FormBuilderSectionComponentEntity> getAllComponentBySection(Integer sectionId);

	@Query(value = "SELECT t1.* FROM FORM_BUILDER_SECTION_COMPONENT t1 "
			+ " INNER JOIN FORM_BUILDER_SECTION t2 ON t1.FORM_BUILDER_SECTION_ID = t2.FORM_BUILDER_SECTION_ID"
			+ " WHERE t2.FORM_BUILDER_ID = ?1 ORDER BY t1.COMPONENT_ORDER_NUMBER", nativeQuery = true)
	List<FormBuilderSectionComponentEntity> fetchByFormId(Integer formId);

	
}
