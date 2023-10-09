package com.polus.formbuilder.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.polus.formbuilder.entity.FormBuilderSectionComponentEntity;

@Repository
public interface FormBuilderSectionComponentEntityRepository  extends JpaRepository<FormBuilderSectionComponentEntity, Integer> {

	@Query(value = "SELECT * FROM FORM_BUILDER_SECTION_COMPONENT WHERE FORM_BUILDER_SECTION_ID = ?1", nativeQuery = true)
	List<FormBuilderSectionComponentEntity> getAllComponentBySection(Integer formId);

	
}
