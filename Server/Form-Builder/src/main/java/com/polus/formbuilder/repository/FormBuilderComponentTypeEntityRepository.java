package com.polus.formbuilder.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

import com.polus.formbuilder.entity.FormBuilderComponentTypeEntity;

@Repository
public interface FormBuilderComponentTypeEntityRepository  extends JpaRepository<FormBuilderComponentTypeEntity, String> {

	@Query(value = "SELECT * FROM FORM_SECTION_COMPONENT_TYPE WHERE IS_ACTIVE = 'Y' ORDER BY SORT_ORDER ASC", nativeQuery = true)
	List<FormBuilderComponentTypeEntity> getAllActiveComponentTypes();

}
