package com.polus.formbuilder.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.stereotype.Repository;

import com.polus.formbuilder.entity.FormBuilderUsageEntity;

@Repository
@EnableJpaRepositories
public interface FormBuilderUsageEntityRepository extends  JpaRepository<FormBuilderUsageEntity, Integer> {

	@Query(value = "SELECT * FROM FORM_BUILDER_USAGE WHERE FORM_BUILDER_ID = ?1 ORDER BY FORM_ORDER_NUMBER", nativeQuery = true)
	List<FormBuilderUsageEntity> fetchByFormId(Integer formId);

}
