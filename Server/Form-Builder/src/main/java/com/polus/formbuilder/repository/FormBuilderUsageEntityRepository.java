package com.polus.formbuilder.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.config.EnableJpaRepositories;
import org.springframework.stereotype.Repository;

import com.polus.formbuilder.entity.FormBuilderUsageEntity;

@Repository
@EnableJpaRepositories
public interface FormBuilderUsageEntityRepository extends  JpaRepository<FormBuilderUsageEntity, Integer> {

}
