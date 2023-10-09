package com.polus.formbuilder.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.polus.formbuilder.entity.FormBuilderProgElementEntity;

@Repository
public interface FormBuilderProgElementEntityRepository  extends JpaRepository<FormBuilderProgElementEntity, Integer> {

}
