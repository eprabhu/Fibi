package com.polus.fibicomp.globalentity.repository;

import java.util.List;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.polus.fibicomp.globalentity.pojo.EntityIndustryClassification;


@Repository
public interface EntityIndustryClassificationRepository extends JpaRepository<EntityIndustryClassification, Integer> {

		@Query(value = "SELECT * FROM ENTITY_INDUSTRY_CLASSIFICATION WHERE ENTITY_ID = :entityId ORDER BY UPDATE_TIMESTAMP DESC", nativeQuery = true)
		List<EntityIndustryClassification> findByEntityId(@Param("entityId") Integer entityId);

		@Modifying
	    @Query(value = "DELETE FROM ENTITY_INDUSTRY_CLASSIFICATION WHERE ENTITY_INDUSTRY_CLASS_ID = :entityIndustryClassId", nativeQuery = true)
	    void deleteByEntityIndustryClassId(@Param("entityIndustryClassId") Integer entityIndustryClassId);

}
