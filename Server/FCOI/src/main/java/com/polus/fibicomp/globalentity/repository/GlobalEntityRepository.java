package com.polus.fibicomp.globalentity.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.polus.fibicomp.globalentity.pojo.Entity;

@Repository
public interface GlobalEntityRepository extends JpaRepository<Entity, Integer> {

	@Query(value = "SELECT * FROM ENTITY WHERE ENTITY_ID = :entityId ORDER BY UPDATE_TIMESTAMP DESC", nativeQuery = true)
	Entity findByEntityId(@Param("entityId") Integer entityId);

	@Query(value = "SELECT COUNT(*) FROM ENTITY WHERE DUNS_NUMBER = :dunsNumber", nativeQuery = true)
	int isDunsNumberExists(@Param("dunsNumber") String dunsNumber);

	@Query(value = "SELECT COUNT(*) FROM ENTITY WHERE UEI_NUMBER = :ueiNumber", nativeQuery = true)
	int isUeiNumberExists(@Param("ueiNumber") String ueiNumber);

	@Query(value = "SELECT COUNT(*) FROM ENTITY WHERE CAGE_NUMBER = :cageNumber", nativeQuery = true)
	int isCageNumberExists(@Param("cageNumber") String cageNumber);

	@Query(value = "SELECT PRIMARY_NAME FROM ENTITY WHERE ENTITY_ID = :entityId", nativeQuery = true)
	String fetchEntityNameByEntityId(@Param("entityId") Integer entityId);

}
