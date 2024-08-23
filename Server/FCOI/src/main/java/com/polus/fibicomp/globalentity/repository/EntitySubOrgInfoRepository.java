package com.polus.fibicomp.globalentity.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.polus.fibicomp.globalentity.pojo.EntitySubOrgInfo;

@Repository
public interface EntitySubOrgInfoRepository extends JpaRepository<EntitySubOrgInfo, Integer> {

	@Query(value = "SELECT * FROM ENTITY_SUB_ORG_INFO WHERE ENTITY_ID = :entityId ORDER BY UPDATE_TIMESTAMP DESC", nativeQuery = true)
	EntitySubOrgInfo findByEntityId(@Param("entityId") Integer entityId);

	@Modifying
    @Query(value = "DELETE FROM ENTITY_SUB_ORG_INFO WHERE ID = :id", nativeQuery = true)
    void deleteByEntitySubOrgInfoId(@Param("id") Integer id);

}
