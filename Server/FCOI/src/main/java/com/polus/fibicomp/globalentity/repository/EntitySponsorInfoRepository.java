package com.polus.fibicomp.globalentity.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.polus.fibicomp.globalentity.pojo.EntitySponsorInfo;

@Repository
public interface EntitySponsorInfoRepository extends JpaRepository<EntitySponsorInfo, Integer> {

	@Query(value = "SELECT * FROM ENTITY_SPONSOR_INFO WHERE ENTITY_ID = :entityId ORDER BY UPDATE_TIMESTAMP DESC", nativeQuery = true)
	EntitySponsorInfo findByEntityId(@Param("entityId") Integer entityId);

	@Modifying
    @Query(value = "DELETE FROM ENTITY_SPONSOR_INFO WHERE ID = :id", nativeQuery = true)
    void deleteByEntitySponsorInfoId(@Param("id") Integer id);

}
