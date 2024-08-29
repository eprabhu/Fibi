package com.polus.integration.award.repository;



import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.jpa.repository.query.Procedure;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import com.polus.integration.award.pojo.COIIntegrationAward;

@Repository
public interface AwardRepository extends JpaRepository<COIIntegrationAward, Integer> {

	@Query("SELECT e FROM COIIntegrationAward e WHERE e.projectNumber = :projectNumber")
	COIIntegrationAward findProjectByProjectNumber(@Param("projectNumber") String projectNumber);

	@Procedure(name = "COI_SYNC_REMOVE_DEACTIVATED_PROJECTS")
	void COI_SYNC_REMOVE_DEACTIVATED_PROJECTS(@Param("AV_MODULE_CODE") Integer moduleCode, @Param("AV_MODULE_ITEM_KEY") String projectNumber);

}
