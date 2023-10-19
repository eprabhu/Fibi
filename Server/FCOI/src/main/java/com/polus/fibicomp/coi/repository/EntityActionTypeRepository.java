package com.polus.fibicomp.coi.repository;

import com.polus.fibicomp.coi.pojo.EntityActionType;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.CrudRepository;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;

import javax.transaction.Transactional;

@Repository
@Transactional
public interface EntityActionTypeRepository extends CrudRepository<EntityActionType, String> {

//    @Query("SELECT at FROM EntityActionType at WHERE at.actionLogTypeCode = :actionLogTypeCode")
//    EntityActionType findByActionTypeCode(@Param("actionLogTypeCode") String actionLogTypeCode);
}
