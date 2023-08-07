package com.polus.fibicomp.coi.repository;

import com.polus.fibicomp.coi.pojo.EntityActionLog;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

@Repository
@Transactional
public interface EntityActionLogRepository extends JpaRepository<EntityActionLog, Integer>, ActionLogRepositoryCustom {
}
