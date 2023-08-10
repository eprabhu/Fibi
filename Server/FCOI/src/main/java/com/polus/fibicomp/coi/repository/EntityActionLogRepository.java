package com.polus.fibicomp.coi.repository;

import com.polus.fibicomp.coi.pojo.EntityActionLog;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.repository.CrudRepository;
import org.springframework.stereotype.Repository;

import javax.transaction.Transactional;

@Repository
@Transactional
public interface EntityActionLogRepository extends JpaRepository<EntityActionLog, Integer> {
}
