package com.polus.repository;


import com.polus.entity.Unit;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;

@Repository
public interface UnitRepository extends JpaRepository<Unit,Integer> {

    @Query("SELECT u FROM Unit u WHERE u.parentUnitNumber IS NULL")
    Unit findRootUnit();
}
