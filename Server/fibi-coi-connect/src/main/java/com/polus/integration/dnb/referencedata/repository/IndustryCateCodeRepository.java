package com.polus.integration.dnb.referencedata.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Modifying;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import com.polus.integration.dnb.referencedata.entity.IndustryCategoryCode;

@Repository
public interface IndustryCateCodeRepository extends JpaRepository<IndustryCategoryCode, Integer> {
	
    @Transactional
    @Modifying
    @Query("DELETE FROM IndustryCategoryCode i WHERE i.industryCategoryTypeCode = :typeCode")
    void deleteByIndustryCategoryTypeCode(String typeCode);
}
