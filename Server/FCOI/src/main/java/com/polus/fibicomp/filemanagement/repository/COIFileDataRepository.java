package com.polus.fibicomp.filemanagement.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.polus.fibicomp.coi.pojo.DisclFileData;

@Repository
public interface COIFileDataRepository extends JpaRepository<DisclFileData, String>{

}
