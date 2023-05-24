package com.polus.fibicomp.coi.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;

import com.polus.fibicomp.coi.pojo.DisclAttachment;
import com.polus.fibicomp.coi.pojo.DisclFileData;

@Repository
public interface COIDisclosureAttachmentRepository extends JpaRepository<DisclAttachment, Integer>{

}
