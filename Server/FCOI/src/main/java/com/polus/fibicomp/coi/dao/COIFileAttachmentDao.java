package com.polus.fibicomp.coi.dao;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.orm.hibernate5.HibernateTemplate;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.coi.dto.COIFileRequestDto;
import com.polus.fibicomp.coi.exception.COIFileAttachmentException;
import com.polus.fibicomp.coi.pojo.DisclAttachment;
import com.polus.fibicomp.coi.repository.COIDisclosureAttachmentRepository;

@Component
public class COIFileAttachmentDao {

	@Autowired
	COIDisclosureAttachmentRepository coiAttachmentRepository;
	
	@Autowired
	private HibernateTemplate hibernateTemplate;
    
    @Transactional(rollbackFor = {COIFileAttachmentException.class})
	public void saveDisclosureAttachmentDetail(COIFileRequestDto request) {
	
		try {
			
			hibernateTemplate.saveOrUpdate(DisclAttachment.builder()
					   .attachmentNumber(getNextAttachmentNumber())
					   .versionNumber(1)
					   .attaStatusCode(request.getAttaStatusCode())
					   .attaTypeCode(request.getAttaTypeCode())
					   .commentId(request.getCommentId())
					   .componentReferenceId(request.getComponentReferenceId())
					   .componentReferenceNumber(request.getComponentReferenceNumber())
					   .componentTypeCode(request.getComponentTypeCode())
					   .fileDataId(request.getFileDataId())
					   .fileName(request.getFile().getName())
					   .mimeType(request.getFile().getContentType())
					   .description(request.getAttaStatusCode())
					   .documentOwnerPersonId(request.getDocumentOwnerPersonId())
					   .updateUser(null)
					   .updateTimestamp(null)
					   .build());
			
		}catch(Exception e) {
			throw new COIFileAttachmentException("Error at saveDisclosureAttachmentDetail in COIFileAttachmentDao" + e.getMessage());
		}	
		
		
	} 
    
    private Integer getNextAttachmentNumber() {
		
		return 1;
	}    

}
