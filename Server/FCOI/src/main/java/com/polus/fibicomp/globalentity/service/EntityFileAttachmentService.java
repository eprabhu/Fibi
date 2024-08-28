package com.polus.fibicomp.globalentity.service;

import java.io.IOException;
import java.util.List;

import javax.servlet.http.HttpServletResponse;

import org.springframework.http.ResponseEntity;
import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.globalentity.dto.EntityFileRequestDto;
import com.polus.fibicomp.globalentity.pojo.EntityAttachment;

public interface EntityFileAttachmentService {

	ResponseEntity<Object> saveFileAttachment(MultipartFile[] files, String formDataJSON);

	String deleteEntityAttachment(EntityFileRequestDto request);

	ResponseEntity<byte[]> downloadEntityAttachment(Integer attachmentId);

	void exportAllEntityAttachments(EntityFileRequestDto request, HttpServletResponse response) throws IOException;

	String updateEntityAttachmentDetails(EntityFileRequestDto request);

	List<EntityAttachment> getAttachmentsBySectionCode(String sectionCode, Integer entityId);

	List<EntityAttachment> getAttachmentsByEntityId(Integer entityId);

}
