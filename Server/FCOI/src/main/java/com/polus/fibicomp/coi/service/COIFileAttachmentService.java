package com.polus.fibicomp.coi.service;

import java.io.IOException;
import java.util.List;

import javax.servlet.http.HttpServletResponse;

import org.springframework.http.ResponseEntity;

import com.polus.fibicomp.coi.dto.COIFileRequestDto;
import com.polus.fibicomp.coi.pojo.DisclAttachment;
import com.polus.fibicomp.coi.pojo.Attachments;
import com.polus.fibicomp.coi.dto.AttachmentsDto;

public interface COIFileAttachmentService {

	String saveFileAttachment(COIFileRequestDto request);

	List<DisclAttachment> getDisclAttachByRefId(Integer refId);

	List<DisclAttachment> getDisclAttachByRefIdAndTypeCode(Integer refId, Integer typeCode);

	List<DisclAttachment> getDisclAttachByCommentId(Integer commentId);

	String updateDisclAttachment(COIFileRequestDto request);

	String deleteDisclAttachment(COIFileRequestDto request);

	ResponseEntity<byte[]> downloadDisclAttachment(Integer attachmentId);

	void exportAllDisclAttachments(COIFileRequestDto request, HttpServletResponse response) throws IOException;

	String updateDisclAttachmentDetails(COIFileRequestDto request);
	
	Attachments saveAttachment(AttachmentsDto request, String personId);

}
