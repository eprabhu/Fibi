package com.polus.fibicomp.coi.service;

import java.io.OutputStream;

import com.polus.fibicomp.coi.dto.COIFileRequestDto;
import com.polus.fibicomp.coi.dto.COIFileResponseDto;

public interface COIFileAttachmentService {

	String saveFileAttachment(COIFileRequestDto request);
	
	OutputStream downloadFileAttachment(String fileDataId);
	
	COIFileResponseDto getAttachmentDetailsForComponentId(String componentType, String componentId);
}
