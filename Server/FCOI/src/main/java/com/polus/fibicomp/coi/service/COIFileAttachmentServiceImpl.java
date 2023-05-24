package com.polus.fibicomp.coi.service;

import java.io.IOException;
import java.io.OutputStream;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.coi.dao.COIFileAttachmentDao;
import com.polus.fibicomp.coi.dto.COIFileRequestDto;
import com.polus.fibicomp.coi.dto.COIFileResponseDto;
import com.polus.fibicomp.coi.exception.COIFileAttachmentException;
import com.polus.fibicomp.filemanagement.FileManagementOutputDto;
import com.polus.fibicomp.filemanagement.FileManagementService;
import com.polus.fibicomp.filemanagement.FileManagmentInputDto;
import com.polus.fibicomp.filemanagement.FileStorageException;
import com.polus.fibicomp.filemanagement.FileSystemFileStorageService;

@Service
public class COIFileAttachmentServiceImpl implements COIFileAttachmentService {

	@Autowired
	FileManagementService fileManagementService;
	
	@Autowired
	COIFileAttachmentDao coiFileAttachmentDao;
	
	private static final String COI_MODULE_CODE = "15";

    public COIFileAttachmentServiceImpl(FileManagementService fileManagementService) {
        this.fileManagementService = fileManagementService;
    }
   
	@Override
	@Transactional(rollbackFor = {COIFileAttachmentException.class, IOException.class})
	public String saveFileAttachment(COIFileRequestDto request) {
			
	try {
	
		FileManagmentInputDto input = FileManagmentInputDto.builder()
														   .file(request.getFile())	
														   .moduleCode(COI_MODULE_CODE)
														   .moduleNumber(request.getComponentReferenceNumber())
														   .build();
		
		FileManagementOutputDto fileOutput =  fileManagementService.saveFile(input);
		
		request.setFileDataId(fileOutput.getFileDataId());
				
			
		coiFileAttachmentDao.saveDisclosureAttachmentDetail(request);
		
		}catch(Exception e) {
			throw new COIFileAttachmentException("Exception in saveFileAttachment in COIFileAttachmentService, "+ e);
		}
		//AuthenticatedUser.getLoginUserName()
		
		return "SUCCESS";
	}

	@Override
	public OutputStream downloadFileAttachment(String fileDataId) {
		// TODO Auto-generated method stub
		return null;
	}

	@Override
	public COIFileResponseDto getAttachmentDetailsForComponentId(String componentType, String componentId) {
		// TODO Auto-generated method stub
		return null;
	}

}
