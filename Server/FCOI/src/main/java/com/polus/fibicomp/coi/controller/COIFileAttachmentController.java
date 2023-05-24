package com.polus.fibicomp.coi.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RequestPart;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.coi.dto.COIFileRequestDto;
import com.polus.fibicomp.coi.service.COIFileAttachmentService;
import com.polus.fibicomp.coi.vo.ConflictOfInterestVO;

@RestController
@RequestMapping("coi/atta")
public class COIFileAttachmentController {

	@Autowired
	COIFileAttachmentService coiFileAttachmentService;
	
	@Value("${app.filemanagement.storage.type}")
	private String storageType;

	
	@PostMapping(value = "saveFile",
		     consumes = {MediaType.MULTIPART_FORM_DATA_VALUE},
		     produces = {MediaType.APPLICATION_JSON_VALUE})
	public ResponseEntity<String> saveFile(COIFileRequestDto request){
		System.out.println(storageType);
		//request.setFile(file);
		String response = coiFileAttachmentService.saveFileAttachment(request);		
		return new ResponseEntity<String>(response, HttpStatus.OK);
		
	}
}
