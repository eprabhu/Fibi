package com.polus.fibicomp.coi.controller;

import java.io.IOException;
import java.util.List;

import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibicomp.coi.dto.COIFileRequestDto;
import com.polus.fibicomp.coi.pojo.DisclAttachment;
import com.polus.fibicomp.coi.service.COIFileAttachmentService;
import com.polus.fibicomp.common.dao.CommonDao;

import io.swagger.v3.oas.annotations.Operation;

@RestController
@RequestMapping("coi/atta")
public class COIFileAttachmentController {

	@Autowired
	COIFileAttachmentService coiFileAttachmentService;

	@Autowired
	private CommonDao commonDao;

	@Value("${app.filemanagement.storage.type}")
	private String storageType;

	@Operation(description = "Consumes: description,attaStatusCode, attaTypeCode, commentId, componentReferenceId, componentReferenceNumber, componentTypeCode,documentOwnerPersonId, file")
	@PostMapping(value = "saveFile", consumes = {MediaType.MULTIPART_FORM_DATA_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
	public ResponseEntity<String> saveFile(COIFileRequestDto request){
		String response = coiFileAttachmentService.saveFileAttachment(request);		
		return new ResponseEntity<String>(response, HttpStatus.OK);
	}

	@Operation(description = "Consumes: refId")
	@GetMapping(value = "/getDisclAttachByRefId/{refId}", produces = {MediaType.APPLICATION_JSON_VALUE})
	public ResponseEntity<Object> getDisclAttachByRefId(@PathVariable(value = "refId", required = true) final Integer refId) {
		List<DisclAttachment> response = coiFileAttachmentService.getDisclAttachByRefId(refId);
		return new ResponseEntity<>(response , HttpStatus.OK);
	}

	@Operation(description = "Consumes: refId, typeCode")
	@GetMapping(value = "/getDisclAttachByRefIdAndTypeCode/{refId}/{typeCode}", produces = {MediaType.APPLICATION_JSON_VALUE})
	public ResponseEntity<Object> getDisclAttachByRefIdAndTypeCode(@PathVariable(value = "refId", required = true) final Integer refId, 
			@PathVariable(value = "typeCode", required = true) final Integer typeCode) {
		List<DisclAttachment> response = coiFileAttachmentService.getDisclAttachByRefIdAndTypeCode(refId,typeCode);
		return new ResponseEntity<>(response , HttpStatus.OK);
	}

	@Operation(summary = "Update disclosure attachment details with file", description = "Consumes: file, description, attaStatusCode, attaTypeCode, commentId, componentReferenceId, componentReferenceNumber, componentTypeCode, documentOwnerPersonId, attachmentNumber")
	@PostMapping(value = "/updateDisclAttachment", consumes = {MediaType.MULTIPART_FORM_DATA_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
	public ResponseEntity<String> updateDisclAttachment(COIFileRequestDto request) {
		String response = coiFileAttachmentService.updateDisclAttachment(request);
		return new ResponseEntity<String>(response, HttpStatus.OK);
	}

	@Operation(summary = "Update disclosure attachment details without file", description = "Consumes: attachmentId, fileDataId, description, attaStatusCode, attaTypeCode, commentId, componentReferenceId, componentReferenceNumber, componentTypeCode, documentOwnerPersonId, attachmentNumber")
	@PostMapping(value = "/updateDisclAttachmentDetails", produces = {MediaType.APPLICATION_JSON_VALUE})
	public ResponseEntity<String> updateDisclAttachmentDetails(@RequestBody COIFileRequestDto request) {
		String response = coiFileAttachmentService.updateDisclAttachmentDetails(request);
		return new ResponseEntity<String>(response, HttpStatus.OK);
	}

	@Operation(description ="Consumes: fileDataId, attachmentId") 
	@PostMapping(value = "/deleteDisclAttachment", produces = {MediaType.APPLICATION_JSON_VALUE})
	public String deleteDisclAttachment(@RequestBody COIFileRequestDto request) {
		String response = coiFileAttachmentService.deleteDisclAttachment(request);
		return commonDao.convertObjectToJSON(response);
	}

	@Operation(description ="Consumes: attachmentId")
	@GetMapping(value = "/downloadDisclAttachment", produces = {MediaType.APPLICATION_JSON_VALUE})
	public ResponseEntity<byte[]> downloadDisclAttachment(HttpServletResponse response, @RequestHeader("attachmentId") Integer attachmentId) {
		return coiFileAttachmentService.downloadDisclAttachment(attachmentId);
	}

	@Operation(description ="Consumes: attachmentIds, componentReferenceId")
	@PostMapping(value = "/exportAllDisclAttachments", produces = {MediaType.APPLICATION_JSON_VALUE})
	public void exportSelectedAttachments(@RequestBody COIFileRequestDto request, HttpServletResponse response) throws IOException {
		coiFileAttachmentService.exportAllDisclAttachments(request, response);
	}

}
