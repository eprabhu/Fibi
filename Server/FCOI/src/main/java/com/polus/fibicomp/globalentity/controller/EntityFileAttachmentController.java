package com.polus.fibicomp.globalentity.controller;

import java.io.IOException;
import java.util.List;

import javax.servlet.http.HttpServletResponse;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.globalentity.dto.EntityFileRequestDto;
import com.polus.fibicomp.globalentity.pojo.EntityAttachment;
import com.polus.fibicomp.globalentity.service.EntityFileAttachmentService;

@RestController
@RequestMapping("coi/entity/attachment")
public class EntityFileAttachmentController {

	@Autowired
	EntityFileAttachmentService entityFileAttachmentService;

	@PostMapping(value = "saveFile", consumes = {MediaType.MULTIPART_FORM_DATA_VALUE}, produces = {MediaType.APPLICATION_JSON_VALUE})
	public ResponseEntity<Object> saveOrUpdateAttachments(@RequestParam(value = "files", required = false) MultipartFile[] files, @RequestParam("formDataJson") String formDataJson) {
		return entityFileAttachmentService.saveFileAttachment(files, formDataJson);
	}

	@PostMapping(value = "/updateAttachmentDetails", produces = {MediaType.APPLICATION_JSON_VALUE})
	public ResponseEntity<String> updateDisclAttachmentDetails(@RequestBody EntityFileRequestDto request) {
		String response = entityFileAttachmentService.updateEntityAttachmentDetails(request);
		return new ResponseEntity<>(response, HttpStatus.OK);
	}

	@PostMapping(value = "/deleteAttachment", produces = {MediaType.APPLICATION_JSON_VALUE})
	public String deleteDisclAttachment(@RequestBody EntityFileRequestDto request) {
		String response = entityFileAttachmentService.deleteEntityAttachment(request);
		return response;
	}

	@GetMapping(value = "/downloadAttachment", produces = {MediaType.APPLICATION_JSON_VALUE})
	public ResponseEntity<byte[]> downloadDisclAttachment(HttpServletResponse response, @RequestHeader("attachmentId") Integer attachmentId) {
		return entityFileAttachmentService.downloadEntityAttachment(attachmentId);
	}

	@PostMapping(value = "/exportAllAttachments", produces = {MediaType.APPLICATION_JSON_VALUE})
	public void exportSelectedAttachments(@RequestBody EntityFileRequestDto request, HttpServletResponse response) throws IOException {
		entityFileAttachmentService.exportAllEntityAttachments(request, response);
	}

	@GetMapping("/getAttachmentsBySectionCode/{sectionCode}/{entityId}")
   	public List<EntityAttachment> getAttachmentsBySectionCode(@PathVariable("sectionCode") String sectionCode, @PathVariable("entityId") Integer entityId) {
   		return entityFileAttachmentService.getAttachmentsBySectionCode(sectionCode, entityId);
   	}

	@GetMapping("/getAttachmentsByEntityId/{entityId}")
   	public List<EntityAttachment> getAttachmentsByEntityId(@PathVariable("entityId") Integer entityId) {
   		return entityFileAttachmentService.getAttachmentsByEntityId(entityId);
   	}

}
