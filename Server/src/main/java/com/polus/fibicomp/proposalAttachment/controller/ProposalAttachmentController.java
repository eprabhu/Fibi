package com.polus.fibicomp.proposalAttachment.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.proposalAttachment.service.ProposalAttachmentService;
import com.polus.fibicomp.proposalAttachment.vo.ProposalAttachmentVO;

@RestController
public class ProposalAttachmentController {

	protected static Logger logger = LogManager.getLogger(ProposalAttachmentController.class.getName());

	@Autowired
	ProposalAttachmentService ProposalAttachmentService;

	@RequestMapping(value = "/fileUploads.pdf", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String uploadMedia(@RequestBody ProposalAttachmentVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for file upload");
		String fileContent = vo.getFileContent();
		String fileName = vo.getFileName();
		Integer remaining = vo.getRemaining();
		Integer length = vo.getLength();
		Integer moduleCode = vo.getModuleCode();
		Long moduleItemKey = vo.getModuleItemKey();
		Long userId = vo.getUserId();
		String contentType = vo.getContentType();
		logger.info("fileContent : " + fileContent);
		logger.info("fileName : " + fileName);
		logger.info("remaining : " + remaining);
		logger.info("length : " + length);
		logger.info("moduleCode : " + moduleCode);
		logger.info("moduleItemKey : " + moduleItemKey);
		logger.info("userId : " + userId);
		logger.info("contentType : " + contentType);
		return ProposalAttachmentService.uploadMedia(fileContent, fileName, remaining, length, moduleCode, moduleItemKey, userId, contentType);
	}

	@RequestMapping(value = "/fileUpload.pdf", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String uploadMedia( @RequestParam(required = true, value = "file") MultipartFile file,
            @RequestParam(required = false, value = "name") String name) {
		logger.info("Requesting for file upload");
		String fileName = file.getName();
		return ProposalAttachmentService.uploadMedia(file, fileName);
	}
}
