package com.polus.fibicomp.support.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.prereview.vo.PreReviewVO;
import com.polus.fibicomp.support.service.SupportService;

@RestController
public class SupportController {

	protected static Logger logger = LogManager.getLogger(SupportController.class.getName());

	@Autowired
	@Qualifier(value = "supportService")
	private SupportService supportService;

	@RequestMapping(value = "/loadSupportQuestions", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String loadSupportQuestion(@RequestBody PreReviewVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Load Support Questions");
		return supportService.loadSupportQuestion(vo);
	}

	@RequestMapping(value = "/createSupportQuestion", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String createSupportQuestion(@RequestBody PreReviewVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for Support Question");
		return supportService.createSupportQuestion(vo);
	}

	@RequestMapping(value = "/addSupportComment", method = RequestMethod.POST)
	public String addPreSupportComment(@RequestParam(value = "files", required = false) MultipartFile[] files, @RequestParam("formDataJson") String formDataJson) {
		logger.info("Requesting for add Support Comment");
		return supportService.addSupportComment(files, formDataJson);
	}

	@RequestMapping(value = "/showUnansweredQuestions", method = RequestMethod.POST)
	public String showUnansweredQuestions(@RequestBody PreReviewVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for showUnansweredQuestions");
		return supportService.showUnansweredQuestions(vo);
	}

	@RequestMapping(value = "/downloadSupportAttachment", method = RequestMethod.GET)
	public ResponseEntity<byte[]> downloadPreReviewAttachment(HttpServletResponse response, @RequestHeader("attachmentId") String attachmentId) {
		logger.info("Requesting for downloadPreReviewAttachment");
		logger.info("attachmentId : " + attachmentId);
		Integer attachmentid = Integer.parseInt(attachmentId);
		return supportService.downloadSupportAttachment(attachmentid);
	}

	@RequestMapping(value = "/addSupportCommentForWaf", method = RequestMethod.POST)
	public String addPreSupportCommentForWaf(@RequestParam(value = "files", required = false) MultipartFile file, @RequestParam("formDataJson") String formDataJson) {
		logger.info("Requesting for add Support Comment");
		return supportService.addPreSupportCommentForWaf(file, formDataJson);
	}

}
