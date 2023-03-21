package com.polus.fibicomp.faq.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.faq.dao.FaqDaoImpl;
import com.polus.fibicomp.faq.service.FaqService;
import com.polus.fibicomp.faq.vo.FaqCategoryVo;

@RestController
public class FaqController {

	protected static Logger logger = LogManager.getLogger(FaqDaoImpl.class.getName());

	@Autowired
	private FaqService faqService;

	@RequestMapping(value = "/saveFaq", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveFaq(@RequestBody FaqCategoryVo vo, HttpServletRequest request, HttpServletResponse response) {
		return faqService.saveUpdateFaq(vo);
	}

	@RequestMapping(value = "/fetchFaqDetails", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String loadcategoryCode(@RequestBody FaqCategoryVo vo, HttpServletRequest request,
			HttpServletResponse response) {
		return faqService.fetchFaqTable(vo);
	}

	@RequestMapping(value = "/addFaqAttachment", method = RequestMethod.POST)
	public String addFaqAttachment(@RequestParam(value = "files", required = false) MultipartFile[] files,
			@RequestParam("formDataJson") String formDataJson) {
		return faqService.addFaqAttachment(files, formDataJson);

	}

	@RequestMapping(value = "/downloadFaqAttachment", method = RequestMethod.GET)
	public ResponseEntity<byte[]> downloadFaqAttachment(HttpServletResponse response, @RequestHeader("faqAttachmentId") String faqAttachmentId) {
		Integer faqAttachmentid = Integer.parseInt(faqAttachmentId);
		return faqService.downloadFaqAttachment(faqAttachmentid);
	}

	@RequestMapping(value = "/deleteFaqAttachment", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteFaqAttachment(@RequestBody FaqCategoryVo vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteFaqAttachment");
		return faqService.deleteFaqAttachment(vo);
	}

	@RequestMapping(value = "/listFaqCategory", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String listFaqCategory(@RequestBody FaqCategoryVo vo, HttpServletRequest request,HttpServletResponse response) {
		return faqService.listFaqCategory(vo);
	}

	@RequestMapping(value = "/addFaqAttachmentForWaf", method = RequestMethod.POST)
	public String addFaqAttachmentForWaf(@RequestBody FaqCategoryVo vo, HttpServletRequest request,HttpServletResponse response) {
		logger.info("Requesting for addFaqAttachmentForWaf");
		return faqService.addFaqAttachmentForWaf(vo);
	}

}

