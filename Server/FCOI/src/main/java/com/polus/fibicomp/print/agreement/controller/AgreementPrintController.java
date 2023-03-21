package com.polus.fibicomp.print.agreement.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibicomp.agreements.vo.AgreementVO;
import com.polus.fibicomp.print.agreement.service.AgreementPrintService;

@RestController
public class AgreementPrintController {

	protected static Logger logger = LogManager.getLogger(AgreementPrintController.class.getName());

	@Autowired
	private AgreementPrintService agreementPrintService;

	@PostMapping(value = "/generateAgreementReport")
	public String generateAgreementReport(@RequestBody AgreementVO agreementVO, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for generateAgreementReport");
		return agreementPrintService.generateAgreementReport(agreementVO, response);
	}

	@PostMapping(value = "/previewAgreementDocument")
	public ResponseEntity<byte[]> previewAgreementDocument(@RequestBody AgreementVO agreementVO, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for previewAgreementDocument");
		return agreementPrintService.previewAgreementDocument(agreementVO);
	}

	@GetMapping(value = "/generateAgreementSummary")
	public ResponseEntity<byte[]> generateAgreementSummary(HttpServletResponse response,
			@RequestHeader(value = "agreementRequestId", required = true) Integer agreementRequestId,
			@RequestHeader(value = "personId", required = true) String personId,
			@RequestHeader(value = "userName", required = true) String userName) throws Exception {
		logger.info("Requesting for generateAgreementSummary");
		logger.info("agreementRequestId : " + agreementRequestId);
		logger.info("personId : " + personId);
		logger.info("userName : " + userName);
		return agreementPrintService.generateAgreementSummary(response, agreementRequestId, personId, userName);
	}

}
