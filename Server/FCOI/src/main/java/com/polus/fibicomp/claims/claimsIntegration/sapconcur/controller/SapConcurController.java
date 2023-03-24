package com.polus.fibicomp.claims.claimsIntegration.sapconcur.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;
import com.polus.fibicomp.claims.claimsIntegration.sapconcur.service.SapConcurService;

@RestController
public class SapConcurController {

	protected static Logger logger = LogManager.getLogger(SapConcurController.class.getName());

	@Autowired
	private SapConcurService sapConcurService;

	@GetMapping(value = "/sapConcur", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public void sapConcur(HttpServletRequest request, HttpServletResponse response) throws Exception {
		logger.info("Request for sapConcur");
		sapConcurService.sapConcurSftp();
		logger.info("sapConcur completed");
	}
}
