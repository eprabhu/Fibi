package com.polus.fibicomp.claims.claimsIntegration.excelity.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;
import com.polus.fibicomp.claims.claimsIntegration.excelity.service.ExcelityService;

@RestController
public class ExcelityController {

	protected static Logger logger = LogManager.getLogger(ExcelityController.class.getName());

	@Autowired
	private ExcelityService excelityService;

	@GetMapping(value = "/excelity", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public void excelity(HttpServletRequest request, HttpServletResponse response) throws Exception {
		logger.info("Request for Excelity");
		excelityService.manpowerSftpForExcelity();
		logger.info("Excelity completed");
	}
}
