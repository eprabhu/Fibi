package com.polus.fibicomp.claims.claimsIntegration.manpowerbasesalary.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibicomp.claims.claimsIntegration.manpowerbasesalary.service.BaseSalaryService;

@RestController
public class BaseSalaryController {

	protected static Logger logger = LogManager.getLogger(BaseSalaryController.class.getName());

	@Autowired
	private BaseSalaryService baseSalaryService;

	@GetMapping(value = "/manpowerBaseSalary", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public void manpowerBaseSalary(HttpServletRequest request, HttpServletResponse response) throws Exception {
		logger.info("Request for ManpowerBaseSalary");
		baseSalaryService.manpowerBaseSalary();
		logger.info("ManpowerBaseSalary completed");
	}
}
