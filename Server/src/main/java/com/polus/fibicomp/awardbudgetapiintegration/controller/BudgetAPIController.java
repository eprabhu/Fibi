package com.polus.fibicomp.awardbudgetapiintegration.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.ResponseEntity;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibicomp.awardbudgetapiintegration.service.BudgetAPIService;
import com.polus.fibicomp.awardbudgetapiintegration.vo.AwardBudgetPrintVO;
import com.polus.fibicomp.awardbudgetapiintegration.vo.BudgetAPIVO;

@RestController
public class BudgetAPIController {

	protected static Logger logger = LogManager.getLogger(BudgetAPIController.class.getName());

	@Autowired
	@Qualifier(value = "budgetAPIService")
	private BudgetAPIService budgetAPIService;

	@PostMapping(value = "/fetchBudgetAPIData")
	public String fetchBudgetAPIData(@RequestBody BudgetAPIVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for fetchBudgetAPIData");
		logger.info("year : {}", vo.getYear());
		logger.info("projectNumber : {}", vo.getProjectNumber());
		return budgetAPIService.fetchBudgetAPIResponseBasedOnParams(vo);
	}

	@PostMapping(value = "/generateAwardBudgetIntegrationReport")
	public ResponseEntity<byte[]> generateAwardBudgetIntegrationReport(HttpServletResponse response, @RequestBody AwardBudgetPrintVO vo) throws Exception {
		logger.info("Requesting for generateAwardBudgetIntegrationReport");
		logger.info("awardId : {}", vo.getAwardId());
		logger.info("ProjectNumber : {}", vo.getProjectNumber());
		logger.info("Year : {}", vo.getYear());
		return budgetAPIService.generateAwardBudgetIntegrationReport(response, vo);
	}

	@GetMapping(value = "/ardpBudgetIntegration", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public void ardpBudgetIntegration() {
		logger.info("Requesting for ARDP Budget Integration API execution");
		budgetAPIService.fetchBudgetAPIResponse();
	}

}
