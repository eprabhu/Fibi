package com.polus.fibicomp.fastintegration.controller;

import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibicomp.fastintegration.pojo.SapCostCenter;
import com.polus.fibicomp.fastintegration.pojo.SapFundCenter;
import com.polus.fibicomp.fastintegration.pojo.SapGrantCode;
import com.polus.fibicomp.fastintegration.pojo.SapProfitCenter;
import com.polus.fibicomp.fastintegration.service.SapIntegrationService;
import com.polus.fibicomp.vo.CommonVO;

@RestController
public class SapIntegrationController {

	protected static Logger logger = LogManager.getLogger(SapIntegrationController.class.getName());

	@Autowired
	@Qualifier(value = "sapIntegrationService")
	private SapIntegrationService sapIntegrationService;

	private static final String SEARCH_STRING = "searchString : {}";

	@PostMapping(value = "/findProfitCenter")
	public List<SapProfitCenter> findProfitCenter(@RequestBody CommonVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for findProfitCenter");
		logger.info(SEARCH_STRING, vo.getSearchString());
		return sapIntegrationService.findProfitCenter(vo.getSearchString());
	}

	@PostMapping(value = "/findFundCenter")
	public List<SapFundCenter> findFundCenter(@RequestBody CommonVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for findFundCenter");
		logger.info(SEARCH_STRING, vo.getSearchString());
		return sapIntegrationService.findFundCenter(vo.getSearchString());
	}

	@PostMapping(value = "/findCostCenter")
	public List<SapCostCenter> findCostCenter(@RequestBody CommonVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for findCostCenter");
		logger.info(SEARCH_STRING, vo.getSearchString());
		return sapIntegrationService.findCostCenter(vo.getSearchString());
	}

	@PostMapping(value = "/findGrantCode")
	public List<SapGrantCode> findGrantCoder(@RequestBody CommonVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for findGrantCode");
		logger.info(SEARCH_STRING, vo.getSearchString());
		return sapIntegrationService.findGrantCode(vo.getSearchString());
	}
	
	@GetMapping(value = "/getProfitCenterApiDetails")
	public void getProfitCenterApiDetails(HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getProfitCenterApi Started");
		sapIntegrationService.getAllProfitCenterAPI();
		logger.info("getProfitCenterApi Completed");
	}
	
	@GetMapping(value = "/getFundCenterApiDetails")
	public void getFundCenterApiDetails(HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getFundCenterApiDetails Started");
		sapIntegrationService.getAllFundCenterAPI();
		logger.info("getFundCenterApiDetails Completed");
	}

	@GetMapping(value = "/getCostCenterApiDetails")
	public void getCostCenterApiDetails(HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getCostCenterApiDetails Started");
		sapIntegrationService.getAllCostCenterAPI();
		logger.info("getCostCenterApiDetails Completed");
	}
	
	@GetMapping(value = "/getGrantCodeApiDetails")
	public void getGrantCodeApiDetails(HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getGrantCodeApiDetails Started");
		sapIntegrationService.getAllGrantCodeFromAPI();
		logger.info("getGrantCodeApiDetails Completed");
	}
	
}
