package com.polus.fibicomp.fastintegration.sapfeedmaintenance.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibicomp.fastintegration.sapfeedmaintenance.service.SapFeedMaintenanceService;
import com.polus.fibicomp.fastintegration.sapfeedmaintenance.vo.SapFeedMaintenanceVO;

@RestController
public class SapFeedMaintenanceController {

	protected static Logger logger = LogManager.getLogger(SapFeedMaintenanceController.class.getName());

	@Autowired
	@Qualifier(value = "sapFeedMaintenanceService")
	private SapFeedMaintenanceService sapFeedMaintenanceService;

	@GetMapping(value = "/fetchSapAwardFeedStatus", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String fetchSapAwardFeedStatus(HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for fetchSapAwardFeedStatus");
		return sapFeedMaintenanceService.fetchSapAwardFeedStatus();
	}

	@PostMapping(value = "/getBatchDetailDashboard", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getBatchDetailDashboard(@RequestBody SapFeedMaintenanceVO vo, HttpServletRequest request) {
		logger.info("Requesting for getBatchDetailDashboard");
		return sapFeedMaintenanceService.getBatchDetailDashboard(vo);
	}

	@PostMapping(value = "/getBatchHistoryDashboard", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getBatchHistoryDashboard(@RequestBody SapFeedMaintenanceVO vo, HttpServletRequest request) {
		logger.info("Requesting for getBatchHistoryDashboard");
		return sapFeedMaintenanceService.getBatchHistoryDashboard(vo);
	}

	@PostMapping(value = "/getSapAwardFeedDetails", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getSapAwardFeedDetails(@RequestBody SapFeedMaintenanceVO vo, HttpServletRequest request) {
		logger.info("Requesting for getSapAwardFeedDetails");
		return sapFeedMaintenanceService.getSapAwardFeedDetails(vo);
	}

	@PostMapping(value = "/updateFeedStatus", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String updateFeedStatus(@RequestBody SapFeedMaintenanceVO vo, HttpServletRequest request) {
		logger.info("Requesting for updateFeedStatus");
		return sapFeedMaintenanceService.updateFeedStatus(vo);
	}

	@PostMapping(value = "/reInterfaceSapAwardFeed", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String reInterfaceSapAwardFeed(@RequestBody SapFeedMaintenanceVO vo, HttpServletRequest request) {
		logger.info("Requesting for reInterfaceSapAwardFeed");
		return sapFeedMaintenanceService.reInterfaceSapAwardFeed(vo);
	}

	@PostMapping(value = "/exportSapGeneratedAttachments")
	public String exportSapGeneratedAttachments(@RequestBody SapFeedMaintenanceVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for exportSapGeneratedAttachments");
		return sapFeedMaintenanceService.exportSapGeneratedAttachments(vo, response);
	}

	@PostMapping(value = "/notifyPI", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String notifyPI(@RequestBody SapFeedMaintenanceVO vo, HttpServletRequest request) {
		logger.info("Requesting for notifyPI");
		return sapFeedMaintenanceService.notifyPI(vo);
	}

	@PostMapping(value = "/sapFeedReTrigger", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String sapFeedReTrigger(@RequestBody SapFeedMaintenanceVO vo, HttpServletRequest request) {
		logger.info("Requesting for sapFeedReTrigger");
		return sapFeedMaintenanceService.sapFeedReTrigger(vo);
	}

	@PostMapping(value = "/generateSapFeedReport")
	public ResponseEntity<byte[]> generateSapFeedReport(HttpServletResponse response, HttpServletRequest request, @RequestBody SapFeedMaintenanceVO vo) {
		logger.info("Requesting for generateSapFeedReport");
		return sapFeedMaintenanceService.generateSapFeedReport(response, vo);
	}

}
