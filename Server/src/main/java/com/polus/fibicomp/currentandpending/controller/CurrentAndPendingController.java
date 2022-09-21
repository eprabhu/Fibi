package com.polus.fibicomp.currentandpending.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;
import com.polus.fibicomp.currentandpending.service.CurrentAndPendingService;
import com.polus.fibicomp.currentandpending.vo.CurrentAndPendingVO;

@RestController
public class CurrentAndPendingController {

	protected static Logger logger = LogManager.getLogger(CurrentAndPendingController.class.getName());

	@Autowired
	private CurrentAndPendingService currentAndPendingService;

	@PostMapping(value = "/getPersonList", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getPersonList(@RequestBody CurrentAndPendingVO currentAndPendingVO, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getPersonList");
		return currentAndPendingService.getPersonList(currentAndPendingVO);
	}

	@PostMapping(value = "/getCPDetailsForSelectedPersons", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getCurrentAndPendingList(@RequestBody CurrentAndPendingVO currentAndPendingVO, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getCPDetailsForSelectedPersons");
		return currentAndPendingService.getCPDetailsForSelectedPersons(currentAndPendingVO);
	}

	@PostMapping(value = "/saveOrUpdateCPReportExtDetail", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveOrUpdateCPReportExtDetail(@RequestBody CurrentAndPendingVO currentAndPendingVO, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveOrUpdateCPReportDetailExt");
		return currentAndPendingService.saveOrUpdateCPReportExtDetail(currentAndPendingVO);
	}

	@PostMapping(value = "/excludeCurrentAndPendingDetails", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String excludeCurrentAndPendingDetails(@RequestBody CurrentAndPendingVO currentAndPendingVO, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for excludeCurrentAndPendingDetails");
		return currentAndPendingService.excludeCurrentAndPendingDetails(currentAndPendingVO);
	}

	@PostMapping(value = "/getCurrentAndPendingList", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getCurrentAndPendingDetails(@RequestBody CurrentAndPendingVO currentAndPendingVO, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getCurrentAndPendingList");
		return currentAndPendingService.getCurrentAndPendingDetails(currentAndPendingVO);
	}

	@PostMapping(value = "/saveOrUpdateCPExternalProjectDetail", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveOrUpdateCPExternalProjectDetail(@RequestBody CurrentAndPendingVO currentAndPendingVO, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveOrUpdateCPExternalProjectDetail");
		return currentAndPendingService.saveOrUpdateCPExternalProjectDetail(currentAndPendingVO);
	}

	@PostMapping(value = "/deleteCPExternalProjectDetail", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteCPExternalProjectDetail(@RequestBody CurrentAndPendingVO currentAndPendingVO, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteCPExternalProjectDetail");
		return currentAndPendingService.deleteCPExternalProjectDetail(currentAndPendingVO);
	}

}
