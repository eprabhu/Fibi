package com.polus.fibicomp.integration.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibicomp.integration.service.IntegrationService;
import com.polus.fibicomp.integration.vo.IntegrationVO;

@RestController
public class IntegrationController {

	@Autowired
	private IntegrationService integrationService;

	protected static Logger logger = LogManager.getLogger(IntegrationController.class.getName());

	@RequestMapping(value = "/addAwardHoursLogRT", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String addAwardHoursLogRT(@RequestBody IntegrationVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for addAwardHoursLogRT");
		return integrationService.addAwardHoursLogRT(vo);
	}

	@RequestMapping(value = "/addExpenseTrackerRT", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String addExpenseTrackerRT(@RequestBody IntegrationVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for addExpenseTrackerRT");
		return integrationService.addExpenseTrackerRT(vo);
	}

	@RequestMapping(value = "/addPublication", method = RequestMethod.POST, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String addPublication(@RequestBody IntegrationVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for addPublication");
		return integrationService.addPublication(vo);
	}

	@RequestMapping(value = "/deleteAwardHoursLogRT", method = RequestMethod.GET)
	public String deleteAwardHoursLogRT(HttpServletResponse response) throws Exception {
		logger.info("Requesting for deleteAwardHoursLogRT");
		return integrationService.deleteAwardHoursLogRT();
	}

	@RequestMapping(value = "/deleteExpenseTrackerRT", method = RequestMethod.GET)
	public String deleteExpenseTrackerRT(HttpServletResponse response) throws Exception {
		logger.info("Requesting for deleteExpenseTrackerRT");
		return integrationService.deleteExpenseTrackerRT();
	}

	@PostMapping(value = "/feedAward", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String seedAward(@RequestBody IntegrationVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for feedAward");
		return integrationService.seedAward(vo);
	}

	@PostMapping(value = "/feedProposal", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String feedProposal(@RequestBody IntegrationVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for feedProposal");
		return integrationService.feedProposal(vo);
	}

}
