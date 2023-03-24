package com.polus.fibicomp.person.timesheetdetail.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibicomp.person.timesheetdetail.service.TimesheetService;
import com.polus.fibicomp.person.timesheetdetail.vo.TimesheetVO;

@RestController
public class TimesheetController {

	protected static Logger logger = LogManager.getLogger(TimesheetController.class.getName());

	@Autowired
	public TimesheetService timesheetService;

	@PostMapping(value = "/loadAwardTimesheetByPersonId", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String loadAwardTimesheetByPersonId(@RequestBody TimesheetVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Request for loadAwardTimesheetByPersonId");
		return timesheetService.loadAwardTimesheetByPersonId(vo);
	}

	@PostMapping(value = "/saveOrUpdateAwardKeyPersonTimesheet", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveOrUpdateAwardKeyPersonTimesheet(@RequestBody TimesheetVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveOrUpdateAwardKeyPersonTimesheet");
		return timesheetService.saveOrUpdateAwardKeyPersonTimesheet(vo);
	}

	@PostMapping(value = "/getAwardKeyPersonTimesheetDetails", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getAwardKeyPersonTimesheetDetails(@RequestBody TimesheetVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getAwardKeyPersonTimesheetDetails");
		return timesheetService.getAwardKeyPersonTimesheetDetails(vo);
	}

}
