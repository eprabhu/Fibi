package com.polus.fibicomp.claims.claimsIntegration.ics.controller;

import java.sql.Timestamp;
import java.time.LocalDate;
import java.util.Date;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibicomp.claims.claimsIntegration.ics.service.IcsService;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.vo.CommonVO;

@RestController
public class IcsController {

	protected static Logger logger = LogManager.getLogger(IcsController.class.getName());

	@Autowired
	private IcsService icsService;

	@Autowired
	CommonDao commonDao;

	@PostMapping(value = "/claimStudentTravelIntegration", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public void claimStudentTravelIntegration(@RequestBody CommonVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("claim student request for claimStudentTravelIntegration");
		logger.info("Start Date {}", vo.getProperty1());
		logger.info("End Date {}", vo.getProperty2());
		icsService.claimStudentTravelIntegration(vo.getProperty1(), vo.getProperty2());
		logger.info("sending ICS Integration Notification");
		logger.info("Claim integration completed");
	}

	@Scheduled(cron = "${claims.student.travel.api.schedule}", zone = Constants.CRON_JOB_TIMEZONE)
	public synchronized void claimStudentTravelIntegration() {
		logger.info("Scheduler request for claimStudentTravelIntegration");
		Date date = commonDao.adjustTimezone(new Date(commonDao.getCurrentTimestamp().getTime()));
		LocalDate startDate = new Timestamp(date.getTime()).toLocalDateTime().toLocalDate().plusDays(1);
		LocalDate endDate = startDate.plusDays(1);
		logger.info("Start Date {}", startDate);
		logger.info("End Date {}", endDate);
		icsService.claimStudentTravelIntegration(startDate.toString(), endDate.toString());
		logger.info("Claim integration completed");
	}

}
