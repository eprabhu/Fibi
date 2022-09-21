package com.polus.fibicomp.notification.email.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.notification.email.service.EmailMaintenanceService;
import com.polus.fibicomp.notification.email.service.EmailService;
import com.polus.fibicomp.notification.email.vo.EmailMaintenanceVO;
import com.polus.fibicomp.notification.email.vo.EmailServiceVO;

import io.jsonwebtoken.Claims;

@RestController
public class EmailController {

	protected static Logger logger = LogManager.getLogger(EmailController.class.getName());

	@Autowired
	@Qualifier(value = "emailMaintenanceService")
	private EmailMaintenanceService emailMaintenanceService;

	@Autowired
	private EmailService emailService;

	@Autowired
	public CommonDao commonDao;

	@Autowired
	private CommonService commonService;

	@PostMapping(value = "/createNotification", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String createNotification(@RequestBody EmailMaintenanceVO vo, HttpServletRequest request,
			HttpServletResponse response) {
		logger.info("Requesting for createNotification");
		return emailMaintenanceService.saveOrUpdateNotification(vo);
	}

	@PostMapping(value = "/modifyNotification", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String modifyNotification(@RequestBody EmailMaintenanceVO vo, HttpServletRequest request,
			HttpServletResponse response) {
		logger.info("Request for modifyNotification");
		return emailMaintenanceService.saveOrUpdateNotification(vo);
	}

	@PostMapping(value = "/getNotifications", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getNotifications(@RequestBody EmailMaintenanceVO vo, HttpServletRequest request,
			HttpServletResponse response) {
		logger.info("Requesting for getNotifications");
		return emailMaintenanceService.getAllNotification(vo);
	}

	@PostMapping(value = "/getNotificationById", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getNotificationById(@RequestBody EmailMaintenanceVO vo, HttpServletRequest request,
			HttpServletResponse response) {
		logger.info("Request for getNotificationById");
		return emailMaintenanceService.getNotificationById(vo);
	}

	@PostMapping(value = "/removeRecipientById", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String removeRecipientById(@RequestBody EmailMaintenanceVO vo, HttpServletRequest request,
			HttpServletResponse response) {
		logger.info("Request for removeNotificationById");
		return emailMaintenanceService.removeNotificationRecipient(vo);
	}

	@PostMapping(value = "/removeNotificationById", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String removeNotificationById(@RequestBody EmailMaintenanceVO vo, HttpServletRequest request,
			HttpServletResponse response) {
		logger.info("Request for removeNotificationById");
		return emailMaintenanceService.removeNotificationById(vo);
	}

	@PostMapping(value = "/sendMailAtPrompt", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String sendMailAtPrompt(@RequestBody EmailServiceVO vo, HttpServletRequest request,
			HttpServletResponse response) {
		logger.info("Request for send Email after prompt");
		emailService.sendEmail(vo);
		return commonDao.convertToJSON("Mail sent successfully");
	}
	
	@GetMapping(value = "/fetchUserNotification", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String fetchUserNotification(HttpServletRequest request, HttpServletResponse response) {
		logger.info("Request for fetchUserNotification");
		Claims claims = commonService.getLoginPersonDetailFromJWT(request);
		String personId = claims.get(Constants.LOGIN_PERSON_ID).toString();
		logger.info("personId : {}", personId);
		return emailMaintenanceService.fetchUserNotification(personId);
	}

	@PostMapping(value = "/activateOrDeactivateUserNotification", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deactivateUserNotification(@RequestBody EmailMaintenanceVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Request for activateOrDeactivateUserNotification");
		logger.info("notificationTypeCode : {}", vo.getNotificationTypeCode());
		return emailMaintenanceService.activateOrDeactivateUserNotification(vo);
	}

}
