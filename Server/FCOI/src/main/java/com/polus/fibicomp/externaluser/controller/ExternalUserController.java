package com.polus.fibicomp.externaluser.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;
import com.polus.fibicomp.externaluser.service.ExternalUserService;
import com.polus.fibicomp.externaluser.vo.HomeVo;
import com.polus.fibicomp.security.AuthenticatedUser;

@RestController
public class ExternalUserController {

	protected static Logger logger = LogManager.getLogger(ExternalUserController.class.getName());
	
	@Autowired
	private ExternalUserService externalUserService;
	
	@PostMapping(value = "/getExternalUserDetails", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getExternalUserDetails(@RequestBody HomeVo vo, HttpServletRequest request) {
		logger.info("Requesting for getExternalUserDetails");
		return externalUserService.fetchExternalUserDetails(vo);
	}
	
	@PostMapping(value = "/updateApproveReject")
	public String updateApproveReject(@RequestBody HomeVo vo, HttpServletRequest request) {
		logger.info("Requesting for saveApproveReject");
		logger.info(vo.getUserName());
		logger.info(vo.getEmailAddress());
		logger.info("User Name: {}", AuthenticatedUser.getLoginUserName());
		return externalUserService.updateFlagByPersonId(vo, AuthenticatedUser.getLoginUserName());
	}
	
	@GetMapping(value = "/getExternalUserFeed")
	public void getExternalUserFeed(HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getExternalUserFeed");
		externalUserService.getExternalUserFeedDetails();
	}
	
	@GetMapping(value = "/sendExternalUserApprovedMail")
	public void sendExternalUserApprovedMail(HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for sendExternalUserApprovedMail");
		externalUserService.externalUserApprovedMail();
	}
	
	@PostMapping(value = "/getExternalUserLoginDetails")
	public String getExternalUserLoginDetails(@RequestBody HomeVo vo,HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getExternalUserLoginDetails");
		return externalUserService.fetchExternalUserLoginDetails(vo);
	}
	
	@GetMapping(value = "/sendRegisteredExternalUserDetails")
	public void sendRegisteredExternalUserDetails(HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getExternalUserFeed");
		externalUserService.sendRegisteredExternalUser();
	}
}
