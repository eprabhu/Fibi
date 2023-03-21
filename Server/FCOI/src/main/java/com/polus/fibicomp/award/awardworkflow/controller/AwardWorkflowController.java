package com.polus.fibicomp.award.awardworkflow.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibicomp.award.awardworkflow.service.AwardWorkflowService;
import com.polus.fibicomp.award.service.AwardConcurrentService;
import com.polus.fibicomp.award.vo.AwardVO;
import com.polus.fibicomp.security.AuthenticatedUser;

@RestController
public class AwardWorkflowController {

	protected static Logger logger = LogManager.getLogger(AwardWorkflowController.class.getName());

	@Autowired
	@Qualifier(value = "awardWorkflowService")
	private AwardWorkflowService awardWorkflowService;

	@Autowired
	private AwardConcurrentService awardConcurrentService;

	@PostMapping(value = "/submitAward", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String submitAward(@RequestBody AwardVO vo, HttpServletRequest request, HttpServletResponse response) throws Exception {
		logger.info("Requesting for submitAward");
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		vo.setUserName(AuthenticatedUser.getLoginUserName());
		vo.setUpdateUser(AuthenticatedUser.getLoginUserName());
		logger.info("awardId : {}", vo.getAwardId());
		logger.info("personId : {} ", vo.getPersonId());
		logger.info("username : {}", vo.getUserName());
		vo = awardWorkflowService.submitAward(vo);
		vo.setIsManpowerIntegrationRequired(true);
		return awardConcurrentService.getAwardDetails(vo);
	}

	@PostMapping(value = "/addAlternativeApprover", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String addAlternativeApprover(@RequestBody AwardVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for addAlternativeApprover");
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		logger.info("personId : {}", vo.getPersonId());
		return awardWorkflowService.addAlternativeApprover(vo);
	}

	@PostMapping(value = "/addSequentialStop", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String addAlternativeStop(@RequestBody AwardVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for addSequentialStop");
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		vo.setUpdateUser(AuthenticatedUser.getLoginUserName());
		logger.info("personId : {}", vo.getPersonId());
		logger.info("updateUser : {}", vo.getUpdateUser());
		return awardWorkflowService.addSequentialStop(vo);
	}

}
