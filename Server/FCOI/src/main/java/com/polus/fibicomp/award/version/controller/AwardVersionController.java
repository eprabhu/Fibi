package com.polus.fibicomp.award.version.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.award.version.service.AwardVersionService;
import com.polus.fibicomp.award.vo.AwardLinkInstituteProposalVO;
import com.polus.fibicomp.award.vo.AwardVO;
import com.polus.fibicomp.security.AuthenticatedUser;

@RestController
public class AwardVersionController {

	protected static Logger logger = LogManager.getLogger(AwardVersionController.class.getName());

	@Autowired
	private AwardVersionService awardVersionService;

	@PostMapping(value = "/createAwardVariationRequest", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String createAwardVariationRequest(@RequestParam(value = "files", required = false) MultipartFile[] files, @RequestParam("formDataJson") String formDataJson, HttpServletRequest request) {
		logger.info("Requesting for createAwardVariationRequest");
		return awardVersionService.createAwardVariationRequest(files, formDataJson, request);
	}

	@PostMapping(value = "/createAwardFromProposal", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public void createAwardFromProposal(@RequestBody AwardLinkInstituteProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for createAwardFromProposal");
		vo.setUpdateUser(AuthenticatedUser.getLoginUserName());
		awardVersionService.createAwardFromProposal(vo);
	}

	@PostMapping(value = "/copyAward", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String createAwardFromProposal(@RequestBody AwardVO awardVO, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for copyAward");
		return awardVersionService.copyAward(awardVO);
	}

	@PostMapping(value = "/createAwardVariationRequestForWaf", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String createAwardVariationRequestForWaf(@RequestBody AwardVO awardVO, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for createAwardVariationRequestForWaf");
		awardVO.setUserFullName(AuthenticatedUser.getLoginUserFullName());
		awardVO.setUpdateUser(AuthenticatedUser.getLoginUserName());
		awardVO.setUserName(AuthenticatedUser.getLoginUserName());
		awardVO.setPersonId(AuthenticatedUser.getLoginPersonId());
		logger.info("login personId : {}", awardVO.getPersonId());
		logger.info("userFullName : {}", awardVO.getUserFullName());
		logger.info("updateUser : {}", awardVO.getUpdateUser());
		logger.info("userName : {}", awardVO.getUserName());
		return awardVersionService.createAwardVariationRequestForWaf(awardVO);
	}

}
