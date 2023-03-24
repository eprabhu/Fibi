package com.polus.fibicomp.award.awardreviewcomment.controller;

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

import com.polus.fibicomp.award.awardreviewcomment.service.AwardReviewCommentService;
import com.polus.fibicomp.award.awardreviewcomment.vo.AwardReviewCommentVO;
import com.polus.fibicomp.security.AuthenticatedUser;


@RestController
public class AwardReviewCommentController {

	protected static Logger logger = LogManager.getLogger(AwardReviewCommentController.class.getName());

	@Autowired
	@Qualifier(value = "awardReviewCommentsService")
	private AwardReviewCommentService awardReviewCommentService;

	@PostMapping(value = "/saveOrUpdateAwardReviewComment", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveOrUpdateAwardReviewComment(@RequestBody AwardReviewCommentVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveOrUpdateAwardReviewComment");
		return awardReviewCommentService.saveOrUpdateAwardReviewComment(vo);
	}

	@PostMapping(value = "/fetchAwardReviewCommentsByAwardId", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String fetchReviewCommentsByParams(@RequestBody AwardReviewCommentVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for fetchAwardReviewCommentsByParams");
		logger.info("awardId : {}", vo.getAwardId());
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		return awardReviewCommentService.fetchAwardReviewCommentsByAwardId(vo);
	}

	@PostMapping(value = "/resolveAwardReviewComment", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String resolveReviewComment(@RequestBody AwardReviewCommentVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for resolveAwardReviewComment");
		logger.info("awardReviewCommentId : " + vo.getAwardReviewCommentId());
		logger.info("personId : " + vo.getPersonId());
		return awardReviewCommentService.resolveAwardReviewComment(vo);
	}

	@PostMapping(value = "/getListOfAwardReviewPersons", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getListOfReviewPersons(@RequestBody AwardReviewCommentVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getListOfAwardReviewPersons");
		logger.info("awardId : " + vo.getAwardId());
		return awardReviewCommentService.getListOfAwardReviewPersons(vo);
	}

	@PostMapping(value = "/deleteAwardReviewComment", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteProposal(@RequestBody AwardReviewCommentVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteAwardReviewComment");
		logger.info("awardReviewCommentId : {}", vo.getAwardReviewCommentId());
		return awardReviewCommentService.deleteAwardReviewComment(vo);
	}

}
