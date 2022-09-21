package com.polus.fibicomp.prereview.controller;

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
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.prereview.service.PreReviewService;
import com.polus.fibicomp.prereview.vo.PreReviewVO;
import com.polus.fibicomp.security.AuthenticatedUser;

@RestController
public class PreReviewController {

	protected static Logger logger = LogManager.getLogger(PreReviewController.class.getName());

	@Autowired
	@Qualifier(value = "preReviewService")
	private PreReviewService preReviewService;

	@PostMapping(value = "/createPreReview", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String createPreReview(@RequestBody PreReviewVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for createPreReview");
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		return preReviewService.createPreReview(vo);
	}

	@PostMapping(value = "/addPreReviewComment")
	public String addPreReviewComment(@RequestParam(value = "files", required = false) MultipartFile[] files, @RequestParam("formDataJson") String formDataJson, HttpServletRequest request) {
//		logger.info("Requesting for addPreReviewComment");
//		PreReviewVO preReviewVO = new PreReviewVO();
//		preReviewVO.setPreReviews(preReviewService.addPreReviewComment(files, formDataJson));
//		String response = committeeDao.convertObjectToJSON(preReviewVO);
//		return response;
		logger.info("Requesting for addPreReviewComment");
        return preReviewService.addPreReviewComment(files, formDataJson, request);
	}

	@PostMapping(value = "/approveOrDisapprovePreReview", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String approveOrDiapprovePreReview(@RequestParam(value = "files", required = false) MultipartFile[] files, @RequestParam("formDataJson") String formDataJson, HttpServletRequest request) {
		logger.info("Requesting for approveOrDisapprovePreReview");
//		logger.info("actionType : " + vo.getActionType());
		return preReviewService.approveOrDisapprovePreReview(files, formDataJson, request);
	}

	@GetMapping(value = "/downloadPreReviewAttachment")
	public ResponseEntity<byte[]> downloadPreReviewAttachment(HttpServletResponse response, @RequestHeader("attachmentId") String attachmentId) {
		logger.info("Requesting for downloadPreReviewAttachment");
		logger.info("attachmentId : ", attachmentId);
		Integer attachmentid = Integer.parseInt(attachmentId);
		return preReviewService.downloadPreReviewAttachment(attachmentid);
	}

	@PostMapping(value = "/fetchSortedReviews")
	public String fetchSortedReviews(@RequestBody PreReviewVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for fetchSortedReviews");
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		return preReviewService.fetchSortedReviews(vo);
	}

	@PostMapping(value = "/loadPreReviews", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String loadPreReview(@RequestBody PreReviewVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Load PreReviews");
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		return preReviewService.loadPreReview(vo);
	}

	@PostMapping(value = "/addPreReviewCommentForWaf")
	public String addPreReviewCommentForWaf(@RequestBody PreReviewVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for addPreReviewCommentForWaf");
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
        return preReviewService.addPreReviewCommentForWaf(vo);
	}

	@PostMapping(value = "/approveOrDisapprovePreReviewForWaf", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String approveOrDisapprovePreReviewForWaf(@RequestBody PreReviewVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for approveOrDisapprovePreReviewForWaf");
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		return preReviewService.approveOrDisapprovePreReviewForWaf(vo);
	}

}
