package com.polus.fibicomp.evaluation.controller;

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

import com.polus.fibicomp.evaluation.service.EvaluationService;
import com.polus.fibicomp.evaluation.vo.EvaluationVO;
import com.polus.fibicomp.grantcall.vo.EvaluationMainPanelVO;
import com.polus.fibicomp.proposal.vo.ProposalVO;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.vo.CommonVO;


@RestController
public class EvaluationController {

	protected static Logger logger = LogManager.getLogger(EvaluationController.class.getName());

	@Autowired
	@Qualifier(value = "evaluationService")
	private EvaluationService evaluationService;

	private static final String PROPOSAL_ID = "proposalId : {}";
	private static final String UPDATE_USER = "updateUser : {}";

	@PostMapping(value = "/addReview", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String addReview(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for addReview");
		return evaluationService.createReview(vo);
	}

	@PostMapping(value = "/addReviewComment")
	public String addReviewComment(@RequestParam(value = "files", required = false) MultipartFile[] files, @RequestParam("formDataJson") String formDataJson) {
		logger.info("Requesting for addReviewComment");
		return evaluationService.addProposalReviewComment(files, formDataJson);
	}

	@PostMapping(value = "/approveOrDisapproveReview", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String completeReview(@RequestParam(value = "files", required = false) MultipartFile[] files, @RequestParam("formDataJson") String formDataJson) {
		logger.info("Requesting for approveOrDisapproveReview");
		return evaluationService.approveOrDisapproveReview(files, formDataJson);
	}

	@GetMapping(value = "/downloadReviewAttachment")
	public ResponseEntity<byte[]> downloadPreReviewAttachment(HttpServletResponse response, @RequestHeader("attachmentId") String attachmentId) {
		logger.info("Requesting for downloadReviewAttachment");
		logger.info("attachmentId : {}" , attachmentId);
		Integer attachmentid = Integer.parseInt(attachmentId);
		return evaluationService.downloadProposalReviewAttachment(attachmentid);
	}

	@PostMapping(value = "/fetchSortedReviewsForEvaluation")
	public String fetchSortedReviews(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for fetchSortedReviews");
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		vo.setLoginPersonUnitNumber(AuthenticatedUser.getLoginPersonUnit());
		return evaluationService.fetchSortedReviews(vo);
	}

	@PostMapping(value = "/getProposalAndReviewSummary")
	public String getProposalAndReviewSummary(@RequestBody CommonVO vo, HttpServletRequest request) {
		logger.info("Requesting for getProposalAndReviewSummary");
		logger.info(PROPOSAL_ID, vo.getProposalId());
		logger.info("personId : {}", vo.getPersonId());
		return evaluationService.getProposalAndReviewSummary(vo.getProposalId(), vo.getPersonId());
	}

	@PostMapping(value = "/deleteReviewComment")
	public String deleteReviewComment(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteReviewComment");
		logger.info(PROPOSAL_ID, vo.getProposalId());
		logger.info("reviewId : {}", vo.getReviewId());
		logger.info("commentId : {}", vo.getReviewCommentId());
		return evaluationService.deleteReviewComment(vo);
	}

	@PostMapping(value = "/addReviewer")
	public String addReviewer(@RequestBody ProposalVO vo, HttpServletRequest request) {
		logger.info("Requesting for addReviewer");
		logger.info("reviewId : {}", vo.getReviewId());
		logger.info("review Deadline Date: {}", vo.getReviewDeadLine());
		return evaluationService.addReviewer(vo);
	}

	@PostMapping(value = "/sendAdminReviewReminderNotification")
	public String sendReviewReminderNotification(@RequestBody ProposalVO vo, HttpServletRequest request) {
		logger.info("Requesting for sendAdminReviewReminderNotification");
		logger.info(PROPOSAL_ID, vo.getProposalId());
		logger.info("userRole : {}", vo.getUserRole());
		return evaluationService.sendAdminReviewReminderNotification(vo);
	}

	@PostMapping(value = "/sendPIReviewReminderNotification")
	public String sendPIReviewReminderNotification(@RequestBody ProposalVO vo, HttpServletRequest request) {
		logger.info("Requesting for sendPIReviewReminderNotification");
		logger.info(PROPOSAL_ID, vo.getProposalId());
		return evaluationService.sendPIReviewReminderNotification(vo);
	}

	@PostMapping(value = "/fetchEvaluationPanelsList", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String fetchEvaluationPanels(@RequestBody EvaluationVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for fetchEvaluationPanels");
		return evaluationService.fetchEvaluationPanels(vo);
	}

	@PostMapping(value = "/saveProposalEvaluationPanelDetails", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveOrUpdateEvaluationPanelDetails(@RequestBody EvaluationVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveProposalEvaluationPanelDetails");
		return evaluationService.saveProposalEvaluationPanelDetails(vo);
	}

	@PostMapping(value = "/startEvaluation", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String startEvaluation(@RequestBody EvaluationVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for startEvaluation");
		logger.info(PROPOSAL_ID, vo.getProposalId());
		logger.info(UPDATE_USER, vo.getUpdateUser());
		return evaluationService.startEvaluationWithFinalEvaluationPanelList(vo);
	}
	
	@PostMapping(value = "/saveAdminEvaluationPanel", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveEvaluation(@RequestBody EvaluationVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for startEvaluation");
		logger.info(PROPOSAL_ID, vo.getProposalId());
		logger.info(UPDATE_USER, vo.getUpdateUser());
		return evaluationService.saveEvaluation(vo);
	}

	@PostMapping(value = "/addEvaluationPanelPerson", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String addKeyPerson(@RequestBody EvaluationVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for addEvaluationPanelPerson");
		logger.info("personId : {}", vo.getPersonId());
		logger.info(UPDATE_USER, vo.getUpdateUser());
		return evaluationService.addEvaluationPanelPerson(vo);
	}

	@PostMapping(value = "/deleteEvaluationPanelPerson", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteEvaluationPanelPersonr(@RequestBody EvaluationVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteEvaluationPanelPersonr");
		logger.info(PROPOSAL_ID, vo.getProposalId());
		logger.info("Proposal Evaluation panel Person Id : {}",vo.getProposalEvaluationPanelPersonId());
		return evaluationService.deleteEvaluationPanelPerson(vo);
	}

	@GetMapping(value = "/getEvaluationPersonsBasedOnRole")
	public String getEvaluationPersonsBasedOnRole(HttpServletResponse response, @RequestHeader(value = "personRoleId", required = true) Integer personRoleId, @RequestHeader(value = "unitNumber", required = true) String unitNumber) {
		logger.info("Requesting for getEvaluationPersonsBasedOnRole");
		logger.info("unitNumber : {}", unitNumber);
		logger.info("personRoleId : {}", personRoleId);
		return evaluationService.getEvaluationPersonsBasedOnRole(personRoleId, unitNumber);
	}

	@PostMapping(value = "/approveOrDisapproveReviewForWaf", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String completeReviewForwaf(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for approveOrDisapproveReviewForWaf");
		return evaluationService.approveOrDisapproveReviewForWaf(vo);
	}

	@PostMapping(value = "/addReviewCommentForWaf")
	public String addReviewCommentForWaf(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for addReviewCommentForWaf");
		return evaluationService.addReviewCommentForWaf(vo);
	}

	@PostMapping(value = "/exportGrantCallEvaluationReport")
	public ResponseEntity<byte[]> exportGrantCallEvaluationReport(@RequestBody EvaluationMainPanelVO vo, HttpServletResponse response) {
		logger.info("Requesting for exportGrantCallEvaluationReport");
		logger.info("grantCallId : {}", vo.getGrantCallId());
		logger.info("exportIndex : {}", vo.getExportIndex());
		return evaluationService.exportGrantCallEvaluationReport(vo, response);
	}

}
