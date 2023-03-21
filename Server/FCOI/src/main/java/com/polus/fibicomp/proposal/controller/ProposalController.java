package com.polus.fibicomp.proposal.controller;

import java.io.ByteArrayInputStream;
import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.core.io.InputStreamResource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.multipart.MultipartHttpServletRequest;

import com.polus.fibicomp.authorization.document.UserDocumentAuthorization;
import com.polus.fibicomp.budget.service.BudgetService;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.compilance.vo.ProtocolVO;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.notification.email.vo.EmailServiceVO;
import com.polus.fibicomp.proposal.pojo.CongressionalDistrict;
import com.polus.fibicomp.proposal.print.service.ProposalPrintService;
import com.polus.fibicomp.proposal.service.ProposalCopyService;
import com.polus.fibicomp.proposal.service.ProposalService;
import com.polus.fibicomp.proposal.vo.MaintainProjectTeamVO;
import com.polus.fibicomp.proposal.vo.ProposalPersonRoleVO;
import com.polus.fibicomp.proposal.vo.ProposalVO;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.vo.CommonVO;

import io.jsonwebtoken.Claims;

@RestController
public class ProposalController {

	protected static Logger logger = LogManager.getLogger(ProposalController.class.getName());

	@Autowired
	@Qualifier(value = "proposalService")
	private ProposalService proposalService;

	@Autowired
	@Qualifier(value = "budgetService")
	private BudgetService budgetService;

	@Autowired
	@Qualifier(value = "proposalPrintService")
	private ProposalPrintService proposalPrintService;

	@Autowired
	@Qualifier(value = "proposalCopyService")
	private ProposalCopyService proposalCopyService;

	@Autowired
	@Qualifier(value = "userDocumentAuthorization")
	private UserDocumentAuthorization documentAuthorization;

	@Autowired
	private CommonService commonService;

	private static final String PROPOSAL_ID = "proposalId : {}";

	@PostMapping(value = "/createProposal", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String createProposal(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for createProposal");
		return proposalService.createProposal(vo);
	}

	@PostMapping(value = "/addProposalAttachment")
	public String addProposalAttachment(@RequestParam(value = "files", required = false) MultipartFile[] files, @RequestParam("formDataJson") String formDataJson) {
		logger.info("Requesting for addProposalAttachment");
		return proposalService.addProposalAttachment(files, formDataJson);
	}

	@PostMapping(value = "/loadProposalById", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public ResponseEntity<String> loadProposalById(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for loadProposalById");
		logger.info(PROPOSAL_ID, vo.getProposalId());
		HttpStatus httpStatus = HttpStatus.OK;
		if (Boolean.FALSE.equals(vo.getIsProposalComparison()) && !documentAuthorization.isAuthorized(3, vo.getProposalId().toString(), AuthenticatedUser.getLoginPersonId())) {
			httpStatus = HttpStatus.FORBIDDEN;
			return new ResponseEntity<>("Not Authorized to view this proposal",httpStatus);
		}
		return new ResponseEntity<>(proposalService.loadProposalById(vo.getProposalId(), AuthenticatedUser.getLoginPersonId(), AuthenticatedUser.getLoginUserName(), vo.getIsProposalComparison()), httpStatus);
	}

	@PostMapping(value = "/deleteProposalKeyword", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteProposalKeyword(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteProposalKeyword");
		logger.info(PROPOSAL_ID, vo.getProposalId());
		logger.info("keywordId : {}", vo.getKeywordId());
		return proposalService.deleteProposalKeyword(vo);
	}

	@PostMapping(value = "/deleteProposalResearchArea", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteProposalResearchArea(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteProposalResearchArea");
		logger.info(PROPOSAL_ID, vo.getProposalId());
		logger.info("researchAreaId : {}", vo.getResearchAreaId());
		return proposalService.deleteProposalResearchArea(vo);
	}

	@PostMapping(value = "/deleteProposalPerson", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteProposalPerson(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteProposalPerson");
		logger.info(PROPOSAL_ID, vo.getProposalId());
		logger.info("proposalPersonId : {}", vo.getProposalPersonId());
		return proposalService.deleteProposalPerson(vo);
	}

	@PostMapping(value = "/deleteProposalSponsor", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteProposalSponsor(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteProposalSponsor");
		logger.info(PROPOSAL_ID, vo.getProposalId());
		logger.info("sponsorId : {}", vo.getSponsorId());
		return proposalService.deleteProposalSponsor(vo);
	}

	@PostMapping(value = "/deleteIrbProtocol", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteIrbProtocol(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteIrbProtocol");
		logger.info(PROPOSAL_ID, vo.getProposalId());
		logger.info("irbProtocolId : {}", vo.getIrbProtocolId());
		return proposalService.deleteIrbProtocol(vo);
	}

	@PostMapping(value = "/deleteProposalAttachment", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteProposalAttachment(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for delete Proposal Attachment");
		logger.info(PROPOSAL_ID, vo.getProposalId());
		logger.info("attachmentId : {}", vo.getAttachmentId());
		return proposalService.deleteProposalAttachment(vo);
	}

	@GetMapping(value = "/downloadProposalAttachment")
	public ResponseEntity<byte[]> downloadProposalAttachment(HttpServletResponse response, @RequestHeader("attachmentId") String attachmentId) {
		logger.info("Requesting for downloadProposalAttachment");
		logger.info("attachmentId : {}", attachmentId);
		Integer attachmentid = Integer.parseInt(attachmentId);
		return proposalService.downloadProposalAttachment(attachmentid);
	}

	@PostMapping(value = "/submitProposal", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String submitProposal(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for submitProposal");
		return proposalService.submitProposal(vo);
	}

	@PostMapping(value = "/approveOrRejectProposal", consumes = MediaType.MULTIPART_FORM_DATA_VALUE, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String approveProposal(@RequestParam(value = "files", required = false) MultipartFile[] files, @RequestParam("formDataJson") String formDataJson) {
		logger.info("Requesting for approveOrRejectProposal");
		return proposalService.approveOrRejectProposal(files, formDataJson);
	}

	@GetMapping(value = "/printProposalPdfReport", produces = MediaType.APPLICATION_PDF_VALUE)
	public ResponseEntity<InputStreamResource> proposalPdfReport(HttpServletResponse response,
			@RequestHeader(value = "proposalId", required = true) String proposalIdInput,
			@RequestHeader(value = "personId", required = true) String personId,
			@RequestHeader(value = "userName", required = true) String userName) {
		try {
			Integer proposalId = Integer.parseInt(proposalIdInput);
			logger.info("Requesting for printProposalPdfReport");
			logger.info(PROPOSAL_ID, proposalId);
			ByteArrayInputStream bis = proposalPrintService.proposalPdfReport(proposalId, personId, userName);
			HttpHeaders headers = new HttpHeaders();
			headers.add("Content-Disposition", "inline; filename=ProposalSummary.pdf");
			return ResponseEntity.ok().headers(headers).contentType(MediaType.APPLICATION_PDF).body(new InputStreamResource(bis));
		} catch (Exception e) {
			return null;
		}
	}

	@GetMapping(value = "/printBudgetPdfReport", produces = MediaType.APPLICATION_PDF_VALUE)
	public ResponseEntity<InputStreamResource> budgetPdfReport(HttpServletResponse response,
			@RequestHeader(value = "proposalId", required = true) String proposalIdInput,
			@RequestHeader(value = "budgetId", required = true) String budgetIdInput) {
		try {
			Integer proposalId = Integer.parseInt(proposalIdInput);
			Integer budgetId = Integer.parseInt(budgetIdInput);
			logger.info("Requesting for printBudgetPdfReport");
			logger.info(" proposalId in printBudgetPdfReport: {}", proposalId);
			logger.info("budgetId : {}", budgetId);
			ByteArrayInputStream bis = proposalPrintService.proposalBudgetDetailsPdfReport(proposalId, budgetId);
			HttpHeaders headers = new HttpHeaders();
			headers.add("Content-Disposition", "inline; filename=BudgetSummary.pdf");
			return ResponseEntity.ok().headers(headers).contentType(MediaType.APPLICATION_PDF)
					.body(new InputStreamResource(bis));
		} catch (Exception e) {
			return null;
		}
	}

	@PostMapping(value = "/deleteProposalSpecialReview", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteProposalSpecialReview(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteProposalSpecialReview");
		logger.info("proposalId in deleteProposalSpecialReview: {}", vo.getProposalId());
		logger.info("proposalSpecialReviewId : {}", vo.getProposalSpecialReviewId());
		return proposalService.deleteProposalSpecialReview(vo);
	}

	@PostMapping(value = "/copyProposal", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String copyProposal(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) throws Exception {
		logger.info("Requesting for copyProposal : {}", vo.getProposalId());
		return proposalCopyService.copyProposal(vo);
	}

	@PostMapping(value = "/sendAttachPINotification", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String sendAttachPINotification(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for sendAttachPINotification");
		logger.info("proposalId in sendAttachPINotification: {}", vo.getProposalId());
		return proposalService.sendAttachPINotification(vo);
	}

	@PostMapping(value = "/sendAttachApproverNotification", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String sendAttachApproverNotification(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for sendAttachApproverNotification");
		logger.info("proposalId in sendAttachApproverNotification: {}", vo.getProposalId());
		return proposalService.sendAttachApproverNotification(vo);
	}

	@PostMapping(value = "/deleteProposal", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteProposal(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteProposal");
		return proposalService.deleteProposal(vo);
	}

	@PostMapping(value = "/maintainProjectTeam", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String maintainProjectTeam(@RequestBody MaintainProjectTeamVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for maintainProjectTeam");
		return proposalService.maintainProjectTeam(vo);
	}

	@PostMapping(value = "/maintainProposalPersonRoles", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String maintainProposalPersonRoles(@RequestBody ProposalPersonRoleVO proposalPersonRoleVO, HttpServletRequest request, HttpServletResponse response) {
		logger.info("maintain ProposalPersonRoles");
		return proposalService.maintainProposalPersonRoles(proposalPersonRoleVO);
	}

	@PostMapping(value = "/fetchProposalPersonRoles", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String fetchProposalPersonRoles(@RequestBody ProposalPersonRoleVO proposalPersonRoleVO, HttpServletRequest request, HttpServletResponse response) {
		logger.info("fetch ProposalPersonRoles");
		return proposalService.fetchProposalPersonRoles(proposalPersonRoleVO);
	}

	@PostMapping(value = "/addScienceKeyword", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String addScienceKeyword(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for Adding ScienceKeyword");
		logger.info("ScienceKeyword : {}", vo.getScienceKeyword());
		return proposalService.addScienceKeyword(vo);
	}

	@PostMapping(value = "/printEntireProposal")
	public void printEntireProposal(HttpServletResponse response, @RequestBody ProposalVO vo, HttpServletRequest request) {
		logger.info("Requesting for downloadProposal");
		logger.info("proposal Id : {}", vo.getProposalId());
		proposalService.printEntireProposal(vo, response);
	}

	@PostMapping(value = "/fetchSortedAttachments")
	public String fetchSortedReviews(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for fetchSortedAttachments");
		return proposalService.fetchSortedAttachments(vo);
	}

	@PostMapping(value = "/fetchActivityType", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String fetchActivityType(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for Activity Type");
		return proposalService.fetchActivityType(vo);
	}

	@GetMapping(value = "/getAllActivityForGrantType", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getAllActivityForGrantType(HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getAllActivityForGrantType");
		return proposalService.getAllActivityForGrantType();
	}

	@PostMapping(value = "/saveRankFromDashboard", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveProposalRankFromDashboard(@RequestBody CommonVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveRankFromDashboard");
		logger.info(PROPOSAL_ID, vo.getProposalId());
		return proposalService.saveProposalRankFromDashboard(vo);
	}

	@PostMapping(value = "/fundTheProposal", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String fundTheProposal(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for Funding Proposal");
		logger.info(PROPOSAL_ID, vo.getProposalId());
		return proposalService.fundTheProposal(vo);
	}

	@PostMapping(value = "/addProposalPersonAttachment")
	public String addProposalPersonAttachment(@RequestParam(value = "files", required = false) MultipartFile[] files, @RequestParam("formDataJson") String formDataJson) {
		logger.info("Requesting for addProposalPersonAttachment");
		return proposalService.addProposalPersonAttachment(files, formDataJson);
	}

	@GetMapping(value = "/downloadProposalPersonAttachment")
	public ResponseEntity<byte[]> downloadProposalPersonAttachment(HttpServletResponse response, @RequestHeader("attachmentId") String attachmentId) {
		logger.info("Requesting for downloadProposalPersonAttachment");
		logger.info("person attachmentId : {}", attachmentId);
		Integer attachmentid = Integer.parseInt(attachmentId);
		return proposalService.downloadProposalPersonAttachment(attachmentid);
	}

	@PostMapping(value = "/saveProposalDetails", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveProposalDetails(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveProposalDetails");
		Claims claims = commonService.getLoginPersonDetailFromJWT(request);
		vo.setUpdateUser(claims.getSubject());
		vo.setPersonId(claims.get(Constants.LOGIN_PERSON_ID).toString());
		logger.info("loginPersonId : {}", vo.getPersonId());
		logger.info("updateUser : {}", vo.getUpdateUser());
		return proposalService.saveOrUpdateProposal(vo);
	}

	@PostMapping(value = "/addKeyPerson", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String addKeyPerson(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for addKeyPerson");
		logger.info(PROPOSAL_ID, vo.getProposalId());
		logger.info("personId : {}", vo.getPersonId());
		logger.info("updateUser : {}", vo.getUpdateUser());
		return proposalService.saveOrUpdateKeyPerson(vo);
	}

	@PostMapping(value = "/addProjectTeam", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String addProjectTeam(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for addProjectTeam");
		return proposalService.saveOrUpdateProjectTeam(vo);
	}

	@PostMapping(value = "/addSpecialReview", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String addSpecialReview(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for addSpecialReview");
		return proposalService.saveOrUpdateSpecialReview(vo);
	}

	@PostMapping(value = "/addFundingSupport", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String addFunds(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for addFundingSupport");
		return proposalService.saveOrUpdateFunds(vo);
	}

	@PostMapping(value = "/addAreaOfResearch", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String addAreaOfResearch(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for addAreaOfResearch");
		return proposalService.saveOrUpdateAreaOfResearch(vo);
	}

	@PostMapping(value = "/loadProposalAttachments", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String loadProposalAttachment(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for loadProposalAttachments");
		logger.info(PROPOSAL_ID, vo.getProposalId());
		logger.info("proposalStatusCode : {}", vo.getProposalStatusCode());
		return proposalService.loadProposalAttachment(vo.getProposalId(), vo.getProposalStatusCode());
	}

	@PostMapping(value = "/saveGrantCallFromProposal", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveGrantCallFromProposal(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveGrantCallFromProposal");
		logger.info(PROPOSAL_ID, vo.getProposalId());
		logger.info("grantCallId : {}", vo.getGrantCallId());
		return proposalService.saveGrantCallFromProposal(vo);
	}

	@PostMapping(value = "/saveDescriptionOfProposal", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveDescriptionOfProposal(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveDescriptionOfProposal");
		return proposalService.saveDescriptionOfProposal(vo);
	}

	@PostMapping(value = "/loadEvaluationDetails", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String loadEvaluationDetials(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for loadEvaluationDetails");
		logger.info(PROPOSAL_ID, vo.getProposalId());
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		return proposalService.loadEvaluationDetails(vo.getProposalId(), vo.getPersonId(), vo.getLoginPersonUnitNumber());
	}

	@PostMapping(value = "/updateAttachmentDetails", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String updateAttachmentDetails(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for updateAttachmentDetails");
		return proposalService.updateAttachmentDetails(vo);
	}

	@PostMapping(value = "/exportSelectedAttachments")
	public void exportSelectedAttachments(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for exportSelectedAttachments");
		proposalService.exportSelectedAttachments(vo, response);
	}

	@PostMapping(value = "/proposalInvitation", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String proposalInvitation(@RequestBody EmailServiceVO emailServiceVO, HttpServletRequest request,
			HttpServletResponse response) {
		logger.info("Requesting for proposalInvitation via email");
		return proposalService.proposalInvitation(emailServiceVO);
	}

	@PostMapping(value = "/updateAllAttachmentStatus", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String updateAllAttachmentStatus(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for updateAllAttachmentStatus");
		return proposalService.updateAllAttachmentStatus(vo);
	}

	@PostMapping(value = "/updateProposaStatusAsInactive", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String updateProposaStatusAsInactive(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for updateProposaStatusAsInactive");
		logger.info(PROPOSAL_ID, vo.getProposalId());
		return proposalService.updateProposalStatusAsInactive(vo);
	}

	@PostMapping(value = "/saveOrUpdateProposalMileStone", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveOrUpdateProposalMileStone(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveOrUpdateProposalMileStone");
		return proposalService.saveOrUpdateProposalMileStone(vo);
	}

	@PostMapping(value = "/deleteProposalMileStone", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteProposalMileStone(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteProposalMileStone");
		logger.info(PROPOSAL_ID, vo.getProposalId());
		logger.info("proposalMileStoneId : {}", vo.getProposalMileStoneId());
		return proposalService.deleteProposalMileStone(vo);
	}

	@PostMapping(value = "/saveOrUpdateProposalKPI", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveOrUpdateProposalKPI(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveOrUpdateProposalKPI");
		return proposalService.saveOrUpdateProposalKPI(vo);
	}

	@PostMapping(value = "/assignAggregatorRoleToPI", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String assignAggregatorRoleToPI(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for assignAggregatorRoleToPI");
		return proposalService.assignAggregatorRoleToPI(vo);
	}
    
	@PostMapping(value = "/fetchScoringCriteriaByProposal", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String fetchScoringCriteriaByProposal(@RequestBody ProposalVO proposalVO, HttpServletRequest request,HttpServletResponse response) {
		logger.info("Requesting for fetchScoringCriteriaByProposal");
		Claims claims = commonService.getLoginPersonDetailFromJWT(request);
		proposalVO.setPersonId(claims.get(Constants.LOGIN_PERSON_ID).toString());
		proposalVO.setUserName(claims.getSubject());
		logger.info("personId : {}", proposalVO.getPersonId());
		logger.info("loginPersonUnitNumber : {}", proposalVO.getLoginPersonUnitNumber());
		logger.info("username: {}", proposalVO.getUserName());
		return proposalService.fetchScoringCriteriaByProposal(proposalVO);
	}
	
	@GetMapping(value = "/downloadWorkflowReviewerAttachment")
	public ResponseEntity<byte[]> downloadWorkflowReviewerAttachment(HttpServletResponse response,@RequestHeader("workflowReviewerAttmntsId") Integer workflowReviewerAttmntsId) {
		logger.info("Requesting for downloadWorkflowReviewerAttachment");
		return proposalService.downloadWorkflowReviewerAttachment(workflowReviewerAttmntsId);
	}
     
	@PostMapping(value = "/saveOrUpdateProposalcomment")
	public String saveOrUpdateProposalComment(@RequestParam(value = "files", required = false) MultipartFile[] files, @RequestParam("formDataJson") String formDataJson) {
		logger.info("Requesting for saveOrUpdateProposalComment");
		return proposalService.saveOrUpdateProposalComment(files, formDataJson);
	}

	@GetMapping(value = "/downloadProposalCommentAttachment")
	public ResponseEntity<byte[]> downloadProposalCommentAttachment(HttpServletResponse response, @RequestHeader("attachmentId") String attachmentId) {
		logger.info("Requesting for downloadProposalCommentAttachment");
		logger.info("attachmentId : {}", attachmentId);
		return proposalService.downloadProposalCommentAttachment(Integer.parseInt(attachmentId));
	}

	@PostMapping(value = "/fetchProposalComments", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String fetchProposalComments(@RequestBody ProposalVO proposalVO, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for fetchProposalComments");
		Claims claims = commonService.getLoginPersonDetailFromJWT(request);
		proposalVO.setPersonId(claims.get(Constants.LOGIN_PERSON_ID).toString());
		proposalVO.setUserName(claims.getSubject());
		logger.info("personId : {}", proposalVO.getPersonId());
		logger.info("loginPersonUnitNumber : {}", proposalVO.getLoginPersonUnitNumber());
		logger.info("username: {}", proposalVO.getUserName());
		return proposalService.fetchProposalComments(proposalVO);
	}

	@PostMapping(value = "/unlinkGrantCallFromProposal", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String unlinkProposalFromGrantCall(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for unlinkGrantCallFromProposal");
		logger.info(PROPOSAL_ID, vo.getProposalId());
		logger.info("grantCallId : {}", vo.getGrantCallId());
		return proposalService.unlinkGrantCallFromProposal(vo);
	}

	@PostMapping(value = "/addProposalAttachmentForWaf")
	public String addProposalAttachmentForWaf(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for addProposalAttachmentForWaf");
		return proposalService.addProposalAttachmentForWaf(vo);
	}

	@PostMapping(value = "/addProposalPersonAttachmentForWaf")
	public String addProposalPersonAttachmentForWaf(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for addProposalPersonAttachmentForWaf");
		return proposalService.addProposalPersonAttachmentForWaf(vo);
	}

	@PostMapping(value = "/deleteWorkflowScoreComments")
	public String deleteWorkflowScoreComments(@RequestBody ProposalVO vo) {
		logger.info("Requesting for deleteWorkflowScoreComments");
		logger.info("workflowReviewerCommentsId : {}", vo.getWorkflowReviewerCommentsId());
		return proposalService.deleteWorkflowScoreComments(vo.getWorkflowReviewerCommentsId());
	}

	@PostMapping(value = "/deleteWorkflowReviewerAttachment")
	public String deleteWorkflowReviewerAttachment(@RequestBody ProposalVO vo) {
		logger.info("Requesting for deleteWorkflowReviewerAttachment");
		logger.info("workflowReviewerAttmntsId : {}", vo.getWorkflowReviewerAttmntsId());
		return proposalService.deleteWorkflowReviewerAttachment(vo.getWorkflowReviewerAttmntsId());
	}

	@PostMapping(value = "/saveWorkflowScoreOrEndorseProposal")
	public String saveWorkflowScoreOrEndorseProposal(@RequestParam("formDataJson") String formDataJson, MultipartHttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveWorkflowScoreOrEndorseProposal");
		return proposalService.saveOrUpdateWorkflowScoreDetails(formDataJson, request);
	}

	@PostMapping(value = "/withdrawProposal", consumes = MediaType.MULTIPART_FORM_DATA_VALUE, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String withdrawProposalNew(@RequestParam(value = "files", required = false) MultipartFile[] files, @RequestParam("formDataJson") String formDataJson) {
		logger.info("Requesting for withdrawProposal");
		return proposalService.withdrawProposal(files, formDataJson);
	}

	@PostMapping(value = "/saveOrUpdateProposalOrganization")
	public String saveOrUpdateProposalOrganization(@RequestBody ProposalVO vo) {
		logger.info("Requesting for saveOrUpdateProposalOrganization");
		return proposalService.saveOrUpdateProposalOrganization(vo);
	}

	@PostMapping(value = "/saveOrUpdateOrganization")
	public String saveOrUpdateOrganization(@RequestBody ProposalVO vo) {
		logger.info("Requesting for saveOrUpdateOrganization");
		return proposalService.saveOrUpdateOrganization(vo);
	}

	@GetMapping(value = "/loadOrganizationDetails/{organizationId}")
	public String loadOrganizationDetails(@PathVariable(value = "organizationId", required = true) final String organizationId) {
		logger.info("Request for loadOrganizationDetails");
		logger.info(organizationId, organizationId);
		return proposalService.loadOrganizationDetails(organizationId);
	}

	@PostMapping(value = "/saveOrUpdateCongDistrict")
	public String saveOrUpdateCongDistrict(@RequestBody ProposalVO vo) {
		logger.info("Requesting for saveOrUpdateCongDistrict");
		return proposalService.saveOrUpdateCongDistrict(vo);
	}

	@DeleteMapping(value = "/deleteProposalOrganization/{proposalOrganizationId}")
	public String deleteProposalOrganization(@PathVariable(value = "proposalOrganizationId", required = true) final Integer proposalOrganizationId) {
		logger.info("Requesting for deleteProposalOrganization");
		return proposalService.deleteProposalOrganization(proposalOrganizationId);
	}

	@DeleteMapping(value = "/deleteProposalCongDistrict/{proposalCongDistrictId}")
	public String deleteProposalCongDistrict(@PathVariable(value = "proposalCongDistrictId", required = true) final Integer proposalCongDistrictId) {
		logger.info("Requesting for deleteProposalCongDistrict");
		return proposalService.deleteProposalCongDistrict(proposalCongDistrictId);
	}

	@PostMapping(value = "/findCongressionalDistricts")
	public List<CongressionalDistrict> findCongressionalDistricts(@RequestBody CommonVO vo) {
		logger.info("Requesting for findCongressionalDistricts");
		logger.info("searchString : {}", vo.getSearchString());
		return proposalService.findCongressionalDistricts(vo.getSearchString());
	}
	
	@PostMapping(value = "/checkUnitRight", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String checkProposalRights(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for createProposalWithRights");
		logger.info("Unit Number: {}", vo.getUnitNumber());
		logger.info("RightName : {}", vo.getRightName());
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		return proposalService.checkProposalRights(vo);
	}

	@PostMapping(value = "/importProposalTemplate")
	public ResponseEntity<String> importProposalTemplate(@RequestParam(value = "files", required = false) MultipartFile[] files, @RequestParam("formDataJson") String formDataJson) {
		logger.info("Requesting for importProposalTemplate");
		ProposalVO vo = proposalService.importProposalTemplate(files, formDataJson);
		return new ResponseEntity<>(proposalService.loadProposalById(vo.getProposalId(), AuthenticatedUser.getLoginPersonId(), AuthenticatedUser.getLoginUserName(), Boolean.FALSE), HttpStatus.OK);
	}

	@PostMapping(value = "/loadProposalKeyPersonAttachments")
	public String loadProposalKeyPersonAttachments(@RequestBody ProposalVO proposalVO, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for loadProposalKeyPersonAttachments");
		return proposalService.loadProposalKeyPersonAttachments(proposalVO);
	}

	@PostMapping(value = "/exportProposalPersonAttachments")
	public void exportProposalPersonAttachments(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for exportProposalPersonAttachments");
		proposalService.exportProposalPersonAttachments(vo, response);
	}

	@PostMapping(value = "/createProposalAdminCorrection/{proposalId}")
	public ResponseEntity<String> createProposalAdminCorrection(@PathVariable(value = "proposalId", required = true) final Integer proposalId) {
		logger.info("Request for createProposalAdminCorrection");
		logger.info("proposalId {}", proposalId);
		return proposalService.createProposalAdminCorrection(proposalId);
	}

	@GetMapping(value = "/showProposalHistory/{proposalId}")
	public String showProposalHistory(@PathVariable(value = "proposalId", required = true) final Integer proposalId) {
		logger.info("Request for showAwardHistory");
		logger.info("proposalId {}", proposalId);
		return proposalService.showProposalHistory(proposalId);
	}

	@PatchMapping(value = "/completeProposalAdminCorrection/{proposalId}")
	public ResponseEntity<String> completeProposalAdminCorrection(@PathVariable(value = "proposalId", required = true) final Integer proposalId) {
		logger.info("Request for completeProposalAdminCorrection");
		return proposalService.completeProposalAdminCorrection(proposalId);
	}

	@GetMapping(value = "/loadPersonnelAttachTypes", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String loadPersonnelAttachTypes() {
        logger.info("Requesting for loadPersonnelAttachTypes");
	    return proposalService.loadProposalKeyPersonnelAttachmentTypes();
	}

	@GetMapping("/loadProposalKeyPersonnelPersons/{proposalId}")
	public String loadProposalKeyPersonnelPersons(@PathVariable("proposalId") Integer proposalId) {
		logger.info("Requesting for loadProposalKeyPersonnelPersons");
		return proposalService.loadProposalKeyPersonnelPersons(proposalId);
	}

	@PostMapping("/deleteKeyPersonnelAttachment")
	public String deleteKeyPersonnelAttachment(@RequestBody ProposalVO proposalVO) {
		logger.info("Requesting for deleteKeyPersonnelAttachment");
		return proposalService.deleteKeyPersonnelAttachment(proposalVO);
	}

	@PostMapping(value = "/uploadProposalPersonAttachment")
	public String uploadProposalPersonAttachment(@RequestParam(value = "files") MultipartFile[] files, @RequestParam("formDataJson") String formDataJson) {
		logger.info("Requesting for uploadProposalPersonAttachment");
		return proposalService.uploadProposalPersonAttachment(files, formDataJson);
	}

	@PostMapping("/updateKeyPersonnelAttachment")
	public String updateKeyPersonnelAttachment(@RequestParam("formDataJson") String formDataJson) {
		logger.info("Requesting for updateKeyPersonnelAttachment");
		return proposalService.updateKeyPersonnelAttachment(formDataJson);
	}
	
	@PatchMapping(value = "/updateProposalPersonCertification")
	public String updateProposalPersonCertification(@RequestBody ProposalVO proposalVO) {
		logger.info("Request for updateProposalPersonCertification proposal id {}", proposalVO.getProposalId());
		return proposalService.updateProposalPersonCertification(proposalVO);
	}
	
	@PostMapping(value = "/proposalPersonsForCertification")
	public String proposalPersonsForCertification(@RequestBody ProposalVO proposalVO) {
		logger.info("Request for proposalPersonsForCertification");
		return proposalService.proposalPersonsForCertification(proposalVO);
	}
	
	@PostMapping(value = "/sendPersonCertificationMail")
	public String sendPersonCertificationMail(@RequestBody ProposalVO proposalVO) {
		logger.info("Request for sendPersonCertificationMail proposal id {}", proposalVO.getProposalId());
		return proposalService.sendPersonCertificationMail(proposalVO);
	}

	@PostMapping(value = "/canDeleteProposal")
	public String canDeleteProposal(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for canDeleteProposal");
		return proposalService.canDeleteProposal(vo);
	}

	@PostMapping(value = "/loadProtocolDetail", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String loadProtocolDetail(@RequestBody ProtocolVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for loadProtocolDetail");
		return proposalService.loadProtocolDetail(vo);
	}

	@PostMapping(value = "/addProposalPersonDegree", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String addProposalPersonDegree(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for addProposalPersonDegree");
		logger.info("proposalPersonId : {}", vo.getProposalPersonDegree().getProposalPersonId());
		return proposalService.addProposalPersonDegree(vo);
	}

	@PostMapping(value = "/getPersonDegree", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getPersonDegree(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getPersonDegree");
		logger.info("proposalPersonId : {}", vo.getProposalPersonId());
		return proposalService.getPersonDegree(vo);
	}

	@DeleteMapping(value = "/deleteProposalPersonDegree/{proposalPersonDegreeId}")
	public String deleteExtReviewerAttachment(@PathVariable(value = "proposalPersonDegreeId", required = true) final Integer proposalPersonDegreeId) {
		logger.info("Requesting for deleteProposalPersonDegree");
		logger.info("proposalPersonDegreeId : {}", proposalPersonDegreeId);
		return proposalService.deleteProposalPersonDegree(proposalPersonDegreeId);
	}
}
