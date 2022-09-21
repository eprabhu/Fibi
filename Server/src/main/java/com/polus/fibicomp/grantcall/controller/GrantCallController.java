package com.polus.fibicomp.grantcall.controller;

import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.authorization.document.UserDocumentAuthorization;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.grantcall.service.GrantCallService;
import com.polus.fibicomp.grantcall.vo.EvaluationMainPanelVO;
import com.polus.fibicomp.grantcall.vo.GrantCallVO;
import com.polus.fibicomp.notification.email.vo.EmailServiceVO;
import com.polus.fibicomp.pojo.Sponsor;
import com.polus.fibicomp.proposal.vo.ProposalVO;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.vo.CommonVO;

@RestController
public class GrantCallController {

	protected static Logger logger =  LogManager.getLogger(GrantCallController.class.getName());

	@Autowired
	@Qualifier(value = "grantCallService")
	private GrantCallService grantCallService;

	@Autowired
	@Qualifier(value = "userDocumentAuthorization")
	private UserDocumentAuthorization documentAuthorization;

	@PostMapping(value = "/loadGrantCallById", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public ResponseEntity<String> loadGrantCallById(@RequestBody GrantCallVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for loadGrantCallById");
		logger.info("grantCallId : {} " ,vo.getGrantCallId());
		vo.setLoginPersonId(AuthenticatedUser.getLoginPersonId());
		logger.info("loginPersonId : {} ", vo.getLoginPersonId());
		if (!documentAuthorization.isAuthorized(Constants.GRANTCALL_MODULE_CODE, vo.getGrantCallId().toString(), vo.getLoginPersonId())) {
			return new ResponseEntity<>("Not Authorized to view this GrantCall", HttpStatus.FORBIDDEN);
		}
		return new ResponseEntity<>(grantCallService.loadGrantCallById(vo), HttpStatus.OK);
	}

	@PostMapping(value = "/createGrantCall", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String createGrantCall(@RequestBody GrantCallVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for createGrantCall");
		return grantCallService.createGrantCall(vo);
	}

	@PostMapping(value = "/publishGrantCall", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String publishGrantCall(@RequestBody GrantCallVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for publishGrantCall");
		return grantCallService.publishGrantCall(vo);
	}

	@PostMapping(value = "/deleteGrantCallKeyword", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteGrantCallKeyword(@RequestBody GrantCallVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteGrantCallKeyword");
		logger.info("grantCallId : {}" , vo.getGrantCallId());
		logger.info("grantKeywordId : {}" , vo.getGrantKeywordId());
		return grantCallService.deleteGrantCallKeyword(vo);
	}

	@PostMapping(value = "/deleteGrantCallContact", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteGrantCallContact(@RequestBody GrantCallVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteGrantCallContact");
		logger.info("grantCallId : {}" , vo.getGrantCallId());
		logger.info("grantContactId : {}" , vo.getGrantContactId());
		return grantCallService.deleteGrantCallContact(vo);
	}

	@PostMapping(value = "/deleteGrantCallAreaOfResearch", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteGrantCallAreaOfResearch(@RequestBody GrantCallVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteGrantCallAreaOfResearch");
		logger.info("grantCallId : {}" , vo.getGrantCallId());
		logger.info("grantResearchAreaId : {}" , vo.getGrantResearchAreaId());
		return grantCallService.deleteGrantCallAreaOfResearch(vo);
	}

	@PostMapping(value = "/deleteGrantCallEligibility", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteGrantCallEligibility(@RequestBody GrantCallVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteGrantCallEligibility");
		logger.info("grantCallId : {}" , vo.getGrantCallId());
		logger.info("grantEligibilityId : {}" , vo.getGrantEligibilityId());
		return grantCallService.deleteGrantCallEligibility(vo);
	}

	@PostMapping(value = "/deleteGrantCallAttachment", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteGrantCallAttachment(@RequestBody GrantCallVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteGrantCallAttachment");
		logger.info("grantCallId : {}" , vo.getGrantCallId());
		logger.info("attachmentId : {}" , vo.getAttachmentId());
		return grantCallService.deleteGrantCallAttachment(vo);
	}

	@PostMapping(value = "/addGrantCallAttachment",  produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String addGrantCallAttachment(@RequestParam(value = "files", required = false) MultipartFile[] files, @RequestParam("formDataJson") String formDataJson) {
		logger.info("Requesting for addGrantCallAttachment");
		return grantCallService.addGrantCallAttachment(files, formDataJson);
	}

    @GetMapping(value = "/downloadGrantCallAttachment")
	public ResponseEntity<byte[]> downloadGrantCallAttachment(HttpServletResponse response, @RequestHeader("attachmentId") String attachmentId) {
		logger.info("Requesting for downloadGrantCallAttachment");
		logger.info("attachmentId : {}" , attachmentId);
		Integer attachmentid = Integer.parseInt(attachmentId);
		return grantCallService.downloadGrantCallAttachment(attachmentid);
	}

	@PostMapping(value = "/copyGrantCall", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String copyGrantCall(@RequestBody GrantCallVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for copyGrantCall");
		logger.info("grantCallId : {}" , vo.getGrantCallId());
		return grantCallService.copyGrantCall(vo);
	}

	@PostMapping(value = "/deleteGrantEligibleDepartments", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteGrantEligibleDepartments(@RequestBody GrantCallVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for delete GrantCallEligibleDepartments");
		logger.info("grantCallId : {}" , vo.getGrantCallId());
		logger.info("getGrantCallEligibleDepartmentId : {}" , vo.getGrantEligibilityDepartmentId());
		return grantCallService.deleteGrantEligibleDepartments(vo);
	}

	@PostMapping(value = "/deleteGrantCall", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteProposal(@RequestBody GrantCallVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteGrantCall");
		return grantCallService.deleteGrantCall(vo);
	}

	@PostMapping(value = "/fetchSponsorsBySponsorType",  produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public List<Sponsor> fetchSponsorsBySponsorType(@RequestBody CommonVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for fetchSponsorsBySponsorType");
		logger.info("searchString : {}" , vo.getSearchString());
		logger.info("sponsorTypeCode : {}" , vo.getSearchString());
		return grantCallService.fetchSponsorsBySponsorType(vo.getSearchString(), vo.getSponsorTypeCode());
	}

	@PostMapping(value = "/fetchFundingSchemeBySponsor", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String fetchFundingSchemeBySponsor(@RequestBody GrantCallVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for fetchFundingSchemeBySponsor");
		logger.info("SponsorTypeCode : {}" , vo.getSponsorTypeCode());
		return grantCallService.fetchFundingSchemeBySponsor(vo);
	}

	@PostMapping(value = "/archiveGrantCall", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String archiveGrantCall(@RequestBody GrantCallVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for archiveGrantCall");
		logger.info("userFullName : {}" , vo.getUpdateUser());
		logger.info("grantCallId : {}" , vo.getGrantCallId());
		return grantCallService.archiveGrantCall(vo);
	}

	@PostMapping(value = "/grandInvitation", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String grandInvitation(@RequestBody EmailServiceVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for grandInvitation via email");
		return grantCallService.grantInvitation(vo);
	}

	@GetMapping(value = "/downloadFundingSchemeAttachment")
	public ResponseEntity<byte[]> downloadFundingSchemeAttachment(HttpServletResponse response, @RequestHeader("attachmentId") String attachmentId) {
		logger.info("Requesting for downloadGrantCallAttachment");
		logger.info("attachmentId : {}" , attachmentId);
		Integer attachmentid = Integer.parseInt(attachmentId);
		return grantCallService.downloadFundingSchemeAttachment(attachmentid);
	}
	
	@PostMapping(value = "/addGrantCallKeyword", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String addGrantCallKeyword(@RequestBody GrantCallVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for Adding ScienceKeyword");
		logger.info("ScienceKeyword : {}" , vo.getScienceKeyword());
		return grantCallService.addGrantCallKeyword(vo);
	}
	
	@PostMapping(value = "/deleteGrantCallRelevantField", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteGrantCallRelevantField(@RequestBody GrantCallVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteGrantCallRelevantField");
		logger.info("grantCallId : {}" , vo.getGrantCallId());
		logger.info("grantCallRelevantId : {}" , vo.getGrantCallRelevantId());
		return grantCallService.deleteGrantCallRelevantField(vo);
	}

	@PostMapping(value = "/deleteGrantEligibility", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteGrantEligibility(@RequestBody GrantCallVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for delete GrantCallEligibleDepartments");
		logger.info("grantCallId : {}" , vo.getGrantCallId());
		logger.info("grantEligibilityId : {}" , vo.getGrantEligibilityId());
		return grantCallService.deleteGrantCallEligibility(vo);
	}

	@PostMapping(value = "/saveOrUpdateGrantCallIOIQuestionnaire", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveOrUpdateGrantCallIOIQuestionnaire(@RequestBody GrantCallVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveOrUpdateGrantCallIOIQuestionnaire");
		return grantCallService.saveOrUpdateGrantCallIOIQuestionnaire(vo);
	}

	@PostMapping(value = "/deleteGrantIOIQuestionnaire", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteGrantIOIQuestionnaire(@RequestBody GrantCallVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for delete deleteGrantIOIQuestionnaire");
		logger.info("grantCallId : {}" , vo.getGrantCallId());
		return grantCallService.deleteGrantIOIQuestionnaire(vo);
	}

	@PostMapping(value = "/fetchGrantCallIOIQuestionnaireByGrantId", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String fetchGrantCallIOIQuestionnaireByGrantId(@RequestBody GrantCallVO vo,HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for fetchGrantCallIOIQuestionnaireByGrantId");
		return grantCallService.fetchGrantCallIOIQuestionnaireByGrantId(vo);
	}

	@PostMapping(value = "/saveOrUpdateProposalEvalautionScore", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveUpdateProposalEvalautionScore(@RequestBody EvaluationMainPanelVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveUpdateProposalEvalautionScore");
		return grantCallService.saveOrUpdateProposalEvalautionScore(vo);
	}
	
	@PostMapping(value = "/getProposalByGrantCallId", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getProposalById(@RequestBody EvaluationMainPanelVO vo,HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getProposalById");
		return grantCallService.getProposalByGrantCallId(vo);
	}

	@PostMapping(value = "/getCriteriaScoreByProposalId", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getCriteriaScoreByProposalId(@RequestBody ProposalVO vo,HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getCriteriaScoreByProposalId");
		String loginPersonId = AuthenticatedUser.getLoginPersonId();
		String loginPersonUnitNumber = AuthenticatedUser.getLoginPersonUnit();
		logger.info("loginPersonId : {}", loginPersonId);
		logger.info("homeUnitNumber : {}", loginPersonUnitNumber);
		return grantCallService.getCriteriaScoreByProposalId(vo.getProposalId(), loginPersonId, loginPersonUnitNumber, AuthenticatedUser.getLoginUserName());
	}

	@PostMapping(value = "/updateProposalEvalautionRank", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String updateProposalEvalautionRank(@RequestBody EvaluationMainPanelVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for updateProposalEvalautionRank");
		return grantCallService.updateProposalEvalautionRank(vo);
	}

	@PostMapping(value = "/saveOrUpdateProposalEvalautionScores", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveOrUpdateProposalEvalautionScores(@RequestBody EvaluationMainPanelVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveOrUpdateProposalEvalautionScores");
		return grantCallService.saveOrUpdateProposalEvalautionScores(vo);
	}

	@PostMapping(value = "/addGrantCallAreaOfResearch", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String addAreaOfResearch(@RequestBody GrantCallVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for addGrantCallAreaOfResearch");
		return grantCallService.saveOrUpdateAreaOfResearch(vo);
	}

	@PostMapping(value = "/addPointOfContact", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String addPointOfContact(@RequestBody GrantCallVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for addPointOfContact");
		return grantCallService.saveOrUpdatePointOfContact(vo);
	}

	@PostMapping(value = "/saveGrantCallDetails", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveGrantCallDetails(@RequestBody GrantCallVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveGrantCallDetails");
		vo.setLoginPersonId(AuthenticatedUser.getLoginPersonId());
		vo.setHomeUnitNumber(AuthenticatedUser.getLoginPersonUnit());
		logger.info("loginPersonId : {}", vo.getLoginPersonId());
		logger.info("homeUnitNumber : {}", vo.getHomeUnitNumber());
		return grantCallService.saveUpdateGrantCall(vo);
	}

	@PostMapping(value = "/saveGrantCallEligibilityCriteria", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveGrantCallEligibilityCriteria(@RequestBody GrantCallVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveGrantCallEligibilityCriteria");
		return grantCallService.saveGrantCallEligibilityCriteria(vo);
	}

	@PostMapping(value = "/updateGrantCallAttachmentDetails", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String updateGrantCallAttachmentDetails(@RequestBody GrantCallVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for updateAttachmentDetails");
		return grantCallService.updateGrantCallAttachmentDetails(vo);
	}

	@PostMapping(value = "/addGrantCallAttachmentForWaf", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String addGrantCallAttachmentForWaf(@RequestBody GrantCallVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for addGrantCallAttachmentForWaf");
		return grantCallService.addGrantCallAttachmentForWaf(vo);
	}

	@PostMapping(value = "/getGrantCallHistory", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getGrantCallHistory(@RequestBody GrantCallVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getGrantCallHistory");
		return grantCallService.getGrantCallHistory(vo);
	}

}
