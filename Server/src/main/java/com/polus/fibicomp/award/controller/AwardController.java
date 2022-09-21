package com.polus.fibicomp.award.controller;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.ResponseStatus;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.authorization.document.UserDocumentAuthorization;
import com.polus.fibicomp.award.pojo.Report;
import com.polus.fibicomp.award.service.AwardConcurrentService;
import com.polus.fibicomp.award.service.AwardService;
import com.polus.fibicomp.award.vo.AwardAttachmentsVO;
import com.polus.fibicomp.award.vo.AwardCostShareVO;
import com.polus.fibicomp.award.vo.AwardDatesandAmountVO;
import com.polus.fibicomp.award.vo.AwardKPIVO;
import com.polus.fibicomp.award.vo.AwardLinkInstituteProposalVO;
import com.polus.fibicomp.award.vo.AwardMilestoneVO;
import com.polus.fibicomp.award.vo.AwardPaymentAndInvoicesVO;
import com.polus.fibicomp.award.vo.AwardSpecialReviewVO;
import com.polus.fibicomp.award.vo.AwardSubContractVO;
import com.polus.fibicomp.award.vo.AwardSummaryVO;
import com.polus.fibicomp.award.vo.AwardVO;
import com.polus.fibicomp.award.vo.MaintainAwardHierarchyVO;
import com.polus.fibicomp.award.vo.PersonnelVO;
import com.polus.fibicomp.award.vo.ReportTermsVO;
import com.polus.fibicomp.notification.email.vo.EmailServiceVO;
import com.polus.fibicomp.pojo.Organization;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.servicerequest.vo.ServiceRequestVO;
import com.polus.fibicomp.vo.CommonVO;

@RestController
public class AwardController {

	protected static Logger logger = LogManager.getLogger(AwardController.class.getName());

	@Autowired
	@Qualifier(value = "awardService")
	private AwardService awardService;

	@Autowired
	@Qualifier(value = "userDocumentAuthorization")
	private UserDocumentAuthorization documentAuthorization;

	@Autowired
	private AwardConcurrentService awardConcurrentService;

	private static final String AWARD_ID = "awardId : {}";
	private static final String AWARD_REPORT_TERMS_ID = "awardReportTermsId : {}";
	private static final String AWARD_NUMBER = "awardNumber : {}";
	private static final String AWARD_PERSON_ID = "awardPersonId : {}";
	private static final String PERSON_ID = "personId : {}";
	private static final String UPDATE_USER = "updateUser : {}";

	@PostMapping(value = "/getAwardSummary")
	public ResponseEntity<String> fetchAwardSummary(@RequestBody CommonVO vo, HttpServletRequest request) throws Exception {
		logger.info("Request for getAwardSummary");
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		logger.info(PERSON_ID, vo.getPersonId());
		logger.info(AWARD_ID, vo.getAwardId());
		HttpStatus httpStatus = HttpStatus.OK;
		if (!documentAuthorization.isAuthorized(1, vo.getAwardId(), vo.getPersonId())) {
			httpStatus = HttpStatus.FORBIDDEN;
			return new ResponseEntity<>("Not Authorized to view this Award", httpStatus);
		}
		return new ResponseEntity<>(awardService.getAwardSummaryData(vo.getAwardId()), httpStatus);
	}

	@PostMapping(value = "/getAwardHierarchy")
	public String fetchAwardHierarchy(@Valid @RequestBody CommonVO vo, HttpServletRequest request) throws Exception {
		logger.info(AWARD_NUMBER, vo.getAwardNumber());
		logger.info("selectedAwardNumber : {}", vo.getSelectedAwardNumber());
		return awardService.getAwardHierarchyData(vo.getAwardNumber(), vo.getSelectedAwardNumber());
	}

	@PostMapping(value = "/saveAwardGeneralInfo")
	public String saveAwardGeneralInfo(@RequestBody AwardVO awardVo, HttpServletRequest request) throws Exception {
		logger.info("Request for saveAwardGeneralInfo");
		awardVo.setPersonId(AuthenticatedUser.getLoginPersonId());
		logger.info(PERSON_ID, awardVo.getPersonId());
		return awardService.saveAwardDetails(awardVo);
	}

	@PostMapping(value = "/getAwardGeneralInfo")
	public ResponseEntity<String> getAwardGeneralInfo(@Valid @RequestBody AwardVO awardVo, HttpServletRequest request) throws Exception {
		logger.info("Request for getAwardGeneralInfo");
		awardVo.setPersonId(AuthenticatedUser.getLoginPersonId());
		logger.info(PERSON_ID, awardVo.getPersonId());
		logger.info(AWARD_ID, awardVo.getAwardId());
		HttpStatus httpStatus = HttpStatus.OK;
		if (!documentAuthorization.isAuthorized(1, awardVo.getAwardId().toString(), awardVo.getPersonId())) {
			httpStatus = HttpStatus.FORBIDDEN;
			return new ResponseEntity<>("Not Authorized to view this Award", httpStatus);
		}
		return new ResponseEntity<>(awardService.getAwardDetails(awardVo), httpStatus);
	}

	@PostMapping(value = "/getAwardLookupData")
	public String getAwardLookupInfo(@RequestBody AwardVO vo, HttpServletRequest request) throws Exception {
		logger.info("Requesting for getAwardLookupData");
		logger.info("leadUnitNumber : {}", vo.getLeadUnitNumber());
		logger.info(PERSON_ID, vo.getPersonId());
		logger.info(AWARD_ID, vo.getAwardId());
		return awardService.getAwardLookupData(vo);
	}

	@PostMapping(value = "/findOrganizations")
	public List<Organization> findAllOrganizations(@RequestBody CommonVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for findOrganizations");
		logger.info("searchString : {}", vo.getSearchString());
		return awardService.findOrganizationList(vo.getSearchString());
	}

	@PostMapping(value = "/saveAwardSpecialReview")
	public String saveAwardSpecialReview(@RequestBody AwardSpecialReviewVO vo, HttpServletRequest request) throws Exception {
		logger.info("Request for saveAwardSpecialReview");
		logger.info(PERSON_ID, vo.getPersonId());
		logger.info(AWARD_ID, vo.getAwardId());
		return awardService.saveAwardSpecialReview(vo);
	}

	@PostMapping(value = "/saveAwardCostShare")
	public String saveAwardCostShare(@RequestBody AwardCostShareVO costShareVO, HttpServletRequest request) throws Exception {
		logger.info("Request for saveAwardCostShare");
		logger.info(PERSON_ID, costShareVO.getPersonId());
		return awardService.saveAwardCostShare(costShareVO);
	}

	@PostMapping(value = "/saveAwardSubContract")
	public String saveAwardSubContract(@RequestBody AwardSubContractVO subContractVO, HttpServletRequest request) throws Exception {
		logger.info("Request for saveAwardSubContracts");
		logger.info(PERSON_ID, subContractVO.getPersonId());
		return awardService.saveAwardSubContracts(subContractVO);
	}

	@PostMapping(value = "/deleteAwardSpecialReview")
	public String deleteAwardSpecialReview(@RequestBody AwardSpecialReviewVO vo, HttpServletRequest request) throws Exception {
		logger.info("Request for deleteAwardSpecialReview");
		logger.info(PERSON_ID, vo.getPersonId());
		return awardService.deleteAwardSpecialReview(vo);
	}

	@PostMapping(value = "/deleteAwardSubContract")
	public String deleteAwardSubContract(@RequestBody AwardSubContractVO vo, HttpServletRequest request) throws Exception {
		logger.info("Request for deleteAwardSubContract");
		logger.info(PERSON_ID, vo.getPersonId());
		return awardService.deleteAwardSubContract(vo);
	}

	@PostMapping(value = "/deleteAwardCostShare")
	public String deleteAwardCostShare(@RequestBody AwardCostShareVO vo, HttpServletRequest request) throws Exception {
		logger.info("Request for deleteAwardSubContract");
		logger.info(PERSON_ID, vo.getPersonId());
		return awardService.deleteAwardCostShare(vo);
	}

	@PostMapping(value = "/saveOrUpdateKeyPersonnel")
	public String saveOrUpdateKeyPersonnel(@RequestBody PersonnelVO personnelVO, HttpServletRequest request) {
			logger.info("Requesting for saveOrUpdateKeyPersonnel");
			logger.info(AWARD_ID, personnelVO.getAwardId());
			return awardService.saveOrUpdateKeyPersonnel(personnelVO);
	}

	@PostMapping(value = "/deleteKeyPersonnel")
	public String maintainKeyPersonnel(@RequestBody PersonnelVO personnelVO, HttpServletRequest request) {
			logger.info("Requesting for deleteKeyPersonnel");
			logger.info(AWARD_ID, personnelVO.getAwardId());
			logger.info(AWARD_PERSON_ID, personnelVO.getAwardPersonalId());
			return awardService.deleteKeyPersonnel(personnelVO);
	}

	@PostMapping(value = "/saveOrUpdateAwardProjectTeam")
	public String saveOrUpdateAwardProjectTeam(@RequestBody PersonnelVO personnelVO, HttpServletRequest request) {
			logger.info("Requesting for maintainAwardProjectTeam");
			logger.info(AWARD_ID, personnelVO.getAwardId());
			return awardService.saveOrUpdateAwardProjectTeam(personnelVO);
	}

	@PostMapping(value = "/deleteAwardProjectTeam")
	public String deleteAwardProjectTeam(@RequestBody PersonnelVO personnelVO, HttpServletRequest request) {
		logger.info("Requesting for deleteKeyPersonnel");
		logger.info(AWARD_ID, personnelVO.getAwardId());
		logger.info(AWARD_PERSON_ID, personnelVO.getAwardPersonalId());
		return awardService.deleteAwardProjectTeam(personnelVO);
	}

	@PostMapping(value = "/saveOrUpdateAwardContact")
	public String saveOrUpdateContacts(@RequestBody PersonnelVO personnelVO, HttpServletRequest request) {
		logger.info("Requesting for maintainAwardProjectTeam");
		logger.info(AWARD_ID, personnelVO.getAwardId());
		return awardService.saveOrUpdateAwardContact(personnelVO);
	}

	@PostMapping(value = "/deleteAwardContact")
	public String deleteContacts(@RequestBody PersonnelVO personnelVO, HttpServletRequest request) {
		logger.info("Requesting for deleteAwardContact");
		logger.info(AWARD_ID, personnelVO.getAwardId());
		logger.info(AWARD_PERSON_ID, personnelVO.getAwardPersonalId());
		return awardService.deleteAwardContact(personnelVO);
	}

	@PostMapping(value = "/maintainSpecialApproval")
	public String maintainSpecialApproval(@RequestBody ReportTermsVO reportTermsVo, HttpServletRequest request) throws Exception {
		logger.info("Requesting for maintainSpecialApproval");
		return awardService.maintainSpecialApproval(reportTermsVo);
	}

	@PostMapping(value = "/getReportLookupData")
	public String getReportLookupData(@RequestBody ReportTermsVO reportTermsVo, HttpServletRequest request) throws Exception {
		logger.info("Request for getReportLookupData");
		logger.info(AWARD_ID, reportTermsVo.getAwardId());
		return awardService.getReportLookupData(reportTermsVo);
	}

	@PostMapping(value = "/getTermsLookupData")
	public String getTermsLookupData(@RequestBody ReportTermsVO reportTermsVo, HttpServletRequest request) throws Exception {
		logger.info("Request for getTermsLookupData");
		logger.info(AWARD_ID, reportTermsVo.getAwardId());
		return awardService.getTermsLookupData(reportTermsVo);
	}

	@PostMapping(value = "/maintainTerms")
	public String maintainTerms(@RequestBody ReportTermsVO reportTermsVo, HttpServletRequest request) throws Exception {
		logger.info("Request for maintainTerms");
		awardService.maintainTerms(reportTermsVo);
		reportTermsVo.setAwardId(reportTermsVo.getAwardSponsorTerm().getAwardId());
		return awardService.getTermsData(reportTermsVo);
	}

	@PostMapping(value = "/maintainReports")
	public String maintainReports(@RequestBody ReportTermsVO reportTermsVo, HttpServletRequest request) throws Exception {
		logger.info("Request for maintainReports");
		return awardService.maintainReports(reportTermsVo);
	}

	@PostMapping(value = "/getReportsData")
	public String getReportsData(@RequestBody ReportTermsVO reportTermsVo, HttpServletRequest request) throws Exception {
		logger.info("Request for getReportsData");
		logger.info(AWARD_ID, reportTermsVo.getAwardId());
		return awardService.getReportsData(reportTermsVo);
	}

	@PostMapping(value = "/getTermsData")
	public String getTermsData(@RequestBody ReportTermsVO reportTermsVo, HttpServletRequest request) throws Exception {
		logger.info("Request for getTermsData");
		logger.info(AWARD_ID, reportTermsVo.getAwardId());
		return awardService.getTermsData(reportTermsVo);
	}

	@PostMapping(value = "/linkInstituteProposalToAward")
	public String linkInstituteProposalToAward(@RequestBody AwardLinkInstituteProposalVO awardLinkInstituteProposalVO, HttpServletRequest request) throws Exception {
		logger.info("Request for linkInstituteProposalToAward");
		logger.info("acType : {}", awardLinkInstituteProposalVO.getAcType());
		logger.info("proposalId : {}", awardLinkInstituteProposalVO.getAwardFundingProposal().getProposalId());
		logger.info(UPDATE_USER, awardLinkInstituteProposalVO.getAwardFundingProposal().getUpdateUser());
		return awardService.linkInstituteProposalToAward(awardLinkInstituteProposalVO);
	}

	@PostMapping(value = "/saveFinalAwardInfo")
	public String saveFinalAwardInfo(@RequestBody AwardVO awardVo, HttpServletRequest request) throws Exception {
		logger.info("Request for saveFinalAwardInfo");
		return awardService.saveAwardFinal(awardVo);
	}

	@PostMapping(value = "/addAwardAttachments", consumes = MediaType.MULTIPART_FORM_DATA_VALUE, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String addAwardAttachments(@RequestParam(value = "files", required = false) MultipartFile[] files, @RequestParam("formDataJson") String formDataJson) {
		logger.info("Request for adding awardAttachment");
		return awardService.addAwardAttachments(files, formDataJson);
	}

	@PostMapping(value = "/getReportTrackingDetails")
	public String getReportTrackingDetails(@RequestBody ReportTermsVO reportTermsVo, HttpServletRequest request) throws Exception {
		logger.info("Request for getReportTrackingDetails");
		logger.info(AWARD_ID, reportTermsVo.getAwardId());
		logger.info(AWARD_REPORT_TERMS_ID, reportTermsVo.getAwardReportTermsId());
		return awardService.getReportTrackingDetails(reportTermsVo);
	}

	@PostMapping(value = "/saveReportTrackingDetails")
	public String saveReportTrackingDetails(@RequestBody ReportTermsVO reportTermsVo, HttpServletRequest request) throws Exception {
		logger.info("Request for saveReportTrackingDetails");
		logger.info(AWARD_REPORT_TERMS_ID, reportTermsVo.getAwardReportTermsId());
		return awardService.saveReportTrackingDetails(reportTermsVo);
	}

	@PostMapping(value = "/getAttachmentDetails")
	public String getAttachmentDetails(@RequestBody AwardAttachmentsVO attachmentVo, HttpServletRequest request) throws Exception {
		logger.info("Request for getAttachmentDetails");
		logger.info(AWARD_ID, attachmentVo.getAwardId());
		return awardService.getAttachmentDetails(attachmentVo);
	}

	@PostMapping(value = "/deleteAwardAttachment", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteAwardAttachment(@RequestBody AwardAttachmentsVO vo, HttpServletRequest request, HttpServletResponse response) throws Exception {
		logger.info("Requesting for deleteAwardAttachment");
		logger.info("awardAttachmentId : {}", vo.getAwardAttachmentId());
		logger.info("FileDataId : {}", vo.getFileDataId());
		return awardService.deleteAwardAttachment(vo);
	}

	@GetMapping(value = "/downloadAwardAttachment")
	public ResponseEntity<byte[]> downloadAwardAttachment(HttpServletResponse response, @RequestHeader("attachmentId") String awardAttachmentId) {
		logger.info("Requesting for downloadAwardAttachment");
		logger.info("awardAttachmentId : {}", awardAttachmentId);
		return awardService.downloadAwardAttachment(Integer.parseInt(awardAttachmentId));
	}

	@PostMapping(value = "/deleteAwardTransaction")
	public String deleteAwardTransaction(@RequestBody AwardDatesandAmountVO vo, HttpServletRequest request) throws Exception {
		logger.info("Request for deleteAwardTransaction");
		return awardService.deleteAwardTransaction(vo);
	}

	@PostMapping(value = "/getAwardHierarchyData")
	public String fetchAwardHierarchyDataList(@Valid @RequestBody CommonVO vo, HttpServletRequest request) throws Exception {
		logger.info("Request for getAwardHierarchyData");
		logger.info(AWARD_NUMBER, vo.getAwardNumber());
		logger.info("selectedAwardNumber : {}", vo.getSelectedAwardNumber());
		return awardService.getAwardHierarchyDataList(vo.getAwardNumber(), vo.getSelectedAwardNumber());
	}

	@PostMapping(value = "/maintainAwardHierarchy")
	public String saveAwardHierarchyData(@RequestBody MaintainAwardHierarchyVO vo, AwardVO awardVO,
			HttpServletRequest request) throws Exception {
		return awardService.saveAwardHierarchyData(vo, awardVO);
	}

	@PostMapping(value = "/deleteAwardKeyword", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteAwardKeyword(@RequestBody AwardVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteAwardKeyword");
		logger.info(AWARD_ID, vo.getAwardId());
		logger.info("awardKeywordId : {}", vo.getAwardKeywordId());
		logger.info("updateUser : {}", vo.getUpdateUser());
		return awardService.deleteAwardKeyword(vo);
	}

	@PostMapping(value = "/deleteAwardPersonUnit", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteAwardPersonUnit(@RequestBody PersonnelVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteAwardPersonUnit");
		logger.info("awardPersonalId : {}", vo.getAwardPersonalId());
		logger.info("awardPersonUnitId: {}", vo.getAwardPersonUnitId()); 
		return awardService.deleteAwardPersonUnit(vo);
	}

	@PostMapping(value = "/addAwardPersonAttachment")
	public String addAwardPersonAttachment(@RequestParam(value = "files", required = false) MultipartFile[] files, @RequestParam("formDataJson") String formDataJson) {
		logger.info("Requesting for addAwardPersonAttachment");
		return awardService.addAwardPersonAttachment(files, formDataJson);
	}

	@GetMapping(value = "/downloadAwardPersonAttachment")
	public ResponseEntity<byte[]> downloadAwardPersonAttachment(HttpServletResponse response, @RequestHeader("attachmentId") String attachmentId) {
		logger.info("Requesting for downloadAwardPersonAttachment");
		logger.info("person attachmentId : {}", attachmentId);
		Integer attachmentid = Integer.parseInt(attachmentId);
		return awardService.downloadAwardPersonAttachment(attachmentid);
	}

	@PostMapping(value = "/showAwardHistory")
	public String showAwardHistory(@RequestBody AwardVO vo, HttpServletRequest request) {
		logger.info("Request for showAwardHistory");
		logger.info(AWARD_NUMBER, vo.getAwardNumber());
		logger.info("isAwardHistoryTab : {}", vo.getIsAwardHistoryTab());
		return awardService.showAwardHistory(vo);
	}

	@PostMapping(value = "/fetchAwardPersonRoles", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String fetchAwardPersonRoles(@RequestBody PersonnelVO personnelVO, HttpServletRequest request, HttpServletResponse response) {
		logger.info("fetch ProposalPersonRoles");
		return awardService.fetchAwardPersonRoles(personnelVO);
	}

	@PostMapping(value = "/addAwardPersonRoles", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String maintainProposalPersonRoles(@RequestBody PersonnelVO personnelVO, HttpServletRequest request, HttpServletResponse response) {
		logger.info("add AwardPersonRoles");
		return awardService.maintainAwardPersonRoles(personnelVO);
	}

	@PostMapping(value = "/deleteAwardPersonRoles", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteAwardPersonRoles(@RequestBody PersonnelVO personnelVO, HttpServletRequest request, HttpServletResponse response) {
		logger.info("deleteAwardPersonRoles");
		return awardService.deleteAwardPersonRoles(personnelVO);
	}

	@PostMapping(value = "/addAwardReportTrackingAttachment", consumes = MediaType.MULTIPART_FORM_DATA_VALUE, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String addAwardReportTrackingAttachment(@RequestParam(value = "files", required = false) MultipartFile[] files, @RequestParam("formDataJson") String formDataJson) {
		logger.info("Requesting for addAwardReportTrackingAttachment");
		return awardService.addAwardReportTrackingAttachment(files, formDataJson);
	}

	@GetMapping(value = "/downloadAwardReportTrackingAttachment")
	public ResponseEntity<byte[]> downloadAwardReportTrackingAttachment(HttpServletResponse response, @RequestHeader("awardReportTrackingFileId") Integer awardReportTrackingFileId) {
		logger.info("Requesting for downloadAwardReportTrackingAttachment");
		logger.info("awardReportTrackingFileId : {}", awardReportTrackingFileId);
		return awardService.downloadAwardReportTrackingAttachment(awardReportTrackingFileId);
	}

	@PostMapping(value = "/deleteAwardReportTrackingAttachment", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteAwardReportTrackingAttachment(@RequestBody AwardVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("deleteAwardReportTrackingAttachment");
		return awardService.deleteAwardReportTrackingAttachment(vo);
	}

	@PostMapping(value = "/withdrawAward", consumes = MediaType.MULTIPART_FORM_DATA_VALUE, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String withdrawAward(@RequestParam(value = "files", required = false) MultipartFile[] files, @RequestParam("formDataJson") String formDataJson) throws Exception {
		logger.info("Request for withdrawAward");
		AwardVO vo = awardService.withdrawAward(files, formDataJson);
		if (vo.isCancelRequest()) {
			vo.setIsMasterAwardCreation(true);
			return awardConcurrentService.getAwardDetails(vo);
		} else {
			vo.setAwardId(vo.getAward().getAwardId());
			return awardConcurrentService.getAwardDetails(vo);
		}
	}

	@PostMapping(value = "/updateAwardAttachmentDetails", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String updateAwardAttachmentDetails(@RequestBody AwardVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for updateAwardAttachmentDetails");
		return awardService.updateAwardAttachmentDetails(vo);
  }

	@GetMapping(value = "/fetchReportByReportClass", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public List<Report> fetchReportByReportClass(HttpServletRequest request, HttpServletResponse response, @RequestParam("reportClassCode") String reportClassCode) {
		logger.info("Requesting for fetchReportByReportClass");
		logger.info("reportClassCode : {}", reportClassCode);
		return awardService.fetchReportByReportClass(reportClassCode);
	}

	@PostMapping(value = "/saveServiceRequestFromAward", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveServiceRequestFromAward(@RequestBody ServiceRequestVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveServiceRequestFromAward");
		logger.info("ServiceRequestId : {}", vo.getServiceRequestId());
		return awardService.saveServiceRequestFromAward(vo);
	}

	@PostMapping(value = "/awardInvitation", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String grandInvitation(@RequestBody EmailServiceVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for awardInvitation via email");
		return awardService.awardInvitation(vo);
	}

	@PostMapping(value = "/saveAwardPaymentAndInvoices", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveAwardPaymentAndInvoices(@RequestBody AwardPaymentAndInvoicesVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveAwardPaymentAndInvoices");
		return awardService.saveAwardPaymentAndInvoices(vo);
	}

	@GetMapping(value = "/getAwardPaymentAndInvoices", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getAwardPaymentAndInvoices(HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getAwardPaymentAndInvoices");
		return awardService.getAwardPaymentAndInvoices();
	}

	@PostMapping(value = "/saveOrUpdateAwardMilestone", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveOrUpdateAwardMilestone(@RequestBody AwardMilestoneVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveOrUpdateAwardMilestone");
		return awardService.saveOrUpdateAwardMilestone(vo);
	}

	@PostMapping(value = "/deleteAwardMilestone", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteAwardMilestone(@RequestBody AwardMilestoneVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for delete AwardMilestone");
		return awardService.deleteAwardMilestone(vo);
	}

	@PostMapping(value = "/saveOrUpdateAwardKPI", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveOrUpdateAwardKPI(@RequestBody AwardKPIVO awardKPIVO, HttpServletRequest request) {
		logger.info("Request for saveOrUpdateAwardKPI");
		return awardService.saveOrUpdateAwardKPI(awardKPIVO);
	}

	@PostMapping(value = "/deleteAwardKPI", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteAwardKPI(@RequestBody AwardKPIVO awardKPIVO, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteAwardKPI");
		return awardService.deleteAwardKPI(awardKPIVO);
	}

	@PostMapping(value = "/exportAllAwardAttachments")
	public void exportSelectedAttachments(@RequestBody AwardVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for exportSelectedAttachments");
		logger.info(AWARD_ID , vo.getAwardId());
		awardService.exportAllAwardAttachments(vo, response);
	}

	@PostMapping(value = "/addAwardAreaOfResearch", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String addAreaOfResearch(@RequestBody AwardVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for addAwardAreaOfResearch");
		return awardService.saveOrUpdateAwardAreaOfResearch(vo);
	}

	@PostMapping(value = "/deleteAwardResearchArea", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteAwardResearchArea(@RequestBody AwardVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteAwardResearchArea");
		logger.info("Award Id : {}", vo.getAwardId());
		logger.info("researchAreaId : {}", vo.getResearchAreaId());
		return awardService.deleteAwardResearchArea(vo);
	}

	@PostMapping(value = "/saveDescriptionOfAward", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveDescriptionOfAward(@RequestBody AwardVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveDescriptionOfAward");
		return awardService.saveDescriptionOfAward(vo);
	}

	@PostMapping(value = "/unlinkGrantCallFromAward", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String unlinkProposalFromGrantCall(@RequestBody AwardVO awardVO, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for unlinkGrantCallFromAward");
		logger.info(AWARD_ID, awardVO.getAwardId());
		return awardService.unlinkGrantCallFromAward(awardVO);
	}

	@PostMapping(value = "/saveAwardWorkflowStatusForSponsor", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveAwardWorkflowStatusForSponsor(@RequestBody AwardVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveAwardWorkflowStatusForSponsor");
		return awardService.saveAwardWorkflowStatusForSponsor(vo);
	}

	@PostMapping(value = "/addAwardPersonAttachmentForWaf", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String addAwardPersonAttachmentForwaf(@RequestBody AwardVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for addAwardPersonAttachmentForWaf");
		return awardService.addAwardPersonAttachmentForwaf(vo);
	}

	@PostMapping(value = "/addAwardAttachmentsForWaf", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String addAwardAttachmentsForwaf(@RequestBody AwardAttachmentsVO awardAttachmentsVO, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Request for adding addAwardAttachmentsForWaf");
		return awardService.addAwardAttachmentsForwaf(awardAttachmentsVO);
	}

	@PostMapping(value = "/addAwardReportTrackingAttachmentForWaf", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String addAwardReportTrackingAttachmentForWaf(@RequestBody AwardVO awardVO, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for addAwardReportTrackingAttachmentForWaf");
		return awardService.addAwardReportTrackingAttachmentForWaf(awardVO);
	}

	@PostMapping(value = "/saveReportTrackingDetailsForWaf")
	public String saveReportTrackingDetailsForWaf(@RequestBody ReportTermsVO reportTermsVo, HttpServletRequest request) throws Exception {
		logger.info("Request for saveReportTrackingDetailsForWaf");
		logger.info("awardReportTermsId : {}" , reportTermsVo.getAwardReportTermsId());
		return awardService.saveReportTrackingDetails(reportTermsVo);
	}

	@PostMapping(value = "/withdrawAwardForWaf")
	public String withdrawAwardForWaf(@RequestBody AwardVO vo, HttpServletRequest request) throws Exception {
		logger.info("Request for withdrawAwardForWaf");
		logger.info("awardId : {}", vo.getAwardId());
		return awardService.withdrawAwardForWaf(vo);
	}

	@ResponseStatus(HttpStatus.BAD_REQUEST)
	@ExceptionHandler(MethodArgumentNotValidException.class)
	public Map<String, String> handleValidationExceptions(MethodArgumentNotValidException ex) {
		Map<String, String> errors = new HashMap<>();
		ex.getBindingResult().getAllErrors().forEach((error) -> {
			String fieldName = ((FieldError) error).getField();
			String errorMessage = error.getDefaultMessage();
			errors.put(fieldName, errorMessage);
		});
		return errors;
	}

	@GetMapping(value = "/getReportTrackingAttachmentVersions/{awardReportTrackingId}")
	public String getReportTrackingAttachmentVersions(@PathVariable(value = "awardReportTrackingId", required = true) final Integer awardReportTrackingId){
		logger.info("Requesting for getReportTrackingAttachmentVersions");
		return awardService.getReportTrackingAttachmentVersions(awardReportTrackingId);
	}

	@PostMapping(value = "/saveOrUpdateReportTracking")
	public String saveOrUpdateReportTracking(@RequestBody ReportTermsVO reportTermsVo, HttpServletRequest request) {
		logger.info("Request for saveOrUpdateReportTracking");
		return awardService.saveOrUpdateReportTracking(reportTermsVo);
	}

	@DeleteMapping(value = "/deleteReportTracking/{awardReportTrackingId}")
	public String deleteReportTracking(@PathVariable(value = "awardReportTrackingId", required = true) final Integer awardReportTrackingId) {
		logger.info("Request for deleteReportTracking");
		return awardService.deleteReportTracking(awardReportTrackingId);
	}

	@PostMapping(value = "/findAward")
	public String findAward(@RequestBody CommonVO vo) {
		logger.info("Request for findAward : {}", vo.getSearchString());
		return awardService.findAward(vo.getSearchString());
	}
	
	@PostMapping(value = "/updateReportTermsInAward", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String updateReportTermsInAward(@RequestBody AwardVO awardVO) {
		logger.info("Requesting for updateReportTermsInAward");
		logger.info(AWARD_ID, awardVO.getAward().getAwardId());
		return awardService.updateReportTermsInAward(awardVO);
  }
  
	@PostMapping(value = "/getAwardVersions", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getAwardVersions(@RequestBody AwardSummaryVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getAwardVersions");
		logger.info(AWARD_NUMBER, vo.getAwardNumber());
		return awardService.getAwardVersions(vo);
	}

	@GetMapping(value = "/getSubModuleCodeBasedOnAwardNumber/{awardNumber}")
	public Set<Integer> getSubModuleCodeBasedOnAwardNumber(@PathVariable final String awardNumber){
		logger.info("Requesting for getSubModuleCodeBasedOnAwardNumber");
		return awardService.getSubModuleCodeBasedOnAwardNumber(awardNumber);
	}

	@DeleteMapping(value = "/deleteAward/{awardId}")
	public String deleteAward(@PathVariable(value = "awardId", required = true) final Integer awardId) {
		logger.info("Requesting for deleteAward");
		logger.info(AWARD_ID, awardId);
		return awardService.deleteAward(awardId);
	}
	
	@GetMapping(value = "/canDeleteAward/{awardId}")
	public String canDeleteAward(@PathVariable final Integer awardId){
		logger.info("Requesting for canDeleteAward");
		return awardService.canDeleteAward(awardId);
	}
}
