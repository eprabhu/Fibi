package com.polus.fibicomp.agreements.controller;

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
import org.springframework.web.bind.annotation.DeleteMapping;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.agreements.pojo.ClausesBank;
import com.polus.fibicomp.agreements.service.AgreementCommentService;
import com.polus.fibicomp.agreements.service.AgreementCopyService;
import com.polus.fibicomp.agreements.service.AgreementService;
import com.polus.fibicomp.agreements.vo.AgreementLinkModuleVO;
import com.polus.fibicomp.agreements.vo.AgreementVO;
import com.polus.fibicomp.agreements.vo.TemplateManagementVO;
import com.polus.fibicomp.authorization.document.UserDocumentAuthorization;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.notification.email.vo.EmailServiceVO;

@RestController
public class AgreementController {

	protected static Logger logger = LogManager.getLogger(AgreementController.class.getName());

	@Autowired
	private AgreementService agreementService;

	@Autowired
	private AgreementCopyService agreementCopyService;

	@Autowired
	@Qualifier(value = "userDocumentAuthorization")
	private UserDocumentAuthorization documentAuthorization;

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private AgreementCommentService agreementCommentService;

	private static final String AGREEMENT_REQUEST_ID = "agreementId : {}";
	private static final String ATTACHMENT_ID = "attachment ID : {}";

	@PostMapping(value = "/addAgreementAttachment")
	public String addAgreementAttachment(@RequestParam(value = "files", required = false) MultipartFile[] files, @RequestParam("formDataJson") String formDataJson, HttpServletResponse response) {
		logger.info("Requesting for addAgreementAttachment");
		return agreementService.addAgreementAttachment(files, formDataJson, response);
	}

	@PostMapping(value = "/createAgreement")
	public String createAgreement(@RequestBody AgreementVO agreementVO, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for createAgreement");
		return agreementService.createAgreement(agreementVO.getPersonId());
	}

	@PostMapping(value = "/saveOrUpdateAgreement", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveOrUpdateAgreement(@RequestBody AgreementVO agreementVO, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveOrUpdateAgreement");
		return agreementService.saveOrUpdateAgreement(agreementVO);
	}

	@PostMapping(value = "/loadAgreementById", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public ResponseEntity<String> loadAgreementData(@RequestBody AgreementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for loadAgreementById");
		logger.info(AGREEMENT_REQUEST_ID, vo.getAgreementRequestId());
		logger.info("personId : {}", vo.getPersonId());
		HttpStatus httpStatus = HttpStatus.OK;
		if (!documentAuthorization.isAuthorized(Constants.AGREEMENT_MODULE_CODE, vo.getAgreementRequestId().toString(), vo.getPersonId())) {
			httpStatus = HttpStatus.FORBIDDEN;
			return new ResponseEntity<>("Not Authorized to view this Agreement",httpStatus);
		}
		return new ResponseEntity<>(agreementService.loadAgreementById(vo), httpStatus);
	}

	@PostMapping(value = "/deleteAgreementAttachment", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteAgreementAttachment(@RequestBody AgreementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteAgreementAttachment");
		logger.info(ATTACHMENT_ID, vo.getAgreementAttachmentId());
		return agreementService.deleteAgreementAttachment(vo);
	}

	@GetMapping(value = "/downloadAgreementAttachment")
	public ResponseEntity<byte[]> downloadAgreementAttachment(HttpServletResponse response, @RequestHeader("attachmentId") String attachmentId, @RequestHeader("exportType") String exportType) {
		logger.info("Requesting for downloadAgreementAttachment");
		logger.info(ATTACHMENT_ID, attachmentId);
		Integer attachmentid = Integer.parseInt(attachmentId);
		return agreementService.downloadAgreementAttachment(attachmentid, exportType);
	}

	/*@PostMapping(value = "/findDepartment")
	public List<Unit> getDepartment(@RequestBody CommonVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getDepartment");
		logger.info("SearchString : {}", vo.getSearchString());
		return commonService.getDepartmentList(vo.getSearchString());
	}*/

	@PostMapping(value = "/saveOrUpdateOrganisation", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveOrUpdateOrganisation(@RequestBody AgreementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveOrUpdateOrganisation");
		return agreementService.saveOrUpdateOrganisation(vo);
	}

	@PostMapping(value = "/saveOrUpdateOrganisationContact", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveOrUpdateOrganisationContact(@RequestBody AgreementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveOrUpdateOrganisationContact");
		return agreementService.saveOrUpdateOrganisationContact(vo);
	}

	@PostMapping(value = "/loadAgreementAttachments", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String loadAgreementAttachments(@RequestBody AgreementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for loadAgreementAttachments");
		return agreementService.loadAgreementAttachments(vo);
	}

	@PostMapping(value = "/submitAgreement", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String submitAgreement(@RequestBody AgreementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for submitAgreement");
		return agreementService.submitAgreement(vo);
	}

	@PostMapping(value = "/deleteAgreementSponsor", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteAgreementSponsor(@RequestBody AgreementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteAgreementSponsor");
		return agreementService.deleteAgreementSponsor(vo.getAgreementSponsorId());
	}

	@PostMapping(value = "/loadAgreementNegotiation", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String loadAgreementNegotiation(@RequestBody AgreementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for loadAgreementNegotiation");
		logger.info("negotiationId : {} ", vo.getNegotiationId());
		logger.info(AGREEMENT_REQUEST_ID, vo.getAgreementRequestId());
		return agreementService.loadAgreementNegotiation(vo);
	}

	@PostMapping(value = "/addAgreementTemplate")
	public String addAgreementTemplate(@RequestParam(value = "files", required = false) MultipartFile[] files, @RequestParam("formDataJson") String formDataJson) {
		logger.info("Requesting for addAgreementTemplate");
		return agreementService.addAgreementTemplate(files, formDataJson);
	}

	@PostMapping(value = "/saveAgreementPerson", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveAgreementPerson(@RequestBody AgreementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveAgreementPerson");
		return agreementService.saveAgreementPerson(vo);
	}

	@PostMapping(value = "/addClausesGroup", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String addClausesGroup(@RequestBody TemplateManagementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for addClauses");
		return agreementService.addClausesGroup(vo);
	}

	@PostMapping(value = "/linkClausesGroupToAgreementType", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String linkClausesGroupToAgreementType(@RequestBody TemplateManagementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for linkClausesGroupToAgreementType");
		return agreementService.linkClausesGroupToAgreementType(vo);
	}

	@GetMapping(value = "/loadAllFormPlaceHolders")
	public String loadAllFormPlaceHolders(HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for loadAllFormPlaceHolders");
		return agreementService.loadAllFormPlaceHolders();
	}

	@GetMapping(value = "/loadAllClauses")
	public String loadAllClauses(HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for loadAllClauses");
		return agreementService.loadAllClauses();
	}

	@PostMapping(value = "/loadAllTemplates")
	public String loadAllTemplates(@RequestBody TemplateManagementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for loadAllTemplates");
		return agreementService.loadAllTemplates(vo);
	}

	@PostMapping(value = "/loadAllClausesTemplates", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String loadAllClausesTemplates(@RequestBody TemplateManagementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for loadAllClausesTemplates");
		return agreementService.loadAllClausesTemplates(vo);
	}

	@PostMapping(value = "/deleteClauses", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteClauses(@RequestBody TemplateManagementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteClauses");
		return agreementService.deleteClauses(vo);
	}

	@PostMapping(value = "/unlinkClausesGroupToAgreementType", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String unlinkClausesGroupToAgreementType(@RequestBody TemplateManagementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for unlinkClausesGroupToAgreementType");
		return agreementService.unlinkClausesGroupToAgreementType(vo);
	}

	@GetMapping(value = "/downloadAgreementTemplate")
	public ResponseEntity<byte[]> downloadAgreementTemplate(HttpServletResponse response, @RequestHeader("attachmentId") String attachmentId) {
		logger.info("Requesting for downloadAgreementAttachment");
		logger.info(ATTACHMENT_ID, attachmentId);
		Integer attachmentid = Integer.parseInt(attachmentId);
		return agreementService.downloadAgreementTemplate(attachmentid);
	}

	@PostMapping(value = "/deleteAgreementTemplate", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteAgreementTemplate(@RequestBody TemplateManagementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteAgreementTemplate");
		logger.info(ATTACHMENT_ID, vo.getTemplateId());
		return agreementService.deleteAgreementTemplate(vo);
	}

	@PostMapping(value = "/loadAllQuestionsPlaceHolders")
	public String loadAllQuestionsPlaceHolders(@RequestBody TemplateManagementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for loadAllQuestionsPlaceHolders");
		logger.info("agreementTypeCode : {}", vo.getAgreementTypeCode());
		return agreementService.loadAllQuestionsPlaceHolders(vo);
	}

	@GetMapping(value = "/getAgreementHistory")
	public String getAgreementHistory(HttpServletResponse response, @RequestHeader(value = "agreementRequestId", required = true) Integer agreementRequestId) {
		logger.info("Requesting for get Agreement History");
		return agreementService.getAgreementHistory(agreementRequestId);
	}

	@PostMapping(value = "/findClauses")
	public List<ClausesBank> findClauses(@RequestBody TemplateManagementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for find Clauses");
		logger.info("SearchString : {}", vo.getSearchString());
		return agreementService.findClauses(vo.getSearchString());
	}

	@PostMapping(value = "/saveOrUpdateAgreementClauses")
	public String saveOrUpdateAgreementClauses(@RequestBody AgreementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveOrUpdateClauses");
		return agreementService.saveOrUpdateAgreementClauses(vo);
	}

	@PostMapping(value = "/deleteAgreementClauses")
	public String deleteAgreementClauses(@RequestBody AgreementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteAgreementClauses");
		return agreementService.deleteAgreementClauses(vo);
	}

	@PostMapping(value = "/deleteAgreementGroup")
	public String deleteAgreementGroup(@RequestBody AgreementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteAgreementGroup");
		return agreementService.deleteAgreementGroup(vo);
	}

	@PostMapping(value = "/loadAgreementClausesByAgreementId", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String loadAgreementClausesByAgreementId(@RequestBody AgreementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for loadAgreementClausesByAgreementId");
		logger.info(AGREEMENT_REQUEST_ID, vo.getAgreementRequestId());
		return agreementService.loadAgreementClausesByAgreementId(vo);
	}

	@PostMapping(value = "/readyToExecute", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String readyToExecute(@RequestBody AgreementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for readyToExecute");
		logger.info(AGREEMENT_REQUEST_ID, vo.getAgreementRequestId());
		return agreementService.readyToExecute(vo);
	}

	@PostMapping(value = "/finalizeAgreement")
	public String finalizeAgreement(@RequestParam(value = "files", required = false) MultipartFile[] files, @RequestParam("formDataJson") String formDataJson, HttpServletResponse response) {
		logger.info("Requesting for finalizeAgreement");
		return agreementService.finalizeAgreement(files, formDataJson, response);
	}

	/*@PostMapping(value = "/addAgreementComment", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String addAgreementComment(@RequestParam(value = "files", required = false) MultipartFile[] files, @RequestParam("formDataJson") String formDataJson) {
		logger.info("Requesting for addAgreementComment");
		return agreementCommentService.addAgreementComment(files, formDataJson);
	}*/

	@PostMapping(value = "/submitAgreementReview", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String submitAgreementReview(@RequestBody AgreementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for submitAgreementReview");
		return agreementService.submitAgreementReview(vo);
	}

	@PostMapping(value = "/startReview", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String startReview(@RequestBody AgreementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for startReview");
		return agreementService.startReview(vo);
	}

	@PostMapping(value = "/completeReview", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String completeReview(@RequestBody AgreementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for completeReview");
		return agreementService.completeReview(vo);
	}

	/*@PostMapping(value = "/loadReviewComments", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String loadReviewComments(@RequestBody AgreementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for loadReviewComments");
		return agreementCommentService.loadReviewComments(vo);
	}*/

	@PostMapping(value = "/markAttachmentAsFinal", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String markAttachmentAsFinal(@RequestBody AgreementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for markAttachmentAsFinal");
		return agreementService.markAttachmentAsFinal(vo);
	}

	@PostMapping(value = "/deleteAgreementSponsorContact")
	public String deleteAgreementSponsorContact(@RequestBody AgreementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteAgreementSponsorContact");
		return agreementService.deleteAgreementSponsorContact(vo);
	}

	@PostMapping(value = "/addToClausesBank")
	public String addToClausesBank(@RequestBody TemplateManagementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for addToClausesBank");
		return agreementService.addToClausesBank(vo);
	}

	@GetMapping(value = "/loadAllClausesBank")
	public String loadAllClausesBank(HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for loadAllClausesBank");
		return agreementService.loadAllClausesBank();
	}

	@PostMapping(value = "/deleteClausesById")
	public String deleteClausesById(@RequestBody TemplateManagementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteClausesById");
		return agreementService.deleteClausesById(vo.getClauseCode());
	}

	/*@PostMapping(value = "/addLocationComment", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String addLocationComment(@RequestParam(value = "files", required = false) MultipartFile[] files, @RequestParam("formDataJson") String formDataJson) {
		logger.info("Requesting for addLocationComment");
		return agreementCommentService.addLocationComment(files, formDataJson);
	}*/

	/*@PostMapping(value = "/deleteAgreementComment")
	public String deleteAgreementComment(@RequestBody AgreementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteAgreementComment");
		return agreementCommentService.deleteAgreementComment(vo);
	}*/

	@PostMapping(value = "/copyAgreement")
	public String copyAgreement(@RequestBody AgreementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for copyAgreement");
		logger.info(AGREEMENT_REQUEST_ID, vo.getAgreementRequestId());
		return commonDao.convertObjectToJSON(agreementCopyService.copyAgreement(vo));
	}

	/*@PostMapping(value = "/deleteCommentAttachment")
	public String deleteCommentAttachment(@RequestBody AgreementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteCommentAttachment");
		return agreementCommentService.deleteCommentAttachment(vo);
	}*/

	@PostMapping(value = "/downloadCommentAttachment")
	public ResponseEntity<byte[]> downloadCommentAttachment(@RequestBody AgreementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for downloadCommentAttachment");
		return agreementCommentService.downloadCommentAttachment(vo);
	}

	@PostMapping(value = "/saveAgreementPeople")
	public String saveAgreementPeople(@RequestBody AgreementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveAgreementPeople");
		return agreementService.saveAgreementPeople(vo);
	}

	@PostMapping(value = "/deleteAgreementPeople")
	public String deleteAgreementPeople(@RequestBody AgreementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteAgreementPeople");
		return agreementService.deleteAgreementPeople(vo);
	}

	@PostMapping(value = "/agreementInvitation")
	public String grandInvitation(@RequestBody EmailServiceVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for agreementInvitation via email");
		return agreementService.agreementInvitation(vo);
	}

	@PostMapping(value = "/returnAgreement", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String returnAgreement(@RequestParam(value = "files", required = false) MultipartFile[] files, @RequestParam("formDataJson") String formDataJson) {
		logger.info("Requesting for returnAgreement");
		return agreementService.returnAgreement(files, formDataJson);
	}

	@GetMapping(value = "/printEntireAgreement")
	public void printEntireAgreement(HttpServletResponse response, @RequestHeader("agreementRequestId") String agreementRequestId,
			@RequestHeader(value = "personId", required = true) String personId,
			@RequestHeader(value = "userName", required = true) String userName) {
		logger.info("Requesting for agreement Request");
		logger.info("Agreement Request Id : {}", agreementRequestId);
		Integer agreementRequestIds = Integer.parseInt(agreementRequestId);
		agreementService.printEntireAgreement(agreementRequestIds, response, personId, userName);
	}

	@PostMapping(value = "/assignAgreementAdmin")
	public String assignAgreementAdmin(@RequestBody AgreementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for assignAgreementAdmin");
		logger.info(AGREEMENT_REQUEST_ID, vo.getAgreementRequestId());
		return agreementService.assignAgreementAdmin(vo, true);
	}

	@PostMapping(value = "/terminateAgreement")
	public String terminateAgreement(@RequestBody AgreementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for terminateAgreement");
		logger.info(AGREEMENT_REQUEST_ID, vo.getAgreementRequestId());
		return agreementService.terminateAgreement(vo);
	}

	@PostMapping(value = "/abandonAgreement")
	public String abandonAgreement(@RequestBody AgreementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for abandonAgreement");
		logger.info(AGREEMENT_REQUEST_ID, vo.getAgreementRequestId());
		return agreementService.abandonAgreement(vo);
	}

	@PostMapping(value = "/transferAgreement")
	public String transferAgreement(@RequestBody AgreementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for transferAgreement");
		logger.info(AGREEMENT_REQUEST_ID, vo.getAgreementRequestId());
		return agreementService.transferAgreement(vo);
	}

	@PostMapping(value = "/reopenAgreement")
	public String reopenAgreement(@RequestBody AgreementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for reopenAgreement");
		logger.info(AGREEMENT_REQUEST_ID, vo.getAgreementRequestId());
		return agreementService.reopenAgreement(vo);
	}

	@PostMapping(value = "/linkModuleToAgreement")
	public String linkModuleToAgreement(@RequestBody AgreementLinkModuleVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for linkModuleToAgreement");
		return agreementService.linkModuleToAgreement(vo);
	}

	@DeleteMapping(value = "/deleteClausesGroup/{clausesGroupCode}")
	public String deleteClausesGroup(@PathVariable(value = "clausesGroupCode", required = true) final Integer clausesGroupCode) {
		logger.info("Requesting for deleteClausesGroup");
		return agreementService.deleteClausesGroup(clausesGroupCode);
	}

	@GetMapping(value = "/getPersonGroup")
	public String getPersonGroup(HttpServletResponse response, @RequestHeader(value = "personId", required = true) String personId) throws Exception {
		logger.info("Requesting for getPersonGroup");
		logger.info("personId : {}", personId);
		return agreementService.getPersonGroup(personId);
	}
	
	@PostMapping(value = "/updateAgreementAttachmentDetails", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String updateAttachmentDetails(@RequestBody AgreementVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for updateAttachmentDetails");
		return agreementService.updateAttachmentDetails(vo);
	}

	@DeleteMapping(value = "/deleteAgreement/{agreementRequestId}")
	public String deleteAgreement(@PathVariable(value = "agreementRequestId", required = true) final Integer agreementRequestId) {
		logger.info("Request for deleteAgreement");
		logger.info("agreementRequestId {}", agreementRequestId);
		return agreementService.deleteAgreement(agreementRequestId);
	}

}
