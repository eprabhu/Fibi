package com.polus.fibicomp.ip.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
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

import com.polus.fibicomp.applicationexception.dto.ApplicationException;
import com.polus.fibicomp.authorization.document.UserDocumentAuthorization;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.ip.service.InstitutionalProposalService;
import com.polus.fibicomp.ip.vo.InstProposalVO;
import com.polus.fibicomp.proposal.vo.ProposalVO;
import com.polus.fibicomp.security.AuthenticatedUser;

@RestController
public class InstitutionalProposalController {

	protected static Logger logger = LogManager.getLogger(InstitutionalProposalController.class.getName());

	@Autowired
	private InstitutionalProposalService institutionalProposalService;

	@Autowired
	private CommonService commonService;

	@Autowired
	private UserDocumentAuthorization documentAuthorization;

	@Autowired
	private CommonDao commonDao;

	@GetMapping(value = "/getNext")
	public Long getNext(HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getNext");
		return commonService.getNextSequenceNumber(Constants.INSTITUTIONAL_PROPSAL_PROPSAL_NUMBER_SEQUENCE);
	}

	@PostMapping(value = "/loadInstProposalById")
	public ResponseEntity<String> loadInstProposalById(@RequestBody InstProposalVO vo, HttpServletRequest request,
			HttpServletResponse response) {
		logger.info("Requesting for loadInstituteProposalById");
		logger.info("proposalId : {} " , vo.getProposalId());
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		vo.setUserName(AuthenticatedUser.getLoginUserName());
		logger.info("loginPersonId : {}", vo.getPersonId());
		logger.info("userName : {}", vo.getUserName());
		HttpStatus httpStatus = HttpStatus.OK; 
        if(!documentAuthorization.isAuthorized(2, vo.getProposalId().toString(), vo.getPersonId())) {
                httpStatus = HttpStatus.FORBIDDEN;        
                return new ResponseEntity<>("Not Authorized to view this Proposal", httpStatus);
        }
		String responseData = institutionalProposalService.loadInstProposalById(vo.getProposalId(), vo.getPersonId());
        return new ResponseEntity<>(responseData, httpStatus);		
	}

	@GetMapping(value = "/downloadInstituteProposalAttachment")
	public ResponseEntity<byte[]> downloadProposalAttachment(HttpServletResponse response,
			@RequestHeader("attachmentId") Integer attachmentId) {
		logger.info("Requesting for downloadInstituteProposalAttachment");
		logger.info("attachmentId : {}" , attachmentId);
		return institutionalProposalService.downloadInstProposalAttachment(attachmentId);
	}

	@PostMapping(value = "/saveOrUpdateInstituteProposal")
	public String saveOrUpdateInstituteProposal(@RequestBody InstProposalVO vo, HttpServletRequest request, HttpServletResponse response) throws ApplicationException {
		logger.info("Requesting for saveOrUpdateInstituteProposal");
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		logger.info("loginPersonId : {}", vo.getPersonId());
		return institutionalProposalService.saveOrUpdateInstituteProposal(vo);
	}

	@GetMapping(value = "/getInstituteProposalAttachments/{proposalId}")
	public String getInstituteProposalAttachments(@PathVariable(value = "proposalId", required = true) final Integer proposalId) {
		logger.info("Requesting for getInstituteProposalAttachments");
		return institutionalProposalService.getInstituteProposalAttachments(proposalId);
	}

	@PostMapping(value = "/createNewIPVersion")
	public String createNewIPVersion(@RequestBody InstProposalVO vo, HttpServletRequest request, HttpServletResponse response) throws ApplicationException {
		logger.info("Requesting for createNewIPVersion");
		logger.info("copying from proposalId : {}", vo.getProposalId());
		return institutionalProposalService.createNewIPVersion(vo);
	}

	@PostMapping(value = "/changeIPStatus")
	public String changeIPStatus(@RequestBody InstProposalVO vo, HttpServletRequest request, HttpServletResponse response) throws ApplicationException {
		logger.info("Requesting for changeIPStatus");
		logger.info("proposalId : {}", vo.getProposalId());
		return institutionalProposalService.changeIPStatus(vo);
	}

	@PostMapping(value = "/saveOrUpdateIPKeyPerson")
	public String saveOrUpdateIPKeyPerson(@RequestBody InstProposalVO vo, HttpServletRequest request, HttpServletResponse response) throws ApplicationException {
		logger.info("Requesting for saveOrUpdateIPKeyPerson");
		return institutionalProposalService.saveOrUpdateIPKeyPerson(vo);
	}

	@PostMapping(value = "/addIPPersonAttachment")
	public String addIPPersonAttachment(@RequestParam(value = "files", required = false) MultipartFile[] files, @RequestParam("formDataJson") String formDataJson) throws ApplicationException {
		logger.info("Requesting for addIPPersonAttachment");
		return institutionalProposalService.addIPPersonAttachment(files, formDataJson);
	}

	@PostMapping(value = "/saveOrUpdateIPSpecialReview")
	public String saveOrUpdateIPSpecialReview(@RequestBody InstProposalVO vo, HttpServletRequest request, HttpServletResponse response) throws ApplicationException {
		logger.info("Requesting for saveOrUpdateIPSpecialReview");
		return institutionalProposalService.saveOrUpdateIPSpecialReview(vo);
	}

	@PostMapping(value = "/saveOrUpdateIPAreaOfResearch")
	public String saveOrUpdateIPAreaOfResearch(@RequestBody InstProposalVO vo, HttpServletRequest request, HttpServletResponse response) throws ApplicationException {
		logger.info("Requesting for saveOrUpdateIPAreaOfResearch");
		return institutionalProposalService.saveOrUpdateIPAreaOfResearch(vo);
	}

	@PostMapping(value = "/saveOrUpdateIPBudgetData")
	public String saveOrUpdateIPBudget(@RequestBody InstProposalVO vo, HttpServletRequest request, HttpServletResponse response) throws ApplicationException {
		logger.info("Requesting for saveOrUpdateIPBudget");
		return institutionalProposalService.saveOrUpdateIPBudget(vo);
	}

	@DeleteMapping(value = "/deleteIPKeyPerson/{proposalId}/{keyPersonId}")
	public String deleteIPKeyPerson(@PathVariable(value = "proposalId", required = true) final Integer proposalId,
			@PathVariable(value = "keyPersonId", required = true) final Integer keyPersonId) {
		logger.info("Request for deleteIPKeyPerson");
		return institutionalProposalService.deleteIPKeyPerson(keyPersonId, proposalId);
	}

	@DeleteMapping(value = "/deleteIPSpecialReview/{proposalId}/{specialReviewId}")
	public String deleteIPSpecialReview(@PathVariable(value = "proposalId", required = true) final Integer proposalId,
			@PathVariable(value = "specialReviewId", required = true) final Integer specialReviewId) {
		logger.info("Request for deleteReportTracking");
		return institutionalProposalService.deleteIPSpecialReview(specialReviewId, proposalId);
	}

	@DeleteMapping(value = "/deleteIPAreaOfResearch/{proposalId}/{areaOfResearchId}")
	public String deleteIPAreaOfResearch(@PathVariable(value = "proposalId", required = true) final Integer proposalId,
			@PathVariable(value = "areaOfResearchId", required = true) final Integer areaOfResearchId) {
		logger.info("Request for deleteIPAreaOfResearch");
		return institutionalProposalService.deleteIPAreaOfResearch(areaOfResearchId, proposalId);
	}

	@DeleteMapping(value = "/deleteIPBudgetData/{proposalId}/{budgetHeaderId}")
	public String deleteIPBudgetData(@PathVariable(value = "proposalId", required = true) final Integer proposalId,
			@PathVariable(value = "budgetHeaderId", required = true) final Integer budgetHeaderId) {
		logger.info("Request for deleteIPBudgetData");
		return institutionalProposalService.deleteIPBudgetData(budgetHeaderId, proposalId);
	}

	@PostMapping(value = "/addIPAttachment")
	public String addIPAttachment(@RequestParam(value = "files", required = false) MultipartFile[] files, @RequestParam("formDataJson") String formDataJson) throws ApplicationException {
		logger.info("Requesting for addIPAttachment");
		return institutionalProposalService.addIPAttachment(files, formDataJson);
	}

	@DeleteMapping(value = "/deleteIPAttachment/{proposalId}/{attachmentId}")
	public String deleteIPAttachment(@PathVariable(value = "proposalId", required = true) final Integer proposalId,
			@PathVariable(value = "attachmentId", required = true) final Integer attachmentId) {
		logger.info("Request for deleteIPAttachment");
		return institutionalProposalService.deleteIPAttachment(attachmentId, proposalId);
	}

	@PostMapping(value = "/updateIPAttachmentDetails")
	public String updateIPAttachmentDetails(@RequestBody InstProposalVO vo, HttpServletRequest request, HttpServletResponse response) throws ApplicationException {
		logger.info("Requesting for updateIPAttachmentDetails");
		return institutionalProposalService.updateIPAttachmentDetails(vo);
	}

	@PostMapping(value = "/submitInstituteProposal")
	public String submitInstituteProposal(@RequestBody InstProposalVO vo) throws ApplicationException {
		logger.info("Request for submitInstituteProposal");
		logger.info("proposalId : {}", vo.getProposalId());
		return institutionalProposalService.submitInstituteProposal(vo);
	}

	@DeleteMapping(value = "/deleteIPKeyword/{proposalId}/{keywordId}")
	public String deleteIPKeyword(@PathVariable(value = "proposalId", required = true) final Integer proposalId,
			@PathVariable(value = "keywordId", required = true) final Integer keywordId) {
		logger.info("Request for deleteIPBudgetData");
		return institutionalProposalService.deleteIPKeyword(keywordId, proposalId);
	}

	@GetMapping(value = "/loadIPBudgetsByProposalId/{proposalId}")
	public String loadIPBudgetsByProposalId(@PathVariable(value = "proposalId", required = true) final Integer proposalId) {
		logger.info("Request for loadIPBudgetsByProposalId");
		return institutionalProposalService.loadIPBudgetsByProposalId(proposalId);
	}

	@PostMapping(value = "/saveIPMoreInformation", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveIPMoreInformation(@RequestBody InstProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveDescriptionOfProposal");
		return institutionalProposalService.saveIPMoreInformation(vo);
	}

	@GetMapping(value = "/getIPActionHistory/{proposalId}")
	public String getIPActionHistory(@PathVariable(value = "proposalId", required = true) final Integer proposalId) {
		logger.info("Request for getIPActionHistory");
		return institutionalProposalService.getIPActionHistory(proposalId);
	}

	@PostMapping(value = "/mergeProposalToIP")
	public ResponseEntity<String> mergeProposalToIP(@RequestBody InstProposalVO vo, HttpServletRequest request, HttpServletResponse response) throws ApplicationException {
		logger.info("Requesting for mergeProposalToIP");
		if (institutionalProposalService.canMergeDevelopmentProposalToIP(vo.getProposalNumber()).equals(Boolean.TRUE)) {
			if (institutionalProposalService.checkForPIDifference(vo.getProposalNumber(), vo.getDevProposalId()).equals(Boolean.TRUE)) {
				return new ResponseEntity<>(commonDao.convertObjectToJSON("You can't merge the development proposal to institutional proposal (PI in development proposal & institutional proposal are different)"), HttpStatus.OK);
			}
			return new ResponseEntity<>(institutionalProposalService.mergeProposalToIP(institutionalProposalService.getCreatedIPVersion(vo), vo), HttpStatus.OK);
		}
		return new ResponseEntity<>(commonDao.convertObjectToJSON("You can't merge the development proposal to institutional proposal since the IP is already linked to an award/ IP status is Funded/IP has a pending version"), HttpStatus.OK);
	}

	@PostMapping(value = "/generateInstituteProposal")
	public String generateInstituteProposal(@RequestBody ProposalVO vo, HttpServletRequest request, HttpServletResponse response) throws ApplicationException {
		logger.info("Requesting for generateInstituteProposal for : {}", vo.getProposalId());
		return institutionalProposalService.generateInstituteProposal(vo);
	}

	@GetMapping(value = "/fetchInstituteProposalComments/{proposalId}")
	public String fetchProposalComments(@PathVariable(value = "proposalId", required = true) final Integer proposalId) {
		logger.info("Requesting for fetchInstituteProposalComments for : {}", proposalId);
		return institutionalProposalService.fetchInstituteProposalComments(proposalId);
	}

	@PostMapping(value = "/saveOrUpdateInstituteProposalComment")
	public String saveOrUpdateProposalComment(@RequestBody InstProposalVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveOrUpdateInstituteProposalComment");
		return institutionalProposalService.saveOrUpdateInstituteProposalComment(vo);
	}

	@PostMapping(value = "/showInstituteProposalHistory")
	public String showInstituteProposalHistory(@RequestBody InstProposalVO vo) {
		logger.info("Request for showInstituteProposalHistory");
		logger.info("proposalNumber {}", vo.getProposalNumber());
		return institutionalProposalService.showInstituteProposalHistory(vo);
	}

}
