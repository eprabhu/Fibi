package com.polus.fibicomp.claims.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.authorization.document.UserDocumentAuthorization;
import com.polus.fibicomp.claims.service.ClaimsService;
import com.polus.fibicomp.claims.vo.ClaimsVO;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.security.AuthenticatedUser;

@RestController
public class ClaimsController {

	protected static Logger logger = LogManager.getLogger(ClaimsController.class.getName());

	@Autowired
	@Qualifier(value = "claimsService")
	private ClaimsService claimsService;

	@Autowired
	private UserDocumentAuthorization documentAuthorization;

	@PostMapping(value = "/saveOrUpdateClaim")
	public String saveOrUpdateClaim(@RequestBody ClaimsVO claimsVO) {
		logger.info("Request for saveOrUpdateClaim");
		return claimsService.saveOrUpdateClaim(claimsVO);
	}

	@PostMapping(value = "/loadClaimDetails")
	public ResponseEntity<String> loadClaimEndorsment(@RequestBody ClaimsVO claimsVO, HttpServletRequest request) {
		logger.info("Request for loadClaimDetails");		
		logger.info("Claim id : {}",claimsVO.getClaimId());
		claimsVO.setPersonId(AuthenticatedUser.getLoginPersonId());
		if (!documentAuthorization.isAuthorized(Constants.CLAIM_MODULE_CODE, claimsVO.getClaimId().toString(), claimsVO.getPersonId())) {
			return new ResponseEntity<>("Not Authorized to view this claim",HttpStatus.FORBIDDEN);
		}
		return new ResponseEntity<>(claimsService.loadClaimEndorsment(claimsVO), HttpStatus.OK);
	}

	@PostMapping(value = "/submitClaim")
	public String submitClaimDetails(@RequestBody ClaimsVO claimsVO) {
		logger.info("Requesting for submitClaim");
		logger.info("Claim id {}",claimsVO.getClaimId());
		claimsVO.setUpdateUser(AuthenticatedUser.getLoginUserName());
		claimsVO.setPersonId(AuthenticatedUser.getLoginPersonId());
		return claimsService.submitClaimDetails(claimsVO);
	}
	
	@PostMapping(value = "/loadClaimReimbursement")
	public String loadClaimReimbursement(@RequestBody ClaimsVO claimsVO) {
		logger.info("Request for loadClaimReimbursement");
		logger.info("Claim id {}",claimsVO.getClaimId());
		return claimsService.loadClaimReimbursement(claimsVO);
	}

	@PostMapping(value = "/loadClaimDetailBreakDown")
	public String loadClaimDetailBreakDown(@RequestBody ClaimsVO claimsVO) {
		logger.info("Request for loadClaimDetailBreakDown");
		logger.info("Claim id {}",claimsVO.getClaimId());
		return claimsService.loadClaimDetailBreakDown(claimsVO);
	}
	
	@PostMapping(value = "/saveOrUpdateClaimBreakDown")
	public String saveOrUpdateClaimBreakDown(@RequestBody ClaimsVO claimsVO) {
		logger.info("Request for saveOrUpdateClaimBreakDown");
		return claimsService.saveOrUpdateClaimBreakDown(claimsVO);
	}
	
	@PostMapping(value = "/saveOrUpdateClaimOverHead")
	public String saveOrUpdateClaimOverHead(@RequestBody ClaimsVO claimsVO) {
		logger.info("Request for saveOrUpdateClaimOverHead");
		return claimsService.saveOrUpdateClaimOverHead(claimsVO);
	}
	
	@PostMapping(value = "/getPrevExcludedClaimSummaryDetails")
	public String getPrevExcludedClaimSummaryDetails(@RequestBody ClaimsVO claimsVO) {
		logger.info("Request for getPrevExcludedClaimSummaryDetails");
		logger.info("Claim id {}",claimsVO.getClaimId());
		return claimsService.getPrevExcludedClaimSummaryDetails(claimsVO);
	}
	
	@PostMapping(value = "/saveOrUpdateClaimAttachment", consumes = MediaType.MULTIPART_FORM_DATA_VALUE, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String addAwardAttachments(@RequestParam(value = "files", required = false) MultipartFile[] files, @RequestParam("formDataJson") String formDataJson) {
		logger.info("Request for adding saveOrUpdateClaimAttachment");
		return claimsService.saveOrUpdateClaimAttachment(files, formDataJson);
	}
	
	@PostMapping(value = "/loadClaimAttachments")
	public String loadClaimAttachments(@RequestBody ClaimsVO claimsVO) {
		logger.info("Request for loadClaimAttachments");
		logger.info("Claim id {}",claimsVO.getClaimId());
		return claimsService.loadClaimAttachments(claimsVO);
	}
	
	@PostMapping(value = "/updateClaimSummaryExcludeFlag")
	public String updateClaimSummaryExcludeFlag(@RequestBody ClaimsVO claimsVO) {
		logger.info("Request for updateClaimSummaryExcludeFlag");
		return claimsService.updateClaimSummaryExcludeFlag(claimsVO);
	}
	
	@PostMapping(value = "/loadClaimAdvance")
	public String loadClaimAdvance(@RequestBody ClaimsVO claimsVO) {
		logger.info("Request for loadClaimAdvance");
		logger.info("Claim id {}",claimsVO.getClaimId());
		return claimsService.loadClaimAdvance(claimsVO);
	}
	
	@GetMapping(value = "/downloadClaimAttachment")
	public ResponseEntity<byte[]> downloadClaimAttachment(HttpServletResponse response, @RequestHeader("attachmentId") String claimAttachmentId) {
		logger.info("Requesting for downloadClaimAttachment");
		logger.info("Claim attachmentId {}",claimAttachmentId);
		return claimsService.downloadClaimAttachment(Integer.parseInt(claimAttachmentId));
	}
	
	@PostMapping(value = "/loadClaimManpower")
	public String loadClaimManpower(@RequestBody ClaimsVO claimsVO) {
		logger.info("Request for loadClaimManpower");
		return claimsService.loadClaimManpower(claimsVO);
	}
	
	@PostMapping(value = "/generateClaimReport")
	public ResponseEntity<byte[]> generateClaims(@RequestBody ClaimsVO vo, HttpServletResponse response) {
		return claimsService.generateClaimReport(vo, response);
	}

	@PostMapping(value = "/loadSubmittedClaimsForAward")
	public String loadClaimsForAward(@RequestBody ClaimsVO claimsVO) {
		logger.info("Request for loadSubmittedClaimsForAward");
		return claimsService.loadSubmittedClaimsForAward(claimsVO);
	}

	@PostMapping(value = "/performClaimFOActions")
	public String performClaimFOActions(@RequestBody ClaimsVO claimsVO) {
		logger.info("Request for performClaimFOActions");
		return claimsService.performClaimFOActions(claimsVO);
	}

	@PostMapping(value = "/saveOrUpdateClaimManpower")
	public String saveOrUpdateClaimManpower(@RequestBody ClaimsVO claimsVO) {
		logger.info("Request for saveOrUpdateClaimManpower");
		return claimsService.saveOrUpdateClaimManpower(claimsVO);
	}

	@GetMapping(value = "/downloadClaimForcastTemplate")
	public ResponseEntity<byte[]> downloadClaimForcastTemplate() {
		return claimsService.downloadClaimForcastTemplate();
	}

	@PatchMapping(value = "/updateAdjustedIndirectCost")
	public String updateAdjustedIndirectCost(@RequestBody ClaimsVO claimsVO) {
		logger.info("Request for updateAdjustedIndirectCost");
		return claimsService.updateAdjustedIndirectCost(claimsVO);
	}

	@GetMapping(value = "/evaluateClaimIndirectCost/{claimId}")
	public String evaluateClaimIndirectCost(@PathVariable(value = "claimId", required = true) final Integer claimId) {
		logger.info("Request for evaluateClaimIndirectCost");
		logger.info("claimId {}", claimId);
		return claimsService.evaluateClaimIndirectCost(claimId);
	}

	@GetMapping(value = "/updateClaimDuration")
	public String updateClaimDuration() {
		logger.info("Request for updateClaimDuration");
		return claimsService.updateClaimDuration();
	}

	@PostMapping(value = "/saveOrUpdateClaimInvoiceDetail")
	public String saveOrUpdateClaimInvoiceDetail(@RequestBody ClaimsVO claimsVO) {
		logger.info("Request for saveOrUpdateClaimInvoiceDetail");
		return claimsService.saveOrUpdateClaimInvoiceDetail(claimsVO);
	}

	@GetMapping(value = "/loadClaimInvoice/{claimId}")
	public String loadClaimInvoiceDetails(@PathVariable(value = "claimId", required = true) final Integer claimId) {
		logger.info("Request for loadClaimInvoice");
		logger.info("claimId {}", claimId);
		return claimsService.loadClaimInvoice(claimId);
	}

	@GetMapping(value = "/loadClaimInvoiceLookups")
	public String loadClaimInvoiceLookups() {
		logger.info("Request for loadClaimInvoiceLookups");
		return claimsService.loadClaimInvoiceLookups();
	}

	@PostMapping(value = "/saveOrUpdateClaimInvoice")
	public String saveOrUpdateClaimInvoice(@RequestBody ClaimsVO claimsVO) {
		logger.info("Request for saveOrUpdateClaimInvoice");
		return claimsService.saveOrUpdateClaimInvoice(claimsVO);
	}

	@DeleteMapping(value = "/deleteClaimInvoiceDetail/{invoiceDetailId}")
	public String deleteClaimInvoiceDetail(@PathVariable(value = "invoiceDetailId", required = true) final Integer invoiceDetailId) {
		logger.info("Request for deleteClaimInvoiceDetail");
		logger.info("invoiceDetailId {}", invoiceDetailId);
		return claimsService.deleteClaimInvoiceDetail(invoiceDetailId);
	}

	@GetMapping(value = "/loadClaimInvoiceSummary/{claimId}")
	public String loadClaimInvoiceSummary(@PathVariable(value = "claimId", required = true) final Integer claimId) {
		logger.info("Request for loadClaimInvoiceSummary");
		logger.info("claimId {}", claimId);
		return claimsService.loadClaimInvoiceSummary(claimId);
	}
	
	@GetMapping(value = "/loadClaimInvoiceSapResponse/{claimId}/{sequenceNumber}")
	public String loadClaimInvoiceSapResponse(@PathVariable final Integer claimId, @PathVariable final Integer sequenceNumber) {
		logger.info("Request for loadClaimInvoiceSapResponse");
		logger.info("claimId {}", claimId);
		return claimsService.loadClaimInvoiceSapResponse(claimId, sequenceNumber);
	}

	@PostMapping(value = "/deleteClaimDetail")
	public String deleteClaimDetail(@RequestBody ClaimsVO claimsVO) {
		logger.info("Request for deleteClaimDetail");
		return claimsService.deleteClaimDetail(claimsVO);
	}

	@PostMapping(value = "/resyncClaimDetail")
	public String resyncClaimDetail(@RequestBody ClaimsVO claimsVO) {
		logger.info("Request for resyncClaimDetail");
		return claimsService.resyncClaimDetail(claimsVO);
	}

}
