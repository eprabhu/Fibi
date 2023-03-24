package com.polus.fibicomp.claims.claimsIntegration.sapfeed.controller;

import java.util.stream.Collectors;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibicomp.claims.claimsIntegration.sapfeed.dto.ClaimInvoiceFeedDto;
import com.polus.fibicomp.claims.claimsIntegration.sapfeed.service.ClaimInvoiceFeedMaintenanceService;
import com.polus.fibicomp.claims.claimsIntegration.sapfeed.service.ClaimInvoiceIntegrationService;
import com.polus.fibicomp.fastintegration.sapfeedmaintenance.vo.SapFeedMaintenanceVO;
import com.polus.fibicomp.security.AuthenticatedUser;

@RestController
public class ClaimIntegrationController {
	
    protected static Logger logger = LogManager.getLogger(ClaimIntegrationController.class.getName());

    @Autowired
    private ClaimInvoiceIntegrationService claimInvoiceIntegrationService;
    
    @Autowired
    private ClaimInvoiceFeedMaintenanceService claimInvoiceFeedMaintenanceService;
	   
	@GetMapping(value = "/claimInvoiceSapIntegration")
	public void claimInvoiceSapIntegration() {
		logger.info("claimInvoiceSapIntegration started");
		claimInvoiceIntegrationService.fastTemplateGeneration(null, null);
	}
	
	@GetMapping(value = {"/processClaimInvoiceFeedResponse","/processClaimInvoiceFeedResponse/{userActionCode}"})
	public void processClaimInvoiceFeedResponse(@PathVariable(value = "userActionCode", required = false) final String userActionCode) {
		logger.info("processClaimInvoiceFeedResponse started");
		claimInvoiceIntegrationService.processClaimInvoiceFeedResponse(userActionCode);
		logger.info("processClaimInvoiceFeedResponse completed");
	}
	
	@GetMapping(value = "/processClaimInvoiceFeedResponseOOE")
	public void processClaimInvoiceFeedResponseOOE() {
		logger.info("processClaimInvoiceFeedResponseOOE started");
		claimInvoiceIntegrationService.processClaimInvoiceFeedResponse(null);
		logger.info("processClaimInvoiceFeedResponseOOE completed");
	}

	@GetMapping(value = "/processClaimInvoiceFeedRequestOOE")
	public void processClaimInvoiceFeedRequestOOE() {
		logger.info("fastTemplateGeneration started");
		claimInvoiceIntegrationService.fastTemplateGeneration(null, null);
	}
	
	@PostMapping(value = "/getSapClaimFeedDashboard")
	public String fibiProgressReportDashBoard(@RequestBody SapFeedMaintenanceVO vo) {
		logger.info("Requesting for getSapClaimFeedDashboard");
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		return claimInvoiceFeedMaintenanceService.getClaimBatchDashboard(vo);
	}
	
	@GetMapping(value = "/loadClaimInvoiceLog/{invoiceId}/{batchId}")
	public String loadClaimInvoiceSummary(@PathVariable final Integer invoiceId, @PathVariable final Integer batchId) {
		logger.info("Request for loadClaimInvoiceLog");
		logger.info("invoiceId {}", invoiceId);
		return claimInvoiceFeedMaintenanceService.loadClaimInvoiceLog(invoiceId, batchId);
	}
	
	@PostMapping(value = "/claimInvoiceNotifyPI")
	public String claimInvoiceNotifyPI(@RequestBody SapFeedMaintenanceVO vo) {
		logger.info("Requesting for claimInvoiceNotifyPI");
		return claimInvoiceFeedMaintenanceService.claimInvoiceNotifyPI(vo);
	}
	
	@PostMapping(value = "/processClaimInvoiceSapFeed")
	public String processClaimInvoiceSapFeed(@RequestBody SapFeedMaintenanceVO vo) {
		logger.info("fastTemplateGeneration started");
		return claimInvoiceIntegrationService.fastTemplateGeneration(vo.getClaimInvoiceFeedDtoList().stream().
				filter(invoice -> invoice != null && invoice.getFeedId() != null).map(ClaimInvoiceFeedDto::getFeedId).collect(Collectors.toList()), vo);
	}
	
	@PostMapping(value = "/updateClaimInvoiceFeedStatus")
	public String updateClaimInvoiceFeedStatus(@RequestBody SapFeedMaintenanceVO vo) {
		logger.info("Requesting for updateClaimInvoiceFeedStatus");
		return claimInvoiceFeedMaintenanceService.updateClaimInvoiceFeedStatus(vo);
	}
	
	@PostMapping(value = "/reInterfaceClaimSapFeed")
	public String reinterfaceClaimSapFeed(@RequestBody SapFeedMaintenanceVO vo) {
		claimInvoiceFeedMaintenanceService.reinterfaceClaimSapFeed(vo);
		return claimInvoiceFeedMaintenanceService.getClaimBatchDashboard(vo);
	}

	@PostMapping(value = "/exportClaimInvoiceAttachments")
	public ResponseEntity<byte[]> exportClaimInvoiceAttachments(@RequestBody SapFeedMaintenanceVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for exportClaimInvoiceAttachments");
		return claimInvoiceFeedMaintenanceService.exportClaimInvoiceAttachments(vo);
	}

	@PostMapping(value = "/generateInvoiceFeedReport")
	public ResponseEntity<byte[]> generateInvoiceFeedReport(HttpServletResponse response, HttpServletRequest request, @RequestBody SapFeedMaintenanceVO vo) {
		logger.info("Requesting for generateInvoiceFeedReport");
		return claimInvoiceFeedMaintenanceService.generateInvoiceFeedReport(response, vo);
	}

}
