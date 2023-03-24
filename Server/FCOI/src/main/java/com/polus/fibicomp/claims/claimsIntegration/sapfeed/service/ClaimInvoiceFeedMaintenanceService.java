package com.polus.fibicomp.claims.claimsIntegration.sapfeed.service;

import com.polus.fibicomp.fastintegration.sapfeedmaintenance.vo.SapFeedMaintenanceVO;

import javax.servlet.http.HttpServletResponse;

import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

@Transactional
@Service(value = "claimInvoiceFeedMaintenance")
public interface ClaimInvoiceFeedMaintenanceService {

    /**
     * @param maintenanceVO
     * @return sap claim dashboard
     */
    public String getClaimBatchDashboard(SapFeedMaintenanceVO maintenanceVO);

	/**
	 * @param invoiceId
	 * @param batchId 
	 * @return claim invoice log
	 */
	public String loadClaimInvoiceLog(Integer invoiceId, Integer batchId);

	/**
	 * @param vo
	 * @return true
	 */
	public String claimInvoiceNotifyPI(SapFeedMaintenanceVO vo);

	/**
	 * @param vo
	 * @return update status
	 */
	public String updateClaimInvoiceFeedStatus(SapFeedMaintenanceVO vo);

	/**
	 * @param vo
	 * @return String
	 */
	public String reinterfaceClaimSapFeed(SapFeedMaintenanceVO vo);

	/**
	 * This method is used to export invoice attachments based on batch id
	 * @param SapFeedMaintenanceVO
	 * @param response
	 */
	public ResponseEntity<byte[]> exportClaimInvoiceAttachments(SapFeedMaintenanceVO vo);

	/**
	 * This method is used to export invoice feed details
	 * @param SapFeedMaintenanceVO
	 * @param response
	 */
	public ResponseEntity<byte[]> generateInvoiceFeedReport(HttpServletResponse response, SapFeedMaintenanceVO vo);

}
