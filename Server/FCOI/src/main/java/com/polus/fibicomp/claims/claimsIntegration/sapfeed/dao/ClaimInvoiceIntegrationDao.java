package com.polus.fibicomp.claims.claimsIntegration.sapfeed.dao;

import java.util.List;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.claims.claimsIntegration.sapfeed.pojo.SapClaimFeed;
import com.polus.fibicomp.claims.claimsIntegration.sapfeed.pojo.SapClaimFeedResponseMessage;
import com.polus.fibicomp.claims.pojo.ClaimInvoiceLog;
import com.polus.fibicomp.fastintegration.vo.IntegrationReportVO;

@Transactional
@Service
public interface ClaimInvoiceIntegrationDao {

	/**
	 * @param batchId
	 * @return List<ClaimInvoiceLog>
	 */
	public List<ClaimInvoiceLog> getClaimInvoiceLogByBatchId(Integer batchId);

	/**
	 * @param claimInvoiceLog
	 */
	public void saveOrUpdateClaimInvoiceLog(ClaimInvoiceLog claimInvoiceLog);

	/**
	 * @param mesg
	 */
	public void saveOrUpdateClaimSapResponseMsg(SapClaimFeedResponseMessage mesg);

	/**
	 * @param vo 
	 * @param object
	 * @return batchId
	 */
	public Integer generateInvoiceBatchId(List<String> feedIds, IntegrationReportVO vo);

	/**
	 * @param claimNumber
	 * @param status
	 */
	public void updateClaimStatus(String claimNumber, String status);

	/**
	 * @param claimNumber
	 * @param batchId
	 * @param userActionCode 
	 */
	public void updateClaimSapFeedStatus(String claimNumber, Integer batchId, String userActionCode);

	/**
	 * @param batchId
	 */
	public void updateClaimBatch(String batchId);

	/**
	 * @param batchId
	 * @param userActionCode 
	 * @param errorClaimNumbers 
	 */
	public void updateFeedStatusToSuccess(String batchId, String userActionCode, List<String> errorClaimNumbers);

	/**
	 * This method is used to get sap claim feed detail based on batch id
	 * @param batchId
	 */
	public List<SapClaimFeed> getClaimInvoiceBatchByBatchId(Integer batchId);

	/**
	 * This method is used to get sap claim feed status based details
	 * @param batchId
	 */
	public List<Integer> getSapClaimCount(Integer batchId);

	/**
	 * @param claimInvoiceLogId
	 */
	ClaimInvoiceLog getClaimInvoiceLogById(Integer claimInvoiceLogId);
}
