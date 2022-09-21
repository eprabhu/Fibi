package com.polus.fibicomp.claims.claimsIntegration.sapfeed.dao;

import com.polus.fibicomp.claims.claimsIntegration.sapfeed.dto.ClaimInvoiceFeedDto;
import com.polus.fibicomp.claims.claimsIntegration.sapfeed.pojo.SapClaimFeed;
import com.polus.fibicomp.claims.claimsIntegration.sapfeed.pojo.SapClaimFeedBatch;
import com.polus.fibicomp.claims.claimsIntegration.sapfeed.pojo.SapClaimFeedResponseMessage;
import com.polus.fibicomp.claims.pojo.ClaimInvoiceLog;
import com.polus.fibicomp.fastintegration.sapfeedmaintenance.vo.SapFeedMaintenanceVO;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

@Transactional
@Service
public interface ClaimInvoiceFeedMaintenanceDao {

    /**
     * this method is used to get default listing in batch detail dashboard which is latest batch detail
     * @return SapClaimFeedBatch
     */
    public SapClaimFeedBatch getClaimLatestBatchDashboard();

    /**
     * this method is used to get claim invoice feed details
     * @param maintenanceVO
     * @return list of claim invoice feed details
     */
    public List<ClaimInvoiceFeedDto> getSapClaimFeedDashBoard(SapFeedMaintenanceVO maintenanceVO);

    /**
     * This method is used to get sap feed batch details by batch id
     * @param batchId
     * @return SapClaimFeedBatch
     */
    public SapClaimFeedBatch getSapFeedClaimBatchById(Integer batchId);

	/**
	 * @param batchId
	 * @return count
	 */
	public Integer getSapClaimBatchCount(Integer batchId);

	/**
	 * @param invoiceId
	 * @param batchId
	 * @return List<ClaimInvoiceLog>
	 */
	public List<ClaimInvoiceLog> loadClaimInvoiceLog(Integer invoiceId, Integer batchId);

	/**
	 * @param userActionCode
	 * @param userComment
	 * @param feedId
	 */
	public void updateSapClaimFeedUserAction(String userActionCode, String userComment, Integer feedId);

	/**
	 * @param invoiceLogId
	 * @param outputDocNumber
	 * @param fiscalyear
	 */
	public void updateClaimInvoiceLog(Integer invoiceLogId, String outputDocNumber, String fiscalyear);

	/**
	 * @param feedId
	 * @param userAction
	 * @param userComment
	 */
	public void updateSapClaimFeed(Integer feedId, String userAction, String userComment);

	/**
	 * @param feedIds
	 * @return List<SapClaimFeed>
	 */
	public List<SapClaimFeed> getClaimFeedByIds(List<Integer> feedIds);

	/**
	 * @param claimInvoiceLogIds
	 * @return List<SapClaimFeedResponseMessage>
	 */
	public List<SapClaimFeedResponseMessage> getSapClaimErrorMsg(List<Integer> claimInvoiceLogIds);
}
