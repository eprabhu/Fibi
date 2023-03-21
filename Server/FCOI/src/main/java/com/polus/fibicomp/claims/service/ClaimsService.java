package com.polus.fibicomp.claims.service;

import java.util.Set;

import javax.servlet.http.HttpServletResponse;

import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;

import com.polus.fibicomp.claims.vo.ClaimsVO;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;

@Transactional
@Service(value = "claimsService")
public interface ClaimsService {

	/**
	 * this function is used to save and update a claim
	 * @param claimsVO
	 * @return
	 */
	public String saveOrUpdateClaim(ClaimsVO claimsVO);

	/**
	 * this function is used to load claim basic details
	 * @param claimsVO
	 * @return basic claim details
	 */
	public String loadClaimEndorsment(ClaimsVO claimsVO);

	/**
	 * This method is used to submit the claim details.
	 * @param vo - Object of ClaimsVO class.
	 * @return A string of details of a claim.
	 */
	public String submitClaimDetails(ClaimsVO vo);

	/**
	 * This method is can claim Take Routing Action
	 * @param ClaimsVO
	 * @return
	 */
	public void canClaimTakeRoutingAction(ClaimsVO claimVO);

	/**
	 * This method is used for sending notification for claim
	 * @param claimVO
	 * @param notificationTypeId
	 * @param dynamicEmailRecipients
	 * @return claimVO object
	 */
	public ClaimsVO sendClaimNotification(ClaimsVO claimVO, Integer notificationTypeId, Set<NotificationRecipient> dynamicEmailRecipients);
	
	/**
	 * this method is used to load claim summary details 
	 * @param claimsVO
	 * @return updated claimVo
	 */
	public String loadClaimReimbursement(ClaimsVO claimsVO);

	/**
	 * this method is used to load claim break down details
	 * @param claimsVO
	 * @return updated claimVo
	 */
	public String loadClaimDetailBreakDown(ClaimsVO claimsVO);

	/**
	 * this method is used to update claim breakdown details
				  
	 * @param claimsVO
	 * @return updated claimVo
						  
	 */
	public String saveOrUpdateClaimBreakDown(ClaimsVO claimsVO);

	/**
	 * this method is used to update claim overhead
	 * @param claimsVO
	 * @return updated claimVo
	 */
	public String saveOrUpdateClaimOverHead(ClaimsVO claimsVO);

	/**
	 * this method is used to get previous excluded claim summary details
	 * @param claimsVO
	 * @return updated claimVo
	 */
	public String getPrevExcludedClaimSummaryDetails(ClaimsVO claimsVO);

	/**
	 * this function is used to add/update claim attachment
	 * @param files
	 * @param formDataJson
	 * @return updated data
	 */
	public String saveOrUpdateClaimAttachment(MultipartFile[] files, String formDataJson);

	/**
	 * @param claimsVO
	 * @return attachment list
	 */
	public String loadClaimAttachments(ClaimsVO claimsVO);

	/**
	 * @param claimsVO
	 * @return updated data
	 */
	public String updateClaimSummaryExcludeFlag(ClaimsVO claimsVO);

	/**
	 * @param claimsVO
	 * @return list of claim advance details
	 */
	public String loadClaimAdvance(ClaimsVO claimsVO);

	/**
	 * @param attachmentId
	 * @return byte stream
	 */
	public ResponseEntity<byte[]> downloadClaimAttachment(Integer attachmentId);

	/**
	 * @param claimsVO
	 * @return claim manpower
	 */
	public String loadClaimManpower(ClaimsVO claimsVO);

	/**
	 * @param vo
	 * @return byte stream
	 */
	public ResponseEntity<byte[]> generateClaimReport(ClaimsVO vo, HttpServletResponse response);

	/**
	 * @param claimsVO
	 * @return list of award claims
	 */
	public String loadSubmittedClaimsForAward(ClaimsVO claimsVO);

	/**
	 * @param claimsVO
	 * @return updated claim status
	 */
	public String performClaimFOActions(ClaimsVO claimsVO);

	/**
	 * @param claimsVO
	 * @return update claim manpower
	 */
	public String saveOrUpdateClaimManpower(ClaimsVO claimsVO);
	
	/**
	 * @return forecast template
	 */
	public ResponseEntity<byte[]> downloadClaimForcastTemplate();

	/**
	 * @return updated adjusted direct cost
	 */
	public String updateAdjustedIndirectCost(ClaimsVO claimsVO);

	/**
	 * @return message and message type
	 */
	public String evaluateClaimIndirectCost(Integer claimId);

	/**
	 * @param claimsVO
	 */
	public void loadClaimDetails(ClaimsVO claimsVO);
	
	/**
	 * @return boolean
	 */
	public String updateClaimDuration();
	
	/**
	 * @param claimsVO
	 * @return updated claim invoice details
	 */
	public String saveOrUpdateClaimInvoiceDetail(ClaimsVO claimsVO);

	/**
	 * @param claimId
	 * @return claim details
	 */
	public String loadClaimInvoice(Integer claimId);

	/**
	 * @return claim invoice related lookups
	 */
	public String loadClaimInvoiceLookups();

	/**
	 * @param claimsVO
	 * @return update claim invoice
	 */
	public String saveOrUpdateClaimInvoice(ClaimsVO claimsVO);

	/**
	 * @param invoiceDetailId
	 * @return boolean
	 */
	public String deleteClaimInvoiceDetail(Integer invoiceDetailId);

	/**
	 * @param claimId
	 * @return invoice summary
	 */
	public String loadClaimInvoiceSummary(Integer claimId);

	/**
	 * @param claimId
	 * @param sequenceNumber
	 * @return sap response msg for the given invoice version
	 */
	public String loadClaimInvoiceSapResponse(Integer claimId, Integer sequenceNumber);

	/**
	 * This method is used to delete claim details
	 * @param claimsVO
	 * @return 
	 */
	public String deleteClaimDetail(ClaimsVO claimsVO);

	/**
	 * This method is used to sync claim details manually
	 * @param claimsVO
	 * @return 
	 */
	public String resyncClaimDetail(ClaimsVO claimsVO);

}
