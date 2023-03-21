package com.polus.fibicomp.claims.dao;

import java.math.BigDecimal;
import java.util.Date;
import java.util.List;
import java.util.Set;

import com.polus.fibicomp.claims.claimsIntegration.sapfeed.pojo.SapClaimFeed;
import com.polus.fibicomp.claims.claimsIntegration.sapfeed.pojo.SapClaimFeedResponseMessage;
import com.polus.fibicomp.claims.pojo.*;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.manpower.pojo.AwardManpower;
import com.polus.fibicomp.manpower.pojo.AwardManpowerResource;
import com.polus.fibicomp.manpower.pojo.Manpower;
import com.polus.fibicomp.view.CustomDataView;

@Transactional
@Service
public interface ClaimsDao {

	/**
	 * this method is used to create a new claim
	 * @param claim
	 * @param acType 
	 * @return 
	 */
	public Claim insertClaim(Claim claim, String acType);

	/**
	 * this method is used to save or update a claim
	 * @param claim
	 */
	public void saveOrUpdateClaim(Claim claim);

	/**
	 * this method is used to get claim details
	 * @param claimId
	 * @return claim
	 */
	public Claim getClaim(Integer claimId);

	/**
	 * this method is used to get ClaimStatus details
	 * @param claimStatusCode
	 * @return claimStatus
	 */
	public ClaimStatus getClaimStatusByStatusCode(String claimStatusCode);

	/**
	 * this method is used to update a claim detail
	 * @param claimId
	 * @param claimStatusCode
	 * @param isSubmit 
	 * @param updatedUser
	 * @param funderApprovalDate
	 */
	public void updateClaimDetailByParams(Integer claimId, String claimStatusCode, Boolean isSubmit, String updatedUser, Date funderApprovalDate);

	/**
	 * this method is used to get claimStatusCode by claimId
	 * @param claimId
	 * @return claimStatusCode
	 */
	public String getClaimStatusCodeByClaimId(Integer claimId);
	
	/**
	 * this method is used to load claim summary details 
	 * @param claimId
	 * @return
	 */
	public List<ClaimSummary> loadClaimSummary(Integer claimId);

	/**
	 * this method is used to load claim break down details
	 * @param claimId
	 * @return
	 */
	public List<ClaimSummaryDetails> loadClaimDetailBreakDown(Integer claimId);

	/**	 
	 * this method is used to update claim breakdown details
	 * @param claimSummaryDetail
	 */
	public void updateClaimBreakDown(ClaimSummaryDetails claimSummaryDetail);

	/**
	 *  this method is used to get overhead percentage of claim
	 * @param claimId
	 * @return overhead
	 */
	public BigDecimal getOverheadPercentage(Integer claimId);

	/**
	 * @param claim
	 */
	public void saveOrUpdateClaimOverHead(Claim claim);

	/**
	 * @param awardNumber
	 * @param internalOrderCode
	 * @param claimId
	 * @return list of prev excluded claim summary details
	 */
	public List<ClaimSummaryDetails> getPrevExcludedClaimSummaryDetails(String awardNumber, String internalOrderCode,
			Integer claimId);

	/**
	 * @param claimAttachment
	 */
	public void saveOrUpdateClaimAttachment(ClaimAttachment claimAttachment);

	/**
	 * this method is used to get new document id for claim attachments
	 * @return document id
	 */
	public Integer generateDocumentId();

	/**
	 * @param claimAttachment
	 */
	public void deleteProtocolAttachment(ClaimAttachment claimAttachment);

	/**
	 * this method is used to version old attachment
	 * @param documentId
	 * @param claimId
	 * @param updateUser 
	 * @param versionNumber 
	 */
	public void archiveOldAttachmentVersion(Integer documentId, Integer claimId, String updateUser, Integer versionNumber);

	/**
	 * @param claimId
	 * @param isPersonHasPermission 
	 * @return list of attachments
	 */
	public List<ClaimAttachment> loadClaimAttachments(Integer claimId, Boolean isPersonHasPermission);

	/**
	 * @param claimId
	 * @param documentId
	 * @param isPersonHasPermission 
	 * @return list of archived attachments
	 */
	public List<ClaimAttachment> loadClaimAttachmentVersions(Integer claimId, Integer documentId, Boolean isPersonHasPermission);

	/**
	 * @return list of attachment types
	 */
	public List<ClaimAttachmentType> loadClaimAttachmentTypes();
	
	/**
	 * @param claimSummaryDetail
	 */
	public void updateClaimSummaryExcludeFlag(ClaimSummaryDetails claimSummaryDetail);

	/**
	 * @param claimId
	 * @return attachment detail
	 */
	public ClaimAttachment getClaimForeCastAttachmentDetail(Integer claimId);

	/**
	 * @param attachmentId
	 * @return claim attachment
	 */
	public ClaimAttachment getClaimAttachmentById(Integer attachmentId);

	/**
	 * @param claimDetailsId
	 * @return claim summary details
	 */
	public ClaimSummaryDetails getClaimSummaryDetailById(Integer claimDetailsId);

	/**
	 * @param claimSummaryDetail
	 */
	public void saveOrUpdateClaimBreakDown(ClaimSummaryDetails claimSummaryDetail);

	/**
	 * @param claimDetailsId
	 * @return get summary details which have prev summary details id of given value
	 */
	public ClaimSummaryDetails getClaimPrevSummaryDetailById(Integer claimDetailsId);

	/**
	 * @param claimDetailsId
	 */
	public void deleteSummaryDetailByPrevSummayId(Integer claimDetailsId);

	/**
	 * @param claimId
	 * @param awardNumber
	 * @param budgetCategoryCode 
	 * @return Cum Expense up to Previous Claim Period
	 */
	public BigDecimal getCumExpenseUptoPrevClaimPeriod(Integer claimId, String awardNumber, String budgetCategoryCode);

	/**
	 * @param awardNumber
	 * @param startDate
	 * @param endDate
	 * @param budgetCategoryCode 
	 * @return expense incurred for this claim period
	 */
	public BigDecimal getExpenseIncuredToThisPeriod(String awardNumber, Date startDate, Date endDate, String budgetCategoryCode);

	/**
	 * @param awardNumber
	 * @param endDate
	 * @param budgetCategoryCode 
	 * @param startDate 
	 * @return commitments amount up to claim period
	 */
	public BigDecimal getCommitmentsAmtUptoPeriod(String awardNumber, Date endDate, String budgetCategoryCode, Date startDate);

	/**
	 * @param awardNumber
	 * @param sequenceNumber
	 * @return To get current and past manpower resource
	 */
	public List<AwardManpowerResource> loadAwardManpowerResource(String awardNumber, Integer sequenceNumber);

	/**
	 * @param claimId
	 * @param budgetCategoryCode
	 * @return claim summary id
	 */
	public Integer getClaimSummaryIdByParams(Integer claimId, String budgetCategoryCode);

	/**
	 * this method is used to get funding scheme specific letter template type code
	 * @param awardId
	 * @return letter template type codes
	 */
	List<Object> getLetterTemplateTypeCodeToExportClaim(Integer awardId);

	/**
	 * @param internalOrderCode
	 * @param fiGlAccounts 
	 * @return summary details with manpower
	 */
	public List<Object[]> getManpowerdetails(Set<String> internalOrderCode);

	/**
	 * @param awardNumber 
	 * @return list of claims
	 */
	public List<Claim> loadSubmittedClaimsForAward(String awardNumber);

	/**
	 * @param awardmanpowerIds
	 * @return list of award manpower
	 */
	public List<AwardManpower> getAllAwardManpower(Set<Integer> awardmanpowerIds);

	/**
	 * @param personIds
	 * @return list of manpower
	 */
	public List<Manpower> getAllManpower(Set<String> personIds);

	/**
	 * @param claimManpower
	 */
	public ClaimManpower saveOrUpdateClaimManpower(ClaimManpower claimManpower);

	/**
	 * @param manpowerResourceIds
	 * @return list of claim manpower
	 */
	public List<ClaimManpower> getAllClaimManpower(Set<Integer> manpowerResourceIds);

	/**
	 * @param awardNumber
	 * @param claimId
	 * @return last award's claim end date
	 */
	public Date getLastClaimEndDate(String awardNumber, Integer claimId);

	/**
	 * @param payrollId
	 * @return manpower details by payroll
	 */
	public List<Object[]> getManpowerdetailsByPayrollId(Set<Integer> payrollId);

	/**
	 * @param claimId
	 * @param budgetCategoryCode 
	 */
	public BigDecimal getClaimSummaryAmountReq(Integer claimId, String budgetCategoryCode);

	/**
	 * @param claimId
	 * @param budgetCategoryCode 
	 */
	public BigDecimal getClaimSummaryAmountReqForAll(Integer claimId, String budgetCategoryCode);

	/**
	 * @param summary
	 */
	public void getBudgetDetails(ClaimSummaryDetails summary);

	/**
	 * @param prevClaimDetailsId
	 * @param currentClaimDetailId 
	 */
	public void deleteSummaryDetailForPrevClaimExcluded(Integer prevClaimDetailsId, Integer currentClaimDetailId);

	/**
	 * @param claim
	 */
	public void updateClaimSummaryTransactions(Claim claim);

	/**
	 * @param claimId
	 * @param claimStatusCode
	 */
	public void updateClaimActionLog(Integer claimId, String claimStatusCode, String acType);

	/**
	 * @param awardExpenseTransactionId
	 * @return ClaimSummaryDetails
	 */
	public ClaimSummaryDetails getClaimSummaryDetailByExpenseTransactionId(Integer awardExpenseTransactionId);

	/**
	 * @param awardId
	 * @return ClaimFundingScheme
	 */
	public ClaimFundingScheme getClaimFundingScheme(Integer awardId);

	/**
	 * @param claimId
	 * @return adjustedIndirectCost
	 */
	public BigDecimal getAdjustedIndirectCost(Integer claimId);

	/**
	 * @param claimId
	 * @param adjustedIndirectCost
	 */
	public void updateAdjustedIndirectCost(BigDecimal adjustedIndirectCost, Integer claimId);

	/**
	 * @param claimId
	 * @return total amount requested for a claim
	 */
	public BigDecimal getTotalAmountRequestedForClaim(Integer claimId);

	/**
	 * @param claimId
	 * @return list of expense transaction ids
	 */
	public List<Integer> getClaimSummaryDetailExpenseTransactionIds(Integer claimId);

	/**
	 * @param claimId
	 * @param awardNumber
	 * @param budgetCategoryCode
	 * @return previous claim summary of award for a particular budget category
	 */
	public ClaimSummary getPrevClaimSummaryOfBudgetCategory(Integer claimId, String awardNumber, String budgetCategoryCode);

	/**
	 * @param claimId
	 * @param budgetCategoryCode
	 * @return claim summary
	 */
	public ClaimSummary getClaimSummaryByParams(Integer claimId, String budgetCategoryCode);

	/**
	 * @param summary
	 */
	public void saveOrUpdateClaimSummary(ClaimSummary summary);
	
	/**
	 * @return list of claims where duration is null
	 */
	public List<Claim> getClaimsWithoutDuration();

	/**
	 * @param claimId
	 * @param awardNumber 
	 * @return sum of prev adjusted indirect cost
	 */
	public BigDecimal getAdjustedIndirectCostInPrevClaims(Integer claimId, String awardNumber);

	/**
	 * @param awardNumber
	 * @return List<Object[]>
	 */
	public List<Object[]> loadClaimsForAward(String awardNumber);

	/**
	 * @param claimId
	 * @param awardNumber 
	 * @return claimId
	 */
	public Integer getPrevClaimId(Integer claimId, String awardNumber);

	/**
	 * @param prevClaimId
	 * @return sum of amount required for next period
	 */
	public BigDecimal totalAmountReqForNextPeriod(Integer prevClaimId);

	/**
	 * @param prevClaimId
	 * @return IdcCumClaimAmtUptoClaim
	 */
	public BigDecimal getIdcCumClaimAmtUptoClaim(Integer prevClaimId);
	
	/**
	 * @param claimInvoiceDetail
	 */
	public void saveOrUpdateClaimInvoiceDetail(ClaimInvoiceDetails claimInvoiceDetail);

	/**
	 * @param claimId
	 * @return claim details
	 */
	public ClaimInvoice loadClaimInvoice(Integer claimId);

	/**
	 * @return list of gl account codes
	 */
	public List<ClaimGlAccount> loadGlAccountCodes();

	/**
	 * @param claimInvoice
	 */
	public void saveOrUpdateClaimInvoice(ClaimInvoice claimInvoice);

	/**
	 * @return list of claim metadata
	 */
	public List<ClaimInvoiceMetadata> getClaimInvoiceMetadata();

	/**
	 * @param awardId
	 * @return 
	 */
	public CustomDataView getAwardCustomData(Integer awardId);

	/**
	 * @param leadUnitNumber
	 * @return BA_CODE
	 */
	public String getCampusForUnit(String leadUnitNumber);

	/**
	 * @param baCode
	 * @param documentTypeCode
	 * @param  isReversalType 
	 * @return ClaimInvoiceMetadata
	 */
	public ClaimInvoiceMetadata getClaimInvoiceMetadataByParams(String baCode, String documentTypeCode, Boolean  isReversalType);

	/**
	 * @param awardNumber
	 * @param claimId
	 * @return claim amount
	 */
	public BigDecimal getClaimAmount(String awardNumber, Integer claimId);

	/**
	 * @param outputGstCategory
	 * @return ClaimOutputGstTaxCode
	 */
	public ClaimOutputGstTaxCode getClaimOutputGstTaxCode(String outputGstCategory);

	/**
	 * @param taxCodes
	 * @return List<ClaimOutputGstTaxCode>
	 */
	public List<ClaimOutputGstTaxCode> getClaimOutputGstTaxCodeInTaxCode(Set<String> taxCodes);

	/**
	 * @param invoiceDetailId
	 */
	public void deleteClaimInvoiceDetail(Integer invoiceDetailId);

	/**
	 * @return List<ClaimOutputGstTaxCode>
	 */
	public List<ClaimOutputGstTaxCode> loadClaimTaxCodes();

	/**
	 * @param claimId
	 * @return List<ClaimInvoiceLog>
	 */
	public List<ClaimInvoiceLog> loadAllClaimInvoice(Integer claimId);

	/**
	 * @param sapClaimFeed
	 */
	public void saveOrUpdateSapClaimFeed(SapClaimFeed sapClaimFeed);

	/**
	 * @param leadUnitNumber
	 * @return
	 */
	public String getBACodeForUnit(String leadUnitNumber);

	/**
	 * @param claimId
	 * @param sequenceNumber
	 * @return sap response msg for the given invoice version
	 */
	public List<SapClaimFeedResponseMessage> loadClaimInvoiceSapResponse(Integer claimId, Integer sequenceNumber);

	/**
	 * @param claimId
	 * @return Boolean
	 */
	public Boolean canPerformRevisionClaim(Integer claimId);

	/**
	 * @param claimId
	 * @return list of ClaimInvoiceDetails
	 */
	public List<ClaimInvoiceDetails> getCliamInvoiceDetailByClaimId(Integer claimId);

	/**
   * @param awardIds
	 * @return List of AwardIds
	 */
	public List<Integer> getAllClaimsBasedOnParams(List<Integer> awardIds);

	/**
	 * This method is used to delete claim details
	 * @param claimId
	 * @return
	 */
	public void deleteClaimDetails(Integer claimId);

	/**
	 * This method is used to check if the transaction detail is previously excluded
	 * @param claimSummaryDetailId
	 * @return
	 */
	public Boolean checkIfPreviouslyExcluded(Integer claimSummaryDetailId);

	/**
	 * This method is used to check if previous claim is in editable mode
	 * @param claimId
	 * @return
	 */
	public Boolean checkIfPrevClaimIsInEditMode(Integer claimId, String awardNumber, List<String> claimStatuses);

	/**
	 * This method is used to check if the claim is last claim
	 * @param claimId
	 * @return
	 */
	public Boolean checkIfLastClaimByParams(Integer claimId, String awardNumber);

	/**
	 * This method is used to get previous claim total amount if the claim is last claim
	 * @param claimId
	 * @param budgetCategoryCode 
	 * @return
	 */

	public BigDecimal getPreviousClaimsTotalAmountById(Integer claimId, String awardNumber, String budgetCategoryCode);

	/**
	 * This method is used to get actual head count of manpower
	 * @param awardManpowerIds
	 * @return
	 */

	public List<Object[]> getActualCountOfManpowerResourceBasedOnJobProfile(Set<Integer> awardManpowerIds);

	/**
	 * This method is used to get approved head count of manpower
	 * @param awardManpowerIds
	 * @return
	 */
	public List<Object[]> getApprovedCountOfManpowerResourceBasedOnJobProfile(Set<Integer> awardManpowerIds);

	/**
	 * This method is used to get Sponsor Funding Scheme Name for claim placeholder
	 * @param grantHeaderId
	 * @return
	 */
	public String getSponsorFundingSchemeByGrantId(Integer grantHeaderId);
}
