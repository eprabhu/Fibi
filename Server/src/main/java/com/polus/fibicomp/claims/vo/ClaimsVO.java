package com.polus.fibicomp.claims.vo;

import java.math.BigDecimal;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import com.polus.fibicomp.award.expense.vo.AwardExpenseTransactionVO;
import com.polus.fibicomp.claims.pojo.Claim;
import com.polus.fibicomp.claims.pojo.ClaimAttachment;
import com.polus.fibicomp.claims.pojo.ClaimAttachmentType;
import com.polus.fibicomp.claims.pojo.ClaimInvoice;
import com.polus.fibicomp.claims.pojo.ClaimInvoiceDetails;
import com.polus.fibicomp.claims.pojo.ClaimInvoiceLog;
import com.polus.fibicomp.claims.pojo.ClaimManpower;
import com.polus.fibicomp.claims.pojo.ClaimOutputGstTaxCode;
import com.polus.fibicomp.claims.pojo.ClaimStatus;
import com.polus.fibicomp.claims.pojo.ClaimSummary;
import com.polus.fibicomp.claims.pojo.ClaimSummaryDetails;
import com.polus.fibicomp.manpower.pojo.AwardManpowerResource;
import com.polus.fibicomp.view.CustomDataView;
import com.polus.fibicomp.workflow.pojo.Workflow;

public class ClaimsVO {

	private Claim claim;
	
	private Integer claimId;

	private String claimNumber;

	private String message;

	private String updateUser;

	private Workflow workflow;

	private String isFinalApprover;

	private String canApproveRouting;

	private List<Workflow> workflowList;

	private String workFlowPersonId;

	private String actionType;

	private String approveComment;

	private Boolean finalApprover = false;

	private Boolean isApproved = false;

	private Boolean isApprover = false;

	private String personId;

	private Integer moduleCode;

	private Integer subModuleCode;

	private Integer approverStopNumber;

	private String subModuleItemKey;

	private Integer workFlowDetailId;

	private String isSubmit;

	private Integer mapNumber;

	private Integer mapId;

	private Integer approverNumber;

	private String submitUserFullName;

	private List<String> availableRights;

	private String fileName;

	private Integer remaining;

	private Integer length;

	private String fileContent;

	private String fileTimestamp;

	private String contentType;

	private List<ClaimSummary> claimSummary;
	
	private Map<String,Object> summaryCalculations;
	
	private List<LinkedHashMap<Object, Object>>  claimSummaryDetails;
	
	private ClaimSummaryDetails claimSummaryDetail;
	
	private Map<Object, Object> claimSummaryCalculations;
	
	private BigDecimal overHeadPercentage = BigDecimal.ZERO;
	
	private String awardNumber;
	
	private String internalOrderCode;
	
	private List<ClaimSummaryDetails> prevClaimSummaryDetails;
	
	private String acType;
	
	private List<ClaimAttachment> claimAttachments;
		
	private Integer documentId;
	
	private List<ClaimAttachmentType> claimAttachmentType;
	
	private Map<String,Object> advanceCalculations;

	private ClaimAttachment claimAttachment;
	
	private List<AwardManpowerResource> currentManPowerResource;
	
	private List<AwardManpowerResource> pastManpowerResource;
	
	private List<AwardManpowerResource> allAwardManpowerResource;
	
	private Integer awardId;
	
	private List<Claim> claims;

	private ClaimStatus claimStatus;
	
	private ClaimManpower claimManpower;
	
	private Date lastClaimEndDate;

	private String claimSubmissionDate;

	private String claimFOApprovalDate;

	private String claimRSOApprovalDate;

	private String claimRDOApprovalDate;

	private String claimStartAndEndYear;

	private Integer currentHeadCount;

	private String claimEndDate;

	private String previousClaimEndDate;

	private Boolean isExcelExport = Boolean.FALSE;

	private String oHPercentage = "0";

	private List<AwardExpenseTransactionVO> awardExpenseTransactions;

	private String actualOrCommitedFlag;

	private BigDecimal adjustedIndirectCost = BigDecimal.ZERO;

	private String projectDuration;

	private String awardStartDate;

	private String awardEndDate;

	private String financialYear;

	private String leadUnitNumber;

	private ClaimInvoiceDetails claimInvoiceDetail;
	
	private ClaimInvoice claimInvoice;
	
	private CustomDataView customDataView;
	
	private ClaimOutputGstTaxCode claimOutputGstTaxCode;
	
	private List<List<ClaimInvoiceLog>> claimInvoiceVersions;

	private String claimStatusCode;

	private Date funderApprovalDate;
	
	private Boolean status = Boolean.FALSE;

	private String sortBy;

	private String reverse;

	private String awardLeadUnitNumber;

	private Boolean claimPreviouslyExcluded;

	private Boolean previousClaimEditable;

	private Boolean enableClaimStartEndDate;

	private List<AwardManpowerResource> actualManpowerCounts;

	private List<AwardManpowerResource> approvedManpowerCounts;
	
	private String claimPeriodStartAndEndYear;
	
	private String claimQuarterFrom;
	
	private String claimQuarterTo;
	
	private String claimHalfYearlyPeriod;
	
	private ClaimSummary summaryOOE_MACSumValues;

	private String lineItemDescriptions;

	private String claimFundingScheme;
	
	public String getClaimFundingScheme() {
		return claimFundingScheme;
	}

	public void setClaimFundingScheme(String claimFundingScheme) {
		this.claimFundingScheme = claimFundingScheme;
	}

	public String getClaimQuarterFrom() {
		return claimQuarterFrom;
	}

	public void setClaimQuarterFrom(String claimQuarterFrom) {
		this.claimQuarterFrom = claimQuarterFrom;
	}

	public String getClaimQuarterTo() {
		return claimQuarterTo;
	}

	public void setClaimQuarterTo(String claimQuarterTo) {
		this.claimQuarterTo = claimQuarterTo;
	}

	public ClaimSummary getSummaryOOE_MACSumValues() {
		return summaryOOE_MACSumValues;
	}

	public void setSummaryOOE_MACSumValues(ClaimSummary summaryOOE_MACSumValues) {
		this.summaryOOE_MACSumValues = summaryOOE_MACSumValues;
	}

	public String getClaimHalfYearlyPeriod() {
		return claimHalfYearlyPeriod;
	}

	public void setClaimHalfYearlyPeriod(String claimHalfYearlyPeriod) {
		this.claimHalfYearlyPeriod = claimHalfYearlyPeriod;
	}

	public String getClaimPeriodStartAndEndYear() {
		return claimPeriodStartAndEndYear;
	}

	public void setClaimPeriodStartAndEndYear(String claimPeriodStartAndEndYear) {
		this.claimPeriodStartAndEndYear = claimPeriodStartAndEndYear;
	}

	public BigDecimal getAdjustedIndirectCost() {
		return adjustedIndirectCost;
	}

	public void setAdjustedIndirectCost(BigDecimal adjustedIndirectCost) {
		this.adjustedIndirectCost = adjustedIndirectCost;
	}

	public List<String> getAvailableRights() {
		return availableRights;
	}

	public void setAvailableRights(List<String> availableRights) {
		this.availableRights = availableRights;
	}

	public Claim getClaim() {
		return claim;
	}

	public void setClaim(Claim claim) {
		this.claim = claim;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Workflow getWorkflow() {
		return workflow;
	}

	public void setWorkflow(Workflow workflow) {
		this.workflow = workflow;
	}

	public String getIsFinalApprover() {
		return isFinalApprover;
	}

	public void setIsFinalApprover(String isFinalApprover) {
		this.isFinalApprover = isFinalApprover;
	}

	public String getCanApproveRouting() {
		return canApproveRouting;
	}

	public void setCanApproveRouting(String canApproveRouting) {
		this.canApproveRouting = canApproveRouting;
	}

	public List<Workflow> getWorkflowList() {
		return workflowList;
	}

	public void setWorkflowList(List<Workflow> workflowList) {
		this.workflowList = workflowList;
	}

	public String getWorkFlowPersonId() {
		return workFlowPersonId;
	}

	public void setWorkFlowPersonId(String workFlowPersonId) {
		this.workFlowPersonId = workFlowPersonId;
	}

	public String getActionType() {
		return actionType;
	}

	public void setActionType(String actionType) {
		this.actionType = actionType;
	}

	public String getApproveComment() {
		return approveComment;
	}

	public void setApproveComment(String approveComment) {
		this.approveComment = approveComment;
	}

	public Boolean getFinalApprover() {
		return finalApprover;
	}

	public void setFinalApprover(Boolean finalApprover) {
		this.finalApprover = finalApprover;
	}

	public Boolean getIsApproved() {
		return isApproved;
	}

	public void setIsApproved(Boolean isApproved) {
		this.isApproved = isApproved;
	}

	public Boolean getIsApprover() {
		return isApprover;
	}

	public void setIsApprover(Boolean isApprover) {
		this.isApprover = isApprover;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public Integer getModuleCode() {
		return moduleCode;
	}

	public void setModuleCode(Integer moduleCode) {
		this.moduleCode = moduleCode;
	}

	public Integer getSubModuleCode() {
		return subModuleCode;
	}

	public void setSubModuleCode(Integer subModuleCode) {
		this.subModuleCode = subModuleCode;
	}

	public Integer getApproverStopNumber() {
		return approverStopNumber;
	}

	public void setApproverStopNumber(Integer approverStopNumber) {
		this.approverStopNumber = approverStopNumber;
	}

	public String getSubModuleItemKey() {
		return subModuleItemKey;
	}

	public void setSubModuleItemKey(String subModuleItemKey) {
		this.subModuleItemKey = subModuleItemKey;
	}

	public Integer getWorkFlowDetailId() {
		return workFlowDetailId;
	}

	public void setWorkFlowDetailId(Integer workFlowDetailId) {
		this.workFlowDetailId = workFlowDetailId;
	}

	public String getIsSubmit() {
		return isSubmit;
	}

	public void setIsSubmit(String isSubmit) {
		this.isSubmit = isSubmit;
	}

	public Integer getMapNumber() {
		return mapNumber;
	}

	public void setMapNumber(Integer mapNumber) {
		this.mapNumber = mapNumber;
	}

	public Integer getMapId() {
		return mapId;
	}

	public void setMapId(Integer mapId) {
		this.mapId = mapId;
	}

	public Integer getApproverNumber() {
		return approverNumber;
	}

	public void setApproverNumber(Integer approverNumber) {
		this.approverNumber = approverNumber;
	}

	public String getSubmitUserFullName() {
		return submitUserFullName;
	}

	public void setSubmitUserFullName(String submitUserFullName) {
		this.submitUserFullName = submitUserFullName;
	}

	public String getFileName() {
		return fileName;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	public Integer getRemaining() {
		return remaining;
	}

	public void setRemaining(Integer remaining) {
		this.remaining = remaining;
	}

	public Integer getLength() {
		return length;
	}

	public void setLength(Integer length) {
		this.length = length;
	}

	public String getFileContent() {
		return fileContent;
	}

	public void setFileContent(String fileContent) {
		this.fileContent = fileContent;
	}

	public String getFileTimestamp() {
		return fileTimestamp;
	}

	public void setFileTimestamp(String fileTimestamp) {
		this.fileTimestamp = fileTimestamp;
	}

	public String getContentType() {
		return contentType;
	}

	public void setContentType(String contentType) {
		this.contentType = contentType;
	}

	public List<ClaimSummary> getClaimSummary() {
		return claimSummary;
	}

	public void setClaimSummary(List<ClaimSummary> claimSummary) {
		this.claimSummary = claimSummary;
	}

	public Map<String, Object> getSummaryCalculations() {
		return summaryCalculations;
	}

	public void setSummaryCalculations(Map<String, Object> summaryCalculations) {
		this.summaryCalculations = summaryCalculations;
	}

	public ClaimSummaryDetails getClaimSummaryDetail() {
		return claimSummaryDetail;
	}

	public void setClaimSummaryDetail(ClaimSummaryDetails claimSummaryDetail) {
		this.claimSummaryDetail = claimSummaryDetail;
	}

	public Map<Object, Object> getClaimSummaryCalculations() {
		return claimSummaryCalculations;
	}

	public void setClaimSummaryCalculations(Map<Object, Object> claimSummaryCalculations) {
		this.claimSummaryCalculations = claimSummaryCalculations;
	}

	public BigDecimal getOverHeadPercentage() {
		return overHeadPercentage;
	}

	public void setOverHeadPercentage(BigDecimal overHeadPercentage) {
		this.overHeadPercentage = overHeadPercentage;
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public String getInternalOrderCode() {
		return internalOrderCode;
	}

	public void setInternalOrderCode(String internalOrderCode) {
		this.internalOrderCode = internalOrderCode;
	}

	public List<ClaimSummaryDetails> getPrevClaimSummaryDetails() {
		return prevClaimSummaryDetails;
	}

	public void setPrevClaimSummaryDetails(List<ClaimSummaryDetails> prevClaimSummaryDetails) {
		this.prevClaimSummaryDetails = prevClaimSummaryDetails;
	}

	public String getAcType() {
		return acType;
	}

	public void setAcType(String acType) {
		this.acType = acType;
	}

	public List<ClaimAttachment> getClaimAttachments() {
		return claimAttachments;
	}

	public void setClaimAttachments(List<ClaimAttachment> claimAttachments) {
		this.claimAttachments = claimAttachments;
	}

	public Integer getDocumentId() {
		return documentId;
	}

	public void setDocumentId(Integer documentId) {
		this.documentId = documentId;
	}

	public List<ClaimAttachmentType> getClaimAttachmentType() {
		return claimAttachmentType;
	}

	public void setClaimAttachmentType(List<ClaimAttachmentType> claimAttachmentType) {
		this.claimAttachmentType = claimAttachmentType;
	}

	public Map<String, Object> getAdvanceCalculations() {
		return advanceCalculations;
	}

	public void setAdvanceCalculations(Map<String, Object> advanceCalculations) {
		this.advanceCalculations = advanceCalculations;
	}

	public ClaimAttachment getClaimAttachment() {
		return claimAttachment;
	}

	public void setClaimAttachment(ClaimAttachment claimAttachment) {
		this.claimAttachment = claimAttachment;
	}

	public List<AwardManpowerResource> getCurrentManPowerResource() {
		return currentManPowerResource;
	}

	public void setCurrentManPowerResource(List<AwardManpowerResource> currentManPowerResource) {
		this.currentManPowerResource = currentManPowerResource;
	}

	public List<AwardManpowerResource> getPastManpowerResource() {
		return pastManpowerResource;
	}

	public void setPastManpowerResource(List<AwardManpowerResource> pastManpowerResource) {
		this.pastManpowerResource = pastManpowerResource;
	}

	public List<AwardManpowerResource> getAllAwardManpowerResource() {
		return allAwardManpowerResource;
	}

	public void setAllAwardManpowerResource(List<AwardManpowerResource> allAwardManpowerResource) {
		this.allAwardManpowerResource = allAwardManpowerResource;
	}

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public List<LinkedHashMap<Object, Object>> getClaimSummaryDetails() {
		return claimSummaryDetails;
	}

	public void setClaimSummaryDetails(List<LinkedHashMap<Object, Object>> claimSummaryDetails) {
		this.claimSummaryDetails = claimSummaryDetails;
	}

	public List<Claim> getClaims() {
		return claims;
	}

	public void setClaims(List<Claim> claims) {
		this.claims = claims;
	}

	public ClaimStatus getClaimStatus() {
		return claimStatus;
	}

	public void setClaimStatus(ClaimStatus claimStatus) {
		this.claimStatus = claimStatus;
	}

	public String getClaimNumber() {
		return claimNumber;
	}

	public void setClaimNumber(String claimNumber) {
		this.claimNumber = claimNumber;
	}

	public ClaimManpower getClaimManpower() {
		return claimManpower;
	}

	public void setClaimManpower(ClaimManpower claimManpower) {
		this.claimManpower = claimManpower;
	}

	public Integer getClaimId() {
		return claimId;
	}

	public void setClaimId(Integer claimId) {
		this.claimId = claimId;
	}

	public Date getLastClaimEndDate() {
		return lastClaimEndDate;
	}

	public void setLastClaimEndDate(Date lastClaimEndDate) {
		this.lastClaimEndDate = lastClaimEndDate;
	}

	public String getClaimSubmissionDate() {
		return claimSubmissionDate;
	}

	public void setClaimSubmissionDate(String claimSubmissionDate) {
		this.claimSubmissionDate = claimSubmissionDate;
	}

	public String getClaimFOApprovalDate() {
		return claimFOApprovalDate;
	}

	public void setClaimFOApprovalDate(String claimFOApprovalDate) {
		this.claimFOApprovalDate = claimFOApprovalDate;
	}

	public String getClaimRSOApprovalDate() {
		return claimRSOApprovalDate;
	}

	public void setClaimRSOApprovalDate(String claimRSOApprovalDate) {
		this.claimRSOApprovalDate = claimRSOApprovalDate;
	}

	public String getClaimRDOApprovalDate() {
		return claimRDOApprovalDate;
	}

	public void setClaimRDOApprovalDate(String claimRDOApprovalDate) {
		this.claimRDOApprovalDate = claimRDOApprovalDate;
	}

	public String getClaimStartAndEndYear() {
		return claimStartAndEndYear;
	}

	public void setClaimStartAndEndYear(String claimStartAndEndYear) {
		this.claimStartAndEndYear = claimStartAndEndYear;
	}

	public Integer getCurrentHeadCount() {
		return currentHeadCount;
	}

	public void setCurrentHeadCount(Integer currentHeadCount) {
		this.currentHeadCount = currentHeadCount;
	}

	public String getClaimEndDate() {
		return claimEndDate;
	}

	public void setClaimEndDate(String claimEndDate) {
		this.claimEndDate = claimEndDate;
	}

	public String getPreviousClaimEndDate() {
		return previousClaimEndDate;
	}

	public void setPreviousClaimEndDate(String previousClaimEndDate) {
		this.previousClaimEndDate = previousClaimEndDate;
	}

	public Boolean getIsExcelExport() {
		return isExcelExport;
	}

	public void setIsExcelExport(Boolean isExcelExport) {
		this.isExcelExport = isExcelExport;
	}

	public String getoHPercentage() {
		return oHPercentage;
	}

	public void setoHPercentage(String oHPercentage) {
		this.oHPercentage = oHPercentage;
	}

	public List<AwardExpenseTransactionVO> getAwardExpenseTransactions() {
		return awardExpenseTransactions;
	}

	public void setAwardExpenseTransactions(List<AwardExpenseTransactionVO> awardExpenseTransactions) {
		this.awardExpenseTransactions = awardExpenseTransactions;
	}

	public String getActualOrCommitedFlag() {
		return actualOrCommitedFlag;
	}

	public void setActualOrCommitedFlag(String actualOrCommitedFlag) {
		this.actualOrCommitedFlag = actualOrCommitedFlag;
	}

	public String getProjectDuration() {
		return projectDuration;
	}

	public void setProjectDuration(String projectDuration) {
		this.projectDuration = projectDuration;
	}

	public String getAwardStartDate() {
		return awardStartDate;
	}

	public void setAwardStartDate(String awardStartDate) {
		this.awardStartDate = awardStartDate;
	}

	public String getAwardEndDate() {
		return awardEndDate;
	}

	public void setAwardEndDate(String awardEndDate) {
		this.awardEndDate = awardEndDate;
	}

	public String getFinancialYear() {
		return financialYear;
	}

	public void setFinancialYear(String financialYear) {
		this.financialYear = financialYear;
	}

	public String getLeadUnitNumber() {
		return leadUnitNumber;
	}

	public void setLeadUnitNumber(String leadUnitNumber) {
		this.leadUnitNumber = leadUnitNumber;
	}

	public ClaimInvoiceDetails getClaimInvoiceDetail() {
		return claimInvoiceDetail;
	}

	public void setClaimInvoiceDetail(ClaimInvoiceDetails claimInvoiceDetail) {
		this.claimInvoiceDetail = claimInvoiceDetail;
	}

	public ClaimInvoice getClaimInvoice() {
		return claimInvoice;
	}

	public void setClaimInvoice(ClaimInvoice claimInvoice) {
		this.claimInvoice = claimInvoice;
	}

	public CustomDataView getCustomDataView() {
		return customDataView;
	}

	public void setCustomDataView(CustomDataView customDataView) {
		this.customDataView = customDataView;
	}

	public ClaimOutputGstTaxCode getClaimOutputGstTaxCode() {
		return claimOutputGstTaxCode;
	}

	public void setClaimOutputGstTaxCode(ClaimOutputGstTaxCode claimOutputGstTaxCode) {
		this.claimOutputGstTaxCode = claimOutputGstTaxCode;
	}

	public String getClaimStatusCode() {
		return claimStatusCode;
	}

	public void setClaimStatusCode(String claimStatusCode) {
		this.claimStatusCode = claimStatusCode;
	}

	public List<List<ClaimInvoiceLog>> getClaimInvoiceVersions() {
		return claimInvoiceVersions;
	}

	public void setClaimInvoiceVersions(List<List<ClaimInvoiceLog>> claimInvoiceVersions) {
		this.claimInvoiceVersions = claimInvoiceVersions;
	}

	public Date getFunderApprovalDate() {
		return funderApprovalDate;
	}

	public void setFunderApprovalDate(Date funderApprovalDate) {
		this.funderApprovalDate = funderApprovalDate;
	}

	public Boolean getStatus() {
		return status;
	}

	public void setStatus(Boolean status) {
		this.status = status;
	}

	public String getSortBy() {
		return sortBy;
	}

	public String getReverse() {
		return reverse;
	}

	public void setSortBy(String sortBy) {
		this.sortBy = sortBy;
	}

	public void setReverse(String reverse) {
		this.reverse = reverse;
	}

	public String getAwardLeadUnitNumber() {
		return awardLeadUnitNumber;
	}

	public void setAwardLeadUnitNumber(String awardLeadUnitNumber) {
		this.awardLeadUnitNumber = awardLeadUnitNumber;
	}

	public Boolean getClaimPreviouslyExcluded() {
		return claimPreviouslyExcluded;
	}

	public void setClaimPreviouslyExcluded(Boolean claimPreviouslyExcluded) {
		this.claimPreviouslyExcluded = claimPreviouslyExcluded;
	}

	public Boolean getPreviousClaimEditable() {
		return previousClaimEditable;
	}

	public void setPreviousClaimEditable(Boolean previousClaimEditable) {
		this.previousClaimEditable = previousClaimEditable;
	}

	public Boolean getEnableClaimStartEndDate() {
		return enableClaimStartEndDate;
	}

	public void setEnableClaimStartEndDate(Boolean enableClaimStartEndDate) {
		this.enableClaimStartEndDate = enableClaimStartEndDate;
	}

	public List<AwardManpowerResource> getActualManpowerCounts() {
		return actualManpowerCounts;
	}

	public void setActualManpowerCounts(List<AwardManpowerResource> actualManpowerCounts) {
		this.actualManpowerCounts = actualManpowerCounts;
	}

	public List<AwardManpowerResource> getApprovedManpowerCounts() {
		return approvedManpowerCounts;
	}

	public void setApprovedManpowerCounts(List<AwardManpowerResource> approvedManpowerCounts) {
		this.approvedManpowerCounts = approvedManpowerCounts;
	}

	public String getLineItemDescriptions() {
		return lineItemDescriptions;
	}

	public void setLineItemDescriptions(String lineItemDescriptions) {
		this.lineItemDescriptions = lineItemDescriptions;
	}

	
}
