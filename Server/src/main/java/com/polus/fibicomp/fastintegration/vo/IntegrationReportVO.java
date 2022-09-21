package com.polus.fibicomp.fastintegration.vo;

import java.io.File;
import java.util.List;
import java.util.Set;

import com.polus.fibicomp.claims.claimsIntegration.sapfeed.dto.ClaimInvoiceFeedDto;
import com.polus.fibicomp.fastintegration.pojo.AwardExpenseTransactionsRT;
import com.polus.fibicomp.fastintegration.pojo.SapAwardFeed;
import com.polus.fibicomp.fastintegration.sapfeedmaintenance.vo.SapFeedMaintenanceVO;

public class IntegrationReportVO {
	private Integer sapFeedTmplFundedDataCount;
	private Integer sapFeedTmplGrantBudMasterDataCount;
	private Integer sapFeedTmplProjectDefDataCount;
	private Integer sapFeedTmplSponsoPrgmDataCount;
	private Integer sapFeedTmplSponsorClassDataCount;
	private Integer sapFeedTmplGrantMasterDataCount;
	private Integer sapFeedTmplWbsDataCount;
	private Integer sapFeedTmplGrantCount;
	private Integer sapFeedTmplFmBudgetDataCount;
	private Integer awardExpenseTrackerRTCount;
	private Boolean isResponseMail = false;
	private StringBuilder emailBody;
	private EmailContent emailContent;
	private Integer batchId;
	private Integer totalFileRowCount;
    private Integer fileId;
    private Integer fileRowIndex;
    private List<AwardExpenseTransactionsRT> awardExpenseTransactionsRT;
    private String fileName;
    private Set<Integer> batchIds;
	private File problematicGrantCodefileName;
	private String businessArea;
    private String departmentName;
    private File attachment;
    private Boolean errorOccured = Boolean.FALSE;
    private List<SapAwardFeed> sapAwardFeeds;
    private SapFeedMaintenanceVO sapFeedMaintenanceVO;
    private Integer claimInvoiceCount;
    private List<ClaimInvoiceFeedDto> sapClaimFeeds;

	public class EmailContent {

		public EmailContent() {
			this.error = new StringBuilder();
			this.success = new StringBuilder();
		}

		private StringBuilder error;

		private StringBuilder success;

		public StringBuilder getError() {
			return error;
		}

		public void setError(StringBuilder error) {
			this.error = error;
		}

		public StringBuilder getSuccess() {
			return success;
		}

		public void setSuccess(StringBuilder success) {
			this.success = success;
		}
	}

    public IntegrationReportVO() {
		this.emailContent = new EmailContent();
	}

	public Integer getFileId() {
		return fileId;
	}

	public void setFileId(Integer fileId) {
		this.fileId = fileId;
	}

	public Integer getSapFeedTmplFundedDataCount() {
		return sapFeedTmplFundedDataCount;
	}

	public void setSapFeedTmplFundedDataCount(Integer sapFeedTmplFundedDataCount) {
		this.sapFeedTmplFundedDataCount = sapFeedTmplFundedDataCount;
	}

	public Integer getSapFeedTmplGrantBudMasterDataCount() {
		return sapFeedTmplGrantBudMasterDataCount;
	}

	public void setSapFeedTmplGrantBudMasterDataCount(Integer sapFeedTmplGrantBudMasterDataCount) {
		this.sapFeedTmplGrantBudMasterDataCount = sapFeedTmplGrantBudMasterDataCount;
	}

	public Integer getSapFeedTmplProjectDefDataCount() {
		return sapFeedTmplProjectDefDataCount;
	}

	public void setSapFeedTmplProjectDefDataCount(Integer sapFeedTmplProjectDefDataCount) {
		this.sapFeedTmplProjectDefDataCount = sapFeedTmplProjectDefDataCount;
	}

	public Integer getSapFeedTmplSponsoPrgmDataCount() {
		return sapFeedTmplSponsoPrgmDataCount;
	}

	public void setSapFeedTmplSponsoPrgmDataCount(Integer sapFeedTmplSponsoPrgmDataCount) {
		this.sapFeedTmplSponsoPrgmDataCount = sapFeedTmplSponsoPrgmDataCount;
	}

	public Integer getSapFeedTmplSponsorClassDataCount() {
		return sapFeedTmplSponsorClassDataCount;
	}

	public void setSapFeedTmplSponsorClassDataCount(Integer sapFeedTmplSponsorClassDataCount) {
		this.sapFeedTmplSponsorClassDataCount = sapFeedTmplSponsorClassDataCount;
	}

	public Integer getSapFeedTmplGrantMasterDataCount() {
		return sapFeedTmplGrantMasterDataCount;
	}

	public void setSapFeedTmplGrantMasterDataCount(Integer sapFeedTmplGrantMasterDataCount) {
		this.sapFeedTmplGrantMasterDataCount = sapFeedTmplGrantMasterDataCount;
	}

	public Integer getSapFeedTmplWbsDataCount() {
		return sapFeedTmplWbsDataCount;
	}

	public void setSapFeedTmplWbsDataCount(Integer sapFeedTmplWbsDataCount) {
		this.sapFeedTmplWbsDataCount = sapFeedTmplWbsDataCount;
	}

	public Integer getSapFeedTmplGrantCount() {
		return sapFeedTmplGrantCount;
	}

	public void setSapFeedTmplGrantCount(Integer sapFeedTmplGrantCount) {
		this.sapFeedTmplGrantCount = sapFeedTmplGrantCount;
	}

//	public StringBuilder getEmailBody() {
//		return emailBody;
//	}
//
//	public void setEmailBody(StringBuilder emailBody) {
//		this.emailBody = emailBody;
//	}

	public Integer getSapFeedTmplFmBudgetDataCount() {
		return sapFeedTmplFmBudgetDataCount;
	}

	public void setSapFeedTmplFmBudgetDataCount(Integer sapFeedTmplFmBudgetDataCount) {
		this.sapFeedTmplFmBudgetDataCount = sapFeedTmplFmBudgetDataCount;
	}

	public Integer getAwardExpenseTrackerRTCount() {
		return awardExpenseTrackerRTCount;
	}

	public void setAwardExpenseTrackerRTCount(Integer awardExpenseTrackerRTCount) {
		this.awardExpenseTrackerRTCount = awardExpenseTrackerRTCount;
	}

	public Boolean getIsResponseMail() {
		return isResponseMail;
	}

	public void setIsResponseMail(Boolean isResponseMail) {
		this.isResponseMail = isResponseMail;
	}

	public Integer getBatchId() {
		return batchId;
	}

	public void setBatchId(Integer batchId) {
		this.batchId = batchId;
	}

	public List<AwardExpenseTransactionsRT> getAwardExpenseTransactionsRT() {
		return awardExpenseTransactionsRT;
	}

	public void setAwardExpenseTransactionsRT(List<AwardExpenseTransactionsRT> awardExpenseTransactionsRT) {
		this.awardExpenseTransactionsRT = awardExpenseTransactionsRT;
	}

	public Integer getFileRowIndex() {
		return fileRowIndex;
	}

	public void setFileRowIndex(Integer fileRowIndex) {
		this.fileRowIndex = fileRowIndex;
	}

	public Integer getTotalFileRowCount() {
		return totalFileRowCount;
	}

	public void setTotalFileRowCount(Integer totalFileRowCount) {
		this.totalFileRowCount = totalFileRowCount;
	}

	public String getFileName() {
		return fileName;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	public Set<Integer> getBatchIds() {
		return batchIds;
	}

	public void setBatchIds(Set<Integer> batchIds) {
		this.batchIds = batchIds;
	}

	public String getBusinessArea() {
		return businessArea;
	}

	public void setBusinessArea(String businessArea) {
		this.businessArea = businessArea;
	}

	public File getAttachment() {
		return attachment;
	}

	public void setAttachment(File attachment) {
		this.attachment = attachment;
	}

	public File getProblematicGrantCodefileName() {
		return problematicGrantCodefileName;
	}

	public void setProblematicGrantCodefileName(File problematicGrantCodefileName) {
		this.problematicGrantCodefileName = problematicGrantCodefileName;
	}

	public String getDepartmentName() {
		return departmentName;
	}

	public void setDepartmentName(String departmentName) {
		this.departmentName = departmentName;
	}

	public StringBuilder getEmailBody() {
		return emailBody;
	}

	public void setEmailBody(StringBuilder emailBody) {
		this.emailBody = emailBody;
	}

	public EmailContent getEmailContent() {
		return emailContent;
	}

	public void setEmailContent(EmailContent emailContent) {
		this.emailContent = emailContent;
	}

	public Boolean getErrorOccured() {
		return errorOccured;
	}

	public void setErrorOccured(Boolean errorOccured) {
		this.errorOccured = errorOccured;
	}

	public List<SapAwardFeed> getSapAwardFeeds() {
		return sapAwardFeeds;
	}

	public void setSapAwardFeeds(List<SapAwardFeed> sapAwardFeeds) {
		this.sapAwardFeeds = sapAwardFeeds;
	}

	public SapFeedMaintenanceVO getSapFeedMaintenanceVO() {
		return sapFeedMaintenanceVO;
	}

	public void setSapFeedMaintenanceVO(SapFeedMaintenanceVO sapFeedMaintenanceVO) {
		this.sapFeedMaintenanceVO = sapFeedMaintenanceVO;
	}

	public Integer getClaimInvoiceCount() {
		return claimInvoiceCount;
	}

	public void setClaimInvoiceCount(Integer claimInvoiceCount) {
		this.claimInvoiceCount = claimInvoiceCount;
	}

	public List<ClaimInvoiceFeedDto> getSapClaimFeeds() {
		return sapClaimFeeds;
	}

	public void setSapClaimFeeds(List<ClaimInvoiceFeedDto> sapClaimFeeds) {
		this.sapClaimFeeds = sapClaimFeeds;
	}

}
