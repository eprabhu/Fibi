package com.polus.fibicomp.fastintegration.sapfeedmaintenance.vo;

import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.fastintegration.pojo.SapAwardFeed;
import com.polus.fibicomp.fastintegration.pojo.SapFeedStatus;
import com.polus.fibicomp.fastintegration.pojo.SapFeedUserAction;
import com.polus.fibicomp.fastintegration.sapfeedmaintenance.dto.SapFeedMaintenanceDto;
import com.polus.fibicomp.fastintegration.vo.IntegrationReportVO.EmailContent;
import com.polus.fibicomp.claims.claimsIntegration.sapfeed.dto.ClaimInvoiceFeedDto;
import com.polus.fibicomp.claims.claimsIntegration.sapfeed.pojo.SapClaimFeed;
import com.polus.fibicomp.claims.claimsIntegration.sapfeed.pojo.SapClaimFeedBatch;
import com.polus.fibicomp.claims.pojo.ClaimInvoiceLog;

public class SapFeedMaintenanceVO {

	private Integer pageNumber;

	private Map<String, String> sort;

	private Integer property1;

	private Integer property2 ;

	private String property3;

	private List<String> property4;

	private Timestamp property5;

	private Timestamp property6;

	private String property7;

	private Integer currentPage;

	private List<SapAwardFeed> sapAwardFeeds;

	private List<SapFeedStatus> sapFeedStatus;

	private Integer feedId;

	private String isAdvanceSearch;

	private String tabName;

	private List<Integer> feedIds;

	private String userComment;

	private String userAction;

	private String changeType;

	private SapAwardFeed sapAwardFeedHistory;

	private Integer batchId;

	private Integer awardId;

	private Award award;

	private Boolean isViewSapFeedRightExist = false;

	private String personId;

	private SapFeedMaintenanceDto sapFeedMaintenanceDto;

	private String message;

	private List<SapFeedUserAction> sapFeedUserActions;

	private SapFeedUserAction sapFeedUserAction;

	private Set<String> failedAwardNumbers;

	private Integer totalCount;
  
    private List<SapClaimFeed> sapClaimFeeds;

	private SapClaimFeedBatch sapClaimFeedBatch;

	private String property8;

	private String property9;

	private String reverse ;

	private String sortBy;
	
	private List<ClaimInvoiceFeedDto> claimInvoiceFeedDtoList;

	private List<String> claimNumbers;

	private List<ClaimInvoiceLog> claimInvoiceLogs;
	
	private String property10 ;

	private String property11;

	private String documentHeading;

    private Boolean isDownload = Boolean.FALSE;

	private EmailContent emailContent;

	public SapClaimFeedBatch getSapClaimFeedBatch() {
		return sapClaimFeedBatch;
	}

	public void setSapClaimFeedBatch(SapClaimFeedBatch sapClaimFeedBatch) {
		this.sapClaimFeedBatch = sapClaimFeedBatch;
	}

	public SapFeedMaintenanceVO() {
		failedAwardNumbers = new HashSet<>();
		sapAwardFeeds = new ArrayList<>();
	}

	public Integer getPageNumber() {
		return pageNumber;
	}

	public void setPageNumber(Integer pageNumber) {
		this.pageNumber = pageNumber;
	}

	public Map<String, String> getSort() {
		return sort;
	}

	public void setSort(Map<String, String> sort) {
		this.sort = sort;
	}

	public Integer getProperty1() {
		return property1;
	}

	public void setProperty1(Integer property1) {
		this.property1 = property1;
	}

	public Integer getProperty2() {
		return property2;
	}

	public void setProperty2(Integer property2) {
		this.property2 = property2;
	}

	public String getProperty3() {
		return property3;
	}

	public void setProperty3(String property3) {
		this.property3 = property3;
	}

	public Integer getCurrentPage() {
		return currentPage;
	}

	public void setCurrentPage(Integer currentPage) {
		this.currentPage = currentPage;
	}

	public List<SapAwardFeed> getSapAwardFeeds() {
		return sapAwardFeeds;
	}

	public void setSapAwardFeeds(List<SapAwardFeed> sapAwardFeeds) {
		this.sapAwardFeeds = sapAwardFeeds;
	}

	public List<SapFeedStatus> getSapFeedStatus() {
		return sapFeedStatus;
	}

	public void setSapFeedStatus(List<SapFeedStatus> sapFeedStatus) {
		this.sapFeedStatus = sapFeedStatus;
	}

	public Integer getFeedId() {
		return feedId;
	}

	public void setFeedId(Integer feedId) {
		this.feedId = feedId;
	}

	public List<String> getProperty4() {
		return property4;
	}

	public void setProperty4(List<String> property4) {
		this.property4 = property4;
	}

	public String getIsAdvanceSearch() {
		return isAdvanceSearch;
	}

	public void setIsAdvanceSearch(String isAdvanceSearch) {
		this.isAdvanceSearch = isAdvanceSearch;
	}

	public String getTabName() {
		return tabName;
	}

	public void setTabName(String tabName) {
		this.tabName = tabName;
	}

	public List<Integer> getFeedIds() {
		return feedIds;
	}

	public void setFeedIds(List<Integer> feedIds) {
		this.feedIds = feedIds;
	}

	public String getUserComment() {
		return userComment;
	}

	public void setUserComment(String userComment) {
		this.userComment = userComment;
	}

	public String getUserAction() {
		return userAction;
	}

	public void setUserAction(String userAction) {
		this.userAction = userAction;
	}

	public String getChangeType() {
		return changeType;
	}

	public void setChangeType(String changeType) {
		this.changeType = changeType;
	}

	public Timestamp getProperty5() {
		return property5;
	}

	public void setProperty5(Timestamp property5) {
		this.property5 = property5;
	}

	public Timestamp getProperty6() {
		return property6;
	}

	public void setProperty6(Timestamp property6) {
		this.property6 = property6;
	}

	public SapAwardFeed getSapAwardFeedHistory() {
		return sapAwardFeedHistory;
	}

	public void setSapAwardFeedHistory(SapAwardFeed sapAwardFeedHistory) {
		this.sapAwardFeedHistory = sapAwardFeedHistory;
	}

	public Integer getBatchId() {
		return batchId;
	}

	public void setBatchId(Integer batchId) {
		this.batchId = batchId;
	}

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public Award getAward() {
		return award;
	}

	public void setAward(Award award) {
		this.award = award;
	}

	public Boolean getIsViewSapFeedRightExist() {
		return isViewSapFeedRightExist;
	}

	public void setIsViewSapFeedRightExist(Boolean isViewSapFeedRightExist) {
		this.isViewSapFeedRightExist = isViewSapFeedRightExist;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public SapFeedMaintenanceDto getSapFeedMaintenanceDto() {
		return sapFeedMaintenanceDto;
	}

	public void setSapFeedMaintenanceDto(SapFeedMaintenanceDto sapFeedMaintenanceDto) {
		this.sapFeedMaintenanceDto = sapFeedMaintenanceDto;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public List<SapFeedUserAction> getSapFeedUserActions() {
		return sapFeedUserActions;
	}

	public void setSapFeedUserActions(List<SapFeedUserAction> sapFeedUserActions) {
		this.sapFeedUserActions = sapFeedUserActions;
	}

	public SapFeedUserAction getSapFeedUserAction() {
		return sapFeedUserAction;
	}

	public void setSapFeedUserAction(SapFeedUserAction sapFeedUserAction) {
		this.sapFeedUserAction = sapFeedUserAction;
	}

	public Set<String> getFailedAwardNumbers() {
		return failedAwardNumbers;
	}

	public void setFailedAwardNumbers(Set<String> failedAwardNumbers) {
		this.failedAwardNumbers = failedAwardNumbers;
	}

  	public List<SapClaimFeed> getSapClaimFeeds() {
		return sapClaimFeeds;
	}

	public void setSapClaimFeeds(List<SapClaimFeed> sapClaimFeeds) {
		this.sapClaimFeeds = sapClaimFeeds;
	}

	public String getProperty7() {
		return property7;
	}

	public void setProperty7(String property7) {
		this.property7 = property7;
	}

	public String getProperty8() {
		return property8;
	}

	public void setProperty8(String property8) {
		this.property8 = property8;
	}

	public String getProperty9() {
		return property9;
	}

	public void setProperty9(String property9) {
		this.property9 = property9;
	}

	public List<ClaimInvoiceFeedDto> getClaimInvoiceFeedDtoList() {
		return claimInvoiceFeedDtoList;
	}

	public void setClaimInvoiceFeedDtoList(List<ClaimInvoiceFeedDto> claimInvoiceFeedDtoList) {
		this.claimInvoiceFeedDtoList = claimInvoiceFeedDtoList;
	}

	public String getProperty10() {
		return property10;
	}

	public void setProperty10(String property10) {
		this.property10 = property10;
	}

	public String getProperty11() {
		return property11;
	}

	public void setProperty11(String property11) {
		this.property11 = property11;
	}

	public Integer getTotalCount() {
		return totalCount;
	}

	public void setTotalCount(Integer totalCount) {
		this.totalCount = totalCount;
	}
	public List<String> getClaimNumbers() {
		return claimNumbers;
	}

	public void setClaimNumbers(List<String> claimNumbers) {
		this.claimNumbers = claimNumbers;
	}

	public List<ClaimInvoiceLog> getClaimInvoiceLogs() {
		return claimInvoiceLogs;
	}

	public void setClaimInvoiceLogs(List<ClaimInvoiceLog> claimInvoiceLogs) {
		this.claimInvoiceLogs = claimInvoiceLogs;
	}

	public String getReverse() {
		return reverse;
	}

	public void setReverse(String reverse) {
		this.reverse = reverse;
	}

	public String getSortBy() {
		return sortBy;
	}

	public void setSortBy(String sortBy) {
		this.sortBy = sortBy;
	}

	public String getDocumentHeading() {
		return documentHeading;
	}

	public void setDocumentHeading(String documentHeading) {
		this.documentHeading = documentHeading;
	}

	public Boolean getIsDownload() {
		return isDownload;
	}

	public void setIsDownload(Boolean isDownload) {
		this.isDownload = isDownload;
	}

	public EmailContent getEmailContent() {
		return emailContent;
	}

	public void setEmailContent(EmailContent emailContent) {
		this.emailContent = emailContent;
	}
  
}
