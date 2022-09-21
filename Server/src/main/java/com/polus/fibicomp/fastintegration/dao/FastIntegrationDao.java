package com.polus.fibicomp.fastintegration.dao;

import java.util.List;
import java.util.Set;

import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.budget.pojo.AwardBudgetDetail;
import com.polus.fibicomp.fastintegration.pojo.AwardExpenseFile;
import com.polus.fibicomp.fastintegration.pojo.AwardExpenseTransactionsRT;
import com.polus.fibicomp.fastintegration.pojo.AwardExpenseTransactionsRTLog;
import com.polus.fibicomp.fastintegration.pojo.AwardRevenueFile;
import com.polus.fibicomp.fastintegration.pojo.AwardRevenueTransactionsRT;
import com.polus.fibicomp.fastintegration.pojo.SapAwardFeed;
import com.polus.fibicomp.fastintegration.pojo.SapAwardFeedBatchFiles;
import com.polus.fibicomp.fastintegration.pojo.SapFeedProbGrantCodeReport;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplFmBudget;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplFundedPrgm;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplGrantBudMaster;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplGrantMaster;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplProjectDef;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplSponsoPrgm;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplSponsorClass;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplWbs;
import com.polus.fibicomp.fastintegration.vo.IntegrationReportVO.EmailContent;

@Transactional
@Service
public interface FastIntegrationDao {
	
	/**
	 * This method is used to save AwardFeedBatchFiles.
	 * @param sapAwardFeedBatchFiles - object of AwardFeedBatchFiles.
	 */
	public void saveOrUpdateAwardFeedBatchFiles(SapAwardFeedBatchFiles sapAwardFeedBatchFiles);

	/**
	 * This method is used to fetch all SapFeedTmplFundedPrgm based on batchId.
	 * @return A list of SapFeedTmplFundedPrgm.
	 */
	public List<SapFeedTmplFundedPrgm> getSapFeedTmplFundedPrgmByBatchId(Integer batchId);

	/**
	 * This method is used to fetch all SapFeedTmplGrantBudMaster based on batchId.
	 * @return A list of SapFeedTmplGrantBudMaster.
	 */
	public List<SapFeedTmplGrantBudMaster> getSapFeedTmplGrantBudMasterByBatchId(Integer batchId);

	/**
	 * This method is used to fetch all SapFeedTmplGrantMaster based on batchId.
	 * @return A list of SapFeedTmplGrantMaster.
	 */
	public List<SapFeedTmplGrantMaster> getSapFeedTmplGrantMasterByBatchId(Integer batchId);

	/**
	 * This method is used to fetch all SapFeedTmplProjectDef based on batchId.
	 * @return A list of SapFeedTmplProjectDef.
	 */
	public List<SapFeedTmplProjectDef> getSapFeedTmplProjectDefByBatchId(Integer batchId);

	/**
	 * This method is used to fetch all SapFeedTmplSponsoPrgm based on batchId.
	 * @return A list of SapFeedTmplSponsoPrgm.
	 */
	public List<SapFeedTmplSponsoPrgm> getSapFeedTmplSponsoPrgmByBatchId(Integer batchId);

	/**
	 * This method is used to fetch all SapFeedTmplSponsorClass based on batchId.
	 * @return A list of SapFeedTmplSponsorClass.
	 */
	public List<SapFeedTmplSponsorClass> getSapFeedTmplSponsorClassByBatchId(Integer batchId);

	/**
	 * This method is used to fetch all SapFeedTmplWbs based on batchId.
	 * @return A list of SapFeedTmplWbs.
	 */
	public List<SapFeedTmplWbs> getSapFeedTmplWbsByBatchId(Integer batchId);

	/**
	 * This method is used to get batchId.
	 * @param feedIds
	 * @return batchId.
	 */
	public Integer generateBatchId(List<String> feedIds);

	/**
	 * This method is used to update SapFeedTmplSponsorClass.
	 * @param sapFeedTmplSponsorClass - object of SapFeedTmplSponsorClass.
	 */
	public void updateSponsorClass(SapFeedTmplSponsorClass sapFeedTmplSponsorClass);

	/**
	 * This method is used to update SapFeedTmplSponsoPrgm.
	 * @param sapFeedTmplSponsoPrgm - object of SapFeedTmplSponsoPrgm.
	 */
	public void updateSponsoPrgm(SapFeedTmplSponsoPrgm sapFeedTmplSponsoPrgm);

	/**
	 * This method is used to update SapFeedTmplGrantMaster.
	 * @param sapFeedTmplGrantMaster - object of SapFeedTmplGrantMaster.
	 */
	public void updateGrantMaster(SapFeedTmplGrantMaster sapFeedTmplGrantMaster);

	/**
	 * This method is used to update SapFeedTmplFundedPrgm.
	 * @param sapFeedTmplFundedPrgm - object of SapFeedTmplFundedPrgm.
	 */
	public void updateFundedPrgm(SapFeedTmplFundedPrgm sapFeedTmplFundedPrgm);

	/**
	 * This method is used to update SapFeedTmplProjectDef.
	 * @param sapFeedTmplProjectDef - object of sapFeedTmplProjectDef.
	 */
	public void updateProjectDef(SapFeedTmplProjectDef sapFeedTmplProjectDef);

	/**
	 * This method is used to update SapFeedTmplGrantBudMaster.
	 * @param sapFeedTmplGrantBudMaster - object of sapFeedTmplGrantBudMaster.
	 */
	public void updateGrantBudMaster(SapFeedTmplGrantBudMaster sapFeedTmplGrantBudMaster);

	/**
	 * This method is used to update SapFeedTmplWbs.
	 * @param  sapFeedTmplWbs - object of SapFeedTmplWbs.
	 */
	public void updateWBS(SapFeedTmplWbs sapFeedTmplWbs);

	/**
	 * This method is used to get all feed Id based on batchId.
	 * @return A list of feed Id.
	 */
	public List<SapAwardFeed> getAllfeedId(Integer batchId);

	/**
	 * This method is used to update SapAwardFeed.
	 * @param  sapAwardFeedId - object of SapAwardFeed.
	 */
	public void updateFeedStatus(SapAwardFeed sapAwardFeedId);

	/**
	 * This method is used to get all sapFeedTmplFmBudget based on batchId.
	 * @return A list of sapFeedTmplFmBudget.
	 */
	public List<SapFeedTmplFmBudget> getSapFeedTmplFmBudgetByBatchId(Integer batchId);

	/**
	 * This method is used to get all feed status based on feedId and batchId.
	 * @return A list of feed status.
	 */
	public List<String> fetchAllFeedStatus(Integer feedId, Integer batchId);

	/**
	 * This method is used to update hold status.
	 * @return 
	 */
	public String sapAwardUpdateHoldStatus();

	/**
	 * This method is used to update SapFeedTmplFmBudget.
	 * @param  sapFeedTmplFmBudget - object of SapFeedTmplFmBudget.
	 */
	public void updateFmBudgets(SapFeedTmplFmBudget sapFeedTmplFmBudget);

	/**
	 * This method is used to save AwardExpenseTransactionsRT.
	 * @param  awardExpenseTransactionsRT - object of AwardExpenseTransactionsRT.
	 * @return awardExpenseTransactionsRT- object of AwardExpenseTransactionsRT.
	 * @throws Exception 
	 */
	public AwardExpenseTransactionsRT saveExpenseTransactionRT(AwardExpenseTransactionsRT awardExpenseTransactionsRT) throws Exception;

	/**
	 * This method is used to delete AwardExpenseTransactionsRT.
	 */
	public void deleteAllExpenseTransactionRT();

	/**
	 * This method is used to save AwardExpenseTransactionsRTLog.
	 * @param  awardExpenseTransactionsRTLog - object of AwardExpenseTransactionsRTLog.
	 * @return awardExpenseTransactionsRTLog - object of AwardExpenseTransactionsRTLog.
	 */
	public AwardExpenseTransactionsRTLog saveAwardExpenseTransactionsRTLog(AwardExpenseTransactionsRTLog awardExpenseTransactionsRTLog);

	/**
	 * This method is used to save AwardExpenseFile.
	 * @param  awardExpenseFiles - object of AwardExpenseFile.
	 * @return awardExpenseFiles -object of AwardExpenseFile.
	 */
	public AwardExpenseFile saveAwardExpenseFiles(AwardExpenseFile awardExpenseFiles);

	/**
	 * This method is used to save AwardExpenseFile.
	 * @param  awardExpenseFiles - object of AwardExpenseFile.
	 */
	public void awardExpenseTrackerRefresh(Integer fileId);

	/**
	 * This method is used to sync award expense tracker based on file Id.
	 * @param fileId 
	 */
	public void awardExpenseTrackerSync(Integer fileId);

	/**
	 * This method is used to awardExpenseTrackerPrevalidation.
	 */
	public void awardExpenseTrackerPrevalidation();

	/**
	 * This method is used to fetch all AwardExpenseTransactionsRT based on fileId.
	 * @return A list of AwardExpenseTransactionsRT.
	 */
	public List<AwardExpenseTransactionsRT> getawardExpenseTransactionRTByFileId(Integer fileId);

	/**
	 * This method is used to get latest award expense tracker file id.
	 * @retur file Id.
	 */
	public Integer getLatestAwardExpenseFile();

	/**
	 * This method is used to fetch data of expense tracking
	 * @return list of expense data transactions
	 */
	public List<Object[]> downloadExpenseTrackerReport();

	/**
	 * This method is used to get the data of expense tracking based on IOcode and account number
	 * @return list of expense data transactions
	 */
	public List<Object[]> downloadExpenseTrackerReportLevel2();

	/**
	 * This method is used to get the data of expense tracking based account number
	 * @return overall details of expense
	 */
	public List<Object[]> downloadExpenseTrackerRepoertLevelOne();

	/**
	 * This method is used to get award number  based on fileId.
	 * @return set of award numbers
	 */
	public Set<String> getawardAwardNumbersByFileId(Integer fileId);

	/**
	 * This method is used to process the sap feed Budget Report
	 * @param batchId
	 */
	public void sapFeedBudgetReport(Integer batchId);

	/**
	 * This method is used to process the sap feed report
	 * @param batchId
	 */
	public void sapFeedReport(Integer batchId);

	/**
	 * This method is used to fetch sap report
	 * @param batchId
	 * @param baCode
	 * @return list of sap feeds
	 */
	public List<Object[]> sapReport(Integer batchId, String businessArea);

	/**
	 * This method is used to fetch  sap feed budget report
	 * @param batchId
	 * @param businessArea
	 * @return list of sap feeds
	 */
	public List<Object[]> exportSapFeedBudgetReport(Integer batchId, String businessArea);

	/**
	 * This method is used to fetch the data that not feeded in sap
	 * @param batchId
	 * @param businessArea
	 * @return list of data not feeded in sap
	 */
	public List<Object[]> exportDataNotFeeded(Integer batchId, String businessArea);

	/**
	 * This method is used to fetch count of sap award.
	 * @param batchId
	 * @return list of count
	 */
	public List<Integer> getSapAwardCount(Integer batchId);

	/**
	 * This method is used to save AwardRevenueFile.
	 * @param  awardRevenueFiles - object of AwardRevenueFile.
	 * @return awardRevenueFiles -object of AwardRevenueFile.
	 */
	public AwardRevenueFile saveAwardRevenueFiles(AwardRevenueFile awardRevenueFiles);

	/**
	 * This method is used to save AwardRevenueTransactionsRT.
	 * @param  awardRevenueTransactionsRT - object of AwardRevenueTransactionsRT.
	 * @return awardRevenueTransactionsRT -object of AwardRevenueTransactionsRT.
	 */
	public AwardRevenueTransactionsRT saveRevenueTransactionRT(AwardRevenueTransactionsRT awardRevenueTransactionsRT);

	/**
	 * This method is used to get award number based on fileId.
	 * @return set of award numbers
	 */
	public Set<String> getRevenueAwardNumbersByFileId(Integer fileId);

	/**
	 * This method is used to refresh award revenue details based on file Id.
	 * @param emailContent 
	 */
	public void awardRevenueTrackerRefresh(Integer fileId, EmailContent emailContent);

	/**
	 * This method is used to sync award revenue details.
	 * @param fileId
	 */
	public void awardRevenueTrackerSync(Integer fileId, EmailContent emailContent);

	/**
	 * This method is used to delete all award revenue details.
	 */
	public void deleteAllRevenueTransactionRT();

	/**
	 * This method is used to fetch data of award revenue transactions
	 * @return list of revenue data transactions
	 */
	public List<Object[]> downloadRevenueTransaction();

	/**
	 * This method is used to get the data of award revenue based account number
	 * @return overall details of award revenue
	 */
	public List<Object[]> downloadRevenueRepoertLevelOne();

	/**
	 * This method is used to get the data of award revenue based on IOcode and account number
	 * @return overall details of award revenue
	 */
	public List<Object[]> downloadRevenueReportLevelTwo();

	public void syncExpenseDataSet();

	/**
	 * This method is used to fetch sap award feed error logs.
	 * @param batchId
	 * @return list of sapAwardFeedBatchErrorLog
	 */

	public List<SapFeedProbGrantCodeReport> fetchSapFeedProbGrantCodeReport(Integer batchId, String businessArea);

	/**
	 * This method is used to get sap award feed details.
	 * @param feedId
	 * @return awardExpenseFiles -object of sapAwardFeed.
	 */
	public SapAwardFeed getSapAwardFeedDetails(Integer feedId);

	/**
	 * This method is used to get sap award feed budget details.
	 * @param awardId
	 * @return list of io code and line item cost.
	 */
	public List<AwardBudgetDetail> getSapAwardBudgetDetailsByAwardId(Integer awardId);

	/**
	 * This method is used to write grant code error details.
	 * @param batchId
	 * @return 
	 */
	public Integer getProblematicGrantCodeReport(Integer batchId);

	/**
	 * This method is used to update the flag to indicate whether the RT table loading is successful for expense and revenue
	 * @param fileId
	 * @param isSuccess
	 * @param fileType
	 * @param totalFileRowCount 
	 */
	public void updateStatusForDataLoadingInRT(Integer fileId, String isSuccess, String fileType, Integer totalFileRowCount);

	/**
	 * This method is used to update the flag to indicate whether the file is moved to system archive table loading is successful for expense and revenue
	 * @param fileId
	 * @param isSuccess
	 * @param fileType
	 */
	public void updateStatusForFileInSystem(Integer fileId, String isSuccess, String fileType);

	/**
	 * This method is used to update the flag to indicate whether the file is moved to remote archive table loading is successful for expense and revenue
	 * @param fileId
	 * @param isSuccess
	 * @param fileType
	 */
	public void updateStatusForFileInRemote(Integer fileId, String isSuccess, String fileType);

	/**
	 * This method is used to get the data for the feed status is 'N' and not equal to 'N'
	 * @param batchId
	 * @param feedStatus
	 * @return list of soft launch report
	 */
	public List<Object[]> exportSoftLaunchAndNoFeedReport(Integer batchId, String feedStatus);

	/**
	 * This method is used to update the soft launch report details
	 * @param batchId
	 */
	public void sapFeedSoftLaunchReport(Integer batchId);

	/**
	 * This method is used to get the details for soft lauch sumary datas
	 * @param batchId
	 * @return list of summary datas
	 */
	public List<Object[]> getSoftLaunchSummaryReport(Integer batchId);

	public Integer getExpenseTransactionCount(Integer fileId);

	public Integer getRevenueTransactionCount(Integer fileId);

	/**
	 * This method is used to update sap award feed batch details
	 * @param batchId
	 */
	public void updateSapAwardFeedBatch(Integer batchId);

	/**
	 * to update claim status with revenue data
	 */
	public void updateClaimWithRevenueData(EmailContent emailContent);

	 /**
   * @param batchId
	 * @return
	 */
	public List<SapAwardFeed> getAllFeedAndNonFeeds(Integer batchId);

	/**
	 * @param sapAwardfeed
	 */
	public void updateFeedStatusForError(SapAwardFeed sapAwardfeed);

	/**
	 * @param feedId
	 * @param batchId
	 * @return
	 */
	public List<String> fetchAllBudgetFeedStatus(Integer feedId, Integer batchId);

}
