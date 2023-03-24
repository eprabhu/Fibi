package com.polus.fibicomp.fastintegration.service;

import java.io.BufferedReader;
import java.io.File;
import java.util.List;
import java.util.Set;

import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplFmBudget;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplFundedPrgm;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplGrantBudMaster;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplGrantMaster;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplProjectDef;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplSponsoPrgm;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplSponsorClass;
import com.polus.fibicomp.fastintegration.pojo.SapFeedTmplWbs;
import com.polus.fibicomp.fastintegration.vo.IntegrationReportVO;
import com.polus.fibicomp.fastintegration.vo.IntegrationReportVO.EmailContent;
import com.polus.fibicomp.vo.CommonVO;

@Transactional
@Service(value = "fastIntegrationService")
public interface FastIntegrationService {

	/**
	 * This method is used for fast award data migration.
	 * @param vo
	 * @return 
	 */
	public String fastDataMigration(IntegrationReportVO vo);

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
	 * this method contains the setupd for upload files
	 * 
	 * @param directoryName
	 * @param files
	 */
	public IntegrationReportVO fetchResponce();

	/**
	 * This method is used to fetch all SapFeedTmplFmBudget based on batchId.
	 * @return A list of SapFeedTmplFmBudget.
	 */
	public List<SapFeedTmplFmBudget> getSapFeedTmplFmBudgetByBatchId(Integer batchId);

	/**
	 * This method is used to process expense tracker file by params.
	 * @return integrationReportVO - object of IntegrationReportVO.
	 * @throws Exception 
	 */
	public IntegrationReportVO fastIntegrationExpenseTransactionRTProcessing(String fileName, BufferedReader fileReader, File file, IntegrationReportVO vo) throws Exception;

	/**
	 * This method is used to awardExpenseTrackerPrevalidation.
	 */
	public void awardExpenseTrackerPrevalidation();

	/**
	 * This method is used to sync award expense tracker based on file Id.
	 * @param fileId 
	 */
	public void awardExpenseTrackerSync(Integer fileId);

	/**
	 * This method is used to save AwardExpenseFile.
	 * @param  awardExpenseFiles - object of AwardExpenseFile.
	 */
	public void awardExpenseTrackerRefresh(Integer fileId);

	/**
	 * This method is used to send mail for response files.
	 * @param  integrationReportVO - object of IntegrationReportVO.
	 */
	public void fastIntegrationResponseMail(IntegrationReportVO integrationReportVO);

	/**
	 * This method is used to update sap award feed based on batch Id.
	 * @param  set of batchIds - batchIds.
	 */
	public void updateSapAwardFeedDetail(Set<Integer> batchIds);

	/**
	 * This method is used to generate the work book for Expense Tracker Report
	 * @param vo
	 * @return object of workbook
	 */
	public XSSFWorkbook getXSSFWorkbookForExpenseTracker(CommonVO vo);

	/**
	 * This method is used to generate the work book for Expense Tracker Report for level2(IOCode based report)
	 * @param vo
	 * @return object of workbook
	 */
	public XSSFWorkbook exportLevelTwoExpenseTransactionReport(CommonVO vo);

	/**
	 * This method is used to generate the work book for Expense Tracker Report for level1(Acconut number based report)
	 * @param vo
	 * @return object of workbook
	 */
	public XSSFWorkbook exportLevelOneExpenseTransactionReport(CommonVO vo);

	/**
	 * This method is used to save award expense file.
	 * @param  file - object of File.
	 * @return integrationReportVO - object of IntegrationReportVO.
	 */
	public IntegrationReportVO saveAwardExpenseFiles(File file);

	/**
	 * This method is used to move file to archive folder by params.
	 * @param  file - object of File.
	 * @param  emailBodye - object of StringBuilder.
	 * @param fileId 
	 */
	public void moveExpenseFile(File file, EmailContent emailBodye, Integer fileId);

	/**
	 * This method is used to get sftp directory.
	 * @param  sftpWorkingDir.
	 * @param  baseDirectory
	 */
	public void getSftpResDirectory(String sftpWorkingDir, String baseDirectory);

	/**
	 * This method is used to export the sap feeds
	 * @param vo
	 * @return object of workbook
	 */
	public XSSFWorkbook exportSapFeedReport(CommonVO vo);

	/**
	 * This method is used to update sap award hold status.
	 */
	public void sapAwardUpdateHoldStatus();

	/**
	 * This method is used to process revenue file by params.
	 * @return integrationReportVO - object of IntegrationReportVO.
	 */
	public IntegrationReportVO fastIntegrationRevenueTransactionRTProcessing(String fileName, BufferedReader fileReader,
			File file, IntegrationReportVO vo, String processingType);

	/**
	 * This method is used to move file to archive folder by params.
	 * @param  file - object of File.
	 * @param  emailBodye - object of StringBuilder.
	 * @param fileId 
	 */
	public void moveRevenueFile(File file, EmailContent emailBodye, Integer fileId, String processingType);

	/**
	 * This method is used to save award revenue file.
	 * @param  file - object of File.
	 * @return integrationReportVO - object of IntegrationReportVO.
	 */
	public IntegrationReportVO saveAwardRevenueFiles(File file);

	/**
	 * This method is used to refresh award revenue details based on file Id.
	 * @param emailContent 
	 */
	public void awardRevenueTrackerRefresh(Integer fileId, EmailContent emailContent);

	/**
	 * This method is used to sync award revenue details.
	 * @param fileId
	 * @param emailContent 
	 */
	public void awardRevenueTrackerSync(Integer fileId, EmailContent emailContent);

	/**
	 * This method is used to fetch data of award revenue transactions
	 * @return list of revenue data transactions
	 */
	public XSSFWorkbook getXSSFWorkbookForRevenueTransaction(CommonVO vo);

	/**
	 * This method is used to get the data of award revenue based account number
	 * @return overall details of award revenue
	 */
	public XSSFWorkbook exportLevelOneRevenueTransactionReport(CommonVO vo);

	/**
	 * This method is used to get the data of award revenue based on IOcode and account number
	 * @return overall details of award revenue
	 */
	public XSSFWorkbook exportLevelTwoRevenueTransactionReport(CommonVO vo);

	/**
	 * This method is used to generate Success mail content for the expense transaction
	 * @param integrationReportVO
	 */
	public void setSuccessEmailContentForExpense(IntegrationReportVO integrationReportVO);

	/**
	 * This method is used to generate Success mail content for the revenue transaction
	 * @param integrationReportVO
	 * @param processingType
	 */
	public void sendSuccessEmailContentForRevenue(IntegrationReportVO integrationReportVO, String processingType);

//	public void sendExpenseTrackerReportMail(IntegrationReportVO vo);

	/**
	 * This method is used to send the success report mail
	 * @param vo
	 * @param subject
	 */
	void sendSuccesReportMail(IntegrationReportVO vo, String subject);

	/**
	 * This method is used to send the error report mail
	 * @param vo
	 * @param subject
	 */
	void sendErrorReportMail(IntegrationReportVO vo, String subject);

	/**
	 * This method is used to send mail when multiple files are included in folder
	 * @param fileData
	 * @param location
	 */
	void sendMultipleFileNotification(File[] fileData, String location);

	/**
	 * This method is used to send the Soft launch Report
	 * @param batchId
	 */
	public void sendSoftLaunchFeedReport(Integer batchId);

	/**
	 * This method is used to prepare sap award feed files
	 * @param batchId
	 * @param vo
	 * @return vo
	 */
	public IntegrationReportVO prepareSapAwardFeedFiles(IntegrationReportVO vo, Integer batchId);

	/**
	 * This method is used to generate template for feed
	 * @param vo
	 */
	public void fastTemplateGeneration(IntegrationReportVO vo);

	/**
	 * @param fileName
	 * @param SFTPWORKINGDIR
	 */
	public void movetoSftp(String fileName, String SFTPWORKINGDIR, EmailContent emailContent);

	/**
	 * @param fileContent
	 */
	public void createFastIntegrationLog(String fileContent);

	/**
	 * @param file
	 * @param emailBody
	 */
	public void moveFile(File file, EmailContent emailBody);

}
