package com.polus.fibicomp.fastintegration.service;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.Arrays;
import java.util.Comparator;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.dao.DataIntegrityViolationException;
import org.springframework.stereotype.Service;

import com.polus.fibicomp.claims.claimsIntegration.excelity.service.SftpConfigurationService;
import com.polus.fibicomp.claims.claimsIntegration.sapconcur.service.SapConcurService;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.fastintegration.dao.FastIntegrationDao;
import com.polus.fibicomp.fastintegration.vo.IntegrationReportVO;

@Service(value = "fastTransactionService")
public class FastTransactionServiceImpl implements FastTransactionService {

	protected static Logger logger = LogManager.getLogger(FastTransactionServiceImpl.class.getName());

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private FastIntegrationService fastIntegrationService;

	@Autowired
	private FastIntegrationDao fastIntegrationDao;

	@Autowired
	private SapConcurService sapConcurService;
	
	@Autowired
	private SftpConfigurationService sftpConfigurationService;

	private static final String REVENUE_INVOICE = "REVENUE_INVOICE";

	@Override
	public void fastDataMigrationForExpenseTracker() {
		Integer initialCount = 0;
		File[] fileData = null;
		String expenseFilePath = sftpConfigurationService.getSftpConfigurationValueAsString(Constants.RISE_FAST_OUTBOUND_EXPENSES);
		if (commonDao.getParameterValueAsBoolean(Constants.IS_SFTP_ENABLED)) {
			fastIntegrationService.getSftpResDirectory(sftpConfigurationService.getSftpConfigurationValueAsString(Constants.SFTP_FAST_OUTBOUND_EXPENSES) , expenseFilePath);
		}
		File directory = new File(expenseFilePath);
		if (directory.isDirectory()) {
			fileData = directory.listFiles();
			Arrays.sort(fileData, Comparator.comparing(File::getName));
			if (fileData.length >1) {
				fastIntegrationService.sendMultipleFileNotification(fileData, "Expense");
			}
		}
		if (fileData != null) {
			for (File file : fileData) {
				logger.info("file going to process(Expense) : {}", file.getName());
				IntegrationReportVO integrationReportVO = new IntegrationReportVO();
				if (file.isFile()) {
					integrationReportVO = fastIntegrationService.saveAwardExpenseFiles(file);
					int totalCount = integrationReportVO.getTotalFileRowCount();
					Integer fileId = integrationReportVO.getFileId();
					BufferedReader fileReader = null;
					try {
						fileReader = new BufferedReader(new FileReader(file.getAbsoluteFile()));
						if (fileReader != null) {
							fileReader.readLine();
							do {
								integrationReportVO = fastIntegrationService.fastIntegrationExpenseTransactionRTProcessing(integrationReportVO.getFileName(), fileReader, file, integrationReportVO);
							} while (integrationReportVO.getTotalFileRowCount()!= null && !integrationReportVO.getTotalFileRowCount().equals(initialCount));
							fileReader.close();
						}
					} catch (DataIntegrityViolationException e) {
						logger.error("Error in method fastIntegrationExpenseTransactionRTProcessing due to DataIntegrityViolationException {}",e.getMessage());
						Integer processed = fastIntegrationDao.getExpenseTransactionCount(fileId);
						fastIntegrationDao.updateStatusForDataLoadingInRT(fileId, Constants.NO, "E", processed);
						integrationReportVO.getEmailContent().setError(new StringBuilder());
						integrationReportVO.getEmailContent().getError().append("Error in fastIntegrationExpenseTransactionRTProcessing : <br/>").append(e).append("<br/>")
						.append("Only ").append(processed).append(" are saved out of ").append(totalCount).append(" in file : ").append(file.getName())
						.append("<br>").append("File ").append(file.getName()).append(" will be not moved to archive").append("<br>");
						integrationReportVO.setErrorOccured(Boolean.TRUE);
						e.printStackTrace();
					} catch (IOException e) {
						logger.error("Error in method fastIntegrationExpenseTransactionRTProcessing due to IOException : {}",e.getMessage());
						integrationReportVO.getEmailContent().getError().append("Error in fastIntegrationExpenseTransactionRTProcessing : <br/>").append(e).append("<br/>");
						e.printStackTrace();
						integrationReportVO.setErrorOccured(Boolean.TRUE);
					} catch (Exception e) {
						logger.error("Error in method fastIntegrationExpenseTransactionRTProcessing : {}",e.getMessage());
						integrationReportVO.getEmailContent().getError().append("Error in fastIntegrationExpenseTransactionRTProcessing : <br/>").append(e).append("<br/>");
						e.printStackTrace();
						integrationReportVO.setErrorOccured(Boolean.TRUE);
					} finally {
						try {
							fileReader.close();
						} catch (IOException e) {
							logger.error("error in fastDataMigrationForExpenseTracker while closing the BufferedReader");
							e.printStackTrace();
						}
					}
					fastIntegrationService.setSuccessEmailContentForExpense(integrationReportVO);
					if (integrationReportVO.getErrorOccured().equals(Boolean.FALSE)) {
						fastIntegrationService.moveExpenseFile(file, integrationReportVO.getEmailContent(), integrationReportVO.getFileId());
					} 
				}	
				logger.info(" before awardExpenseTrackerRefresh file Id : {}", integrationReportVO.getFileId());
				fastIntegrationService.awardExpenseTrackerRefresh(integrationReportVO.getFileId());
				logger.info(" after awardExpenseTrackerRefresh");
				logger.info(" before awardExpenseTrackerSync file Id : {}", integrationReportVO.getFileId());
				fastIntegrationService.awardExpenseTrackerSync(integrationReportVO.getFileId());
				logger.info("after awardExpenseTrackerSync");
				fastIntegrationService.sendSuccesReportMail(integrationReportVO, new StringBuilder("Integration status report for expense tracker ").
							append(commonDao.getDateFromTimestampZoneFormat(Constants.CRON_JOB_TIMEZONE, Constants.LONG_DATE_FORMAT)).toString());
				fastIntegrationService.sendErrorReportMail(integrationReportVO, new StringBuilder("ERROR : Integration status report for expense tracker ")
						.append(commonDao.getDateFromTimestampZoneFormat(Constants.CRON_JOB_TIMEZONE, Constants.LONG_DATE_FORMAT)).toString());
				if (integrationReportVO.getErrorOccured().equals(Boolean.TRUE)) {
					return;
				}
			}
		}
	}

	@Override
	public void fastIntegrationRevenueTransactionRTProcessing(String processingType) {
		IntegrationReportVO integrationReportVO = new IntegrationReportVO();
		Integer initialCount = 0;
		File[] fileData = null;
		String errorMessage = "Error in method fastIntegrationRevenueTransactionRTProcessing ";
		String processType = processingType.equals(REVENUE_INVOICE) ? " for Revenue Invoice " : " for Revenue ";
		String revenue = sftpConfigurationService.getSftpConfigurationValueAsString(Constants.RISE_REVENUE_OUTBOUND);
		String invoice = sftpConfigurationService.getSftpConfigurationValueAsString(Constants.RISE_REVENUE_INVOICE_OUTBOUND);
		if (commonDao.getParameterValueAsBoolean(Constants.IS_SFTP_ENABLED)) {
				if (processingType.equals(REVENUE_INVOICE)) {
					sapConcurService.getSapConcurResDirectory(sftpConfigurationService.getSftpConfigurationValueAsString(Constants.SFTP_REVENUE_INVOICE_OUTBOUND), invoice, integrationReportVO.getEmailContent());
				} else {
					sapConcurService.getSapConcurResDirectory(sftpConfigurationService.getSftpConfigurationValueAsString(Constants.SFTP_REVENUE_OUTBOUND), revenue, integrationReportVO.getEmailContent());
				}
		}

		File directory = new File(processingType.equals(REVENUE_INVOICE) ? invoice : revenue);
		if (directory.isDirectory()) {
			fileData = directory.listFiles();
			Arrays.sort(fileData, Comparator.comparing(File::getName));
			if (fileData.length >1) {
				fastIntegrationService.sendMultipleFileNotification(fileData, processingType);
			}
		}
		if (fileData != null) {
			for (File file : fileData) {
				logger.info("file going to process type {} is {}",processingType, file.getName());
				if (file.isFile()) {
					integrationReportVO = fastIntegrationService.saveAwardRevenueFiles(file);
					int totalCount = integrationReportVO.getTotalFileRowCount();
					Integer fileId = integrationReportVO.getFileId();
					BufferedReader fileReader = null;
					try {
						fileReader = new BufferedReader(new FileReader(file.getAbsoluteFile()));
						fileReader.readLine();
						do {
							integrationReportVO = fastIntegrationService.fastIntegrationRevenueTransactionRTProcessing(integrationReportVO.getFileName(),
									fileReader, file, integrationReportVO, processingType);
						} while (integrationReportVO.getTotalFileRowCount() != null && !integrationReportVO.getTotalFileRowCount().equals(initialCount)); 
						fileReader.close();
					} catch (DataIntegrityViolationException e) {
						logger.error(errorMessage.concat(processType) , e.getMessage());
						Integer processed = fastIntegrationDao.getRevenueTransactionCount(fileId);
						fastIntegrationDao.updateStatusForDataLoadingInRT(fileId, Constants.NO, "E", processed);
						integrationReportVO.setErrorOccured(Boolean.TRUE);
						integrationReportVO.getEmailContent().getError().append(errorMessage).append(processType).append(" : <br/>").append(e).append("<br/>")
						.append("Only ").append(processed).append(" are saved out of ").append(totalCount).append(" in file : ").append(file.getName())
						.append("<br>").append("File ").append(file.getName()).append(" will be not moved to archive").append("<br>");
						e.printStackTrace();
					} catch (IOException e) {
						logger.error(errorMessage.concat(processType) ," due to IOException {}",e.getMessage());
						integrationReportVO.getEmailContent().getError().append(errorMessage).append(processType).append(" : <br/>").append(e).append("<br/>");
					} catch (Exception e) {
						logger.error(errorMessage.concat(processType), e.getMessage());
						integrationReportVO.getEmailContent().getError().append(errorMessage).append(processType).append(" : <br/>").append(e).append("<br/>");
					} finally {
						try {
							fileReader.close();
						} catch (IOException e) {
							logger.error(errorMessage.concat(processType) ," while closing the BufferedReader");
							e.printStackTrace();
						}
					}
					fastIntegrationService.sendSuccessEmailContentForRevenue(integrationReportVO,processingType);
					if (integrationReportVO.getErrorOccured().equals(Boolean.FALSE)) {
						fastIntegrationService.moveRevenueFile(file, integrationReportVO.getEmailContent(), integrationReportVO.getFileId(),processingType);
					} 
				}
				logger.info(" before awardRevenueTrackerRefresh");
				fastIntegrationService.awardRevenueTrackerRefresh(integrationReportVO.getFileId(), integrationReportVO.getEmailContent());
				logger.info(" after awardRevenueTrackerRefresh");
				logger.info(" before awardRevenueTrackerSync");
				fastIntegrationService.awardRevenueTrackerSync(integrationReportVO.getFileId(), integrationReportVO.getEmailContent());
				logger.info("after awardRevenueTrackerSync");
				if (processingType.equals("REVENUE_INVOICE")) {
					logger.info(" before updateClaimWithRevenueData");
					fastIntegrationDao.updateClaimWithRevenueData(integrationReportVO.getEmailContent());
					logger.info("after updateClaimWithRevenueData");
				}			
				fastIntegrationService.sendSuccesReportMail(integrationReportVO, (new StringBuilder("Integration status report").append(processType))
						.append(commonDao.getDateFromTimestampZoneFormat(Constants.CRON_JOB_TIMEZONE, Constants.LONG_DATE_FORMAT)).toString());
				fastIntegrationService.sendErrorReportMail(integrationReportVO , (new StringBuilder("ERROR : Integration status report").append(processType))
						.append(commonDao.getDateFromTimestampZoneFormat(Constants.CRON_JOB_TIMEZONE, Constants.LONG_DATE_FORMAT)).toString());
				if (integrationReportVO.getErrorOccured().equals(Boolean.TRUE)) {
					return;
				}
			}
		} else {
			if (integrationReportVO.getEmailContent() != null && integrationReportVO.getEmailContent().getError().length() > 0) {
				fastIntegrationService.sendErrorReportMail(integrationReportVO , (new StringBuilder("ERROR : Integration status report").append(processType))
						.append(commonDao.getDateFromTimestampZoneFormat(Constants.CRON_JOB_TIMEZONE, Constants.LONG_DATE_FORMAT)).toString());
			}
		}
	}

}
