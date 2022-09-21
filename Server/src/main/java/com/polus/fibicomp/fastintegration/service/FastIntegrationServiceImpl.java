package com.polus.fibicomp.fastintegration.service;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.sql.Timestamp;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.TimeZone;
import java.util.Vector;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import javax.transaction.Transactional;

import org.apache.commons.collections.CollectionUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.util.CellRangeAddress;
import org.apache.poi.xssf.usermodel.XSSFCellStyle;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import com.jcraft.jsch.Channel;
import com.jcraft.jsch.ChannelSftp;
import com.jcraft.jsch.ChannelSftp.LsEntry;
import com.jcraft.jsch.JSch;
import com.jcraft.jsch.SftpException;
import com.polus.fibicomp.award.dao.AwardDao;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.service.AwardService;
import com.polus.fibicomp.award.vo.AwardVO;
import com.polus.fibicomp.businessrule.service.BusinessRuleService;
import com.polus.fibicomp.claims.claimsIntegration.excelity.service.SftpConfigurationService;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.dashboard.service.DashboardService;
import com.polus.fibicomp.fastintegration.dao.FastIntegrationDao;
import com.polus.fibicomp.fastintegration.pojo.AwardExpenseFile;
import com.polus.fibicomp.fastintegration.pojo.AwardExpenseTransactionsRT;
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
import com.polus.fibicomp.fastintegration.sapfeedmaintenance.service.SapFeedMaintenanceService;
import com.polus.fibicomp.fastintegration.sapfeedmaintenance.vo.SapFeedMaintenanceVO;
import com.polus.fibicomp.fastintegration.vo.IntegrationReportVO;
import com.polus.fibicomp.fastintegration.vo.IntegrationReportVO.EmailContent;
import com.polus.fibicomp.notification.email.service.EmailService;
import com.polus.fibicomp.notification.email.vo.EmailServiceVO;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.polus.fibicomp.print.service.PrintService;
import com.polus.fibicomp.vo.CommonVO;

@Transactional
@Service(value = "fastIntegrationService")
public class FastIntegrationServiceImpl implements FastIntegrationService {

    @Autowired
    private FastIntegrationDao fastIntegrationDao;

    @Autowired
    public CommonDao commonDao;

    @Autowired
    private SftpConfigurationService sftpConfigurationService;
    
    @Autowired
    public EmailService emailService;

    @Value("${system.timezone}")
    private String timezone;

    @Autowired
    private CommonService commonService;

    @Autowired
    private DashboardService dashboardService;

    @Autowired
    private AwardDao awardDao;

    @Autowired
    private BusinessRuleService businessRuleService;

    @Autowired
    private AwardService awardService;

	private String revenueArchive;

	protected static Logger logger = LogManager.getLogger(FastIntegrationServiceImpl.class.getName());

	private static final String AWARD_NUMBER = "Award Number : ";
	private static final String ACCOUNT_NUMBER = ",  Account Number : " ;
	private static final String EMAILBODY_STRUCTURE= "Please go to " ;
	private static final String FAST_DATE_FORMAT = "yyyyMMdd";
	private static final String EMAILBODY_FILE_LOC = "  to view the files<br/>" ;
	private static final String REGULAR_EXPRESSION = "^\"|\"$";

	@Autowired
	public PrintService printService;

	@Autowired
	public SapFeedMaintenanceService sapFeedMaintenanceService;

	@Override
	public String fastDataMigration(IntegrationReportVO vo) {
		SapFeedMaintenanceVO sapFeedMaintenanceVO = vo.getSapFeedMaintenanceVO();
		try {
			fastTemplateGeneration(vo);
			if (sapFeedMaintenanceVO != null) {
				sapFeedMaintenanceService.getBatchDetailDashboard(sapFeedMaintenanceVO);
			}
		 } catch (Exception e) {
			logger.info("Error in fast execution fastDataMigration {}", e.getMessage());
			createFastIntegrationLog("Error in fast execution fastDataMigration {}" + e);
			e.printStackTrace();
		}
		return commonDao.convertObjectToJSON(sapFeedMaintenanceVO);
	}

	@Override
	public void fastTemplateGeneration(IntegrationReportVO vo) {
		try {
			List<SapAwardFeed> sapAwardFeeds = vo.getSapAwardFeeds();
			List<String> feedIds = new ArrayList<>();
			if (sapAwardFeeds != null && !sapAwardFeeds.isEmpty()) {
				sapAwardFeeds.stream().forEach(sapAwardFeed -> {
					feedIds.add(sapAwardFeed.getFeedId().toString());
				});
			}
			Integer batchId = fastIntegrationDao.generateBatchId(feedIds);
			if (batchId != null) {
				prepareSapAwardFeedFiles(vo, batchId);
			}
		} catch (Exception e) {
			logger.info("Error in fast execution fastTemplateGeneration {}", e.getMessage());
			createFastIntegrationLog("Error in fast execution fastTemplateGeneration {}" + e);
			e.printStackTrace();
		}
	}

	@Override
	public IntegrationReportVO prepareSapAwardFeedFiles(IntegrationReportVO vo, Integer batchId) {
		try {
		String sapFeedTmplFunded = "3_rise_funded_prgm";
		String sapFeedTmplGrantBudMaster = "7_rise_grant_bud_master";
		String sapFeedTmplGrantMaster = "6_rise_grant_master";
		String sapFeedTmplProjectDef = "4_rise_project_def";
		String sapFeedTmplSponsoPrgm = "1_rise_sponsor_prgm";
		String sapFeedTmplSponsorClass = "2_rise_sponsor_class";
		String sapFeedTmplWbs = "5_rise_wbs";
		String sapFeedTmplFmBudget = "8_rise_fund_bud_master";
		List<SapFeedTmplFundedPrgm> sapFeedTmplFundedData = getSapFeedTmplFundedPrgmByBatchId(batchId);
		List<SapFeedTmplGrantBudMaster> sapFeedTmplGrantBudMasterData = getSapFeedTmplGrantBudMasterByBatchId(batchId);
		List<SapFeedTmplGrantMaster> sapFeedTmplGrantMasterData = getSapFeedTmplGrantMasterByBatchId(batchId);
		List<SapFeedTmplProjectDef> sapFeedTmplProjectDefData = getSapFeedTmplProjectDefByBatchId(batchId);
		List<SapFeedTmplSponsoPrgm> sapFeedTmplSponsoPrgmData = getSapFeedTmplSponsoPrgmByBatchId(batchId);
		List<SapFeedTmplSponsorClass> sapFeedTmplSponsorClassData = getSapFeedTmplSponsorClassByBatchId(batchId);
		List<SapFeedTmplWbs> sapFeedTmplWbsData = getSapFeedTmplWbsByBatchId(batchId);
		List<SapFeedTmplFmBudget> sapFeedTmplFmBudgetDatas = getSapFeedTmplFmBudgetByBatchId(batchId);
		if (sapFeedTmplFundedData != null && !sapFeedTmplFundedData.isEmpty()) {
			exportSapFeedTmplFundedDataToCSV(sapFeedTmplFunded, sapFeedTmplFundedData, vo, batchId);
			createFastIntegrationLog("Total SapFeedTmplFundedPrgm data count :" + vo.getSapFeedTmplFundedDataCount());
		} else {
			vo.setSapFeedTmplFundedDataCount(0);
		}
		if (sapFeedTmplGrantMasterData != null && !sapFeedTmplGrantMasterData.isEmpty()) {
			exportSapFeedTmplGrantMasterData(sapFeedTmplGrantMaster, sapFeedTmplGrantMasterData, vo, batchId);
			createFastIntegrationLog("Total SapFeedTmplGrantMaster data count :" + vo.getSapFeedTmplGrantCount());
		} else {
			vo.setSapFeedTmplGrantMasterDataCount(0);
		}
		if (sapFeedTmplGrantBudMasterData != null && !sapFeedTmplGrantBudMasterData.isEmpty()) {
			exportSapFeedTmplGrantBudMasterData(sapFeedTmplGrantBudMaster, sapFeedTmplGrantBudMasterData, vo, batchId);
			createFastIntegrationLog(
					"Total SapFeedTmplGrantBudMaster data count :" + vo.getSapFeedTmplGrantBudMasterDataCount());
		} else {
			vo.setSapFeedTmplGrantBudMasterDataCount(0);
		}
		if (sapFeedTmplProjectDefData != null && !sapFeedTmplProjectDefData.isEmpty()) {
			exportSapFeedTmplProjectDefData(sapFeedTmplProjectDef, sapFeedTmplProjectDefData, vo, batchId);
			createFastIntegrationLog(
					"Total SapFeedTmplProjectDef data count :" + vo.getSapFeedTmplProjectDefDataCount());
		} else {
			vo.setSapFeedTmplProjectDefDataCount(0);
		}
		if (sapFeedTmplSponsoPrgmData != null && !sapFeedTmplSponsoPrgmData.isEmpty()) {
			exportSapFeedTmplSponsoPrgmData(sapFeedTmplSponsoPrgm, sapFeedTmplSponsoPrgmData, vo, batchId);
			createFastIntegrationLog(
					"Total SapFeedTmplSponsoPrgm data count :" + vo.getSapFeedTmplSponsoPrgmDataCount());
		} else {
			vo.setSapFeedTmplSponsoPrgmDataCount(0);
		}
		if (sapFeedTmplSponsorClassData != null && !sapFeedTmplSponsorClassData.isEmpty()) {
			exportSapFeedTmplSponsorClassData(sapFeedTmplSponsorClass, sapFeedTmplSponsorClassData, vo, batchId);
			createFastIntegrationLog(
					"Total SapFeedTmplSponsorClass data count :" + vo.getSapFeedTmplSponsorClassDataCount());
		} else {
			vo.setSapFeedTmplSponsorClassDataCount(0);
		}
		if (sapFeedTmplWbsData != null && !sapFeedTmplWbsData.isEmpty()) {
			exportSapFeedTmplWbsData(sapFeedTmplWbs, sapFeedTmplWbsData, vo, batchId);
			createFastIntegrationLog("Total SapFeedTmplWbs data count :" + vo.getSapFeedTmplWbsDataCount());
		} else {
			vo.setSapFeedTmplWbsDataCount(0);
		}
		if (sapFeedTmplFmBudgetDatas != null && !sapFeedTmplFmBudgetDatas.isEmpty()) {
			exportSapFeedTmplFmBudget(sapFeedTmplFmBudget, sapFeedTmplFmBudgetDatas, vo, batchId);
			createFastIntegrationLog("Total SapFeedTmplFmBudget data count :" + vo.getSapFeedTmplFmBudgetDataCount());
		} else {
			vo.setSapFeedTmplFmBudgetDataCount(0);
		}
		generateReportForFastIntegration(batchId);
		Integer value = generateProblematicGrantCodeReport(batchId);
		vo.setBatchId(batchId);
		if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_FAST_INTEGRATION_EMAIL)) {
			List<SapAwardFeed> sapAwardFeeds = fastIntegrationDao.getAllfeedId(batchId);
			List<Integer> sapAwardCount = fastIntegrationDao.getSapAwardCount(batchId);
			vo.setEmailContent(setFileEmailBody(sapAwardFeeds,vo.getEmailContent(), vo.getIsResponseMail(), sapAwardCount));
			sendSuccesReportMail(vo, new StringBuilder("Integration file generated status Report for Batch ID : ").append(batchId.toString()).append( "  ")
					.append(commonDao.getDateFromTimestampZoneFormat(Constants.CRON_JOB_TIMEZONE, Constants.LONG_DATE_FORMAT)).toString());
			List<String> baCodes = getBusinessAreaCodes();
			for (String businessArea : baCodes) {
				CommonVO commonVo = new CommonVO();
				createSapFeedReport(commonVo, batchId, businessArea, vo);
			}
			sendSapFeedReportBasedOnBatchId(batchId);
			sendSoftLaunchFeedReport(batchId);
			if(value != null) {
				sendProblematicGrantCodeNotification(vo);
			}
			logger.info("mail sending completed in fastDataMigration");
		  }
		} catch (Exception e) {
			logger.info("Error in fast execution prepareSapAwardFeedFiles {}", e.getMessage());
			createFastIntegrationLog("Error in fast execution prepareSapAwardFeedFiles {}" + e);
			e.printStackTrace();
			vo.getEmailContent().getError().append("Exception while processing files: ").append(e).append("<br/>");
		}
		return vo;
	}

	private void createSapFeedReport(CommonVO vo, Integer batchId, String businessArea, IntegrationReportVO integrationVo) {
		try {
		StringBuilder emailBody = new StringBuilder("");
		XSSFWorkbook workbook = new XSSFWorkbook();
		XSSFSheet sheet = workbook.createSheet("SAP Feed Report" + businessArea);
        commonService.addDetailsInHeader(workbook, sheet);
		String departmentName = setCompanyNameBasedOnBaCode(businessArea);
		createSapFeedReportByParams(vo, batchId, businessArea, sheet, workbook, departmentName);
		XSSFSheet sapFeedBudgetReportSheet = createSapFeedBudgetReportByParam(vo, batchId, businessArea, workbook, departmentName);
		XSSFSheet sapFeedDataNoFeedReportSheet = createSapDataNoFeed(vo, businessArea, workbook, departmentName, batchId);
		String fileName = new StringBuilder("SAP Feed Report ").append(departmentName).append("_").append(batchId.toString()).append("_").append(createdDateWithTime()).append(".xlsx").toString();
		File report  = commonService.createfileInUploads(workbook, fileName);
		emailBody.append("Dear All, <br/> Please find the SAP Feed Report for the date  ").append(formatDate()).append("  attached herewith.");
		integrationVo.setAttachment(report);
		integrationVo.setEmailBody(emailBody);
		integrationVo.setAttachment(report);
		integrationVo.setBatchId(batchId);
		integrationVo.setDepartmentName(departmentName);
		integrationVo.setBusinessArea(businessArea);
		if (sheet.getLastRowNum() > 1 && sheet.getRow(1) != null || sapFeedBudgetReportSheet.getLastRowNum() > 1 && sapFeedBudgetReportSheet.getRow(1) != null
				|| sapFeedDataNoFeedReportSheet.getLastRowNum() > 1 && sapFeedDataNoFeedReportSheet.getRow(1) != null) {
			sendSapFeedReportMail(integrationVo);
		} else {
			emailService.deleteAttachment(report);
		}
		} catch(Exception e) {
			logger.info("error in createSapFeedReport",e.getMessage());
			e.printStackTrace();
		}
	}

	private XSSFSheet createSapFeedBudgetReportByParam(CommonVO vo, Integer batchId, String businessArea, XSSFWorkbook workbook, String departmentName) {
		XSSFSheet sapFeedBudgetReportSheet = workbook.createSheet("SAP Feed Budget Report" + businessArea);
		vo.setDocumentHeading(new StringBuilder("SAP Feed Budget Report for ").append(departmentName).toString());
		List<Object[]> sapFeedBugdetReport = fastIntegrationDao.exportSapFeedBudgetReport(batchId, businessArea);
		Object[] tableHeadingRows = {"Batch Id", "Award Number", "Account Number", "Award Version", "Variation Type",
				"Account Type", "L2 WBS Number", "Fund Code", "Process", "Amount in Feed", "Current Line Item Cost",
				"Previous Line Item Cost", "Bussiness Area","Grant Code","Profit Center"};
		dashboardService.prepareExcelSheet(sapFeedBugdetReport, sapFeedBudgetReportSheet, tableHeadingRows, workbook, vo);
		return sapFeedBudgetReportSheet;
	}

	private void createSapFeedReportByParams(CommonVO vo, Integer batchId, String businessArea, XSSFSheet sheet, XSSFWorkbook workbook, String departmentName) {
		vo.setDocumentHeading(new StringBuilder("SAP Feed Report for ").append(departmentName).toString());
		List<Object[]> sapReport = fastIntegrationDao.sapReport(batchId, businessArea);
		Object[] tableHeadingRow = {"Batch Id", "Award Number", "Account Number", "Award Version", "Variation Type",
				"Account Type", "Title", "Current Start Date", "Previous Start Date", "Current End Date",
				"Previous End Date", "L2 WBS Number", "Cost Element Description", "Fund Code",
				"Current Principal Investigator", "Previous Principal Investigator", "L1 WBS Description",
				"L2 WBS Description", "Business Area","Grant Code","Profit Center"};
		dashboardService.prepareExcelSheet(sapReport, sheet, tableHeadingRow, workbook, vo);
	}

	private XSSFSheet createSapDataNoFeed(CommonVO vo, String businessArea, XSSFWorkbook workbook, String departmentName,  Integer batchId) {
		XSSFSheet sapFeedDataNoFeedReportSheet = workbook.createSheet(new StringBuilder("Data No Feed Report").append(businessArea).toString());
		vo.setDocumentHeading(new StringBuilder("SAP Feed Data No Feed Report for ").append(departmentName).toString());
		List<Object[]> sapReport = fastIntegrationDao.exportDataNotFeeded(batchId, businessArea);
		Object[] tableHeadingRow = { "Award Number","Account Number","Account Type","Lead Unit Number","Lead Unit Name",
				"Award Title","Principal Investigator","Sponsor Code","Sponsor Name","Sponsor Award Number","Start Date",
				"End date", "Variation Type", "Variation Subject", "System Comment", "Update Timestamp", "Submission Date", "Business Area"};
		dashboardService.prepareExcelSheet(sapReport, sapFeedDataNoFeedReportSheet, tableHeadingRow, workbook, vo);
		return sapFeedDataNoFeedReportSheet;
	}

	private Integer generateProblematicGrantCodeReport(Integer batchId) {
		return fastIntegrationDao.getProblematicGrantCodeReport(batchId);
	}

	private void sendSapFeedBudgetReportMail(IntegrationReportVO vo) {
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		String emailAddress = "";
		emailServiceVO.setFileName(vo.getAttachment());
		emailServiceVO.setModuleCode(Constants.AWARD_MODULE_CODE);	
		emailServiceVO.setBody(vo.getEmailBody().toString());	
		emailServiceVO.setSubject(new StringBuilder(vo.getDepartmentName()).append(" SAP FEED REPORT : ")
				.append(commonDao.getDateFromTimestampZoneFormat(Constants.CRON_JOB_TIMEZONE, Constants.LONG_DATE_FORMAT)).toString());
		sendMailBasedOnBaCode(emailAddress, emailServiceVO, vo);
	}

	private String setCompanyNameBasedOnBaCode(String businessArea) {
		String companyName = "";
		if(businessArea != null && businessArea.equals(Constants.NIE_BUSINESS_AREA_CODE)) {
			companyName = "NIE";
		} else if(businessArea!= null && businessArea.equals(Constants.LKC_BUSINESS_AREA_CODE)) {
			companyName = "LKC";
		} else if(businessArea!= null && businessArea.equals(Constants.RSIS_BUSINESS_AREA_CODE)) {
			companyName = "RSIS";
		} 
		return companyName;
	}

	private void sendSapFeedReportBasedOnBatchId(Integer batchId) {
		try {
			String fileName = new StringBuilder("SAP Feed Report for NTU_").append(batchId.toString()).append("_").append(createdDateWithTime()).append(".xlsx").toString();
			String departmentName = "NTU";
			CommonVO vo = new CommonVO();
			IntegrationReportVO integrationVo = new IntegrationReportVO();
			StringBuilder emailBody = new StringBuilder("");
			vo.setDocumentHeading("SAP Feed Report for NTU");
			XSSFWorkbook workbook = new XSSFWorkbook();
			XSSFSheet sheet = workbook.createSheet(vo.getDocumentHeading());
            commonService.addDetailsInHeader(workbook, sheet);
			createSapFeedReportForNTU(vo, batchId, sheet, workbook, departmentName);
			XSSFSheet sapFeedBudgetReportSheet = createSapFeedBudgetReportForNTU(vo, batchId, workbook, departmentName);
			XSSFSheet sapFeedDataNoFeedReportSheet = createSapDataNoFeed(vo, workbook, departmentName, batchId);
			File report = commonService.createfileInUploads(workbook, fileName);
			emailBody.append("Dear All, <br/> Please find the SAP Feed Report for the date  ").append(formatDate()).append("  attached herewith.").toString();
			integrationVo.setAttachment(report);
			integrationVo.setEmailBody(emailBody);
			integrationVo.setAttachment(report);
			integrationVo.setBatchId(batchId);
			integrationVo.setDepartmentName(departmentName);
			if (sheet.getLastRowNum() > 1 && sheet.getRow(1) != null || sapFeedBudgetReportSheet.getLastRowNum() > 1 && sapFeedBudgetReportSheet.getRow(1) != null
					|| sapFeedDataNoFeedReportSheet.getLastRowNum() > 1 && sapFeedDataNoFeedReportSheet.getRow(1) != null) {
				sendSapFeedBudgetReportMail(integrationVo);
			} else {
			  emailService.deleteAttachment(report);
			}
		} catch (Exception e) {
			logger.info("sendSapFeedReportBasedOnBatchId {}", e.getMessage());
			e.printStackTrace();
		}
	}

	private XSSFSheet createSapDataNoFeed(CommonVO vo, XSSFWorkbook workbook, String departmentName, Integer batchId) {
		XSSFSheet sapFeedDataNoFeedReportSheet = workbook.createSheet("SAP Feed Data No Feed");
		vo.setDocumentHeading("SAP Feed Report For Data No Feed NTU");
		List<Object[]> sapFeedDataNoFeedReport = fastIntegrationDao.exportDataNotFeeded(batchId, null);
		Object[] tableHeadingRowForDataNoFeed = { "Award Number","Account Number","Account Type","Lead Unit Number","Lead Unit Name",
				"Award Title","Principal Investigator","Sponsor Code","Sponsor Name","Sponsor Award Number","Start Date",
				"End date", "Variation Type", "Variation Subject", "System Comment", "Update Timestamp", "Submission Date", "Business Area"};
		dashboardService.prepareExcelSheet(sapFeedDataNoFeedReport, sapFeedDataNoFeedReportSheet, tableHeadingRowForDataNoFeed, workbook, vo);
		return sapFeedDataNoFeedReportSheet;
	}

	private XSSFSheet createSapFeedBudgetReportForNTU(CommonVO vo, Integer batchId, XSSFWorkbook workbook, String departmentName) {
		XSSFSheet sheetSapFeedBudgetReport = workbook.createSheet("SAP Feed Budget Report");
		vo.setDocumentHeading("SAP Feed Budget Report for NTU");
		List<Object[]> sapFeedBugdetReport = fastIntegrationDao.exportSapFeedBudgetReport(batchId, null);
		Object[] tableHeadingRows = { "Batch Id", "Award Number", "Account Number", "Award Version", "Variation Type",
				"Account Type", "L2 WBS Number", "Fund Code", "Process", "Amount in Feed", "Current Line Item Cost",
				"Previous Line Item Cost", "Bussiness Area","Grant Code","Profit Center"};
		dashboardService.prepareExcelSheet(sapFeedBugdetReport, sheetSapFeedBudgetReport, tableHeadingRows, workbook, vo);
		return sheetSapFeedBudgetReport;
	}

	private void createSapFeedReportForNTU(CommonVO vo, Integer batchId, XSSFSheet sheet, XSSFWorkbook workbook, String departmentName) {
		List<Object[]> sapReport = fastIntegrationDao.sapReport(batchId, null);
		Object[] tableHeadingRow = { "Batch Id", "Award Number", "Account Number", "Award Version",
				"Variation Type", "Account Type", "Title", "Current Start Date", "Previous Start Date",
				"Current End Date", "Previous End Date", "L2 WBS Number", "Cost Element Description", "Fund Code",
				"Current Principal Investigator", "Previous Principal Investigator", "L1 WBS Description",
				"L2 WBS Description", "Business Area","Grant Code","Profit Center" };
		dashboardService.prepareExcelSheet(sapReport, sheet, tableHeadingRow, workbook, vo);
	}

	private String formatDate() {
		return new SimpleDateFormat("MMMM dd").format(commonDao.getCurrentDate());
	}

	private void sendSapFeedReportMail(IntegrationReportVO vo) {
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		String emailAddress = "";
		emailServiceVO.setFileName(vo.getAttachment());
		emailServiceVO.setModuleCode(Constants.AWARD_MODULE_CODE);	
		emailServiceVO.setBody(vo.getEmailBody().toString());	
		emailServiceVO.setSubject(new StringBuilder(vo.getDepartmentName()).append("  SAP FEED REPORT : ")
				.append(commonDao.getDateFromTimestampZoneFormat(Constants.CRON_JOB_TIMEZONE, Constants.LONG_DATE_FORMAT)).toString());
		sendMailBasedOnBaCode(emailAddress, emailServiceVO, vo);
	}

	private void sendProblematicGrantCodeNotification(IntegrationReportVO vo) {
		try {
	  
		List<SapFeedProbGrantCodeReport>  sapFeedProbGrantCodeReportDatas = new ArrayList<>();		
		List<String> baCodes = getBusinessAreaCodes();
		for(String businessArea: baCodes) {
			sendProblematicGrantCodesBasedOnBaCode(vo, sapFeedProbGrantCodeReportDatas, businessArea);
		}
		sendProblematicGrantCodesBasedOnBaCode(vo, sapFeedProbGrantCodeReportDatas, null);
		} catch (Exception e) {
			logger.info("Error in sendProblematicGrantCodeNotification {}", e.getMessage());
			e.printStackTrace();
		}	
	}		

	private void sendProblematicGrantCodesBasedOnBaCode(IntegrationReportVO vo, List<SapFeedProbGrantCodeReport>  sapFeedProbGrantCodeReportDatas,  String businessArea) {
		try {
		StringBuilder emailBody = new StringBuilder("");
		vo.setDepartmentName((businessArea != null) ? setCompanyNameBasedOnBaCode(businessArea) : "NTU");
		vo.setBusinessArea(businessArea);
		vo.setFileName(new StringBuilder("problematic_grant_code_").append(vo.getDepartmentName()).append("_").append(vo.getBatchId().toString()).append("_").append(createdDateWithTime()).append(".xlsx").toString());
		List<SapFeedProbGrantCodeReport> sapFeedProbGrantCodeReports = fastIntegrationDao.fetchSapFeedProbGrantCodeReport(vo.getBatchId(), businessArea);		
		if (sapFeedProbGrantCodeReports != null && !sapFeedProbGrantCodeReports.isEmpty()) {		
			vo.setEmailBody(setProblematicGrantCodeFileEmailBody(sapFeedProbGrantCodeReports, emailBody, vo));
			if (vo.getProblematicGrantCodefileName() != null) {
				sendProblematicGrantCodeMail(vo);
			}
		 }
		} catch (Exception e) {
			logger.info("error in sendProblematicGrantCodesBasedOnBaCode {}", e.getMessage());
			e.printStackTrace();
		}
	}

	private void sendProblematicGrantCodeMail(IntegrationReportVO vo) {	
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		String emailAddress = "";
		emailServiceVO.setFileName(vo.getProblematicGrantCodefileName());
		emailServiceVO.setModuleCode(Constants.AWARD_MODULE_CODE);	
		emailServiceVO.setBody(vo.getEmailBody().toString());	
		emailServiceVO.setSubject(new StringBuilder(vo.getDepartmentName()).append(" Problematic grant code report for Batch ID : ").append(vo.getBatchId()).toString());
		sendMailBasedOnBaCode(emailAddress, emailServiceVO, vo);
	}

	private void sendMailBasedOnBaCode(String emailAddress, EmailServiceVO emailServiceVO, IntegrationReportVO vo) {
		Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
		if(vo.getBusinessArea()!= null && vo.getBusinessArea().equals(Constants.NIE_BUSINESS_AREA_CODE)) {
			emailAddress = commonDao.getParameterValueAsString(Constants.SAP_EMAIL_NIE_ADDRESS);	
		} else if(vo.getBusinessArea()!= null && vo.getBusinessArea().equals(Constants.LKC_BUSINESS_AREA_CODE)) {
			emailAddress = commonDao.getParameterValueAsString(Constants.SAP_EMAIL_LKC_ADDRESS);	
		} else if(vo.getBusinessArea()!= null && vo.getBusinessArea().equals(Constants.RSIS_BUSINESS_AREA_CODE)) {
			emailAddress = commonDao.getParameterValueAsString(Constants.SAP_EMAIL_RSIS_ADDRESS);	
		} else {
			emailAddress = commonDao.getParameterValueAsString(Constants.SAP_EMAIL_NTU_ADDRESS);	
		}
		if (emailAddress != null && !emailAddress.isEmpty()) {
			String emailCCAddress = commonDao.getParameterValueAsString(Constants.FAST_INTEGRATION_EMAIL_CC_ADDRESS);
			if (emailCCAddress != null && !emailCCAddress.isEmpty()) {
			String[] singleEmailCCAddress = emailCCAddress.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)");
			if (singleEmailCCAddress.length > 0) {
				for(String recipeientMailCCAddress :singleEmailCCAddress) {
					commonService.setNotificationRecipientsforNonEmployees(recipeientMailCCAddress, Constants.NOTIFICATION_RECIPIENT_TYPE_CC,
							dynamicEmailRecipients);
				}
			}
			}
			String[] singleEmailAddress = emailAddress.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)");
			if (singleEmailAddress.length > 0) {
				for(String recipeientMailAddress :singleEmailAddress) {
					commonService.setNotificationRecipientsforNonEmployees(recipeientMailAddress, Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients);
				}
			}
			emailServiceVO.setRecipients(dynamicEmailRecipients);
			emailServiceVO.setSubModuleCode(Constants.AWARD_SUBMODULE_CODE.toString());
			emailService.sendEmail(emailServiceVO);
		}
	}

	private StringBuilder setProblematicGrantCodeFileEmailBody(List<SapFeedProbGrantCodeReport> sapFeedProbGrantCodeReports, StringBuilder emailBody, IntegrationReportVO vo) {
		File file = null;
		try {
				XSSFWorkbook workbook = new XSSFWorkbook();
				XSSFSheet sheet = workbook.createSheet("Problematic Grant Code Details");
                commonService.addDetailsInHeader(workbook, sheet);
				file = prepareExcelSheetForProblematicGrantcodes(sapFeedProbGrantCodeReports, sheet, workbook, vo, vo.getFileName());
				if (sheet.getLastRowNum() > 1) {
				emailBody.append("Dear All, <br/> Please find the Problematic Grant Code Details for the date  ").append(formatDate()).append("  attached herewith.");
				vo.setProblematicGrantCodefileName(file);
		     } 
		} catch (Exception e) {
			logger.info("Error in setProblematicGrantCodeFileEmailBody {}", e.getMessage());
			e.printStackTrace();
		} 
		return emailBody;
	}

	private File prepareExcelSheetForProblematicGrantcodes(List<SapFeedProbGrantCodeReport> sapFeedProbGrantCodeReports, XSSFSheet sheet, XSSFWorkbook workbook,  IntegrationReportVO vo, String fileName) throws IOException {
		XSSFCellStyle tableHeadStyle = workbook.createCellStyle();
		XSSFCellStyle tableBodyStyle = workbook.createCellStyle();
		sheet.addMergedRegion(new CellRangeAddress(0, 0, 0, 4));
		printService.prepareHeadingRowForExcel(sheet, workbook, tableHeadStyle, tableBodyStyle, "Problematic Grant Code Details", 0);
		int rowNumber = 0;
		++rowNumber;
		rowNumber = prepareHaedingForProblematicGrantCode(sheet, rowNumber, tableHeadStyle);
		prepareGrantCodeBudgetDetailsForExcel(sheet, rowNumber, sapFeedProbGrantCodeReports, tableBodyStyle, workbook);
		printService.autoSizeColumns(workbook, rowNumber);
		return commonService.createfileInUploads(workbook, fileName);
	}

	private int prepareHaedingForProblematicGrantCode(XSSFSheet sheet, Integer rowNumber, XSSFCellStyle tableHeadStyle) {
		Row headingRow = sheet.createRow(rowNumber++);
		
		Cell batchIdCell = headingRow.createCell(0);
		batchIdCell.setCellStyle(tableHeadStyle);
		batchIdCell.setCellValue("Batch ID");

		Cell grantCodeCell = headingRow.createCell(1);
		grantCodeCell.setCellStyle(tableHeadStyle);
		grantCodeCell.setCellValue("Grant Code");

		Cell awardNumberCell = headingRow.createCell(2);
		awardNumberCell.setCellStyle(tableHeadStyle);
		awardNumberCell.setCellValue("Award Nummber");

		Cell ioCodeCell = headingRow.createCell(3);
		ioCodeCell.setCellStyle(tableHeadStyle);
		ioCodeCell.setCellValue("Account Number");
		
		Cell awardVersionCell = headingRow.createCell(4);
		awardVersionCell.setCellStyle(tableHeadStyle);
		awardVersionCell.setCellValue("Award Version");
		
		Cell businessAreaCell = headingRow.createCell(5);
		businessAreaCell.setCellStyle(tableHeadStyle);
		businessAreaCell.setCellValue("Business Area");

		Cell variationTypeCodeCell = headingRow.createCell(6);
		variationTypeCodeCell.setCellStyle(tableHeadStyle);
		variationTypeCodeCell.setCellValue("Variation Type Code");

		Cell variationTypeCell = headingRow.createCell(7);
		variationTypeCell.setCellStyle(tableHeadStyle);
		variationTypeCell.setCellValue("Variation Type");

		Cell accountTypeCell = headingRow.createCell(8);
		accountTypeCell.setCellStyle(tableHeadStyle);
		accountTypeCell.setCellValue("Account Type");
		
		Cell titleCell = headingRow.createCell(9);
		titleCell.setCellStyle(tableHeadStyle);
		titleCell.setCellValue("Title");

		Cell internalOrderCodeCell = headingRow.createCell(10);
		internalOrderCodeCell.setCellStyle(tableHeadStyle);
		internalOrderCodeCell.setCellValue("Internal Order Code");
		
		Cell processCell = headingRow.createCell(11);
		processCell.setCellStyle(tableHeadStyle);
		processCell.setCellValue("Process");
		
		Cell fundCodeCell = headingRow.createCell(12);
		fundCodeCell.setCellStyle(tableHeadStyle);
		fundCodeCell.setCellValue("Fund Code");

		Cell amountInFeedCell = headingRow.createCell(13);
		amountInFeedCell.setCellStyle(tableHeadStyle);
		amountInFeedCell.setCellValue("Amount in Feed");
		
		Cell curLineItemCostCell = headingRow.createCell(14);
		curLineItemCostCell.setCellStyle(tableHeadStyle);
		curLineItemCostCell.setCellValue("Current Line Item Cost");
		
		Cell prevLineItemCostCell = headingRow.createCell(15);
		prevLineItemCostCell.setCellStyle(tableHeadStyle);
		prevLineItemCostCell.setCellValue("Previous Line Item Cost");

		Cell curPICell = headingRow.createCell(16);
		curPICell.setCellStyle(tableHeadStyle);
		curPICell.setCellValue("Current Principal Investigator");
		
		Cell costElementDescriptionCell = headingRow.createCell(17);
		costElementDescriptionCell.setCellStyle(tableHeadStyle);
		costElementDescriptionCell.setCellValue("Cost Element Description");

		Cell profitCenterCell = headingRow.createCell(18);
		profitCenterCell.setCellStyle(tableHeadStyle);
		profitCenterCell.setCellValue("Profit Center");

		Cell costCenterCell = headingRow.createCell(19);
		costCenterCell.setCellStyle(tableHeadStyle);
		costCenterCell.setCellValue("Cost Center");

		Cell fundCenterCell = headingRow.createCell(20);
		fundCenterCell.setCellStyle(tableHeadStyle);
		fundCenterCell.setCellValue("Fund Center");
		return rowNumber;
	}

	private void prepareGrantCodeBudgetDetailsForExcel(XSSFSheet sheet, int rowNumber, List<SapFeedProbGrantCodeReport> sapFeedProbGrantCodeReports, XSSFCellStyle tableBodyStyle, XSSFWorkbook workbook) {
		for (SapFeedProbGrantCodeReport sapFeedProbGrantCodeReport : sapFeedProbGrantCodeReports) {
			Row row = sheet.createRow(rowNumber++);
			int cellNumber = 0;

			Cell batchIdValueCell = row.createCell(cellNumber++);
			batchIdValueCell.setCellStyle(tableBodyStyle);
			batchIdValueCell.setCellValue(sapFeedProbGrantCodeReport.getBatchId());

			Cell grantCodeValueCell = row.createCell(cellNumber++);
			grantCodeValueCell.setCellStyle(tableBodyStyle);
			grantCodeValueCell.setCellValue(sapFeedProbGrantCodeReport.getGrantCode());

			Cell awardNumberValueCell = row.createCell(cellNumber++);
			awardNumberValueCell.setCellStyle(tableBodyStyle);
			awardNumberValueCell.setCellValue(sapFeedProbGrantCodeReport.getAwardNumber());

			Cell accountNumberValueCell = row.createCell(cellNumber++);
			accountNumberValueCell.setCellStyle(tableBodyStyle);
			accountNumberValueCell.setCellValue(sapFeedProbGrantCodeReport.getAccountNumber());
			
			Cell sequenceNumberValueCell = row.createCell(cellNumber++);
			sequenceNumberValueCell.setCellStyle(tableBodyStyle);
			sequenceNumberValueCell.setCellValue(sapFeedProbGrantCodeReport.getSequenceNumber());

			Cell businessAreaValueCell = row.createCell(cellNumber++);
			businessAreaValueCell.setCellStyle(tableBodyStyle);
			businessAreaValueCell.setCellValue(sapFeedProbGrantCodeReport.getBusinessArea());

			Cell variationTypeCodeValueCell = row.createCell(cellNumber++);
			variationTypeCodeValueCell.setCellStyle(tableBodyStyle);
			variationTypeCodeValueCell.setCellValue(sapFeedProbGrantCodeReport.getVariationTypeCode());

			Cell variationTypeCell = row.createCell(cellNumber++);
			variationTypeCell.setCellStyle(tableBodyStyle);
			variationTypeCell.setCellValue(sapFeedProbGrantCodeReport.getVariationType());

			Cell accountTypeValueCell = row.createCell(cellNumber++);
			accountTypeValueCell.setCellStyle(tableBodyStyle);
			accountTypeValueCell.setCellValue(sapFeedProbGrantCodeReport.getAccountType());

			Cell titleValueCell = row.createCell(cellNumber++);
			titleValueCell.setCellStyle(tableBodyStyle);
			titleValueCell.setCellValue(sapFeedProbGrantCodeReport.getTitle());

			Cell ioCodeValueCell = row.createCell(cellNumber++);
			ioCodeValueCell.setCellStyle(tableBodyStyle);
			ioCodeValueCell.setCellValue(sapFeedProbGrantCodeReport.getIoCode());

			Cell processValueCell = row.createCell(cellNumber++);
			processValueCell.setCellStyle(tableBodyStyle);
			processValueCell.setCellValue(sapFeedProbGrantCodeReport.getProcess());
			
			Cell fundCodeValueCell = row.createCell(cellNumber++);
			fundCodeValueCell.setCellStyle(tableBodyStyle);
			fundCodeValueCell.setCellValue(sapFeedProbGrantCodeReport.getFundCode());

			Cell budgetAmountValueCell = row.createCell(cellNumber++);
			budgetAmountValueCell.setCellStyle(tableBodyStyle);
			if(sapFeedProbGrantCodeReport.getBudgetAmount() != null) {
				budgetAmountValueCell.setCellValue(Double.valueOf(sapFeedProbGrantCodeReport.getBudgetAmount().toString()));
			} else {
				budgetAmountValueCell.setCellValue(" ");
			}

			Cell curLineItemCostValueCell = row.createCell(cellNumber++);
			curLineItemCostValueCell.setCellStyle(tableBodyStyle);
			if(sapFeedProbGrantCodeReport.getCurLineItemCost() != null) {
				curLineItemCostValueCell.setCellValue(Double.valueOf(sapFeedProbGrantCodeReport.getCurLineItemCost().toString()));
			} else {
				curLineItemCostValueCell.setCellValue(" ");
			}
			

			Cell prevLineItemCostCell = row.createCell(cellNumber++);
			prevLineItemCostCell.setCellStyle(tableBodyStyle);
			if(sapFeedProbGrantCodeReport.getPrevLineItemCost() != null) {
				prevLineItemCostCell.setCellValue(Double.valueOf(sapFeedProbGrantCodeReport.getPrevLineItemCost().toString()));
			} else {
				prevLineItemCostCell.setCellValue(" ");
			}
			
			
			Cell curPiNameCell = row.createCell(cellNumber++);
			curPiNameCell.setCellStyle(tableBodyStyle);
			curPiNameCell.setCellValue(sapFeedProbGrantCodeReport.getCurPiName());

			Cell costElementDescriptionValueCell = row.createCell(cellNumber++);
			costElementDescriptionValueCell.setCellStyle(tableBodyStyle);
			costElementDescriptionValueCell.setCellValue(sapFeedProbGrantCodeReport.getCostElementDescription());

			Cell profitCenterCell =row.createCell(cellNumber++);
			profitCenterCell.setCellStyle(tableBodyStyle);
			profitCenterCell.setCellValue(sapFeedProbGrantCodeReport.getProfitCenter());

			Cell costCenterCell = row.createCell(cellNumber++);
			costCenterCell.setCellStyle(tableBodyStyle);
			costCenterCell.setCellValue(sapFeedProbGrantCodeReport.getCostCenter());

			Cell fundCenterCell = row.createCell(cellNumber++);
			fundCenterCell.setCellStyle(tableBodyStyle);
			fundCenterCell.setCellValue(sapFeedProbGrantCodeReport.getFundCenter());
		}
		
	}

	private EmailContent setFileEmailBody(List<SapAwardFeed> sapAwardFeeds, EmailContent emailBody, Boolean isResponseMail, List<Integer> sapAwardCount) {
		emailBody.getSuccess().append("<b>Summary of Interface Programme</b><br/>");
		emailBody.getSuccess().append("Total Awards sent in this Batch :   ").append(sapAwardCount.get(3).toString()).append("<br/>");
		emailBody.getSuccess().append("Total awards failed to sent :   ").append(sapAwardCount.get(4).toString()).append("<br/>");
		if (Boolean.TRUE.equals(isResponseMail)) {
		emailBody.getSuccess().append("Total Successful Interface in this Batch:   ").append(sapAwardCount.get(1).toString()).append("<br/>");
		emailBody.getSuccess().append("Total Error in interface in this Batch:    ").append(sapAwardCount.get(2).toString()).append("<br/>");
		emailBody.getSuccess().append("No response recorded in this Batch: ").append(sapAwardCount.get(3).toString()).append("<br/>");
		}
		if (Boolean.FALSE.equals(isResponseMail)) {
			List<Integer> sapFeedErrorAwardIds = new ArrayList<>();
			List<Integer> sapFeedSuccessAwardIds = new ArrayList<>();
			emailBody.getSuccess().append(EMAILBODY_STRUCTURE).append(sftpConfigurationService.getSftpConfigurationValueAsString(Constants.RISE_FAST_INBOUND)).append(EMAILBODY_FILE_LOC);
			for (SapAwardFeed sapAwardFeedDetail : sapAwardFeeds) {
				if (sapAwardFeedDetail.getFeedStatus().equals("F")) {
					sapFeedSuccessAwardIds.add(sapAwardFeedDetail.getAwardId());
				} else if (sapAwardFeedDetail.getFeedStatus().equals("P")) {
					sapFeedErrorAwardIds.add(sapAwardFeedDetail.getAwardId());
				}
			}
			emailBody.getSuccess().append("<br/> <b>List of Awards failed to interface due to error</b> <br/>");
			if (!sapFeedErrorAwardIds.isEmpty()) {
				sapFeedErrorAwardIds.forEach(sapFeedErrorAwardId -> {
					Award award = awardDao.getAwardDetailsById(sapFeedErrorAwardId);
					if (award != null) {
						emailBody.getSuccess().append(AWARD_NUMBER).append(award.getAwardNumber()).append(ACCOUNT_NUMBER).append(award.getAccountNumber()).append("<br/><br/>");
					}
				});	
			} else {
				emailBody.getSuccess().append("<br/>No award failed<br/>");
			}
			emailBody.getSuccess().append("<br/> <b>List of Awards successfully interfaced</b> <br/>");
			if (!sapFeedSuccessAwardIds.isEmpty()) {
				sapFeedSuccessAwardIds.forEach(sapFeedSuccessAwardId -> {
					Award award = awardDao.getAwardDetailsById(sapFeedSuccessAwardId);
					if (award != null) {
						emailBody.getSuccess().append(AWARD_NUMBER).append(award.getAwardNumber()).append(ACCOUNT_NUMBER).append(award.getAccountNumber()).append("<br/><br/>");
					}
				});	
			} else {
				emailBody.getSuccess().append("<br/>No award interfaced<br/>");
			}
		} else {
			emailBody.getSuccess().append(EMAILBODY_STRUCTURE).append(sftpConfigurationService.getSftpConfigurationValueAsString(Constants.RISE_FAST_OUTBOUND_ARCHIVE_RESPONSE)).append(EMAILBODY_FILE_LOC);
			emailBody.getSuccess().append("<br/> <b>List of Awards successfully interfaced</b> <br/>");
			for (SapAwardFeed sapAwardFeedDetail : sapAwardFeeds) {
				if (sapAwardFeedDetail.getFeedStatus().equals("R")) {
					Award award = awardDao.getAwardDetailsById(sapAwardFeedDetail.getAwardId());
					if (award != null) {
						emailBody.getSuccess().append(AWARD_NUMBER).append(sapAwardFeedDetail.getAwardNumber()).append(ACCOUNT_NUMBER).append(award.getAccountNumber()).append("<br/><br/>");
					}
				}
			}
			emailBody.getSuccess().append("<br/> <b>List of Awards which got error in response</b> <br/>");
			for (SapAwardFeed sapAwardFeedDetail : sapAwardFeeds) {
				if (sapAwardFeedDetail.getFeedStatus().equals("E")) {
					Award award = awardDao.getAwardDetailsById(sapAwardFeedDetail.getAwardId());
					if (award != null) {
						emailBody.getError().append(AWARD_NUMBER).append(sapAwardFeedDetail.getAwardNumber()).append(ACCOUNT_NUMBER).append(award.getAccountNumber()).append("<br/><br/>");
					}
				}
			}
			emailBody.getSuccess().append("<br/> <b>List of Awards without any response</b><br/>");
			for (SapAwardFeed sapAwardFeedDetail : sapAwardFeeds) {
				if (sapAwardFeedDetail.getFeedStatus().equals("F")) {
					Award award = awardDao.getAwardDetailsById(sapAwardFeedDetail.getAwardId());
					if (award != null) {
						emailBody.getSuccess().append(AWARD_NUMBER).append(sapAwardFeedDetail.getAwardNumber()).append(ACCOUNT_NUMBER).append(award.getAccountNumber()).append("<br/><br/>");
					}
				}
			}
		}
		emailBody.getSuccess().append("<br/><b>Note: Successful interface of award is determined solely based on 'S' response received on all interface files.</b></br>");
		if (emailBody.getError().length() != 0) {
			emailBody.getError().append("<br/><b>Note: Successful interface of award is determined solely based on 'S' response received on all interface files.</b></br>");
		}
		return emailBody;
	}

	private EmailContent setExpenseTrackerEmailBody(Set<String> accountNumbers,EmailContent emailBody) {
		emailBody.getSuccess().append(EMAILBODY_STRUCTURE).append(sftpConfigurationService.getSftpConfigurationValueAsString(Constants.RISE_FAST_OUTBOUND_ARCHIVE_EXPENSE)).append(EMAILBODY_FILE_LOC);
		emailBody.getSuccess().append("<br/>Successful Interface for: <br/>");
		if (accountNumbers != null && !accountNumbers.isEmpty()) {
			for (String accountNumber : accountNumbers) {
				emailBody.getSuccess().append(" Account Number : ").append(accountNumber).append("<br/><br/>");
			}
		}
		return emailBody;
	}

	@Override
	public List<SapFeedTmplFundedPrgm> getSapFeedTmplFundedPrgmByBatchId(Integer batchId) {
		return fastIntegrationDao.getSapFeedTmplFundedPrgmByBatchId(batchId);
	}

	@Override
	public List<SapFeedTmplGrantBudMaster> getSapFeedTmplGrantBudMasterByBatchId(Integer batchId) {
		return fastIntegrationDao.getSapFeedTmplGrantBudMasterByBatchId(batchId);
	}

	@Override
	public List<SapFeedTmplGrantMaster> getSapFeedTmplGrantMasterByBatchId(Integer batchId) {
		return fastIntegrationDao.getSapFeedTmplGrantMasterByBatchId(batchId);
	}

	@Override
	public List<SapFeedTmplProjectDef> getSapFeedTmplProjectDefByBatchId(Integer batchId) {
		return fastIntegrationDao.getSapFeedTmplProjectDefByBatchId(batchId);
	}

	@Override
	public List<SapFeedTmplSponsoPrgm> getSapFeedTmplSponsoPrgmByBatchId(Integer batchId) {
		return fastIntegrationDao.getSapFeedTmplSponsoPrgmByBatchId(batchId);
	}

	@Override
	public List<SapFeedTmplSponsorClass> getSapFeedTmplSponsorClassByBatchId(Integer batchId) {
		return fastIntegrationDao.getSapFeedTmplSponsorClassByBatchId(batchId);
	}

	@Override
	public List<SapFeedTmplWbs> getSapFeedTmplWbsByBatchId(Integer batchId) {
		return fastIntegrationDao.getSapFeedTmplWbsByBatchId(batchId);
	}

	private void exportSapFeedTmplFmBudget(String fileName, List<SapFeedTmplFmBudget> sapFeedTmplFmBudgetDatas, IntegrationReportVO vo, Integer batchId) {
		String newFileName = null;
		BufferedWriter fileWriter = null;
		try {
			File file = createFile(fileName, batchId);
			String files = file.getAbsolutePath();
			char ch = '"';
			fileWriter = new BufferedWriter(new FileWriter(files));
			fileWriter.write("LINE_ITEM, BCS_VALUE_TYPE, PROCESS, DOC_TYPE, "
					+ "BUDGET_VERSION, DOCUMENT_DATE, YEAR, BUDGET_TYPE, FM_GRANT, "
					+ "FUND_CODE, FUND_CENTER, COMMITMENT_ITEM, FUNCTIONAL_AREA, FUNDED_PROGRAM, "
					+ "BUDGET_AMOUNT, LOCAL_CURRENCY, DISTRIBUTION_KEY, LINE_ITEM_TEXT,BATCH_ID, CREATE_STATUS");
			for (SapFeedTmplFmBudget sapFeedTmplFmBudgetData : sapFeedTmplFmBudgetDatas) {
				if( sapFeedTmplFmBudgetData.getLineItemText() != null) {
					sapFeedTmplFmBudgetData.setLineItemText(sapFeedTmplFmBudgetData.getLineItemText().replaceAll("[\\r\\n]+", ""));
					sapFeedTmplFmBudgetData.setLineItemText(sapFeedTmplFmBudgetData.getLineItemText().replaceAll("\"", ""));
				}
				String line = String.format("%s,%s,%s,%s,%s,%s,%d,%s,%s,%s,%s,%s,%s,%s,%f,%s,%s,%s,%d,%s",
						ch + sapFeedTmplFmBudgetData.getLineItem() + ch,
						ch + sapFeedTmplFmBudgetData.getBcsValueType() + ch,
						ch + sapFeedTmplFmBudgetData.getProcess() + ch, ch + sapFeedTmplFmBudgetData.getDocType() + ch,
						ch + sapFeedTmplFmBudgetData.getBudgetVersion() + ch,
						ch + commonService.convertDateFormatBasedOnTimeZone(
								sapFeedTmplFmBudgetData.getDocumentDate().getTime(), FAST_DATE_FORMAT) + ch,
						sapFeedTmplFmBudgetData.getYear(), ch + sapFeedTmplFmBudgetData.getBudgetType() + ch,
						ch + sapFeedTmplFmBudgetData.getFmGrant() + ch, ch + sapFeedTmplFmBudgetData.getFundCode() + ch,
						ch + sapFeedTmplFmBudgetData.getFundCenter() + ch,
						ch + sapFeedTmplFmBudgetData.getCommitmentItem() + ch,
						ch + sapFeedTmplFmBudgetData.getFunctionalArea() + ch,
						ch + sapFeedTmplFmBudgetData.getFundedProgram() + ch, sapFeedTmplFmBudgetData.getBudgetAmount(),
						ch + sapFeedTmplFmBudgetData.getLocalCurrency() + ch,
						ch + sapFeedTmplFmBudgetData.getDistributionKey() + ch,
						ch + sapFeedTmplFmBudgetData.getLineItemText() + ch, sapFeedTmplFmBudgetData.getBatchId(),
						ch + sapFeedTmplFmBudgetData.getCreateStatus() + ch);
				fileWriter.newLine();
				fileWriter.write(line);
			}
			fileWriter.close();
			newFileName = rename(file);
			vo.setSapFeedTmplFmBudgetDataCount(count(newFileName));
			saveSapAwardFeedBatchFiles(vo.getSapFeedTmplGrantBudMasterDataCount(), newFileName, batchId);
		} catch (Exception e) {
			logger.info("error in exportSapFeedTmplFmBudget {}", e.getMessage());
			createFastIntegrationLog("error in exportSapFeedTmplFmBudget" + e);
			e.printStackTrace();
			vo.getEmailContent().getError().append("Error in exportSapFeedTmplFmBudget : <br/> ").append(e).append("<br/>");
		} finally {
			try {
				fileWriter.close();
			} catch (IOException e) {
				vo.getEmailContent().getError().append("Error in exportSapFeedTmplFmBudget while closing the BufferedWriter : <br/> ").append(e).append("<br/>");
				e.printStackTrace();
			}
		}
		moveTemplateFile(new File(newFileName), vo.getEmailContent());
	}

	private void exportSapFeedTmplFundedDataToCSV(String fileName, List<SapFeedTmplFundedPrgm> sapFeedTmplFundedData, IntegrationReportVO vo, Integer batchId) throws IOException {
		String newFileName = null;
		BufferedWriter fileWriter = null;
		try {
			File file = createFile(fileName, batchId);
			String files = file.getAbsolutePath();
			fileWriter = new BufferedWriter(new FileWriter(files));
			char ch = '"';
			fileWriter.write("FUNDED_PROGRAM, PROGRAM_DESCRIPTION, FUNDED_PROGRAM_TYPE , BATCH_ID, CREATE_STATUS");
			for (SapFeedTmplFundedPrgm sapFeedTmplFundedPrgm : sapFeedTmplFundedData) {
				if( sapFeedTmplFundedPrgm.getProgramDescription() != null) {
					sapFeedTmplFundedPrgm.setProgramDescription(sapFeedTmplFundedPrgm.getProgramDescription().replaceAll("[\\r\\n]+", ""));
					sapFeedTmplFundedPrgm.setProgramDescription(sapFeedTmplFundedPrgm.getProgramDescription().replaceAll("\"", ""));
				}
				String line = String.format("%s,%s,%s,%d,%s", ch + sapFeedTmplFundedPrgm.getFundedProgram() + ch,
						ch + sapFeedTmplFundedPrgm.getProgramDescription() + ch,
						ch + sapFeedTmplFundedPrgm.getFundedProgramType() + ch, sapFeedTmplFundedPrgm.getBatchId(),
						ch + sapFeedTmplFundedPrgm.getCreateStatus() + ch);
				fileWriter.newLine();
				fileWriter.write(line);
			}
			fileWriter.close();
			newFileName = rename(file);
			// new SFTPHandler(host, port, username,
			// password).uploadFile(remotePath,localfilePath);
			vo.setSapFeedTmplFundedDataCount(count(newFileName));
			saveSapAwardFeedBatchFiles(vo.getSapFeedTmplFundedDataCount(), newFileName, batchId);
		} catch (Exception e) {
			logger.info("error in exportSapFeedTmplFundedDataToCSV {}", e.getMessage());
			createFastIntegrationLog("error in exportSapFeedTmplFundedDataToCSV" + e);
			e.printStackTrace();
			vo.getEmailContent().getError().append("Error in exportSapFeedTmplFundedDataToCSV :  <br/> ").append(e).append("<br/>");
		} finally {
			try {
				fileWriter.close();
			} catch (IOException e) {
				vo.getEmailContent().getError().append("Error in exportSapFeedTmplFundedDataToCSV while closing the BufferedWriter : <br/> ").append(e).append("<br/>");
				e.printStackTrace();
			}
		}
		moveTemplateFile(new File(newFileName), vo.getEmailContent());
	}

	private String rename(File file) throws IOException {
		String files = file.getAbsolutePath();
		String newfiles = files.substring(0, files.lastIndexOf("."));
		newfiles = newfiles + ".csv";
		String str = newfiles;
		if (file.renameTo(new File(str))) {
			return str;
		} else {
			return files;
		}
	}

	private void exportSapFeedTmplGrantBudMasterData(String fileName, List<SapFeedTmplGrantBudMaster> sapFeedTmplGrantBudMaster, IntegrationReportVO vo, Integer batchId) {
		String newFileName = null;
		BufferedWriter fileWriter = null;
		try {
			File file = createFile(fileName, batchId);
			String files = file.getAbsolutePath();
			char ch = '"';
			fileWriter = new BufferedWriter(new FileWriter(files));
			fileWriter.write("PROCESS, GRANT_CODE,GM_DOC_TYPE,HEADER_DESCRIPTION, FUND_CODE, "
					+ "SPONSOR_PROGRAM, SPONSOR_CLASS, BUDGET_AMOUNT, POSTING_DATE, DOCUMENT_DATE, "
					+ "BUDGET_VERSION, LINE_ITEM_TEXT, GRANT_CUR, COMMITMENT_ITEM, FUNDED_PROGRAM, FUNCTIONAL_AREA , FUND_CENTER, DISTR_KEY, YEAR, FM_AREA,"
					+ "BATCH_ID, CREATE_STATUS");
			for (SapFeedTmplGrantBudMaster sapFeedTmplGrantBudMasterData : sapFeedTmplGrantBudMaster) {
				if (sapFeedTmplGrantBudMasterData.getHeaderDescription() != null) {
					sapFeedTmplGrantBudMasterData.setHeaderDescription(sapFeedTmplGrantBudMasterData.getHeaderDescription().replaceAll("[\\r\\n]+", ""));
					sapFeedTmplGrantBudMasterData.setHeaderDescription(sapFeedTmplGrantBudMasterData.getHeaderDescription().replaceAll("\"", ""));
				} if (sapFeedTmplGrantBudMasterData.getLineItemText() != null) {
					sapFeedTmplGrantBudMasterData.setLineItemText(sapFeedTmplGrantBudMasterData.getLineItemText().replaceAll("[\\r\\n]+", ""));
					sapFeedTmplGrantBudMasterData.setLineItemText(sapFeedTmplGrantBudMasterData.getLineItemText().replaceAll("\"", ""));
				}
				String line = String.format("%s,%s,%s,%s,%s,%s,%s,%f,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%d,%s,%d,%s",
						ch + sapFeedTmplGrantBudMasterData.getProcess() + ch,
						ch + sapFeedTmplGrantBudMasterData.getGrantCode() + ch,
						ch + sapFeedTmplGrantBudMasterData.getGmDocType() + ch,
						ch + sapFeedTmplGrantBudMasterData.getHeaderDescription() + ch,
						ch + sapFeedTmplGrantBudMasterData.getFundCode() + ch,
						ch + sapFeedTmplGrantBudMasterData.getSponsorProgram() + ch,
						ch + sapFeedTmplGrantBudMasterData.getSponsorClass() + ch,
						sapFeedTmplGrantBudMasterData.getBudgetAmount(),
						ch + commonService.convertDateFormatBasedOnTimeZone(
								sapFeedTmplGrantBudMasterData.getPostingDate().getTime(), FAST_DATE_FORMAT) + ch,
						ch + commonService.convertDateFormatBasedOnTimeZone(
								sapFeedTmplGrantBudMasterData.getDocumentDate().getTime(), FAST_DATE_FORMAT) + ch,
						ch + sapFeedTmplGrantBudMasterData.getBudgetVersion() + ch,
						ch + sapFeedTmplGrantBudMasterData.getLineItemText() + ch,
						ch + sapFeedTmplGrantBudMasterData.getGrantCur() + ch,
						ch + sapFeedTmplGrantBudMasterData.getCommitmentItem() + ch,
						ch + sapFeedTmplGrantBudMasterData.getFundedProgram() + ch,
						ch + sapFeedTmplGrantBudMasterData.getFunctionalArea() + ch,
						ch + sapFeedTmplGrantBudMasterData.getFundCenter() + ch,
						ch + sapFeedTmplGrantBudMasterData.getDistrKey() + ch,
						sapFeedTmplGrantBudMasterData.getYear(),
						ch + sapFeedTmplGrantBudMasterData.getFmArea() + ch, sapFeedTmplGrantBudMasterData.getBatchId(),
						ch + sapFeedTmplGrantBudMasterData.getCreateStatus() + ch);
				fileWriter.newLine();
				fileWriter.write(line);
			}
			fileWriter.close();
			newFileName = rename(file);
			vo.setSapFeedTmplGrantBudMasterDataCount(count(newFileName));
			saveSapAwardFeedBatchFiles(vo.getSapFeedTmplGrantBudMasterDataCount(), newFileName, batchId);
		} catch (Exception e) {
			logger.info("error in exportSapFeedTmplGrantBudMasterData {}", e.getMessage());
			createFastIntegrationLog("error in exportSapFeedTmplGrantBudMasterData" + e);
			e.printStackTrace();
			vo.getEmailContent().getError().append("Error in exportSapFeedTmplGrantBudMasterData : <br/> ").append(e).append("<br/>");
		} finally {
			try {
				fileWriter.close();
			} catch (IOException e) {
				vo.getEmailContent().getError().append("Error in exportSapFeedTmplGrantBudMasterData while closing the BufferedWriter : <br/> ").append(e).append("<br/>");
				e.printStackTrace();
			}
		}
		moveTemplateFile(new File(newFileName), vo.getEmailContent());
	}

	private void exportSapFeedTmplGrantMasterData(String fileName, List<SapFeedTmplGrantMaster> sapFeedTmplGrantMaster, IntegrationReportVO vo, Integer batchId) {
		String newFileName  = null;
		BufferedWriter fileWriter = null;
		try {
			File file = createFile(fileName, batchId);
			String files = file.getAbsolutePath();
			char ch = '"';
			fileWriter = new BufferedWriter(new FileWriter(files));
			fileWriter.write("GRANT_CODE, COMPANY_CODE, GRANT_CURRENCY,  FUND_CODE ,SPONSOR_PROGRAM, SPONSOR_CLASS,"
					+ "BATCH_ID, CREATE_STATUS");
			for (SapFeedTmplGrantMaster sapFeedTmplGrantMasterData : sapFeedTmplGrantMaster) {
				String line = String.format("%s,%s,%s,%s,%s,%s,%d,%s",
						ch + sapFeedTmplGrantMasterData.getGrantCode() + ch,
						ch + sapFeedTmplGrantMasterData.getCompanyCode() + ch,
						ch + sapFeedTmplGrantMasterData.getGrantCurrency() + ch,
						ch + sapFeedTmplGrantMasterData.getFundCode() + ch,
						ch + sapFeedTmplGrantMasterData.getSponsorProgram() + ch,
						ch + sapFeedTmplGrantMasterData.getSponsorClass() + ch, 
						sapFeedTmplGrantMasterData.getBatchId(),
						ch + sapFeedTmplGrantMasterData.getCreateStatus() + ch);
				fileWriter.newLine();
				fileWriter.write(line);
			}
			fileWriter.close();
			newFileName = rename(file);
			vo.setSapFeedTmplGrantMasterDataCount(count(newFileName));
			saveSapAwardFeedBatchFiles(vo.getSapFeedTmplGrantMasterDataCount(), newFileName, batchId);
		} catch (Exception e) {
			logger.info("error in exportSapFeedTmplGrantMasterData {}", e.getMessage());
			createFastIntegrationLog("error in exportSapFeedTmplGrantMasterData" + e);
			e.printStackTrace();
			vo.getEmailContent().getError().append("Error in exportSapFeedTmplGrantMasterData :  <br/> ").append(e).append("<br/>");
		} finally {
			try {
				fileWriter.close();
			} catch (IOException e) {
				vo.getEmailContent().getError().append("Error in exportSapFeedTmplGrantMasterData while closing the BufferedWriter : <br/> ").append(e).append("<br/>");
				e.printStackTrace();
			}
		}
		moveTemplateFile(new File(newFileName), vo.getEmailContent());
	}

	private void exportSapFeedTmplProjectDefData(String fileName, List<SapFeedTmplProjectDef> sapFeedTmplProjectDef, IntegrationReportVO vo, Integer batchId) {
		String newFileName = null;
		BufferedWriter fileWriter = null;
		try {
			File file = createFile(fileName, batchId);
			String files = file.getAbsolutePath();
			char ch = '"';
			fileWriter = new BufferedWriter(new FileWriter(files));
			fileWriter.write("PROJECT_DEFINITION, "
					+ "PROJECT_PROFILE, SHORT_DESCRIPTION, NUM_OF_THE_RESPONSIBLE_PERSON, CONTROLLING_AREA,"
					+ "COMPANY_CODE, BUSINESS_AREA, PLANT, FUNCTIONAL_AREA,"
					+ "PROFIT_CENTER, PROJECT_CURRENCY, START_DATE, FINISH_DATE,"
					+ "FACTORY_CALENDER_KEY, BUDGET_PROFILE, PLANNING_PROFILE, AWARD_STATUS,"
					+ "BATCH_ID, CREATE_STATUS");
			for (SapFeedTmplProjectDef sapFeedTmplProjectDefData : sapFeedTmplProjectDef) {
				if (sapFeedTmplProjectDefData.getShortDescription() != null) {
					sapFeedTmplProjectDefData.setShortDescription(sapFeedTmplProjectDefData.getShortDescription().replaceAll("[\\r\\n]+", ""));
					sapFeedTmplProjectDefData.setShortDescription(sapFeedTmplProjectDefData.getShortDescription().replaceAll("\"", ""));
				} 
				String line = String.format("%s,%s,%s,%s" + ",%s,%s,%s,%s,%s,%s,%s,%s,%s" + ",%s,%s,%s,%s,%d,%s",
						ch + sapFeedTmplProjectDefData.getProjectDefinition() + ch,
						ch + sapFeedTmplProjectDefData.getProjectProfile() + ch,
						ch + sapFeedTmplProjectDefData.getShortDescription() + ch,
						ch + sapFeedTmplProjectDefData.getNumOfTheResponsiblePerson() + ch,
						ch + sapFeedTmplProjectDefData.getControllingArea() + ch,
						ch + sapFeedTmplProjectDefData.getCompanyCode() + ch,
						ch + sapFeedTmplProjectDefData.getBusinessArea() + ch,
						ch + sapFeedTmplProjectDefData.getPlant() + ch,
						ch + sapFeedTmplProjectDefData.getFunctionalArea() + ch,
						ch + sapFeedTmplProjectDefData.getProfitCenter() + ch,
						ch + sapFeedTmplProjectDefData.getProjectCurrency() + ch,
						ch + commonService.convertDateFormatBasedOnTimeZone(
								sapFeedTmplProjectDefData.getStartDate().getTime(), FAST_DATE_FORMAT) + ch,
						ch + commonService.convertDateFormatBasedOnTimeZone(
								sapFeedTmplProjectDefData.getFinishDate().getTime(), FAST_DATE_FORMAT) + ch,
						ch + sapFeedTmplProjectDefData.getFactoryCalenderKey() + ch,
						ch + sapFeedTmplProjectDefData.getBudgetrofile() + ch,
						ch + sapFeedTmplProjectDefData.getPlanningProfile() + ch,
						ch + sapFeedTmplProjectDefData.getAwardStatus() + ch, sapFeedTmplProjectDefData.getBatchId(),
						ch + sapFeedTmplProjectDefData.getCreateStatus() + ch);
				fileWriter.newLine();
				fileWriter.write(line);
			}
			fileWriter.close();
			newFileName = rename(file);
			vo.setSapFeedTmplProjectDefDataCount(count(newFileName));
			saveSapAwardFeedBatchFiles(vo.getSapFeedTmplProjectDefDataCount(), newFileName, batchId);
		} catch (Exception e) {
			logger.info("error in exportSapFeedTmplProjectDefData {}", e.getMessage());
			createFastIntegrationLog("error in exportSapFeedTmplProjectDefData" + e);
			e.printStackTrace();
			vo.getEmailContent().getError().append("Error in exportSapFeedTmplProjectDefData : <br/> ").append(e).append(" <br/>");
		} finally {
			try {
				fileWriter.close();
			} catch (IOException e) {
				vo.getEmailContent().getError().append("Error in exportSapFeedTmplProjectDefData while closing the BufferedWriter : <br/> ").append(e).append("<br/>");
				e.printStackTrace();
			}
		}
		moveTemplateFile(new File(newFileName), vo.getEmailContent());
	}

	private void exportSapFeedTmplSponsoPrgmData(String fileName, List<SapFeedTmplSponsoPrgm> sapFeedTmplSponsoPrgmData, IntegrationReportVO vo, Integer batchId) {
		String newFileName = null;
		BufferedWriter fileWriter = null;
		try {
		    File file = createFile(fileName, batchId);
			String files = file.getAbsolutePath();
			char ch = '"';
			fileWriter = new BufferedWriter(new FileWriter(files));
			fileWriter.write("SPONSOR_PROGRAM, PROGRAM_DESCRIPTION, BUSINESS_AREA, BATCH_ID, CREATE_STATUS");
			for (SapFeedTmplSponsoPrgm sapFeedTmplSponsoPrgm : sapFeedTmplSponsoPrgmData) {
				if (sapFeedTmplSponsoPrgm.getProgramDescription() != null) {
					sapFeedTmplSponsoPrgm.setProgramDescription(sapFeedTmplSponsoPrgm.getProgramDescription().replaceAll("[\\r\\n]+", ""));
					sapFeedTmplSponsoPrgm.setProgramDescription(sapFeedTmplSponsoPrgm.getProgramDescription().replaceAll("\"", ""));
				} 
				String line = String.format("%s,%s,%s,%d,%s", ch + sapFeedTmplSponsoPrgm.getSponsorProgram() + ch,
						ch + sapFeedTmplSponsoPrgm.getProgramDescription() + ch, ch + sapFeedTmplSponsoPrgm.getBusinessArea() + ch,
						sapFeedTmplSponsoPrgm.getBatchId(),	ch + sapFeedTmplSponsoPrgm.getCreateStatus() + ch);
				fileWriter.newLine();
				fileWriter.write(line);
			}
			fileWriter.close();
			newFileName = rename(file);
			vo.setSapFeedTmplSponsoPrgmDataCount(count(newFileName));
			saveSapAwardFeedBatchFiles(vo.getSapFeedTmplSponsoPrgmDataCount(), newFileName, batchId);
		} catch (Exception e) {
			logger.info("error in exportSapFeedTmplSponsoPrgmData {}", e.getMessage());
			createFastIntegrationLog("error in exportSapFeedTmplSponsoPrgmData" + e);
			e.printStackTrace();
			vo.getEmailContent().getError().append("Error in exportSapFeedTmplSponsoPrgmData : <br/> ").append(e).append("<br/>");
		} finally {
			try {
				fileWriter.close();
			} catch (IOException e) {
				vo.getEmailContent().getError().append("Error in exportSapFeedTmplSponsoPrgmData while closing the BufferedWriter : <br/> ").append(e).append("<br/>");
				e.printStackTrace();
			}
		}
		moveTemplateFile(new File(newFileName), vo.getEmailContent());
	}

	private void exportSapFeedTmplSponsorClassData(String fileName,
			List<SapFeedTmplSponsorClass> sapFeedTmplSponsorClassData, IntegrationReportVO vo,
			Integer batchId) {
		String newFileName = null;
		BufferedWriter fileWriter = null;
		try {
			File file = createFile(fileName, batchId);
			String files = file.getAbsolutePath();
			char ch = '"';
			fileWriter = new BufferedWriter(new FileWriter(files));
			fileWriter.write("SPONSOR_CLASS, CLASS_DESCRIPTION, CLASS_TYPE," + "BATCH_ID, CREATE_STATUS");
			for (SapFeedTmplSponsorClass sapFeedTmplSponsorClass : sapFeedTmplSponsorClassData) {
				if (sapFeedTmplSponsorClass.getClassDescription() != null) {
					sapFeedTmplSponsorClass.setClassDescription(sapFeedTmplSponsorClass.getClassDescription().replaceAll("[\\r\\n]+", ""));
					sapFeedTmplSponsorClass.setClassDescription(sapFeedTmplSponsorClass.getClassDescription().replaceAll("\"", ""));
				} 
				String line = String.format("%s,%s,%s,%d,%s", ch + sapFeedTmplSponsorClass.getSponsorClass() + ch,
						ch + sapFeedTmplSponsorClass.getClassDescription() + ch,
						ch + sapFeedTmplSponsorClass.getClassType() + ch, sapFeedTmplSponsorClass.getBatchId(),
						ch + sapFeedTmplSponsorClass.getCreateStatus() + ch);
				fileWriter.newLine();
				fileWriter.write(line);
			}
			fileWriter.close();
			newFileName = rename(file);
			vo.setSapFeedTmplSponsorClassDataCount(count(newFileName));
			saveSapAwardFeedBatchFiles(vo.getSapFeedTmplSponsorClassDataCount(), newFileName, batchId);
		} catch (Exception e) {
			logger.info("error in exportSapFeedTmplSponsorClassData {}", e.getMessage());
			createFastIntegrationLog("error in exportSapFeedTmplSponsorClassData" + e);
			e.printStackTrace();
			vo.getEmailContent().getError().append("Error in exportSapFeedTmplSponsorClassData : <br/> ").append(e).append("<br/>");
		} finally {
			try {
				fileWriter.close();
			} catch (IOException e) {
				vo.getEmailContent().getError().append("Error in exportSapFeedTmplSponsorClassData while closing the BufferedWriter : <br/> ").append(e).append("<br/>");
				e.printStackTrace();
			}
		}
		moveTemplateFile(new File(newFileName), vo.getEmailContent());
	}

	private void exportSapFeedTmplWbsData(String fileName, List<SapFeedTmplWbs> sapFeedTmplWbsData, IntegrationReportVO vo, Integer batchId) {
		String newFileName = null;
		BufferedWriter fileWriter = null;
		try {
			File file = createFile(fileName, batchId);
			String files = file.getAbsolutePath();
			char ch = '"';
			fileWriter = new BufferedWriter(new FileWriter(files));
			fileWriter.write("WBS_ELEMENT, PROJECT, WBS_LEVEL, "
					+ "WBS_ELEMENT_HIERARCHY, SHORT_DESCRIPTION, PERSON_RESPONSIBLE_NUMBER, "
					+ "COMPANY_CODE, BUSINESS_AREA, CONTROLLING_AREA, " + "PROFIT_CENTER, PROJECT_TYPE, PLANT, "
					+ "BASIC_START_DATE, BASIC_FINISH_DATE, KEYWORD_ID,"
					+ "RESPONSIBLE_COST_CENTER, ACCOUNT_ASSIGNMENT_ELEMENT, BILLING_ELEMENT,"
					+ "CURRENCY, OBJECT_CLASS, FACTORY_CALENDER, " + "FUND, PRNCP_INVESTGTR, GST_CLAIMABLE ,"
					+ "BATCH_ID,CREATE_STATUS");
			for (SapFeedTmplWbs sapFeedTmplWbs : sapFeedTmplWbsData) {
				if (sapFeedTmplWbs.getShortDescription() != null) {
					sapFeedTmplWbs.setShortDescription(sapFeedTmplWbs.getShortDescription().replaceAll("[\\r\\n]+", ""));
					sapFeedTmplWbs.setShortDescription(sapFeedTmplWbs.getShortDescription().replaceAll("\"", ""));
				} 
				String line = String.format(
						"%s,%s,%d,%s,%s,%s,%s" + ",%s,%s,%s,%s,%s,%s,%s,%s,%s," + "%s,%s,%s,%s,%s,%s,%s,%s,%d,%s",
						ch + sapFeedTmplWbs.getWbsElement() + ch, ch + sapFeedTmplWbs.getProject() + ch,
						sapFeedTmplWbs.getWbsLevel(), ch + sapFeedTmplWbs.getWbsElementHierarchy() + ch,
						ch + sapFeedTmplWbs.getShortDescription() + ch,
						ch + sapFeedTmplWbs.getPersonResponsibleNumber() + ch,
						ch + sapFeedTmplWbs.getCompanyCode() + ch, ch + sapFeedTmplWbs.getBusinessArea() + ch,
						ch + sapFeedTmplWbs.getControllingArea() + ch, ch + sapFeedTmplWbs.getProfitCenter() + ch,
						ch + sapFeedTmplWbs.getProjectType() + ch, ch + sapFeedTmplWbs.getPlant() + ch,
						ch + commonService.convertDateFormatBasedOnTimeZone(
								sapFeedTmplWbs.getBasicStartDate().getTime(), FAST_DATE_FORMAT) + ch,
						ch + commonService.convertDateFormatBasedOnTimeZone(
								sapFeedTmplWbs.getBasicFinishDate().getTime(), FAST_DATE_FORMAT) + ch,
						ch + sapFeedTmplWbs.getKeywordId() + ch, ch + sapFeedTmplWbs.getResponsibleCostCenter() + ch,
						ch + sapFeedTmplWbs.getAccountAssignmentElement() + ch,
						ch + sapFeedTmplWbs.getBillingElement() + ch, ch + sapFeedTmplWbs.getCurrency() + ch,
						ch + sapFeedTmplWbs.getObjectClass() + ch, ch + sapFeedTmplWbs.getFactoryCalender() + ch,
						ch + sapFeedTmplWbs.getFund() + ch, ch + sapFeedTmplWbs.getPrncpInvestgtr() + ch,
						ch + sapFeedTmplWbs.getGstClaimable() + ch, sapFeedTmplWbs.getBatchId(),
						ch + sapFeedTmplWbs.getCreateStatus() + ch);
				fileWriter.newLine();
				fileWriter.write(line);
			}
			fileWriter.close();
			newFileName = rename(file);
			vo.setSapFeedTmplWbsDataCount(count(newFileName));
			saveSapAwardFeedBatchFiles(vo.getSapFeedTmplWbsDataCount(), newFileName, batchId);
		} catch (Exception e) {
			logger.info("error in exportSapFeedTmplWbsData {}", e.getMessage());
			createFastIntegrationLog("error in exportSapFeedTmplWbsData" + e);
			e.printStackTrace();
			vo.getEmailContent().getError().append("Error in exportSapFeedTmplWbsData : <br/> ").append(e).append("<br/>");
		} finally {
			try {
				fileWriter.close();
			} catch (IOException e) {
				vo.getEmailContent().getError().append("Error in exportSapFeedTmplWbsData while closing the BufferedWriter : <br/> ").append(e).append("<br/>");
				e.printStackTrace();
			}
		}
		moveTemplateFile(new File(newFileName), vo.getEmailContent());
	}

	private Integer count(String filename) throws IOException {
		InputStream is = new BufferedInputStream(new FileInputStream(filename));
		try {
			byte[] c = new byte[1024];
			int count = 0;
			int readChars = 0;
			boolean empty = true;
			while ((readChars = is.read(c)) != -1) {
				empty = false;
				for (int i = 0; i <readChars; ++i) {
					if (c[i] == '\n') {
						++count;
					}
				}
			}
			return (count == 0 && !empty) ? 1 : --count;
		} finally {
			is.close();
		}
	}

	public void createFastIntegrationLog(String fileContent) {
		BufferedWriter fileOutputStream = null;
		try {
			String fileName = "fastLog";
			String date = convertDateFormatBasedOnTimeZone(commonDao.getCurrentTimestamp().getTime(),
					Constants.FAST_INTEGRATION_LOG_DATE_FORMAT);
			DateFormat dateFormat = new SimpleDateFormat("dd-MM-yyyy HH:mm:ss");
			Date currentDate = new Date(commonDao.getCurrentTimestamp().getTime());
			dateFormat.setTimeZone(TimeZone.getTimeZone(timezone));
			String fileNameWithPath = sftpConfigurationService.getSftpConfigurationValueAsString(Constants.FAST_LOG_FILE_PATH) + File.separator + fileName + "_" + date + ".log";
			File file = new File(fileNameWithPath);
			if (file.exists()) {
				fileOutputStream = new BufferedWriter(new FileWriter(fileNameWithPath, true));
			} else {
				fileOutputStream = new BufferedWriter(new FileWriter(fileNameWithPath));
			}
			fileContent = fileContent + " - " + dateFormat.format(currentDate);
			fileOutputStream.newLine();
			fileOutputStream.write(fileContent);
			fileOutputStream.close();
		} catch (Exception e) {
			logger.error("Exception in method createFastIntegrationLog : {} ", e.getMessage());
			e.printStackTrace();
		} finally {
			try {
				fileOutputStream.close();
			} catch (IOException e) {
				logger.error("Exception in method createFastIntegrationLog while close the stream : {}", e.getMessage());
				e.printStackTrace();
			}
		}
	}

	private String createdDateWithTime() {
		String date = convertDateFormatBasedOnTimeZone(commonDao.getCurrentTimestamp().getTime(),
				Constants.FAST_INTEGRATION_LOG_DATE_FORMAT);
		SimpleDateFormat time = new SimpleDateFormat("HHmmss");
		time.setTimeZone(TimeZone.getTimeZone(timezone));
		String createdTime = time.format(new Date());
		return date + createdTime;
	}

	public File createFile(String fileName, Integer batchId) {
		BufferedWriter fileOutputStream = null;
		try {
			String fileNameWithPath = new StringBuilder(sftpConfigurationService.getSftpConfigurationValueAsString(Constants.RISE_FAST_INBOUND)).append(File.separator).append(fileName).append( "_").append(batchId.toString()).append("_").append(createdDateWithTime()).append(".tmp").toString();
			File file = new File(fileNameWithPath);
			fileOutputStream = new BufferedWriter(new FileWriter(fileNameWithPath));
			fileOutputStream.close();
			return file;
		} catch (Exception e) {
			logger.error("Exception in method createFile : {} ", e);
			e.printStackTrace();
			return null;
		} finally {
			try {
				fileOutputStream.close();
			} catch (IOException e) {
				logger.error("Exception in method createFile while closing the stream : {} ", e);
				e.printStackTrace();
			}
		}
	}

	private String convertDateFormatBasedOnTimeZone(Long dateValue, String dateFormat) {
		Date date = new Date(dateValue);
		return new SimpleDateFormat(dateFormat).format(commonDao.adjustTimezone(date));
	}

	private void sendResponceMail(IntegrationReportVO vo, Integer batchId) {
		sendSuccesReportMail(vo, new StringBuilder("Integration status report for response Batch ID : ").append(batchId.toString())
				.append(" ").append(commonDao.getDateFromTimestampZoneFormat(Constants.CRON_JOB_TIMEZONE, Constants.LONG_DATE_FORMAT)).toString());
		sendErrorReportMail(vo, new StringBuilder("ERROR : Integration status report for response Batch ID : ").append(batchId.toString()).append(" ")
				.append(commonDao.getDateFromTimestampZoneFormat(Constants.CRON_JOB_TIMEZONE, Constants.LONG_DATE_FORMAT)).toString());
	}

	private void saveSapAwardFeedBatchFiles(Integer noOfRecords, String batchFileName, Integer batchId) {
		if (noOfRecords == null) {
			noOfRecords = 0;
		}
		SapAwardFeedBatchFiles sapAwardFeedBatchFiles = new SapAwardFeedBatchFiles();
		String fileName = new File(batchFileName).getName();
		sapAwardFeedBatchFiles.setBatchFileName(fileName);
		sapAwardFeedBatchFiles.setBatchId(batchId);
		sapAwardFeedBatchFiles.setNoOfRecords(noOfRecords);
		sapAwardFeedBatchFiles.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		sapAwardFeedBatchFiles.setUpdateUser("admin");
		fastIntegrationDao.saveOrUpdateAwardFeedBatchFiles(sapAwardFeedBatchFiles);
	}

	@Override
	public IntegrationReportVO fetchResponce() {
		IntegrationReportVO integrationReportVO = new IntegrationReportVO();
		integrationReportVO.setIsResponseMail(Boolean.TRUE);
		String batchId = null;
		String response = sftpConfigurationService.getSftpConfigurationValueAsString(Constants.RISE_FAST_OUTBOUND_RESPONSE);
		Set<Integer> batchIds = new HashSet<>();
		if (commonDao.getParameterValueAsBoolean(Constants.IS_SFTP_ENABLED)) {
			getSftpResDirectory(sftpConfigurationService.getSftpConfigurationValueAsString(Constants.SFTP_FAST_OUTBOUND_RESPONSES), response);
		}
		File directory = new File(response);
		if (directory.isDirectory()) {
			File[] fileData = directory.listFiles();
			if (fileData != null) {
				if (fileData.length >8) {
					sendMultipleFileNotification(fileData, "Response ");
				}
				for (File file : fileData) {
					if (file.isFile()) {
						String fileName = file.getName();
						String temp = fileName;
						temp = temp.replaceAll("_\\d+(?=\\.)", "");
						temp = temp.replaceAll("resp_", "").substring(2);
						temp = temp.replaceAll("rise_", "");
						Pattern value = Pattern.compile("\\d+");
						Matcher data = value.matcher(temp);
						data.find();
						batchId = data.group();
						batchIds.add(Integer.parseInt(batchId));
						integrationReportVO.setBatchIds(batchIds);
						readResponce(file, integrationReportVO, Integer.parseInt(batchId));
					}
				}
				if (batchId != null) {
					integrationReportVO.setBatchId(Integer.parseInt(batchId));
				}
			}
		}
		return integrationReportVO;
	}

	private void sentAwardActiveNotification(String awardIds) {
		if (awardIds != null) {
			for (String awardId : awardIds.split(",")) {
				if (awardId != null && !awardId.equals("")) {
					AwardVO awardVO = new AwardVO();
					awardVO.setAward(awardDao.getAwardDetailsById(Integer.parseInt(awardId)));
					businessRuleService.sendFinalApprovalNotification(awardVO);
				}
			}
		}
	}

	@Override
	public void getSftpResDirectory(String sftpWorkingDir, String baseDirectory) {
		String SFTPHOST = sftpConfigurationService.getSftpConfigurationValueAsString(Constants.SFTP_HOST);
		int SFTPPORT = Integer.parseInt(sftpConfigurationService.getSftpConfigurationValueAsString(Constants.SFTP_PORT));
		String SFTPUSER = sftpConfigurationService.getSftpConfigurationValueAsString(Constants.SFTP_USER);
		com.jcraft.jsch.Session session = null;
		Channel channel = null;
		ChannelSftp channelSftp = null;
		logger.info("preparing the host information for sftp.");
		try {
			JSch jsch = new JSch();
			session = jsch.getSession(SFTPUSER, SFTPHOST, SFTPPORT);
			session.setPassword(sftpConfigurationService.getSftpConfigurationValueAsString(Constants.SFTP_PASSWORD));
			java.util.Properties config = new java.util.Properties();
			config.put("StrictHostKeyChecking", "no");
			session.setConfig(config);
			session.connect();
			logger.info("Host connected.");
			channel = session.openChannel("sftp");
			channel.connect();
			logger.info("sftp channel opened and connected.");
			channelSftp = (ChannelSftp) channel;
			channelSftp.cd(sftpWorkingDir);
			List<String> list = new ArrayList<>();
			Vector<LsEntry> files = channelSftp.ls(sftpWorkingDir);
			Collections.sort(files, (o1, o2) -> o1.getFilename().compareTo(o2.getFilename()));
			for (LsEntry entry : files) {
				if (!entry.getFilename().equals(".") && !entry.getFilename().equals("..")) {	
					list.add(entry.getFilename());
					logger.info("copying {} from {} to {}", entry.getFilename(),sftpWorkingDir,baseDirectory);
					channelSftp.get(entry.getFilename(), baseDirectory);
				}
			}
			checkForTheCorretFiles(files, baseDirectory);
		} catch (Exception ex) {
			logger.info("Exception found while tranfer the response {}", ex.getMessage());
			ex.printStackTrace();
		} finally {
			channelSftp.exit();
			logger.info("sftp Channel exited.");
			channel.disconnect();
			logger.info("Channel disconnected.");
			session.disconnect();
			logger.info("Host Session disconnected.");
		}
	}

	private void checkForTheCorretFiles(Vector<LsEntry> files, String baseDirectory) {
		Stream<Path> walk = null;
		try {
	     List<String> sftpFiles = files.stream().map(LsEntry::getFilename).collect(Collectors.toList());
	      walk = Files.walk(Paths.get(baseDirectory));
	     List<String> loclFiles = walk.filter(Files::isRegularFile).map(x -> x.getFileName().toString()).collect(Collectors.toList());
	     @SuppressWarnings("unchecked")
	     List<String> loclFilee =  (List<String>)(CollectionUtils.subtract(sftpFiles, loclFiles));
	     loclFilee.remove(".");
	     loclFilee.remove("..");
	      if(sftpFiles.size() != loclFiles.size()) {
		      logger.info("files not moved from sftp to locals are : {}", loclFilee);
	      }
		} catch (IOException e) {
			e.printStackTrace();
			return;
		}finally {
			if (walk != null) {
			walk.close();
			}
		}
	}

	@Override
	public void updateSapAwardFeedDetail(Set<Integer> batchIds) {
		if (batchIds != null && !batchIds.isEmpty()) {
			for (Integer batchId : batchIds) {
				logger.info("Request for updateSapAwardFeedDetail : {}", batchId);
				updateSapAwardFeed(batchId);
			}
		}
	}

	@Override
	public void fastIntegrationResponseMail(IntegrationReportVO integrationReportVO) {
		logger.info("Request for fastIntegrationResponseMail");
		if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_FAST_INTEGRATION_EMAIL)) {
			Set<Integer> batchIds = integrationReportVO.getBatchIds();
			if (batchIds != null && !batchIds.isEmpty()) {
				for (Integer batchId : batchIds) {
					List<SapAwardFeed> sapAwardFeeds = fastIntegrationDao.getAllfeedId(batchId);
					List<Integer> sapAwardCount = fastIntegrationDao.getSapAwardCount(batchId);
					integrationReportVO.setEmailContent(setFileEmailBody(sapAwardFeeds, integrationReportVO.getEmailContent(), integrationReportVO.getIsResponseMail(), sapAwardCount));
					sendResponceMail(integrationReportVO, batchId);
				}
			}
		}
	}

	private void readResponce(File file, IntegrationReportVO integrationReportVO, Integer batchId) {
		try {
			String fileName = extractFileName(file);
			updateResponce(fileName, file, integrationReportVO.getEmailContent());
		} catch (Exception e) {
			logger.error("Exception in method readResponce  {} ", e.getMessage());
			e.printStackTrace();
			integrationReportVO.getEmailContent().getError().append("Error in readResponce").append(e).append("<br/>");
		}
	}

	private void updateSapAwardFeed(Integer batchId) {
		logger.info("Request for updateSapAwardFeed " + batchId);
		SapAwardFeed sapAwardfeed = new SapAwardFeed();
		List<SapAwardFeed> sapAwardFeedIds = fastIntegrationDao.getAllFeedAndNonFeeds(batchId);
		if (sapAwardFeedIds != null && !sapAwardFeedIds.isEmpty()) {
			for (SapAwardFeed sapAwardFeedDetail : sapAwardFeedIds) {
				if (!sapAwardFeedDetail.getFeedStatus().equals("N")) {
					boolean isFeedStatus = false;
					List<String> feedStatuses = fastIntegrationDao.fetchAllFeedStatus(sapAwardFeedDetail.getFeedId(), batchId);
					if (feedStatuses != null && !feedStatuses.isEmpty() && !feedStatuses.contains(null)) {
						sapAwardfeed.setFeedId(sapAwardFeedDetail.getFeedId());
						sapAwardfeed.setCreateTimestamp(commonDao.getCurrentTimestamp());
						if (feedStatuses.contains("E")) {
							isFeedStatus = true;
						}
						if (isFeedStatus) {
							sapAwardfeed.setFeedStatus("E");
							List<String> budgetFeedStatuses = fastIntegrationDao.fetchAllBudgetFeedStatus(sapAwardFeedDetail.getFeedId(), batchId);
							if (budgetFeedStatuses != null && !budgetFeedStatuses.isEmpty() && !budgetFeedStatuses.contains(null) && budgetFeedStatuses.contains("E")) {
								awardService.updateAwardBudgetStatusBasedOnBatchId(sapAwardFeedDetail.getBatchId(), sapAwardFeedDetail.getAwardNumber(), Constants.AWARD_BUDGET_STATUS_CODE_ERROR_IN_POSTING, Constants.AWARD_BUDGET_STATUS_CODE_ACTIVE);
							} else {
								awardService.updateAwardBudgetStatusBasedOnBatchId(sapAwardFeedDetail.getBatchId(), sapAwardFeedDetail.getAwardNumber(), Constants.AWARD_BUDGET_STATUS_CODE_POSTED, Constants.AWARD_BUDGET_STATUS_CODE_ACTIVE);
							}
							fastIntegrationDao.updateFeedStatusForError(sapAwardfeed);
						} else {
							sapAwardfeed.setFeedStatus("R");
							awardService.updateAwardBudgetStatusBasedOnBatchId(sapAwardFeedDetail.getBatchId(), sapAwardFeedDetail.getAwardNumber(), Constants.AWARD_BUDGET_STATUS_CODE_POSTED, Constants.AWARD_BUDGET_STATUS_CODE_ACTIVE);
							fastIntegrationDao.updateFeedStatus(sapAwardfeed);
						}
						fastIntegrationDao.updateSapAwardFeedBatch(batchId);
					}
				} else {
					awardService.updateAwardBudgetStatusBasedOnBatchId(sapAwardFeedDetail.getBatchId(), sapAwardFeedDetail.getAwardNumber(), Constants.AWARD_BUDGET_STATUS_CODE_POSTED, Constants.AWARD_BUDGET_STATUS_CODE_ACTIVE);
				}
			}
		}
	}

	private String extractFileName(File file) {
		String fileName = file.getName();
		String temp = fileName;
		temp = temp.replaceAll("_\\d+(?=\\.)", "");
		temp = temp.replaceAll("resp_", "");
//		temp = temp.replaceAll("rise_", "");
        temp = temp.replaceAll("_\\d+(?=\\.)", "");
        temp = temp.substring(0, temp.lastIndexOf("."));
        return temp;
    }

    private void updateResponce(String fileName, File file, EmailContent emailBody) {
        try {
            switch (fileName) {
                case "3_rise_funded_prgm":
                    updateFundedPrgm(file, emailBody);
                    break;
                case "7_rise_grant_bud_master":
                    updateGrantBudMaster(file, emailBody);
                    break;
                case "6_rise_grant_master":
                    updateGrantMaster(file, emailBody);
                    break;
                case "4_rise_project_def":
                    updateProjectDef(file, emailBody);
                    break;
                case "5_rise_wbs":
                    updateWbs(file, emailBody);
                    break;
                case "2_rise_sponsor_class":
                    updateSponsorClass(file, emailBody);
                    break;
                case "1_rise_sponsor_prgm":
                    updateSponsorPrgm(file, emailBody);
                    break;
                case "8_rise_fund_bud_master":
                    updateFmBudget(file, emailBody);
                    break;
                default:
                    break;
            }
        } catch (Exception e) {
            e.printStackTrace();
            emailBody.getError().append("Error in response <br/>").append(e).append("<br/>");
        }
    }

    private void updateFmBudget(File file, EmailContent emailBody) {
        BufferedReader fileReader = null;
        Boolean errorInProcess = Boolean.FALSE;
        try {
            final int FUNDED_PROGRAM = 5;
            final int BATCH_ID = 7;
            final int FEED_STATUS = 8;
            final int MESSAGE = 9;
            Set<String> fm_budget = new HashSet<>();
            List<SapFeedTmplFmBudget> sapFeedTmplFmBudgets = new ArrayList<>();
            List<SapFeedTmplFmBudget> dupSapFeedTmplFmBudgets = new ArrayList<>();
            fileReader = new BufferedReader(new FileReader(file.getAbsoluteFile()));
            fileReader.readLine();
            String line = "";
            String message = "";
            while ((line = fileReader.readLine()) != null) {
                String[] tokens = line.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)");
                if (tokens.length > 0) {
                    while (tokens.length < 4) {
                        String tempLine = fileReader.readLine();
                        if (tempLine == null) {
                            break;
                        }
                        line = line + tempLine;
                        tokens = line.split("\",\"", -1);
                    }
                    SapFeedTmplFmBudget sapFeedTmplFmBudget = new SapFeedTmplFmBudget(Integer.parseInt(tokens[BATCH_ID].replace("\"", "")), tokens[FEED_STATUS].replace("\"", ""), tokens[MESSAGE].replace("\"", ""), tokens[FUNDED_PROGRAM].replace("\"", ""));
                    sapFeedTmplFmBudgets.add(sapFeedTmplFmBudget);
                    fm_budget.add(sapFeedTmplFmBudget.getFundedProgram());
                }
            }
            for (String fmBudget : fm_budget) {
                message = "";
                for (SapFeedTmplFmBudget sapFeedTmplFmBudget : sapFeedTmplFmBudgets) {
                    if (sapFeedTmplFmBudget.getFundedProgram().equals(fmBudget)) {
                        message = message + " " + sapFeedTmplFmBudget.getErrorMessage();
                        sapFeedTmplFmBudget.setErrorMessage(message);
                        dupSapFeedTmplFmBudgets.add(sapFeedTmplFmBudget);
                    } else {
                        continue;
                    }
                }
            }
            for (SapFeedTmplFmBudget sapFeedTmplFmBudget : dupSapFeedTmplFmBudgets) {
                fastIntegrationDao.updateFmBudgets(sapFeedTmplFmBudget);
            }
            fileReader.close();
        } catch (Exception e) {
            logger.error("Error in method updateFmBudget", e);
            e.printStackTrace();
            errorInProcess = Boolean.TRUE;
            emailBody.getError().append("Error in updateFmBudget  <br/> ").append(e).append("<br/>")
                    .append("File ").append(file.getName()).append(" will be not moved to archive").append("<br>");
        } finally {
            try {
                fileReader.close();
            } catch (IOException e) {
                emailBody.getError().append("Error in updateFmBudget while closing BufferedWriter  <br/> ").append(e).append("<br/>")
                        .append("File ").append(file.getName()).append(" will be not moved to archive").append("<br>");
                errorInProcess = Boolean.TRUE;
                e.printStackTrace();
            }
        }
        moveFile(file, emailBody, errorInProcess);
    }

    @Override
    public List<SapFeedTmplFmBudget> getSapFeedTmplFmBudgetByBatchId(Integer batchId) {
        return fastIntegrationDao.getSapFeedTmplFmBudgetByBatchId(batchId);
    }

    private void updateWbs(File file, EmailContent emailBody) {
        BufferedReader fileReader = null;
        Boolean errorInProcess = Boolean.FALSE;
        try {
            final int WBS_ELEMENT = 0;
            final int BATCH_ID = 4;
            final int FEED_STATUS = 5;
            final int MESSAGE = 6;
            Set<String> wbs_element = new HashSet<>();
            List<SapFeedTmplWbs> sapFeedTmplWbses = new ArrayList<>();
            List<SapFeedTmplWbs> dupSapFeedTmplWbses = new ArrayList<>();
            fileReader = new BufferedReader(new FileReader(file.getAbsoluteFile()));
            fileReader.readLine();
            String line = "";
            String message = "";
            while ((line = fileReader.readLine()) != null) {
                String[] tokens = line.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)");
                if (tokens.length > 0) {
                    while (tokens.length < 4) {
                        String tempLine = fileReader.readLine();
                        if (tempLine == null) {
                            break;
                        }
                        line = line + tempLine;
                        tokens = line.split("\",\"", -1);
                    }
                    SapFeedTmplWbs sapFeedTmplWbs = new SapFeedTmplWbs(tokens[WBS_ELEMENT].replace("\"", ""),
                            Integer.parseInt(tokens[BATCH_ID].replace("\"", "")), tokens[FEED_STATUS].replace("\"", ""), tokens[MESSAGE].replace("\"", ""));
                    sapFeedTmplWbses.add(sapFeedTmplWbs);
                    wbs_element.add(sapFeedTmplWbs.getWbsElement());
                }
            }
            for (String wbsElement : wbs_element) {
                message = "";
                for (SapFeedTmplWbs sapFeedTmplWbs : sapFeedTmplWbses) {
                    if (sapFeedTmplWbs.getWbsElement().equals(wbsElement)) {
                        message = message + " " + sapFeedTmplWbs.getErrorMessage();
                        sapFeedTmplWbs.setErrorMessage(message);
                        dupSapFeedTmplWbses.add(sapFeedTmplWbs);
                    } else {
                        continue;
                    }
                }
            }
            for (SapFeedTmplWbs sapFeedTmplWbs : dupSapFeedTmplWbses) {
                fastIntegrationDao.updateWBS(sapFeedTmplWbs);
            }
            fileReader.close();
        } catch (Exception e) {
            logger.error("Error in method updateWbs", e);
            e.printStackTrace();
            errorInProcess = Boolean.TRUE;
            emailBody.getError().append("Error in updateWbs <br/>").append(e).append("<br/>")
                    .append("File ").append(file.getName()).append(" will be not moved to archive").append("<br>");
        } finally {
            try {
                fileReader.close();
            } catch (IOException e) {
                emailBody.getError().append("Error in updateWbs while closing BufferedReader <br/>").append(e).append("<br/>")
                        .append("File ").append(file.getName()).append(" will be not moved to archive").append("<br>");
                errorInProcess = Boolean.TRUE;
                e.printStackTrace();
            }
        }
        moveFile(file, emailBody, errorInProcess);
    }

    private void updateGrantBudMaster(File file, EmailContent emailBody) {
        BufferedReader fileReader = null;
        Boolean errorInProcess = Boolean.FALSE;
        try {
            final int SPONSOR_PROGRAM = 3;
            final int BATCH_ID = 9;
            final int FEED_STATUS = 10;
            final int MESSAGE = 11;
            final int GRANT_CODE = 1;
            final int FUND_CODE = 2;
            final int SPONSOR_CLASS = 4;
            final int PROCESS = 0;
            Set<String> grant_code = new HashSet<String>();
            Set<String> sponsor_class = new HashSet<String>();
            List<SapFeedTmplGrantBudMaster> sapFeedTmplGrantBudMasters = new ArrayList<>();
            List<SapFeedTmplGrantBudMaster> dupSapFeedTmplGrantBudMasters = new ArrayList<>();
            fileReader = new BufferedReader(new FileReader(file.getAbsoluteFile()));
            fileReader.readLine();
            String line = "";
            String message = "";
            while ((line = fileReader.readLine()) != null) {
                String[] tokens = line.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)");
                if (tokens.length > 0) {
                    while (tokens.length < 8) {
                        String tempLine = fileReader.readLine();
                        if (tempLine == null) {
                            break;
                        }
                        line = line + tempLine;
                        tokens = line.split("\",\"", -1);
                    }
                    SapFeedTmplGrantBudMaster sapFeedTmplGrantBudMaster = new SapFeedTmplGrantBudMaster(
                            tokens[SPONSOR_PROGRAM].replace("\"", ""), Integer.parseInt(tokens[BATCH_ID].replace("\"", "")),
                            tokens[FEED_STATUS].replace("\"", ""), tokens[MESSAGE].replace("\"", ""),
                            tokens[GRANT_CODE].replace("\"", ""), tokens[FUND_CODE].replace("\"", ""),
                            tokens[SPONSOR_CLASS].replace("\"", ""), tokens[PROCESS].replace("\"", ""));
                    sapFeedTmplGrantBudMasters.add(sapFeedTmplGrantBudMaster);
                    grant_code.add(sapFeedTmplGrantBudMaster.getGrantCode());
                    sponsor_class.add(sapFeedTmplGrantBudMaster.getSponsorClass());
                }
            }
            for (String grantCode : grant_code) {
                for (String sponsorClass : sponsor_class) {
                    message = "";
                    for (SapFeedTmplGrantBudMaster sapFeedTmplGrantBudMaster : sapFeedTmplGrantBudMasters) {
                        if (sapFeedTmplGrantBudMaster.getSponsorClass().equals(sponsorClass) && sapFeedTmplGrantBudMaster.getGrantCode().equals(grantCode)) {
                            message = message + " " + sapFeedTmplGrantBudMaster.getErrorMessage();
                            sapFeedTmplGrantBudMaster.setErrorMessage(message);
                            dupSapFeedTmplGrantBudMasters.add(sapFeedTmplGrantBudMaster);
                        } else {
                            continue;
                        }
                    }
                }
            }
            for (SapFeedTmplGrantBudMaster sapFeedTmplGrantBudMaster : dupSapFeedTmplGrantBudMasters) {
                fastIntegrationDao.updateGrantBudMaster(sapFeedTmplGrantBudMaster);
            }
            fileReader.close();
        } catch (Exception e) {
            logger.error("Error in method updateGrantBudMaster", e);
            e.printStackTrace();
            errorInProcess = Boolean.TRUE;
            emailBody.getError().append("Error in updateGrantBudMaster  <br/>").append(e).append("<br/>")
                    .append("File ").append(file.getName()).append(" will be not moved to archive").append("<br>");
        } finally {
            try {
                fileReader.close();
            } catch (IOException e) {
                emailBody.getError().append("Error in updateGrantBudMaster while closing BufferedReader <br/>").append(e).append("<br/>")
                        .append("File ").append(file.getName()).append(" will be not moved to archive").append("<br>");
                errorInProcess = Boolean.TRUE;
                e.printStackTrace();
            }
        }
        if (errorInProcess.equals(Boolean.FALSE)) {
            moveFile(file, emailBody);
        }
    }

    private void updateProjectDef(File file, EmailContent emailBody) {
        BufferedReader fileReader = null;
        Boolean errorInProcess = Boolean.FALSE;
        try {
            final int PROJECT_DEFINITION = 0;
            final int BATCH_ID = 5;
            final int FEED_STATUS = 6;
            final int MESSAGE = 7;
            Set<String> project_definition = new HashSet<>();
            List<SapFeedTmplProjectDef> sapFeedTmplProjectDefs = new ArrayList<>();
            List<SapFeedTmplProjectDef> dupSapFeedTmplProjectDef = new ArrayList<>();
            fileReader = new BufferedReader(new FileReader(file.getAbsoluteFile()));
            fileReader.readLine();
            String line = "";
            String message = "";
            while ((line = fileReader.readLine()) != null) {
                String[] tokens = line.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)");
                if (tokens.length > 0) {
                    while (tokens.length < 4) {
                        String tempLine = fileReader.readLine();
                        if (tempLine == null) {
                            break;
                        }
                        line = line + tempLine;
                        tokens = line.split("\",\"", -1);
                    }
                    SapFeedTmplProjectDef sapFeedTmplProjectDef = new SapFeedTmplProjectDef(tokens[PROJECT_DEFINITION].replace("\"", ""),
                            Integer.parseInt(tokens[BATCH_ID].replace("\"", "")), tokens[FEED_STATUS].replace("\"", ""), tokens[MESSAGE].replace("\"", ""));
                    sapFeedTmplProjectDefs.add(sapFeedTmplProjectDef);
                    project_definition.add(sapFeedTmplProjectDef.getProjectDefinition());
                }
            }
            for (String projectDefinition : project_definition) {
                message = "";
                for (SapFeedTmplProjectDef sapFeedTmplProjectDef : sapFeedTmplProjectDefs) {
                    if (sapFeedTmplProjectDef.getProjectDefinition().equals(projectDefinition)) {
                        message = message + " " + sapFeedTmplProjectDef.getErrorMessage();
                        sapFeedTmplProjectDef.setErrorMessage(message);
                        dupSapFeedTmplProjectDef.add(sapFeedTmplProjectDef);
                    } else {
                        continue;
                    }
                }
            }
            for (SapFeedTmplProjectDef sapFeedTmplProjectDef : dupSapFeedTmplProjectDef) {
                fastIntegrationDao.updateProjectDef(sapFeedTmplProjectDef);
            }
            fileReader.close();
        } catch (Exception e) {
            logger.error("Error in method updateProjectDef", e);
            e.printStackTrace();
            errorInProcess = Boolean.TRUE;
            emailBody.getError().append("Error in updateProjectDef <br/> " + e + "<br/>")
                    .append("File ").append(file.getName()).append(" will be not moved to archive").append("<br>");
        } finally {
            try {
                fileReader.close();
            } catch (IOException e) {
                emailBody.getError().append("Error in updateProjectDef while closing BufferedReader <br/> " + e + "<br/>")
                        .append("File ").append(file.getName()).append(" will be not moved to archive").append("<br>");
                errorInProcess = Boolean.TRUE;
                e.printStackTrace();
            }
        }
        moveFile(file, emailBody, errorInProcess);
    }

    private void updateGrantMaster(File file, EmailContent emailBody) {
        BufferedReader fileReader = null;
        Boolean errorInProcess = Boolean.FALSE;
        try {
            final int BATCH_ID = 2;
            final int FEED_STATUS = 3;
            final int MESSAGE = 4;
            final int GRANT_CODE = 0;
            final int COMPANY_CODE = 1;
            Set<String> grant_code = new HashSet<>();
            List<SapFeedTmplGrantMaster> sapFeedTmplGrantMasters = new ArrayList<>();
            List<SapFeedTmplGrantMaster> dupSapFeedTmplGrantMasters = new ArrayList<>();
            fileReader = new BufferedReader(new FileReader(file.getAbsoluteFile()));
            fileReader.readLine();
            String line = "";
            String message = "";
            while ((line = fileReader.readLine()) != null) {
                String[] tokens = line.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)");
                if (tokens.length > 0) {
                    while (tokens.length < 5) {
                        String tempLine = fileReader.readLine();
                        if (tempLine == null) {
                            break;
                        }
                        line = line + tempLine;
                        tokens = line.split("\",\"", -1);
                    }
                    SapFeedTmplGrantMaster sapFeedTmplGrantMaster = new SapFeedTmplGrantMaster(
                            Integer.parseInt(tokens[BATCH_ID].replace("\"", "")), tokens[FEED_STATUS].replace("\"", ""),
                            tokens[MESSAGE].replace("\"", ""), tokens[GRANT_CODE].replace("\"", ""), tokens[COMPANY_CODE].replace("\"", ""));
                    sapFeedTmplGrantMasters.add(sapFeedTmplGrantMaster);
                    grant_code.add(sapFeedTmplGrantMaster.getGrantCode());
                }
            }
            for (String grantCode : grant_code) {
                message = "";
                for (SapFeedTmplGrantMaster sapFeedTmplGrantMaster : sapFeedTmplGrantMasters) {
                    if (sapFeedTmplGrantMaster.getGrantCode().equals(grantCode)) {
                        message = message + " " + sapFeedTmplGrantMaster.getErrorMessage();
                        sapFeedTmplGrantMaster.setErrorMessage(message);
                        dupSapFeedTmplGrantMasters.add(sapFeedTmplGrantMaster);
                    } else {
                        continue;
                    }
                }
            }
            for (SapFeedTmplGrantMaster sapFeedTmplGrantMaster : dupSapFeedTmplGrantMasters) {
                fastIntegrationDao.updateGrantMaster(sapFeedTmplGrantMaster);
            }
            fileReader.close();
        } catch (Exception e) {
            logger.error("Error in method updateGrantMaster", e);
            e.printStackTrace();
            errorInProcess = Boolean.TRUE;
            emailBody.getError().append("Error in updateGrantMaster <br/> ").append(e).append("<br/>")
                    .append("File ").append(file.getName()).append(" will be not moved to archive").append("<br>");
        } finally {
            try {
                fileReader.close();
            } catch (IOException e) {
                emailBody.getError().append("Error in updateGrantMaster while closing BufferedReader <br/> ").append(e).append("<br/>")
                        .append("File ").append(file.getName()).append(" will be not moved to archive").append("<br>");
                e.printStackTrace();
                errorInProcess = Boolean.TRUE;
            }
        }
        moveFile(file, emailBody, errorInProcess);
    }

    private void updateFundedPrgm(File file, EmailContent emailBody) {
        BufferedReader fileReader = null;
        Boolean errorInProcess = Boolean.FALSE;
        try {
            final int FUNDED_PROGRAM = 0;
            final int BATCH_ID = 1;
            final int FEED_STATUS = 2;
            final int MESSAGE = 3;
            Set<String> funded_program = new HashSet<>();
            List<SapFeedTmplFundedPrgm> sapFeedTmplFundedPrgms = new ArrayList<>();
            List<SapFeedTmplFundedPrgm> dupSapFeedTmplFundedPrgms = new ArrayList<>();
            fileReader = new BufferedReader(new FileReader(file.getAbsoluteFile()));
            fileReader.readLine();
            String line = "";
            String message = "";
            while ((line = fileReader.readLine()) != null) {
                String[] tokens = line.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)");
                if (tokens.length > 0) {
                    while (tokens.length < 4) {
                        String tempLine = fileReader.readLine();
                        if (tempLine == null) {
                            break;
                        }
                        line = line + tempLine;
                        tokens = line.split("\",\"", -1);
                    }
                    SapFeedTmplFundedPrgm sapFeedTmplFundedPrgm = new SapFeedTmplFundedPrgm(tokens[FUNDED_PROGRAM].replace("\"", ""),
                            Integer.parseInt(tokens[BATCH_ID].replace("\"", "")), tokens[FEED_STATUS].replace("\"", ""), tokens[MESSAGE].replace("\"", ""));
                    sapFeedTmplFundedPrgms.add(sapFeedTmplFundedPrgm);
                    funded_program.add(sapFeedTmplFundedPrgm.getFundedProgram());
                }
            }
            for (String fundedProgram : funded_program) {
                message = "";
                for (SapFeedTmplFundedPrgm sapFeedTmplFundedPrgm : sapFeedTmplFundedPrgms) {
                    if (sapFeedTmplFundedPrgm.getFundedProgram().equals(fundedProgram)) {
                        message = message + " " + sapFeedTmplFundedPrgm.getErrorMessage();
                        sapFeedTmplFundedPrgm.setErrorMessage(message);
                        dupSapFeedTmplFundedPrgms.add(sapFeedTmplFundedPrgm);
                    } else {
                        continue;
                    }
                }
            }
            for (SapFeedTmplFundedPrgm sapFeedTmplFundedPrgm : dupSapFeedTmplFundedPrgms) {
                fastIntegrationDao.updateFundedPrgm(sapFeedTmplFundedPrgm);
            }
            fileReader.close();
        } catch (Exception e) {
            logger.error("Error in method updatefundedPrgm", e);
            e.printStackTrace();
            errorInProcess = Boolean.TRUE;
            emailBody.getError().append("Error in updateFundedPrgm <br/> ").append(e).append("<br/>")
                    .append("File ").append(file.getName()).append(" will be not moved to archive").append("<br>");
        } finally {
            try {
                fileReader.close();
            } catch (IOException e) {
                emailBody.getError().append("Error in updateFundedPrgm while closing BufferedReader <br/> ").append(e).append("<br/>")
                        .append("File ").append(file.getName()).append(" will be not moved to archive").append("<br>");
                errorInProcess = Boolean.TRUE;
                e.printStackTrace();
            }
        }
        moveFile(file, emailBody, errorInProcess);
    }

    private void updateSponsorClass(File file, EmailContent emailBody) {
        BufferedReader fileReader = null;
        Boolean errorInProcess = Boolean.FALSE;
        try {
            final int SPONSOR_CLASS = 0;
            final int BATCH_ID = 1;
            final int FEED_STATUS = 2;
            final int MESSAGE = 3;
            Set<String> sponsor_class = new HashSet<>();
            List<SapFeedTmplSponsorClass> sapFeedTmplSponsorClasses = new ArrayList<>();
            List<SapFeedTmplSponsorClass> dupSapFeedTmplSponsorClasses = new ArrayList<>();
            fileReader = new BufferedReader(new FileReader(file.getAbsoluteFile()));
            fileReader.readLine();
            String line = "";
            String message = "";
            while ((line = fileReader.readLine()) != null) {
                String[] tokens = line.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)");
                if (tokens.length > 0) {
                    while (tokens.length < 4) {
                        String tempLine = fileReader.readLine();
                        if (tempLine == null) {
                            break;
                        }
                        line = line + tempLine;
                        tokens = line.split("\",\"", -1);
                    }
                    SapFeedTmplSponsorClass sapFeedTmplSponsorClass = new SapFeedTmplSponsorClass(tokens[SPONSOR_CLASS].replace("\"", ""),
                            Integer.parseInt(tokens[BATCH_ID].replace("\"", "")),
                            tokens[FEED_STATUS].replace("\"", ""), tokens[MESSAGE].replace("\"", ""));
                    sapFeedTmplSponsorClasses.add(sapFeedTmplSponsorClass);
                    sponsor_class.add(sapFeedTmplSponsorClass.getSponsorClass());
                }
            }
            for (String sponsorClass : sponsor_class) {
                message = "";
                for (SapFeedTmplSponsorClass sapFeedTmplSponsorClass : sapFeedTmplSponsorClasses) {
                    if (sapFeedTmplSponsorClass.getSponsorClass().equals(sponsorClass)) {
                        message = message + " " + sapFeedTmplSponsorClass.getErrorMessage();
                        sapFeedTmplSponsorClass.setErrorMessage(message);
                        dupSapFeedTmplSponsorClasses.add(sapFeedTmplSponsorClass);
                    } else {
                        continue;
                    }
                }
            }
            for (SapFeedTmplSponsorClass sapFeedTmplSponsorClass : dupSapFeedTmplSponsorClasses) {
                fastIntegrationDao.updateSponsorClass(sapFeedTmplSponsorClass);
            }
            fileReader.close();
        } catch (Exception e) {
            logger.error("Error in method updateSponsorClass", e);
            e.printStackTrace();
            errorInProcess = Boolean.TRUE;
            emailBody.getError().append("Error in updateSponsorClass <br/>  ").append(e).append("<br/>")
                    .append("File ").append(file.getName()).append(" will be not moved to archive").append("<br>");
        } finally {
            try {
                fileReader.close();
            } catch (IOException e) {
                emailBody.getError().append("Error in updateSponsorClass while closing the BufferedReader <br/>  ").append(e).append("<br/>")
                        .append("File ").append(file.getName()).append(" will be not moved to archive").append("<br>");
                errorInProcess = Boolean.TRUE;
                e.printStackTrace();
            }
        }
        moveFile(file, emailBody, errorInProcess);
    }

    private void updateSponsorPrgm(File file, EmailContent emailBody) {
        BufferedReader fileReader = null;
        Boolean errorInProcess = Boolean.FALSE;
        try {
            final int SPONSOR_PROGRAM = 0;
            final int BATCH_ID = 1;
            final int FEED_STATUS = 2;
            final int MESSAGE = 3;
            Set<String> sponsor_program = new HashSet<>();
            List<SapFeedTmplSponsoPrgm> sapFeedTmplSponsoPrgms = new ArrayList<>();
            List<SapFeedTmplSponsoPrgm> dupSapFeedTmplSponsoPrgms = new ArrayList<>();
            fileReader = new BufferedReader(new FileReader(file.getAbsoluteFile()));
            fileReader.readLine();
            String line = "";
            String message = "";
            while ((line = fileReader.readLine()) != null) {
                String[] tokens = line.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)");
                if (tokens.length > 0) {
                    while (tokens.length < 4) {
                        String tempLine = fileReader.readLine();
                        if (tempLine == null) {
                            break;
                        }
                        line = line + tempLine;
                        tokens = line.split("\",\"", -1);
                    }
                    SapFeedTmplSponsoPrgm sapFeedTmplSponsoPrgm = new SapFeedTmplSponsoPrgm(tokens[SPONSOR_PROGRAM].replace("\"", ""),
                            Integer.parseInt(tokens[BATCH_ID].replace("\"", "")), tokens[FEED_STATUS].replace("\"", ""), tokens[MESSAGE].replace("\"", ""));
                    sapFeedTmplSponsoPrgms.add(sapFeedTmplSponsoPrgm);
                    sponsor_program.add(sapFeedTmplSponsoPrgm.getSponsorProgram());
                }
            }
            for (String sponsorProgram : sponsor_program) {
                message = "";
                for (SapFeedTmplSponsoPrgm sapFeedTmplSponsoPrgm : sapFeedTmplSponsoPrgms) {
                    if (sapFeedTmplSponsoPrgm.getSponsorProgram().equals(sponsorProgram)) {
                        message = message + " " + sapFeedTmplSponsoPrgm.getErrorMessage();
                        sapFeedTmplSponsoPrgm.setErrorMessage(message);
                        dupSapFeedTmplSponsoPrgms.add(sapFeedTmplSponsoPrgm);
                    } else {
                        continue;
                    }
                }
            }
            for (SapFeedTmplSponsoPrgm sapFeedTmplSponsoPrgm : dupSapFeedTmplSponsoPrgms) {
                fastIntegrationDao.updateSponsoPrgm(sapFeedTmplSponsoPrgm);
            }
            fileReader.close();
        } catch (Exception e) {
            logger.error("Error in method updateSponsorPrgm", e);
            e.printStackTrace();
            errorInProcess = Boolean.TRUE;
            emailBody.getError().append("Error in updateSponsorPrgm  <br/> ").append(e).append("<br/>")
                    .append("File ").append(file.getName()).append(" will be not moved to archive").append("<br>");
        } finally {
            try {
                fileReader.close();
            } catch (IOException e) {
                emailBody.getError().append("Error in updateSponsorPrgm while closing BufferedReader <br/> ").append(e).append("<br/>")
                        .append("File ").append(file.getName()).append(" will be not moved to archive").append("<br>");
                errorInProcess = Boolean.TRUE;
                e.printStackTrace();
            }
        }
        moveFile(file, emailBody, errorInProcess);
    }

    @Override
    public void moveExpenseFile(File file, EmailContent emailBody, Integer fileId) {
        try {
            Path temp = Files.move(Paths.get(file.getPath()), Paths.get(sftpConfigurationService.getSftpConfigurationValueAsString(Constants.RISE_FAST_OUTBOUND_ARCHIVE_EXPENSE) + "/" + file.getName()),
                    StandardCopyOption.REPLACE_EXISTING);
            if (temp != null) {
                fastIntegrationDao.updateStatusForFileInSystem(fileId, Constants.YES, "E");
                logger.info("file successfully moved from {} -> {}", file.getPath(), temp);
                if (commonDao.getParameterValueAsBoolean(Constants.IS_SFTP_ENABLED)) {
                    moveSftpDirectorys(sftpConfigurationService.getSftpConfigurationValueAsString(Constants.SFTP_FAST_OUTBOUND_EXPENSES), file.getName(), 
                    		sftpConfigurationService.getSftpConfigurationValueAsString(Constants.SFTP_FAST_OUTBOUND_EXPENSES_ARCHIVE), fileId, "");;
                }
            } else {
                logger.info("File not moved : {} ", file.getPath());
            }
        } catch (Exception e) {
            logger.error("Exception in method moveFile : {} ", e);
            e.printStackTrace();
            emailBody.getError().append("Exception in method moveFile <br/><br/> ").append(e).append("<br/>");
        }
    }

    @Override
    public void moveFile(File file, EmailContent emailBody) {
        try {
            Path temp = Files.move(Paths.get(file.getPath()), Paths.get(sftpConfigurationService.getSftpConfigurationValueAsString(Constants.RISE_FAST_OUTBOUND_ARCHIVE_RESPONSE) + "/" + file.getName()),
                    StandardCopyOption.REPLACE_EXISTING);
            if (temp != null) {
                logger.info("file successfully moved from {} -> {} ", file.getPath(), temp);
                if (commonDao.getParameterValueAsBoolean(Constants.IS_SFTP_ENABLED)) {
                    moveSftpDirectorys(sftpConfigurationService.getSftpConfigurationValueAsString(Constants.SFTP_FAST_OUTBOUND_RESPONSES), file.getName(), sftpConfigurationService.getSftpConfigurationValueAsString(Constants.SFTP_FAST_OUTBOUND_RESPONSE_ARCHIVE), null,"");
                }
            } else {
                logger.info("File not moved : {} ", file.getPath());
            }
        } catch (Exception e) {
            logger.error("Exception in method moveFile : {} ", e);
            e.printStackTrace();
            emailBody.getError().append("Exception in method moveFile <br/><br/> ").append(e).append("<br/>");
        }
    }

    private void moveTemplateFile(File file, EmailContent emailContent) {
        if (commonDao.getParameterValueAsBoolean(Constants.IS_SFTP_ENABLED)) {
            movetoSftp(file.getParent() + "/" + file.getName(), sftpConfigurationService.getSftpConfigurationValueAsString(Constants.SFTP_FAST_INBOUND), emailContent);
        }
    }

    public void moveSftpDirectorys(String sourceDir, String fileName, String destination, Integer fileId, String processingType ) throws Exception {
        String sftpHost = sftpConfigurationService.getSftpConfigurationValueAsString(Constants.SFTP_HOST);
        int sftpPort = Integer.parseInt(sftpConfigurationService.getSftpConfigurationValueAsString(Constants.SFTP_PORT));
        String sftpUser = sftpConfigurationService.getSftpConfigurationValueAsString(Constants.SFTP_USER);
        com.jcraft.jsch.Session session = null;
        Channel channel = null;
        ChannelSftp channelSftp = null;
        logger.info("preparing the host information for sftp.");
        try {
            JSch jsch = new JSch();
            session = jsch.getSession(sftpUser, sftpHost, sftpPort);
            session.setPassword(sftpConfigurationService.getSftpConfigurationValueAsString(Constants.SFTP_PASSWORD));
            java.util.Properties config = new java.util.Properties();
            config.put("StrictHostKeyChecking", "no");
            session.setConfig(config);
            session.connect();
            logger.info("Host connected.");
            channel = session.openChannel("sftp");
            channel.connect();
            logger.info("sftp channel opened and connected.");
            channelSftp = (ChannelSftp) channel;
            channelSftp.cd(sourceDir);
            List<String> list = new ArrayList<>();
            Vector<LsEntry> files = channelSftp.ls(sourceDir);
            for (LsEntry entry : files) {
                if (!entry.getFilename().equals(".") && !entry.getFilename().equals("..") && fileName.equals(entry.getFilename())) {
                    list.add(entry.getFilename());
                    String sourceFile = new StringBuilder(sourceDir).append(entry.getFilename()).toString();
                    String destinationFile = new StringBuilder(destination).append(entry.getFilename()).toString();
                    channelSftp.rename(sourceFile, destinationFile);
                    channelSftp.cd(destination);
                    if (checkFileInArchive(channelSftp, fileName)) {
                        if (fileId != null) {
                            if (processingType.equals("REVENUE_INVOICE"))
                                fastIntegrationDao.updateStatusForFileInRemote(fileId, Constants.YES,
                                        (sftpConfigurationService.getSftpConfigurationValueAsString(Constants.SFTP_REVENUE_INVOICE_OUTBOUND).equals(sourceDir) ? "R" : "E"));
                            else
                                fastIntegrationDao.updateStatusForFileInRemote(fileId, Constants.YES,
                                        (sftpConfigurationService.getSftpConfigurationValueAsString(Constants.SFTP_REVENUE_OUTBOUND).equals(sourceDir) ? "R" : "E"));
                        }
                        logger.info("file successfully moved from {} -> {} ", sourceFile, destinationFile);
                    }
                }
            }
        } catch (Exception ex) {
            logger.info("Exception found while tranfer the response.{}", ex);
            ex.printStackTrace();
            throw ex;
        } finally {
            channelSftp.exit();
            logger.info("sftp Channel exited.");
            channel.disconnect();
            logger.info("Channel disconnected.");
            session.disconnect();
            logger.info("Host Session disconnected.");
        }
    }

    private boolean checkFileInArchive(ChannelSftp channelSftp, String fileName) {
        try {
            channelSftp.lstat(fileName);
            return true;
        } catch (SftpException e) {
            logger.error("Error occured in checkFileInArchive due to : {}", e.getMessage());
            e.printStackTrace();
            return false;
        }
    }

    public void movetoSftp(String fileName, String SFTPWORKINGDIR, EmailContent emailContent) {
        String SFTPHOST = sftpConfigurationService.getSftpConfigurationValueAsString(Constants.SFTP_HOST);
        int SFTPPORT = Integer.parseInt(sftpConfigurationService.getSftpConfigurationValueAsString(Constants.SFTP_PORT));
        String SFTPUSER = sftpConfigurationService.getSftpConfigurationValueAsString(Constants.SFTP_USER);
        com.jcraft.jsch.Session session = null;
        Channel channel = null;
        ChannelSftp channelSftp = null;
        logger.info("preparing the host information for sftp.");
        try {
            JSch jsch = new JSch();
            session = jsch.getSession(SFTPUSER, SFTPHOST, SFTPPORT);
            session.setPassword(sftpConfigurationService.getSftpConfigurationValueAsString(Constants.SFTP_PASSWORD));
            java.util.Properties config = new java.util.Properties();
            config.put("StrictHostKeyChecking", "no");
            session.setConfig(config);
            session.connect();
            logger.info("Host connected.");
            channel = session.openChannel("sftp");
            channel.connect();
            logger.info("sftp channel opened and connected.");
            channelSftp = (ChannelSftp) channel;
            channelSftp.cd(SFTPWORKINGDIR);
            File f = new File(fileName);
            channelSftp.put(new FileInputStream(f), f.getName());
        } catch (Exception ex) {
            logger.error("Exception found while transfer the response ", ex);
            emailContent.getError().append("Exception found while transfer the response : <br/> ").append(ex).append("<br/>");
            ex.printStackTrace();
        } finally {
            channelSftp.exit();
            logger.info("sftp Channel exited.");
            channel.disconnect();
            logger.info("Channel disconnected.");
            session.disconnect();
            logger.info("Host Session disconnected.");
        }
    }

    @Override
    public IntegrationReportVO fastIntegrationExpenseTransactionRTProcessing(String fileName, BufferedReader fileReader, File file, IntegrationReportVO vo) {
        try {
            IntegrationReportVO integrationReportVO = expenseTrackingRTProcessing(fileName, fileReader, vo.getFileId(), vo.getTotalFileRowCount(), vo.getFileRowIndex());
            vo.setTotalFileRowCount(integrationReportVO.getTotalFileRowCount());
            vo.setFileRowIndex(integrationReportVO.getFileRowIndex());
            vo.setEmailContent(integrationReportVO.getEmailContent());
            vo.setErrorOccured(integrationReportVO.getErrorOccured());
        } catch (Exception e) {
            logger.error("Error in method fastIntegrationExpenseTransactionRTProcessing {}", e.getMessage());
            e.printStackTrace();
        }
        return vo;
    }

    @Override
    public void sendSuccesReportMail(IntegrationReportVO vo, String subject) {
        if (vo.getEmailContent().getSuccess().length() != 0) {
            EmailServiceVO emailServiceVO = new EmailServiceVO();
            setMailData(emailServiceVO);
            emailServiceVO.setBody(vo.getEmailContent().getSuccess().toString());
            emailServiceVO.setSubject(subject);
            emailService.sendEmail(emailServiceVO);
            if (vo.getEmailContent().getError().length() == 0) {
                vo.setEmailContent(vo.new EmailContent());
            }
        }
    }

    @Override
    public void sendErrorReportMail(IntegrationReportVO vo, String subject) {
        if (vo.getEmailContent().getError().length() != 0) {
            EmailServiceVO emailServiceVO = new EmailServiceVO();
            setMailData(emailServiceVO);
            emailServiceVO.setBody(vo.getEmailContent().getError().toString());
            emailServiceVO.setSubject(subject);
            emailService.sendEmail(emailServiceVO);
            vo.setEmailContent(vo.new EmailContent());
        }
    }

    private void setMailData(EmailServiceVO emailServiceVO) {
        emailServiceVO.setModuleCode(Constants.AWARD_MODULE_CODE);
        Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
        String emailAddress = commonDao.getParameterValueAsString(Constants.FAST_INTEGRATION_EMAIL_RECIPIENT);
        if (emailAddress != null && !emailAddress.isEmpty()) {
            String[] singleEmailAddress = emailAddress.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)");
            if (singleEmailAddress.length > 0) {
                for (String recipeientMailAddress : singleEmailAddress) {
                    commonService.setNotificationRecipientsforNonEmployees(recipeientMailAddress, Constants.NOTIFICATION_RECIPIENT_TYPE_TO,
                            dynamicEmailRecipients);
                }
            }
            emailServiceVO.setRecipients(dynamicEmailRecipients);
            emailServiceVO.setSubModuleCode(Constants.AWARD_SUBMODULE_CODE.toString());
        }
    }

    public IntegrationReportVO expenseTrackingRTProcessing(String fileName, BufferedReader fileReader, Integer fileId, Integer totalFileRowCount, Integer fileRowIndex) {
        IntegrationReportVO vo = new IntegrationReportVO();
        int fileRowCount = 0;
        int processed = (fileRowIndex > 0) ? fileRowIndex : 0;
        try {
            final int ACCOUNT_NUMBER = 0;
            final int INTERNAL_ORDER_CODE = 1;
            final int REMARKS = 2;
            final int AMOUNT_IN_FMA_CURRENCY = 3;
            final int ACTUAL_OR_COMMITTED_FLAG = 4;
            final int PO_PR_FLAG = 5;
            final int BANK_CLEARING_DATE = 6;
            final int FM_POSTING_DATE = 7;
            final int FI_POSTING_DATE = 8;
            final int VENDOR_CODE = 9;
            final int VENDOR_NAME = 10;
            final int DOCUMENT_DATE = 11;
            final int FI_GL_ACCOUNT = 12;
            final int FI_GL_DESCRIPTION = 13;
            final int GR_DATE = 14;
            final int INVOICE_DATE = 15;
            final int PR_DATE = 16;
            final int REQUESTER_NAME = 17;
            final int TRANSACTION_REFERENCE_NUMBER = 18;
            final int DOCUMENT_NUMBER = 19;
            final int ITEM_NUMBER = 20;
            final int REFERENCE_DOCUMENT_CATEGORY = 21;
            final int REFERENCE_ORG_UNIT = 22;
            final int ACCT_ASSIGNMENT_NUMBER = 23;
            final int SCHEDULE_LINE_NUMBER = 24;
            final int CONDITION_COUNTER = 25;
            final int REFERENCE_PROCEDURE = 26;
            final int DOCUMENT_NUMBER_FM_LINE_ITEM = 27;
            final int NUMBER_OF_POST_FM_LINE_ITEM = 28;
            final int COMPANY_CODE = 29;
            final int VALUE_TYPE = 30;
            final int AMOUNT_TYPE = 31;
            final int FISCAL_YEAR = 32;
            final int TRANSACTION_NUMBER = 33;
            final int PO_DATE = 34;
            final int BATCH_ID = 35;
            final int FM_AREA = 36;
            final int FUND = 37;
            final int FUND_CENTER = 38;
            final int FUNCTIONAL_AREA = 39;
            final int PREDECESSOR_DOC_NUMBER = 40;
            fileRowCount = count(fileName);
            String line = "";
            String result = "";
            int minLineNumber = fileRowIndex;
            int maxLineNumber = 100;
            int lineNumberToRead = maxLineNumber + minLineNumber;
            Pattern pattern = Pattern.compile("[^A-Za-z0-9]");
            boolean dateVale = false;
            Matcher match;
            while ((line = fileReader.readLine()) != null) {
                String[] tokens = line.split("\",\"", -1);
                if (tokens.length > 0) {
                    while (tokens.length < 41) {
                        minLineNumber++;
                        --totalFileRowCount;
                        String tempLine = fileReader.readLine();
                        if (tempLine == null) {
                            break;
                        }
                        line = line + tempLine;
                        tokens = line.split("\",\"", -1);
                    }
                    try {
                        result = getAmountInFmaCurrency(tokens[AMOUNT_IN_FMA_CURRENCY]);
                    } catch (Exception e) {
                        result = getAmountInFmaCurrency(tokens[AMOUNT_IN_FMA_CURRENCY]);
                    }
                    if (result == null) {
                        result = "0";
                    }
                    tokens[ACCOUNT_NUMBER] = tokens[ACCOUNT_NUMBER].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[INTERNAL_ORDER_CODE] = tokens[INTERNAL_ORDER_CODE].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[REMARKS] = tokens[REMARKS].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[ACTUAL_OR_COMMITTED_FLAG] = tokens[ACTUAL_OR_COMMITTED_FLAG].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[PO_PR_FLAG] = tokens[PO_PR_FLAG].replaceAll(REGULAR_EXPRESSION, "");
                    match = pattern.matcher(tokens[BANK_CLEARING_DATE]);
                    if (match != null) {
                        dateVale = match.find();
                        if (dateVale || tokens[BANK_CLEARING_DATE].equals(Constants.DECIMAL_FORMAT) || tokens[BANK_CLEARING_DATE].equals(" ") || tokens[BANK_CLEARING_DATE].isEmpty()) {
                            tokens[BANK_CLEARING_DATE] = null;
                        } else {
                            tokens[BANK_CLEARING_DATE] = tokens[BANK_CLEARING_DATE].replaceAll(REGULAR_EXPRESSION, "");
                        }
                    }
                    match = pattern.matcher(tokens[FM_POSTING_DATE]);
                    if (match != null) {
                        dateVale = match.find();
                        if (dateVale || tokens[FM_POSTING_DATE].equals(Constants.DECIMAL_FORMAT) || tokens[FM_POSTING_DATE].equals(" ") || tokens[FM_POSTING_DATE].isEmpty()) {
                            tokens[FM_POSTING_DATE] = null;
                        } else {
                            tokens[FM_POSTING_DATE] = tokens[FM_POSTING_DATE].replaceAll(REGULAR_EXPRESSION, "");
                        }
                    }
                    if (match != null) {
                        match = pattern.matcher(tokens[FI_POSTING_DATE]);
                        dateVale = match.find();
                        if (dateVale || tokens[FI_POSTING_DATE].equals(Constants.DECIMAL_FORMAT) || tokens[FI_POSTING_DATE].equals(" ") || tokens[FI_POSTING_DATE].isEmpty()) {
                            tokens[FI_POSTING_DATE] = null;
                        } else {
                            tokens[FI_POSTING_DATE] = tokens[FI_POSTING_DATE].replaceAll(REGULAR_EXPRESSION, "");
                        }
                    }
                    tokens[VENDOR_CODE] = tokens[VENDOR_CODE].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[VENDOR_NAME] = tokens[VENDOR_NAME].replaceAll(REGULAR_EXPRESSION, "");
                    match = pattern.matcher(tokens[DOCUMENT_DATE]);
                    if (match != null) {
                        dateVale = match.find();
                        if (dateVale || tokens[DOCUMENT_DATE].equals(Constants.DECIMAL_FORMAT) || tokens[DOCUMENT_DATE].equals(" ") || tokens[DOCUMENT_DATE].isEmpty()) {
                            tokens[DOCUMENT_DATE] = null;
                        } else {
                            tokens[DOCUMENT_DATE] = tokens[DOCUMENT_DATE].replaceAll(REGULAR_EXPRESSION, "");
                        }
                    }
                    tokens[FI_GL_ACCOUNT] = tokens[FI_GL_ACCOUNT].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[FI_GL_DESCRIPTION] = tokens[FI_GL_DESCRIPTION].replaceAll(REGULAR_EXPRESSION, "");
                    match = pattern.matcher(tokens[GR_DATE]);
                    if (match != null) {
                        dateVale = match.find();
                        if (dateVale || tokens[GR_DATE].equals(Constants.DECIMAL_FORMAT) || tokens[GR_DATE].equals(" ") || tokens[GR_DATE].isEmpty()) {
                            tokens[GR_DATE] = null;
                        } else {
                            tokens[GR_DATE] = tokens[GR_DATE].replaceAll(REGULAR_EXPRESSION, "");
                        }
                    }
                    match = pattern.matcher(tokens[INVOICE_DATE]);
                    if (match != null) {
                        dateVale = match.find();
                        if (dateVale || tokens[INVOICE_DATE].equals(Constants.DECIMAL_FORMAT) || tokens[INVOICE_DATE].equals(" ") || tokens[INVOICE_DATE].isEmpty()) {
                            tokens[INVOICE_DATE] = null;
                        } else {
                            tokens[INVOICE_DATE] = tokens[INVOICE_DATE].replaceAll(REGULAR_EXPRESSION, "");
                        }
                    }
                    match = pattern.matcher(tokens[PR_DATE]);
                    if (match != null) {
                        dateVale = match.find();
                        if (dateVale || tokens[PR_DATE].equals(Constants.DECIMAL_FORMAT) || tokens[PR_DATE].equals(" ") || tokens[PR_DATE].isEmpty()) {
                            tokens[PR_DATE] = null;
                        } else {
                            tokens[PR_DATE] = tokens[PR_DATE].replaceAll(REGULAR_EXPRESSION, "");
                        }
                    }
                    tokens[REQUESTER_NAME] = tokens[REQUESTER_NAME].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[TRANSACTION_REFERENCE_NUMBER] = tokens[TRANSACTION_REFERENCE_NUMBER].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[DOCUMENT_NUMBER] = tokens[DOCUMENT_NUMBER].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[ITEM_NUMBER] = tokens[ITEM_NUMBER].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[REFERENCE_DOCUMENT_CATEGORY] = tokens[REFERENCE_DOCUMENT_CATEGORY].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[REFERENCE_ORG_UNIT] = tokens[REFERENCE_ORG_UNIT].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[ACCT_ASSIGNMENT_NUMBER] = tokens[ACCT_ASSIGNMENT_NUMBER].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[SCHEDULE_LINE_NUMBER] = tokens[SCHEDULE_LINE_NUMBER].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[CONDITION_COUNTER] = tokens[CONDITION_COUNTER].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[REFERENCE_PROCEDURE] = tokens[REFERENCE_PROCEDURE].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[DOCUMENT_NUMBER_FM_LINE_ITEM] = tokens[DOCUMENT_NUMBER_FM_LINE_ITEM].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[NUMBER_OF_POST_FM_LINE_ITEM] = tokens[NUMBER_OF_POST_FM_LINE_ITEM].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[COMPANY_CODE] = tokens[COMPANY_CODE].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[VALUE_TYPE] = tokens[VALUE_TYPE].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[AMOUNT_TYPE] = tokens[AMOUNT_TYPE].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[FISCAL_YEAR] = tokens[FISCAL_YEAR].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[TRANSACTION_NUMBER] = tokens[TRANSACTION_NUMBER].replaceAll(REGULAR_EXPRESSION, "");
                    match = pattern.matcher(tokens[PO_DATE]);
                    if (match != null) {
                        dateVale = match.find();
                        if (dateVale || tokens[PO_DATE].equals(Constants.DECIMAL_FORMAT) || tokens[PO_DATE].equals(" ") || tokens[PO_DATE].isEmpty()) {
                            tokens[PO_DATE] = null;
                        } else {
                            tokens[PO_DATE] = tokens[PO_DATE].replaceAll(REGULAR_EXPRESSION, "");
                        }
                    }
                    tokens[BATCH_ID] = tokens[BATCH_ID].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[FM_AREA] = tokens[FM_AREA].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[FUND] = tokens[FUND].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[FUND_CENTER] = tokens[FUND_CENTER].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[FUNCTIONAL_AREA] = tokens[FUNCTIONAL_AREA].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[PREDECESSOR_DOC_NUMBER] = tokens[PREDECESSOR_DOC_NUMBER].replaceAll(REGULAR_EXPRESSION, "");
                    AwardExpenseTransactionsRT awardExpenseTransactionsRT = new AwardExpenseTransactionsRT(
                            tokens[ACCOUNT_NUMBER], tokens[INTERNAL_ORDER_CODE], tokens[REMARKS],
                            new BigDecimal(result), tokens[ACTUAL_OR_COMMITTED_FLAG], tokens[PO_PR_FLAG],
                            tokens[BANK_CLEARING_DATE], tokens[FM_POSTING_DATE], tokens[FI_POSTING_DATE],
                            tokens[VENDOR_CODE], tokens[VENDOR_NAME], tokens[DOCUMENT_DATE], tokens[FI_GL_ACCOUNT],
                            tokens[FI_GL_DESCRIPTION], tokens[GR_DATE], tokens[INVOICE_DATE], tokens[PR_DATE],
                            tokens[REQUESTER_NAME], tokens[TRANSACTION_REFERENCE_NUMBER], tokens[DOCUMENT_NUMBER],
                            tokens[ITEM_NUMBER], tokens[REFERENCE_DOCUMENT_CATEGORY], tokens[REFERENCE_ORG_UNIT],
                            tokens[ACCT_ASSIGNMENT_NUMBER], tokens[SCHEDULE_LINE_NUMBER], tokens[CONDITION_COUNTER],
                            tokens[REFERENCE_PROCEDURE], tokens[DOCUMENT_NUMBER_FM_LINE_ITEM], tokens[NUMBER_OF_POST_FM_LINE_ITEM],
                            tokens[COMPANY_CODE], tokens[VALUE_TYPE], tokens[AMOUNT_TYPE], tokens[FISCAL_YEAR], tokens[TRANSACTION_NUMBER], tokens[PO_DATE],
                            tokens[BATCH_ID], tokens[FM_AREA], tokens[FUND], tokens[FUND_CENTER], tokens[FUNCTIONAL_AREA], tokens[PREDECESSOR_DOC_NUMBER]);
                    saveExpenseTrackerData(vo.getEmailContent(), fileId, awardExpenseTransactionsRT);
                    processed++;
                }
                minLineNumber++;
                --totalFileRowCount;
                if (totalFileRowCount < 0 && minLineNumber == fileRowCount) {
                    totalFileRowCount = 0;
                }
                if (minLineNumber == lineNumberToRead) {
                    logger.info("Total row Inserted {} ", minLineNumber);
                    break;
                }
            }
            if (totalFileRowCount < 0) {
                totalFileRowCount = 0;
            }
            fastIntegrationDao.updateStatusForDataLoadingInRT(fileId, Constants.YES, "E", processed);
            vo.setTotalFileRowCount(totalFileRowCount);
            vo.setFileRowIndex(minLineNumber);
        } catch (Exception e) {
            logger.error("Error in method expenseTrackingRTProcessing", e);
            e.printStackTrace();
            vo.setErrorOccured(Boolean.TRUE);
            fastIntegrationDao.updateStatusForDataLoadingInRT(fileId, Constants.NO, "E", processed);
            vo.getEmailContent().getError().append("Error in expenseTrackingRTProcessing : <br/>").append(e).append("<br/>")
                    .append("Only ").append(processed).append(" are saved out of ").append(fileRowCount).append(" in file : ").append(fileName)
                    .append("<br>").append("File ").append(fileName).append(" will be not moved to archive").append("<br>");
        }
        return vo;
    }

    private void saveExpenseTrackerData(EmailContent emailBody, Integer fileId,
                                        AwardExpenseTransactionsRT awardExpenseTransactionsRT) {
        try {
            awardExpenseTransactionsRT.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
            awardExpenseTransactionsRT.setFileId(fileId);
            if (awardExpenseTransactionsRT.getAccountNumber() != null && !awardExpenseTransactionsRT.getAccountNumber().isEmpty()) {
                String accountNumber = awardExpenseTransactionsRT.getAccountNumber().substring(0, 15);
                Award award = awardDao.fetchAwardDetailsbyAccountNumber(accountNumber, Constants.AWARD_FINAL_STATUS_ACTIVE);
                if (award != null) {
                    awardExpenseTransactionsRT.setAwardNumber(award.getAwardNumber());
                }
            }
            fastIntegrationDao.saveExpenseTransactionRT(awardExpenseTransactionsRT);
        } catch (Exception e) {
            logger.error("Error in method saveExpenseTrackerData", e);
            e.printStackTrace();
            emailBody.getError().append("Error in saveExpenseTrackerData : <br/>").append(e).append("<br/>");
        }
    }

    private String getAmountInFmaCurrency(String amount) {
        String result = "";
        if (amount.startsWith("\""))
            amount = amount.substring(1, amount.length() - 1);
        String sign = amount.substring(amount.length() - 1);
        if (sign != null && sign.equals("-")) {
            result = amount.substring(0, amount.length() - 1);
            result = result.trim();
            result = sign + result;
        } else {
            result = amount.trim();
        }
        result = result.replace(",", "");

        return result;
    }

    private void exportAwardExpenseTransactionsRTDataToCSV(List<AwardExpenseTransactionsRT> awardExpenseTransactionRT,
                                                           StringBuilder emailBody, Integer fileId) {
        String awardExpenseTransactionsRT = "Expense_Tracker_Final";
        File file = createFile(awardExpenseTransactionsRT, fileId);
        String files = file.getAbsolutePath();
        char ch = '"';
        BufferedWriter fileWriter = null;
        try {
            fileWriter = new BufferedWriter(new FileWriter(files));
            fileWriter.write(
                    "ACCOUNT_NUMBER,INTERNAL_ORDER_CODE,REMARKS,AMOUNT_IN_FMA_CURRENCY ,ACTUAL_OR_COMMITTED_FLAG,PO_PR_FLAG,BANK_CLEARING_DATE, FM_POSTING_DATE,FI_POSTING_DATE,VENDOR_CODE,VENDOR_NAME, DOCUMENT_DATE, FI_GL_ACCOUNT, FI_GL_DESCRIPTION, GR_DATE, INVOICE_DATE, PR_DATE,REQUESTER_NAME,TRANSACTION_REFERENCE_NUMBER,DOCUMENT_NUMBER, PO_DATE,ERROR_STATUS,ERROR_MESSAGE");
            for (AwardExpenseTransactionsRT awardExpenseTransactionRTData : awardExpenseTransactionRT) {
                String line = String.format("%s,%s,%s,%f,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s,%s",
                        ch + awardExpenseTransactionRTData.getAccountNumber() + ch,
                        ch + awardExpenseTransactionRTData.getInternalOrderCode() + ch,
                        ch + awardExpenseTransactionRTData.getRemarks().replaceAll("^\"+|\"+$", "") + ch,
                        awardExpenseTransactionRTData.getAmountInFmaCurrency(),
                        ch + awardExpenseTransactionRTData.getActualOrCommittedFlag() + ch,
                        ch + awardExpenseTransactionRTData.getPoPrFlag() + ch,
                        ch + awardExpenseTransactionRTData.getBankClearingDate() + ch,
                        ch + awardExpenseTransactionRTData.getFmPostingDate() + ch,
                        ch + awardExpenseTransactionRTData.getFiPostingDate() + ch,
                        ch + awardExpenseTransactionRTData.getVendorCode() + ch,
                        ch + awardExpenseTransactionRTData.getVendorName() + ch,
                        ch + awardExpenseTransactionRTData.getDocumentDate() + ch,
                        ch + awardExpenseTransactionRTData.getFiGlAccount() + ch,
                        ch + awardExpenseTransactionRTData.getFiGlDescription().replaceAll("^\"+|\"+$", "") + ch,
                        ch + awardExpenseTransactionRTData.getGrDate() + ch,
                        ch + awardExpenseTransactionRTData.getInvoiceDate() + ch,
                        ch + awardExpenseTransactionRTData.getPrDate() + ch,
                        ch + awardExpenseTransactionRTData.getRequesterName() + ch,
                        ch + awardExpenseTransactionRTData.getTransactionReferenceNumber() + ch,
                        ch + awardExpenseTransactionRTData.getDocumentNumber() + ch,
                        ch + awardExpenseTransactionRTData.getPoDate() + ch,
                        ch + awardExpenseTransactionRTData.getErrorStatus() + ch,
                        ch + awardExpenseTransactionRTData.getErrorMessage() + ch);
                fileWriter.newLine();
                fileWriter.write(line);
            }
            fileWriter.close();
            rename(file);
        } catch (Exception e) {
            logger.error("Error in method exportAwardExpenseTransactionsRTDataToCSV", e);
            e.printStackTrace();
            emailBody.append("Error in exportAwardExpenseTransactionsRTDataToCSV : <br/>").append(e).append("<br/>");
        } finally {
            try {
                fileWriter.close();
            } catch (IOException e) {
                emailBody.append("Error in exportAwardExpenseTransactionsRTDataToCSV while closing BufferedWriter : <br/>").append(e).append("<br/>");
                e.printStackTrace();
            }
        }
    }

    private List<AwardExpenseTransactionsRT> getawardExpenseTransactionRTByFileId(Integer fileId) {
        return fastIntegrationDao.getawardExpenseTransactionRTByFileId(fileId);
    }

    @Override
    public void awardExpenseTrackerPrevalidation() {
        fastIntegrationDao.awardExpenseTrackerPrevalidation();
    }

    @Override
    public void awardExpenseTrackerRefresh(Integer fileId) {
        if (fileId != null) {
            fastIntegrationDao.awardExpenseTrackerRefresh(fileId);
        }
    }

    @Override
    public void awardExpenseTrackerSync(Integer fileId) {
        fastIntegrationDao.awardExpenseTrackerSync(fileId);
        fastIntegrationDao.syncExpenseDataSet();
    }

    @Override
    public XSSFWorkbook getXSSFWorkbookForExpenseTracker(CommonVO vo) {
        XSSFWorkbook workbook = new XSSFWorkbook();
        XSSFSheet sheet = workbook.createSheet("Report");
        commonService.addDetailsInHeader(workbook, sheet);
        List<Object[]> reportData = fastIntegrationDao.downloadExpenseTrackerReport();
        Object[] tableHeadingRow = {"Unit Number", "Unit Name", "Campus", "ACCOUNT_NUMBER L1 WBS", "L2_WBS Number", "REMARKS", "AMOUNT",
                "ACTUAL_OR_COMMITTED_FLAG", "PO_PR_FLAG", "BANK_CLEARING_DATE", "FM_POSTING_DATE", "FI_POSTING_DATE"
                , "VENDOR_CODE", "VENDOR_NAME", "DOCUMENT_DATE", "FI_GL_ACCOUNT", "FI_GL_DESCRIPTION", "GR_DATE", "INVOICE_DATE",
                "PR DATE", "REQUESTOR_NAME", "Transaction Reference Number", "Document Number", "PO Date", "ITEM_NUMBER", "REFERENCE_DOCUMENT_CATEGORY", "REFERENCE_ORG_UNIT", "ACCT_ASSIGNMENT_NUMBER",
                "SCHEDULE_LINE_NUMBER", "CONDITION_COUNTER", "REFERENCE_PROCEDURE", "DOCUMENT_NUMBER_FM_LINE_ITEM", "NUMBER_OF_POST_FM_LINE_ITEM", "COMPANY_CODE",
                "VALUE_TYPE", "AMOUNT_TYPE", "FISCAL_YEAR", "BATCH_ID", "FM_AREA", "FUND", "FUND_CENTER", "FUNCTIONAL_AREA"};
        dashboardService.prepareExcelSheet(reportData, sheet, tableHeadingRow, workbook, vo);
        return workbook;
    }

    @Override
    public XSSFWorkbook exportLevelTwoExpenseTransactionReport(CommonVO vo) {
        XSSFWorkbook workbook = new XSSFWorkbook();
        XSSFSheet sheet = workbook.createSheet(vo.getDocumentHeading());
        commonService.addDetailsInHeader(workbook,sheet);
        List<Object[]> reportData = fastIntegrationDao.downloadExpenseTrackerReportLevel2();
        Object[] tableHeadingRow = {
                "Unit Number", "Unit Name", "Campus", "ACCOUNT_NUMBER L1 WBS", "L2_WBS Number", "Budgeted Amount", "Cumulative Expense ", "Balance",
                "Cumulative Committed Amount", "Balanceless committed Amount"};
        dashboardService.prepareExcelSheet(reportData, sheet, tableHeadingRow, workbook, vo);
        return workbook;
    }

    @Override
    public XSSFWorkbook exportLevelOneExpenseTransactionReport(CommonVO vo) {
        XSSFWorkbook workbook = new XSSFWorkbook();
        XSSFSheet sheet = workbook.createSheet(vo.getDocumentHeading());
        commonService.addDetailsInHeader(workbook, sheet);
        List<Object[]> reportData = fastIntegrationDao.downloadExpenseTrackerRepoertLevelOne();
        Object[] tableHeadingRow = {
                "Unit Number", "Unit Name", "Campus", "ACCOUNT_NUMBER L1 WBS", "Budgeted Amount", "Total Expense ", "Total Balance",
                "Total Committed Amount", "Total Balanceless committed Amount"};
        dashboardService.prepareExcelSheet(reportData, sheet, tableHeadingRow, workbook, vo);
        return workbook;
    }

    @Override
    public IntegrationReportVO saveAwardExpenseFiles(File file) {
        try {
            String fileName = null;
            IntegrationReportVO vo = new IntegrationReportVO();
            fileName = file.toString();
            AwardExpenseFile awardExpenseFiles = new AwardExpenseFile();
            awardExpenseFiles.setFileName(file.getName());
            awardExpenseFiles.setNoOfRecords(count(fileName));
            awardExpenseFiles.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
            awardExpenseFiles.setInsertedInRT(Constants.NO);
            awardExpenseFiles.setSystemArchived(Constants.NO);
            awardExpenseFiles.setRemoteArchived(Constants.NO);
            awardExpenseFiles = fastIntegrationDao.saveAwardExpenseFiles(awardExpenseFiles);
            vo.setFileId(awardExpenseFiles.getFileId());
            vo.setTotalFileRowCount(count(fileName));
            vo.setFileName(fileName);
            vo.setFileRowIndex(0);
            fastIntegrationDao.deleteAllExpenseTransactionRT();
            return vo;
        } catch (Exception e) {
            logger.error("Error in method saveAwardExpenseFiles {}", e.getMessage());
            e.printStackTrace();
            return null;
        }

    }

    private Set<String> getawardAwardNumbersByFileId(Integer fileId) {
        return fastIntegrationDao.getawardAwardNumbersByFileId(fileId);
    }

    private void generateReportForFastIntegration(Integer batchId) {
        fastIntegrationDao.sapFeedReport(batchId);
        fastIntegrationDao.sapFeedBudgetReport(batchId);
        fastIntegrationDao.sapFeedSoftLaunchReport(batchId);
    }

    private List<String> getBusinessAreaCodes() {
        List<String> businessAreas = new ArrayList<>();
        businessAreas.add(Constants.NIE_BUSINESS_AREA_CODE);
        businessAreas.add(Constants.LKC_BUSINESS_AREA_CODE);
        businessAreas.add(Constants.RSIS_BUSINESS_AREA_CODE);
        return businessAreas;
    }

    @Override
    public XSSFWorkbook exportSapFeedReport(CommonVO vo) {
        XSSFWorkbook workbook = new XSSFWorkbook();
        vo.setDocumentHeading("SAP Feed Report");
        exportSapFeedReportByBatchId(vo, workbook, getBusinessAreaCodes());
        vo.setDocumentHeading("SAP Feed Budget Report");
        exportSapFeedBudgetReportByBatchId(vo, workbook, getBusinessAreaCodes());
        vo.setDocumentHeading("Data Not Feeded");
        exportDataNotFeededByParams(vo, workbook, getBusinessAreaCodes());
        return workbook;
    }

    private XSSFWorkbook exportSapFeedReportByBatchId(CommonVO vo, XSSFWorkbook workbook, List<String> businessAreas) {
        Integer batchId = vo.getProperty1() == null ? null : Integer.parseInt(vo.getProperty1());
        for (String businessArea : businessAreas) {
            XSSFSheet sheet = workbook.createSheet(vo.getDocumentHeading() + " for " + businessArea);
            commonService.addDetailsInHeader(workbook, sheet);
            List<Object[]> sapReport = fastIntegrationDao.sapReport(batchId, businessArea);
            Object[] tableHeadingRow = {"Batch Id", "Award Number", "Account Number",
                    "Award Version", "Variation Type", "Account Type", "Title", "Current Start Date", "Previous Start Date", "Current End Date",
                    "Previous End Date", "L2 WBS Number", "Cost Element Description", "Fund Code", "Current Principal Investigator", "Previous Principal Investigator", "L1 WBS Description",
                    "L2 WBS Description", "Business Area"};
            dashboardService.prepareExcelSheet(sapReport, sheet, tableHeadingRow, workbook, vo);
        }
        exportSapFeedReportForBusinessAreaNTU(batchId, workbook, vo);
        return workbook;
    }

    private XSSFWorkbook exportSapFeedReportForBusinessAreaNTU(Integer batchId, XSSFWorkbook workbook, CommonVO vo) {
        XSSFSheet sheet = workbook.createSheet(vo.getDocumentHeading() + " for others");
        commonService.addDetailsInHeader(workbook, sheet);
        List<Object[]> sapReport = fastIntegrationDao.sapReport(batchId, null);
        Object[] tableHeadingRow = {"Batch Id", "Award Number", "Account Number",
                "Award Version", "Variation Type", "Account Type", "Title", "Current Start Date", "Previous Start Date", "Current End Date",
                "Previous End Date", "L2 WBS Number", "Cost Element Description", "Fund Code", "Current Principal Investigator", "Previous Principal Investigator", "L1 WBS Description",
                "L2 WBS Description", "Business Area"};
        dashboardService.prepareExcelSheet(sapReport, sheet, tableHeadingRow, workbook, vo);
        return workbook;
    }

    private XSSFWorkbook exportSapFeedBudgetReportByBatchId(CommonVO vo, XSSFWorkbook workbook,
                                                            List<String> businessAreas) {
        Integer batchId = vo.getProperty1() == null ? null : Integer.parseInt(vo.getProperty1());
        for (String businessArea : businessAreas) {
            XSSFSheet sheet = workbook.createSheet(vo.getDocumentHeading() + " for " + businessArea);
            commonService.addDetailsInHeader(workbook, sheet);
            List<Object[]> sapReport = fastIntegrationDao.exportSapFeedBudgetReport(batchId, businessArea);
            Object[] tableHeadingRow = {"Batch Id", "Award Number", "Account Number", "Award Version",
                    "Variation Type", "Account Type", "L2 WBS Number", "Fund Code", "Process", "Amount in Feed",
                    "Current Line Item Cost", "Previous Line Item Cost", "Bussiness Area"};
            dashboardService.prepareExcelSheet(sapReport, sheet, tableHeadingRow, workbook, vo);
        }
        exportSapFeedBudgetReportForBusinessAreaNTU(batchId, workbook, vo);
        return workbook;
    }

    private void exportSapFeedBudgetReportForBusinessAreaNTU(Integer batchId, XSSFWorkbook workbook,
                                                             CommonVO vo) {
        XSSFSheet sheet = workbook.createSheet(vo.getDocumentHeading() + " for NTU");
        commonService.addDetailsInHeader(workbook, sheet);
        List<Object[]> sapReport = fastIntegrationDao.exportSapFeedBudgetReport(batchId, null);
        Object[] tableHeadingRow = {"Batch Id", "Award Number", "Account Number", "Award Version", "Variation Type", "Account Type", "L2 WBS Number", "Fund Code", "Process", "Amount in Feed", "Current Line Item Cost", "Previous Line Item Cost", "Bussiness Area"};
        dashboardService.prepareExcelSheet(sapReport, sheet, tableHeadingRow, workbook, vo);
    }

    private XSSFWorkbook exportDataNotFeededByParams(CommonVO vo, XSSFWorkbook workbook, List<String> businessAreas) {
        Integer batchId = vo.getProperty1() == null ? null : Integer.parseInt(vo.getProperty1());
        for (String businessArea : businessAreas) {
            XSSFSheet sheet = workbook.createSheet(vo.getDocumentHeading() + "for" + businessArea);
            commonService.addDetailsInHeader(workbook, sheet);
            List<Object[]> sapReport = fastIntegrationDao.exportDataNotFeeded(batchId, businessArea);
            Object[] tableHeadingRow = {"Award Number", "Account Number", "Account Type", "Lead Unit Number", "Lead Unit Name",
                    "Award Title", "Principal Investigator", "Sponsor Code", "Sponsor Name", "Sponsor Award Number", "Start Date",
                    "End date", "Variation Type", "Variation Subject", "System Comment", "Update Timestamp", "Submission Date", "Business Area"};
            dashboardService.prepareExcelSheet(sapReport, sheet, tableHeadingRow, workbook, vo);
        }
        exportSapDataNoFeedReportByBatchId(batchId, workbook, vo);
        return workbook;
    }

    private void exportSapDataNoFeedReportByBatchId(Integer batchId, XSSFWorkbook workbook, CommonVO vo) {
        XSSFSheet sheet = workbook.createSheet(vo.getDocumentHeading() + " for NTU");
        commonService.addDetailsInHeader(workbook, sheet);
        List<Object[]> sapReport = fastIntegrationDao.exportDataNotFeeded(batchId, null);
        Object[] tableHeadingRow = {"Award Number", "Account Number", "Account Type", "Lead Unit Number", "Lead Unit Name",
                "Award Title", "Principal Investigator", "Sponsor Code", "Sponsor Name", "Sponsor Award Number", "Start Date",
                "End date", "Variation Type", "Variation Subject", "System Comment", "Update Timestamp", "Submission Date", "Business Area"};
        dashboardService.prepareExcelSheet(sapReport, sheet, tableHeadingRow, workbook, vo);
    }

    @Override
    public void sapAwardUpdateHoldStatus() {
        if (commonDao.getParameterValueAsBoolean(Constants.ENABLE_FAST_INTEGRATION_EMAIL)) {
            String awardIds = fastIntegrationDao.sapAwardUpdateHoldStatus();
            sentAwardActiveNotification(awardIds);
        }
    }

    @Override
    public IntegrationReportVO fastIntegrationRevenueTransactionRTProcessing(String fileName, BufferedReader fileReader,
                                                                             File file, IntegrationReportVO vo, String processingType) {
        try {
            IntegrationReportVO integrationReportVO;
            if(processingType.equals("REVENUE_INVOICE"))
                integrationReportVO = revenueTransactionRTProcessingInvoice(fileName, fileReader, vo.getFileId(), vo.getTotalFileRowCount(), vo.getFileRowIndex(), processingType);
            else
                integrationReportVO = revenueTransactionRTProcessing(fileName, fileReader, vo.getFileId(), vo.getTotalFileRowCount(), vo.getFileRowIndex(), processingType);
            vo.setTotalFileRowCount(integrationReportVO.getTotalFileRowCount());
            vo.setFileRowIndex(integrationReportVO.getFileRowIndex());
            vo.setEmailContent(integrationReportVO.getEmailContent());
            vo.setErrorOccured(integrationReportVO.getErrorOccured());
        } catch (Exception e) {
            logger.error("Error in method fastIntegrationRevenueTransactionRTProcessing {} for " + processingType, e.getMessage());
            vo.getEmailContent().getError().append("Error in method fastIntegrationRevenueTransactionRTProcessing for ").append(processingType).append(e).append("<br/>");
            e.printStackTrace();
            return vo;
        }
        return vo;
    }

    private Set<String> getRevenueAwardNumbersByFileId(Integer fileId) {
        return fastIntegrationDao.getRevenueAwardNumbersByFileId(fileId);
    }

    private EmailContent setAwardRevenueEmailBody(Set<String> accountNumbers, EmailContent emailContent, String processingType) {
        if(processingType.equals("REVENUE_INVOICE"))
            revenueArchive = sftpConfigurationService.getSftpConfigurationValueAsString(Constants.SFTP_REVENUE_INVOICE_OUTBOUND_ARCHIVE);
        else
            revenueArchive = sftpConfigurationService.getSftpConfigurationValueAsString(Constants.SFTP_REVENUE_OUTBOUND_ARCHIVE);
        emailContent.getSuccess().append(EMAILBODY_STRUCTURE + revenueArchive + EMAILBODY_FILE_LOC);
        emailContent.getSuccess().append("<br/>Successful Interface for: <br/>");
        if (accountNumbers != null && !accountNumbers.isEmpty()) {
            for (String accountNumber : accountNumbers) {
                emailContent.getSuccess().append(" Account Number : ").append(accountNumber).append("<br/><br/>");
            }
        }
        return emailContent;
    }

    public IntegrationReportVO revenueTransactionRTProcessing(String fileName, BufferedReader fileReader, Integer fileId, Integer totalFileRowCount, Integer fileRowIndex, String processingType) {
        IntegrationReportVO vo = new IntegrationReportVO();
        int processed = (fileRowIndex > 0) ? fileRowIndex : 0;
        int fileRowCount = 0;
        try {
            final int ACCOUNT_NUMBER = 0;
            final int INTERNAL_ORDER_CODE = 1;
            final int REMARKS = 2;
            final int AMOUNT_IN_FMA_CURRENCY = 3;
            final int ACTUAL_OR_COMMITTED_FLAG = 4;
            final int ENTRY_DATE = 5;
            final int FI_POSTING_DATE = 6;
            final int BP_CODE = 7;
            final int BP_NAME = 8;
            final int DOCUMENT_DATE = 9;
            final int FI_GL_ACCOUNT = 10;
            final int FI_GL_DESCRIPTION = 11;
            final int TRANSACTION_REFERENCE_NUMBER = 12;
            final int REFERENCE_DOCUMENT_NUMBER = 13;
            final int REFERENCE_POSTING_LINE = 14;
            final int GUID = 15;
            final int GMIA_DOCNR = 16;
            final int DOCLN = 17;
            final int RBUKRS = 18;
            final int RVALUETYPE_9 = 19;
            final int RYEAR = 20;
            final int GL_SIRID = 21;
            final int BATCH_ID = 22;
            final int GRANT_NBR = 23;
            final int SPONSOR_PROGRAM = 24;
            final int SPONSOR_CLASS = 25;
            final int FUND = 26;
            final int FUND_CENTER = 27;
            fileRowCount = count(fileName);
            String line = "";
            String result = "";
            int minLineNumber = fileRowIndex;
            int maxLineNumber = 100;
            int lineNumberToRead = maxLineNumber + minLineNumber;
            while ((line = fileReader.readLine()) != null) {
                String[] tokens = line.split("\",\"", -1);
                if (tokens.length > 0) {
                    while (tokens.length < 28) {
                        minLineNumber++;
                        --totalFileRowCount;
                        String tempLine = fileReader.readLine();
                        if (tempLine == null) {
                            break;
                        }
                        line = line + tempLine;
                        tokens = line.split("\",\"", -1);
                    }
                    try {
                        result = getAmountInFmaCurrency(tokens[AMOUNT_IN_FMA_CURRENCY]);
                    } catch (Exception e) {
                        result = getAmountInFmaCurrency(tokens[AMOUNT_IN_FMA_CURRENCY]);
                    }
                    if (result == null) {
                        result = "0";
                    }
                    tokens[ACCOUNT_NUMBER] = tokens[ACCOUNT_NUMBER].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[INTERNAL_ORDER_CODE] = tokens[INTERNAL_ORDER_CODE].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[REMARKS] = tokens[REMARKS].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[ACTUAL_OR_COMMITTED_FLAG] = tokens[ACTUAL_OR_COMMITTED_FLAG].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[ENTRY_DATE] = tokens[ENTRY_DATE].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[FI_POSTING_DATE] = tokens[FI_POSTING_DATE].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[BP_CODE] = tokens[BP_CODE].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[BP_NAME] = tokens[BP_NAME].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[DOCUMENT_DATE] = tokens[DOCUMENT_DATE].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[FI_GL_ACCOUNT] = tokens[FI_GL_ACCOUNT].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[FI_GL_DESCRIPTION] = tokens[FI_GL_DESCRIPTION].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[TRANSACTION_REFERENCE_NUMBER] = tokens[TRANSACTION_REFERENCE_NUMBER].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[REFERENCE_DOCUMENT_NUMBER] = tokens[REFERENCE_DOCUMENT_NUMBER].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[REFERENCE_POSTING_LINE] = tokens[REFERENCE_POSTING_LINE].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[GUID] = tokens[GUID].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[GMIA_DOCNR] = tokens[GMIA_DOCNR].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[DOCLN] = tokens[DOCLN].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[RBUKRS] = tokens[RBUKRS].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[RVALUETYPE_9] = tokens[RVALUETYPE_9].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[RYEAR] = tokens[RYEAR].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[GL_SIRID] = tokens[GL_SIRID].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[BATCH_ID] = tokens[BATCH_ID].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[GRANT_NBR] = tokens[GRANT_NBR].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[SPONSOR_PROGRAM] = tokens[SPONSOR_PROGRAM].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[SPONSOR_CLASS] = tokens[SPONSOR_CLASS].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[FUND] = tokens[FUND].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[FUND_CENTER] = tokens[FUND_CENTER].replaceAll(REGULAR_EXPRESSION, "");
                    AwardRevenueTransactionsRT awardRevenueTransactionsRT = new AwardRevenueTransactionsRT(
                            tokens[ACCOUNT_NUMBER], tokens[INTERNAL_ORDER_CODE], tokens[REMARKS],
                            new BigDecimal(result), tokens[ACTUAL_OR_COMMITTED_FLAG], tokens[ENTRY_DATE], tokens[FI_POSTING_DATE],
                            tokens[BP_CODE], tokens[BP_NAME], tokens[DOCUMENT_DATE], tokens[FI_GL_ACCOUNT],
                            tokens[FI_GL_DESCRIPTION], tokens[TRANSACTION_REFERENCE_NUMBER],
                            tokens[REFERENCE_DOCUMENT_NUMBER], tokens[REFERENCE_POSTING_LINE],
                            tokens[GUID], tokens[GMIA_DOCNR], tokens[DOCLN],
                            tokens[RBUKRS], tokens[RVALUETYPE_9], tokens[RYEAR],
                            tokens[GL_SIRID], tokens[BATCH_ID], tokens[GRANT_NBR],
                            tokens[SPONSOR_PROGRAM], tokens[SPONSOR_CLASS], tokens[FUND],
                            tokens[FUND_CENTER]);
                    saveRevenueTransactionData(vo.getEmailContent(), fileId, awardRevenueTransactionsRT);
                    processed++;
                }
                minLineNumber++;
                --totalFileRowCount;
                if (totalFileRowCount < 0 && minLineNumber == fileRowCount) {
                    totalFileRowCount = 0;
                }
                if (minLineNumber == lineNumberToRead) {
                    logger.info("Total row Inserted {} ", minLineNumber);
                    break;
                }
            }
            if (totalFileRowCount < 0) {
                totalFileRowCount = 0;
            }
            fastIntegrationDao.updateStatusForDataLoadingInRT(fileId, Constants.YES, "R", processed);
            vo.setTotalFileRowCount(totalFileRowCount);
            vo.setFileRowIndex(minLineNumber);
        } catch (Exception e) {
            logger.error("Error in method revenueTransactionRTProcessing", e);
            e.printStackTrace();
            vo.setErrorOccured(Boolean.TRUE);
            fastIntegrationDao.updateStatusForDataLoadingInRT(fileId, Constants.NO, "R", processed);
            vo.getEmailContent().getError().append("Error in revenueTransactionRTProcessing for ").append(processingType).append(" : <br/>").append(e).append("<br/>")
                    .append("Only ").append((processed)).append(" are saved out of ").append(fileRowCount).append(" in file : ").append(fileName)
                    .append("<br>").append("File ").append(fileName).append(" will be not moved to archive").append("<br>");
        }
        return vo;
    }

    @Override
    public void moveRevenueFile(File file, EmailContent emailBody, Integer fileId, String processingType) {
        try {
            if(processingType.equals("REVENUE_INVOICE"))
                revenueArchive = sftpConfigurationService.getSftpConfigurationValueAsString(Constants.RISE_REVENUE_INVOICE_OUTBOUND_ARCHIVE);
            else
                revenueArchive = sftpConfigurationService.getSftpConfigurationValueAsString(Constants.RISE_REVENUE_OUTBOUND_ARCHIVE);

            Path temp = Files.move(Paths.get(file.getPath()), Paths.get(revenueArchive + "/" + file.getName()), StandardCopyOption.REPLACE_EXISTING);
            if (temp != null) {
                logger.info("file successfully moved from {} -> {}", file.getPath(), temp);
                fastIntegrationDao.updateStatusForFileInSystem(fileId, Constants.YES, "R");
                if (commonDao.getParameterValueAsBoolean(Constants.IS_SFTP_ENABLED)) {
                    if (processingType.equals("REVENUE_INVOICE")) {
                    	moveSftpDirectorys(sftpConfigurationService.getSftpConfigurationValueAsString(Constants.SFTP_REVENUE_INVOICE_OUTBOUND), file.getName(),
                                sftpConfigurationService.getSftpConfigurationValueAsString(Constants.SFTP_REVENUE_INVOICE_OUTBOUND_ARCHIVE), fileId, processingType);
                    } else {
                    	moveSftpDirectorys(sftpConfigurationService.getSftpConfigurationValueAsString(Constants.SFTP_REVENUE_OUTBOUND), file.getName(),
                                sftpConfigurationService.getSftpConfigurationValueAsString(Constants.SFTP_REVENUE_OUTBOUND_ARCHIVE), fileId, processingType);
                    }                
                }
            } else {
                logger.info("File not moved : {} ", file.getPath());
            }
        } catch (Exception e) {
                logger.error("Exception in method moveRevenueFile : {} ", e.getMessage());
                e.printStackTrace();
                emailBody.getError().append("Exception in method moveRevenueFile <br/><br/> ").append(e.getMessage()).append("<br/>");
        }
    }

    @Override
    public IntegrationReportVO saveAwardRevenueFiles(File file) {
        try {
            String fileName = null;
            IntegrationReportVO vo = new IntegrationReportVO();
            fileName = file.toString();
            AwardRevenueFile awardRevenueFiles = new AwardRevenueFile();
            awardRevenueFiles.setFileName(file.getName());
            awardRevenueFiles.setNoOfRecords(count(fileName));
            awardRevenueFiles.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
            awardRevenueFiles.setInsertedInRT(Constants.NO);
            awardRevenueFiles.setSystemArchived(Constants.NO);
            awardRevenueFiles.setRemoteArchived(Constants.NO);
            awardRevenueFiles = fastIntegrationDao.saveAwardRevenueFiles(awardRevenueFiles);
            vo.setFileId(awardRevenueFiles.getFileId());
            vo.setTotalFileRowCount(count(fileName));
            vo.setFileName(fileName);
            vo.setFileRowIndex(0);
            fastIntegrationDao.deleteAllRevenueTransactionRT();
            return vo;
        } catch (Exception e) {
            logger.error("Error in method saveAwardRevenueFiles {}", e.getMessage());
            e.printStackTrace();
            return null;
        }
    }

    private void saveRevenueTransactionData(EmailContent emailBody, Integer fileId, AwardRevenueTransactionsRT awardRevenueTransactionsRT) {
        try {
            awardRevenueTransactionsRT.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
            awardRevenueTransactionsRT.setFileId(fileId);
            fastIntegrationDao.saveRevenueTransactionRT(awardRevenueTransactionsRT);
        } catch (Exception e) {
            logger.error("Error in method saveRevenueTransactionData", e);
            e.printStackTrace();
            emailBody.getError().append("Error in saveRevenueTransactionData : <br/>").append(e).append("<br/>");
        }
    }

    @Override
    public void awardRevenueTrackerRefresh(Integer fileId, EmailContent emailContent) {
    	if (fileId != null) {
            fastIntegrationDao.awardRevenueTrackerRefresh(fileId, emailContent);
        }
    }

    @Override
    public void awardRevenueTrackerSync(Integer fileId, EmailContent emailContent) {
    	fastIntegrationDao.awardRevenueTrackerSync(fileId, emailContent);
    }

    @Override
    public XSSFWorkbook getXSSFWorkbookForRevenueTransaction(CommonVO vo) {
        XSSFWorkbook workbook = new XSSFWorkbook();
        XSSFSheet sheet = workbook.createSheet("Report");
        commonService.addDetailsInHeader(workbook, sheet);
        List<Object[]> reportData = fastIntegrationDao.downloadRevenueTransaction();
        Object[] tableHeadingRow = {"UNIT_NUMBER", "UNIT_NAME", "CAMPUS", "REVENUE_TRACKER_ID", "AWARD_NUMBER", "ACCOUNT_NUMBER", "INTERNAL_ORDER_CODE", "REMARKS",
                "AMOUNT_IN_FMA_CURRENCY", "ACTUAL_OR_COMMITTED_FLAG", "ENTRY_DATE", "FI_POSTING_DATE", "BP_CODE", "BP_NAME", "DOCUMENT_DATE",
                "FI_GL_ACCOUNT", "FI_GL_DESCRIPTION", "TRANSACTION_REFERENCE_NUMBER", "REFERENCE_DOCUMENT_NUMBER", "REFERENCE_POSTING_LINE", "GUID",
                "GMIA_DOCNR", "DOCLN", "RBUKRS", "RVALUETYPE_9", "RYEAR", "GL_SIRID", "BATCH_ID",
                "GRANT_NBR", "SPONSOR_PROGRAM", "SPONSOR_CLASS", "FUND", "FUND_CENTER"};
        dashboardService.prepareExcelSheet(reportData, sheet, tableHeadingRow, workbook, vo);
        return workbook;
    }

    @Override
    public XSSFWorkbook exportLevelOneRevenueTransactionReport(CommonVO vo) {
        XSSFWorkbook workbook = new XSSFWorkbook();
        XSSFSheet sheet = workbook.createSheet(vo.getDocumentHeading());
        commonService.addDetailsInHeader(workbook, sheet);
        List<Object[]> reportData = fastIntegrationDao.downloadRevenueRepoertLevelOne();
        Object[] tableHeadingRow = {"Unit Number", "Unit Name", "Campus", "ACCOUNT_NUMBER L1 WBS", "Total Cost", "Budgeted Amount", "Balance"};
        dashboardService.prepareExcelSheet(reportData, sheet, tableHeadingRow, workbook, vo);
        return workbook;
    }

    @Override
    public XSSFWorkbook exportLevelTwoRevenueTransactionReport(CommonVO vo) {
        XSSFWorkbook workbook = new XSSFWorkbook();
        XSSFSheet sheet = workbook.createSheet(vo.getDocumentHeading());
        commonService.addDetailsInHeader(workbook, sheet);
        List<Object[]> reportData = fastIntegrationDao.downloadRevenueReportLevelTwo();
        Object[] tableHeadingRow = {"Unit Number", "Unit Name", "Campus", "ACCOUNT_NUMBER L1 WBS", "L2_WBS Number", "Budgeted Amount", "Cumulative Revenue ", "Balance"};
        dashboardService.prepareExcelSheet(reportData, sheet, tableHeadingRow, workbook, vo);
        return workbook;
    }


    @Override
    public void setSuccessEmailContentForExpense(IntegrationReportVO integrationReportVO) {
        Set<String> accountNumbers = getawardAwardNumbersByFileId(integrationReportVO.getFileId());
        if (accountNumbers != null && !accountNumbers.isEmpty()) {
            integrationReportVO.setEmailContent(setExpenseTrackerEmailBody(accountNumbers, integrationReportVO.getEmailContent()));
        }
    }

    @Override
    public void sendSuccessEmailContentForRevenue(IntegrationReportVO integrationReportVO, String processingType) {
        Set<String> accountNumbers = getRevenueAwardNumbersByFileId(integrationReportVO.getFileId());
        if (accountNumbers != null && !accountNumbers.isEmpty()) {
            integrationReportVO.setEmailContent((setAwardRevenueEmailBody(accountNumbers, integrationReportVO.getEmailContent(), processingType)));
        }
    }

    @Override
    public void sendSoftLaunchFeedReport(Integer batchId) {
        XSSFWorkbook workbook = new XSSFWorkbook();
        createSoftLaunchSummary(workbook, batchId);
        createSoftLaunchFeedReport(workbook, batchId);
        createSoftLaunchNoFeedReport(workbook, batchId);
        File report = commonService.createfileInUploads(workbook, "Soft_Launch_Report.xlsx");
        EmailServiceVO emailServiceVO = new EmailServiceVO();
        emailServiceVO.setBody(new StringBuilder("Soft Launch Report for the batch id  ").append(batchId.toString()).append(" is attached in here").toString());
        emailServiceVO.setSubject(new StringBuilder("Soft-Launch - Status Report from ")
				.append(commonDao.getDateFromTimestampZoneFormat(Constants.CRON_JOB_TIMEZONE, Constants.LONG_DATE_FORMAT)).toString());
        emailServiceVO.setFileName(report);
        emailServiceVO.setRecipients(getFastIntegrationMailRecipient());
        emailService.sendEmail(emailServiceVO);
    }

    private void createSoftLaunchSummary(XSSFWorkbook workbook, Integer batchId) {
        CommonVO vo = new CommonVO();
        String sheetName = new StringBuilder("Soft Launch Summary of batch Id : ").append(batchId.toString()).toString();
        XSSFSheet sapFeedBudgetReportSheet = workbook.createSheet(sheetName);
        commonService.addDetailsInHeader(workbook,sapFeedBudgetReportSheet);
        vo.setDocumentHeading(sheetName);
        List<Object[]> sapFeedBugdetReport = fastIntegrationDao.getSoftLaunchSummaryReport(batchId);
        Object[] tableHeadingRows = {"Variation Type", "Total"};
        dashboardService.prepareExcelSheet(sapFeedBugdetReport, sapFeedBudgetReportSheet, tableHeadingRows, workbook, vo);
    }

    private void createSoftLaunchFeedReport(XSSFWorkbook workbook, Integer batchId) {
        CommonVO vo = new CommonVO();
        String sheetName = new StringBuilder("Soft Launch Feedeed Report  of batch Id : ").append(batchId.toString()).toString();
        XSSFSheet sapFeedBudgetReportSheet = workbook.createSheet(sheetName);
        commonService.addDetailsInHeader(workbook,sapFeedBudgetReportSheet);
        vo.setDocumentHeading(sheetName);
        List<Object[]> sapFeedBugdetReport = fastIntegrationDao.exportSoftLaunchAndNoFeedReport(batchId, Constants.YES);
        Object[] tableHeadingRows = {"AWARD_NUMBER", "ACCOUNT_NUMBER", "IS_100_PCT_INTERNAL", "ACCOUNT_TYPE",
                "LEAD_UNIT_NUMBER", "LEAD_UNIT_NAME", "AWARD_TITLE", "PI_NAME", "SPONSOR_CODE", "SPONSOR_NAME",
                "SPONSOR_AWARD_NUMBER", "VARIATION_TYPE", "VARIATION_SUBJECT", "SUBMISSION_DATE"};
        dashboardService.prepareExcelSheet(sapFeedBugdetReport, sapFeedBudgetReportSheet, tableHeadingRows, workbook, vo);
    }

    private void createSoftLaunchNoFeedReport(XSSFWorkbook workbook, Integer batchId) {
        CommonVO vo = new CommonVO();
        String sheetName = new StringBuilder("Soft Launch No Feed Report  of batch Id : ").append(batchId.toString()).toString();
        XSSFSheet sapFeedBudgetReportSheet = workbook.createSheet(sheetName);
        commonService.addDetailsInHeader(workbook,sapFeedBudgetReportSheet);
        vo.setDocumentHeading(sheetName);
        List<Object[]> sapFeedBugdetReport = fastIntegrationDao.exportSoftLaunchAndNoFeedReport(batchId, Constants.NO);
        Object[] tableHeadingRows = {"AWARD_NUMBER", "ACCOUNT_NUMBER", "IS_100_PCT_INTERNAL", "ACCOUNT_TYPE",
                "LEAD_UNIT_NUMBER", "LEAD_UNIT_NAME", "AWARD_TITLE", "PI_NAME", "SPONSOR_CODE", "SPONSOR_NAME",
                "SPONSOR_AWARD_NUMBER", "VARIATION_TYPE", "VARIATION_SUBJECT", "SUBMISSION_DATE"};
        dashboardService.prepareExcelSheet(sapFeedBugdetReport, sapFeedBudgetReportSheet, tableHeadingRows, workbook, vo);
    }

    private Set<NotificationRecipient> getFastIntegrationMailRecipient() {
        Set<NotificationRecipient> dynamicEmailRecipients = new HashSet<>();
        String emailAddress = commonDao.getParameterValueAsString(Constants.FAST_INTEGRATION_EMAIL_RECIPIENT);
        if (emailAddress != null && !emailAddress.isEmpty()) {
            List<String> emails = Arrays.asList(emailAddress.split(",(?=(?:[^\"]*\"[^\"]*\")*[^\"]*$)"));
            emails.forEach(email -> commonService.setNotificationRecipientsforNonEmployees(email,
                    Constants.NOTIFICATION_RECIPIENT_TYPE_TO, dynamicEmailRecipients));
        }
        return dynamicEmailRecipients;
    }

    @Override
    public void sendMultipleFileNotification(File[] fileData, String location) {
        StringBuilder fileNames = new StringBuilder();
        for (File file : fileData) {
            fileNames.append(file.getName()).append("<br>");
        }
        if (fileNames.length() != 0) {
            IntegrationReportVO vo = new IntegrationReportVO();
            vo.getEmailContent().getError().append("In ").append(location).append(" Multiple Files are included in todays list <br>").append(fileNames);
            sendErrorReportMail(vo, "WARNING : Multiple Files");
        }
    }

    private void moveFile(File file, EmailContent emailBody, Boolean errorInProcess) {
        if (errorInProcess.equals(Boolean.FALSE)) {
            moveFile(file, emailBody);
        }
    }

    public IntegrationReportVO revenueTransactionRTProcessingInvoice(String fileName, BufferedReader fileReader, Integer fileId, Integer totalFileRowCount, Integer fileRowIndex, String processingType) {
        IntegrationReportVO vo = new IntegrationReportVO();
        int processed = (fileRowIndex > 0) ? fileRowIndex : 0;
        int fileRowCount = 0;
        try {
            final int REFERENCE_DOCUMENT_NUMBER = 0;
            final int REFERENCE_POSTING_LINE = 1;
            final int GUID = 2;
            final int GMIA_DOCNR = 3;
            final int DOCLN = 4;
            final int RBUKRS = 5;
            final int RVALUETYPE_9 = 6;
            final int RYEAR = 7;
            final int GL_SIRID = 8;
            final int PAYMENT_DOC_NUMBER = 9;
            final int PAYMENT_DATE = 10;
            final int PAYMENT_FISCAL_YEAR = 11;
            final int BATCH_ID = 12;

            fileRowCount = count(fileName);
            String line = "";
            String result = "";
            int minLineNumber = fileRowIndex;
            int maxLineNumber = 100;
            int lineNumberToRead = maxLineNumber + minLineNumber;
            while ((line = fileReader.readLine()) != null) {
                String[] tokens = line.split("\",\"", -1);
                logger.info(tokens);
                if (tokens.length > 0) {
                    while (tokens.length < 10) {
                        minLineNumber++;
                        --totalFileRowCount;
                        String tempLine = fileReader.readLine();
                        if (tempLine == null) {
                            break;
                        }
                        line = line + tempLine;
                        tokens = line.split("\",\"", -1);
                    }
                    if (result == null) {
                        result = "0";
                    }
                    logger.info(line);
                    tokens[REFERENCE_DOCUMENT_NUMBER] = tokens[REFERENCE_DOCUMENT_NUMBER].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[REFERENCE_POSTING_LINE] = tokens[REFERENCE_POSTING_LINE].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[GUID] = tokens[GUID].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[DOCLN] = tokens[DOCLN].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[RBUKRS] = tokens[RBUKRS].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[RVALUETYPE_9] = tokens[RVALUETYPE_9].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[RYEAR] = tokens[RYEAR].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[GL_SIRID] = tokens[GL_SIRID].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[PAYMENT_DOC_NUMBER] = tokens[PAYMENT_DOC_NUMBER].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[PAYMENT_DATE] = tokens[PAYMENT_DATE].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[GMIA_DOCNR] = tokens[GMIA_DOCNR].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[BATCH_ID] = tokens[BATCH_ID].replaceAll(REGULAR_EXPRESSION, "");
                    tokens[PAYMENT_FISCAL_YEAR] = tokens[PAYMENT_FISCAL_YEAR].replaceAll(REGULAR_EXPRESSION, "");

                    AwardRevenueTransactionsRT awardRevenueTransactionsRT = new AwardRevenueTransactionsRT();
                    awardRevenueTransactionsRT.setReferenceDocumentNumber(tokens[REFERENCE_DOCUMENT_NUMBER]);
                    awardRevenueTransactionsRT.setReferencePostingLine(tokens[REFERENCE_POSTING_LINE]);
                    awardRevenueTransactionsRT.setGuid(tokens[GUID]);
                    awardRevenueTransactionsRT.setDocln(tokens[DOCLN]);
                    awardRevenueTransactionsRT.setRbukrs(tokens[RBUKRS]);
                    awardRevenueTransactionsRT.setRvalueType9(tokens[RVALUETYPE_9]);
                    awardRevenueTransactionsRT.setrYear(tokens[RYEAR]);
                    awardRevenueTransactionsRT.setGlSirid(tokens[GL_SIRID]);
                    awardRevenueTransactionsRT.setPaymentDocNumber(tokens[PAYMENT_DOC_NUMBER]);
                    awardRevenueTransactionsRT.setPaymentDate(convertStringToTimestamp(tokens[PAYMENT_DATE]));
                    awardRevenueTransactionsRT.setBatchId(tokens[BATCH_ID]);
                    awardRevenueTransactionsRT.setPaymentFiscalYear(tokens[PAYMENT_FISCAL_YEAR]);
                    awardRevenueTransactionsRT.setGmiaDocnr(tokens[GMIA_DOCNR]);
                    saveRevenueTransactionData(vo.getEmailContent(), fileId, awardRevenueTransactionsRT);
                    processed++;
                }
                minLineNumber++;
                --totalFileRowCount;
                if (totalFileRowCount < 0 && minLineNumber == fileRowCount) {
                    totalFileRowCount = 0;
                }
                if (minLineNumber == lineNumberToRead) {
                    logger.info("Total row Inserted {} ", minLineNumber);
                    break;
                }
            }
            if (totalFileRowCount < 0) {
                totalFileRowCount = 0;
            }
            fastIntegrationDao.updateStatusForDataLoadingInRT(fileId, Constants.YES, "R", processed);
            vo.setTotalFileRowCount(totalFileRowCount);
            vo.setFileRowIndex(minLineNumber);
        } catch (Exception e) {
            logger.error("Error in method revenueTransactionRTProcessingInvoice", e);
            vo.setErrorOccured(Boolean.TRUE);
            fastIntegrationDao.updateStatusForDataLoadingInRT(fileId, Constants.NO, "R", processed);
            vo.getEmailContent().getError().append("Error in revenueTransactionRTProcessingInvoice for ").append(processingType).append(" : <br/>").append(e).append("<br/>")
                    .append("Only ").append((processed)).append(" are saved out of ").append(fileRowCount).append(" in file : ").append(fileName)
                    .append("<br>").append("File ").append(fileName).append(" will be not moved to archive").append("<br>");
        }
        return vo;
    }

    public static Timestamp convertStringToTimestamp(String strDate) {
        try {
            DateFormat formatter = new SimpleDateFormat("yyyyMMdd");
            // you can change format of date
            Date date = formatter.parse(strDate);
            Timestamp timeStampDate = new Timestamp(date.getTime());

            return timeStampDate;
        } catch (Exception e) {
            System.out.println("Exception :" + e);
            return null;
        }
    }
}
