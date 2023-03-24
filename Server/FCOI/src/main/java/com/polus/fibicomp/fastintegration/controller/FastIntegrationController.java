package com.polus.fibicomp.fastintegration.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.dashboard.service.DashboardService;
import com.polus.fibicomp.fastintegration.service.FastIntegrationService;
import com.polus.fibicomp.fastintegration.service.FastTransactionService;
import com.polus.fibicomp.fastintegration.vo.IntegrationReportVO;
import com.polus.fibicomp.vo.CommonVO;

@RestController
public class FastIntegrationController {

	protected static Logger logger = LogManager.getLogger(FastIntegrationController.class.getName());

	@Autowired
	private FastIntegrationService fastIntegrationService;

	@Autowired
	public CommonDao commonDao;

	@Autowired
	private DashboardService dashboardService;
	
	@Value("${system.timezone}")
	private String timezone;

	@Autowired
	private FastTransactionService fastTransactionService;

	@PostMapping(value = "/fastIntegration", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String fastIntegration(@RequestBody IntegrationReportVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Request for fastIntegration");
		return fastIntegrationService.fastDataMigration(vo);
	}

	@GetMapping(value = {"/fastIntegrationResponseProcessing","/fastResponseProcessing"}, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public void fastIntegrationResponceProcessing(HttpServletRequest request, HttpServletResponse response) {
		logger.info("Request for fastIntegrationResponseProcessing");
		IntegrationReportVO integrationReportVO = fastIntegrationService.fetchResponce();
		fastIntegrationService.updateSapAwardFeedDetail(integrationReportVO.getBatchIds());
		fastIntegrationService.sapAwardUpdateHoldStatus();
		fastIntegrationService.fastIntegrationResponseMail(integrationReportVO);
	}

	@GetMapping(value = {"/fastIntegrationExpenseTransactionRTProcessing","/fastExpenseTransactionRTProcessing"}, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public void fastIntegrationExpenseTransactionRTProcessing(HttpServletRequest request, HttpServletResponse response) {
		logger.info("Request for fastIntegrationExpenseTransactionRTProcessing");
		fastTransactionService.fastDataMigrationForExpenseTracker();
		logger.info("Expense file integration completed");
	}
	
	@GetMapping(value = "/exportExpenseTransactionReport")
	public ResponseEntity<byte[]> exportExpenseTransactionReport(HttpServletRequest request) throws Exception {
		logger.info("Requesting for exportExpenseReport");
		CommonVO vo = new CommonVO();
		vo.setExportType("xlsx");
		vo.setDocumentHeading("Expense_Tracker_transaction");
		XSSFWorkbook workbook = fastIntegrationService.getXSSFWorkbookForExpenseTracker(vo);
		return dashboardService.getResponseEntityForDownload(vo, workbook);
	}

	@GetMapping(value = "/exportLevelTwoExpenseTransactionReport")
	public ResponseEntity<byte[]> exportLevelTwoExpenseTransactionReport(HttpServletRequest request) throws Exception {
		logger.info("Requesting for exportLevelTwoExpenseTransactionReport");
		CommonVO vo = new CommonVO();
		vo.setExportType("xlsx");
		vo.setDocumentHeading("Expense_Tracker_transaction_level2");
		XSSFWorkbook workbook = fastIntegrationService.exportLevelTwoExpenseTransactionReport(vo);
		return dashboardService.getResponseEntityForDownload(vo, workbook);
	}

	@GetMapping(value = "/exportLevelOneExpenseTransactionReport")
	public ResponseEntity<byte[]> exportLevelOneExpenseTransactionReport(HttpServletRequest request) throws Exception {
		logger.info("Requesting for exportLevelOneExpenseTransactionReport");
		CommonVO vo = new CommonVO();
		vo.setExportType("xlsx");
		vo.setDocumentHeading("Expense_Tracker_transaction_level1");
		XSSFWorkbook workbook = fastIntegrationService.exportLevelOneExpenseTransactionReport(vo);
		return dashboardService.getResponseEntityForDownload(vo, workbook);
	}

	@GetMapping(value = "/exportSapFeedReport")
	public ResponseEntity<byte[]> exportSapFeedReport(HttpServletRequest request) throws Exception {
		logger.info("Requesting for exportSapFeedReport");
		CommonVO vo = new CommonVO();
		vo.setExportType("xlsx");
		vo.setProperty1(request.getHeader("batchId"));
		XSSFWorkbook workbook = fastIntegrationService.exportSapFeedReport(vo);
		return dashboardService.getResponseEntityForDownload(vo, workbook);
	}

	@GetMapping(value = {"/fastIntegrationRevenueTransactionRTProcessing","/fastRevenueTransactionRTProcessing"}, produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public void fastIntegrationRevenueTransactionRTProcessing(HttpServletRequest request, HttpServletResponse response,
															  @RequestParam(defaultValue = "REVENUE",required = false) String processingType) {
		logger.info("Request for fastIntegrationRevenueTransactionRTProcessing {} ", processingType);
		fastTransactionService.fastIntegrationRevenueTransactionRTProcessing(processingType);
		logger.info("Revenue file integration completed");
	}

	@GetMapping(value = "/exportRevenueTransactionReport")
	public ResponseEntity<byte[]> exportRevenueTransactionReport(HttpServletRequest request) throws Exception {
		logger.info("Requesting for exportRevenueReport");
		CommonVO vo = new CommonVO();
		vo.setExportType("xlsx");
		vo.setDocumentHeading("Revenue_transaction");
		XSSFWorkbook workbook = fastIntegrationService.getXSSFWorkbookForRevenueTransaction(vo);
		return dashboardService.getResponseEntityForDownload(vo, workbook);
	}

	@GetMapping(value = "/exportLevelOneRevenueTransactionReport")
	public ResponseEntity<byte[]> exportLevelOneRevenueTransactionReport(HttpServletRequest request) throws Exception {
		logger.info("Requesting for exportLevelOneRevenueTransactionReport");
		CommonVO vo = new CommonVO();
		vo.setExportType("xlsx");
		vo.setDocumentHeading("Revenue_transaction_level1");
		XSSFWorkbook workbook = fastIntegrationService.exportLevelOneRevenueTransactionReport(vo);
		return dashboardService.getResponseEntityForDownload(vo, workbook);
	}

	@GetMapping(value = "/exportLevelTwoRevenueTransactionReport")
	public ResponseEntity<byte[]> exportLevelTwoRevenueTransactionReport(HttpServletRequest request) throws Exception {
		logger.info("Requesting for exportLevelTwoRevenueTransactionReport");
		CommonVO vo = new CommonVO();
		vo.setExportType("xlsx");
		vo.setDocumentHeading("Revenue_transaction_level2");
		XSSFWorkbook workbook = fastIntegrationService.exportLevelTwoRevenueTransactionReport(vo);
		return dashboardService.getResponseEntityForDownload(vo, workbook);
	}

	@GetMapping(value = "/fastIntegrationRevenuePayments")
	public void fastIntegrationRevenuePayments() throws Exception {
		logger.info("Request for fastIntegrationRevenuePayments");
		fastTransactionService.fastIntegrationRevenueTransactionRTProcessing("REVENUE_INVOICE");
		logger.info("Revenue Invoice file integration completed");
	}
	
	@GetMapping(value = "/fastIntegrationTemplateGeneration")
	public void fastDataMigration() throws Exception {
		logger.info("Scheduler request for fastIntegrationTemplateGeneration");
		fastIntegrationService.fastTemplateGeneration(new IntegrationReportVO());
		logger.info("FastIntegrationTemplateGeneration integration completed");
	}
}
