package com.polus.fibicomp.report.controller;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RestController;

import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.dashboard.service.DashboardService;
import com.polus.fibicomp.report.service.ReportService;
import com.polus.fibicomp.report.vo.ReportVO;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.vo.CommonVO;

@RestController
public class ReportController {

	protected static Logger logger = LogManager.getLogger(ReportController.class.getName());

	@Autowired
	@Qualifier(value = "reportService")
	private ReportService reportService;
	
	@Autowired
	private DashboardService dashboardService;

	@Autowired
	private CommonDao commonDao;

	@PostMapping(value = "/getReportMetaData")
	public ResponseEntity<String> fetchReportDetails(@RequestBody ReportVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getReportMetaData");
		logger.info("reportTypeId : {} ", vo.getReportTypeId());
		HttpStatus status = HttpStatus.OK;
		return new ResponseEntity<>(commonDao.convertObjectToJSON(reportService.fetchReportDetails(vo)), status);
	}

	@PostMapping(value = "/exportReportDatas")
	public ResponseEntity<byte[]> exportDashboardData(HttpServletRequest request, @RequestBody CommonVO vo) throws Exception {
		logger.info("Requesting for exportReportDatas");
		XSSFWorkbook workbook = dashboardService.getXSSFWorkbookForDashboard(vo);
		return dashboardService.getResponseEntityForDownload(vo, workbook);
	}

	@PostMapping(value = "/saveOrUpdateReportTemplate", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String saveOrUpdateReportTemplate(@RequestBody ReportVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for saveOrUpdateReportTemplate");
		return reportService.saveOrUpdateReportTemplate(vo);
	}

	@PostMapping(value = "/getReportTemplateById", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String getReportTemplateById(@RequestBody ReportVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getReportTemplateById");
		logger.info("reportTemplateId : {}", vo.getReportTemplateId());
		return reportService.getReportTemplateById(vo, request.getHeader(Constants.HEADER_STRING));
	}

	@PostMapping(value = "/fetchAllReportTemplates", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String fetchAllReportTemplates(@RequestBody ReportVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for fetchAllReportTemplates");
		return reportService.fetchAllReportTemplates(vo);
	}

	@PostMapping(value = "/generateReport")
	public String generateReport(@RequestBody ReportVO vo,HttpServletRequest request,HttpServletResponse response) {
		logger.info("Requesting for generateReport");
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		logger.info("loginPersonId : {}", vo.getPersonId());
		return reportService.generateReport(vo);
	}

	@PostMapping(value = "/exportGeneratedReport")
	public ResponseEntity<byte[]> exportGeneratedReport(HttpServletRequest request, @RequestBody ReportVO vo) {
		logger.info("Requesting for exportGeneratedReport");
		vo.setPersonId(AuthenticatedUser.getLoginPersonId());
		logger.info("loginPersonId : {}", vo.getPersonId());
		return reportService.exportGeneratedReport(vo);
	}

	@PostMapping(value = "/getNumberOfRecords")
	public String getNumberOfRecords(@RequestBody ReportVO vo,HttpServletRequest request,HttpServletResponse response) {
		logger.info("Requesting for getNumberOfRecords");
		return reportService.getNumberOfRecords(vo);
	}

	@PostMapping(value = "/deleteReportTemplate", produces = MediaType.APPLICATION_JSON_UTF8_VALUE)
	public String deleteReportTemplate(@RequestBody ReportVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for deleteReportTemplate");
		logger.info("reportTemplateId : {}", vo.getReportTemplateId());
		return reportService.deleteReportTemplate(vo);
	}

	@PostMapping("/exportAuditReport")
	public ResponseEntity<byte[]> exportAuditReport(@RequestBody CommonVO vo) throws Exception {
		logger.info("Request for Administrative Report type {} ", vo.getType());
		vo.setExportType("xlsx");
		XSSFWorkbook workbook = reportService.getXSSFWorkbookForAdministrativeReport(vo);
		return dashboardService.getResponseEntityForDownload(vo, workbook);
	}

	@GetMapping("/auditReportTypes")
	public String getAuditReportTypes() {
		return reportService.getAuditReportTypes();
	}

	@PostMapping(value = "/generateReportFromBirt")
	public void generateReportFromBirt(@RequestBody ReportVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for generateReportFromBirt");
		logger.info("reportTemplateId : {}", vo.getReportTypeId());
		reportService.generateReportFromBirt(vo, request, response);
	}

	@PostMapping(value = "/getParameterDetails")
	public Object getParameterDetails(@RequestBody ReportVO vo, HttpServletRequest request, HttpServletResponse response) {
		logger.info("Requesting for getParameterDetails");
		logger.info("reportTemplateId : {}", vo.getReportTypeId());
		return reportService.getParameterDetails(vo.getReportTypeId(), request.getHeader(Constants.HEADER_STRING));
	}

}
