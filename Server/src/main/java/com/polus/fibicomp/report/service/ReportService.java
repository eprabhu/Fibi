package com.polus.fibicomp.report.service;

import com.polus.fibicomp.vo.CommonVO;

import java.util.List;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.report.vo.Field;
import com.polus.fibicomp.report.vo.ReportVO;

@Transactional
@Service
public interface ReportService {

	/**
	 * This method is used to fetch the details of the report based on report template id
	 * @param vo
	 * @return details of report as JSON format.
	 */
	public ReportVO fetchReportDetails(ReportVO vo);

	/**
	 * This method is used to save or update user defined report template from the system templates
	 * @param vo
	 * @return saved report template
	 */
	public String saveOrUpdateReportTemplate(ReportVO vo);

	/**
	 * This method is used to fetch report template by report template id
	 * @param vo
	 * @param token 
	 * @return corresponding report template 
	 */
	public String getReportTemplateById(ReportVO vo, String token);

	/**
	 * This method is used to list all report template based on the person id
	 * @param vo
	 * @return list of report templates
	 */
	public String fetchAllReportTemplates(ReportVO vo);

	/**
	 * This method is used to generate the report based on the given criteria
	 * @param vo
	 * @return list of data based on the report and criteria
	 */
	public String generateReport(ReportVO vo);

	/**
	 * This method is used to export the details in excel format based to the report and the given criteria 
	 * @param vo
	 * @return byte array of generated report
	 */
	public ResponseEntity<byte[]> exportGeneratedReport(ReportVO vo);

	/**
	 * This method is used to get the total number of records for generated report
	 * @param vo
	 * @return total number of records for selected report
	 */
	public String getNumberOfRecords(ReportVO vo);

	/**
	 * This method is used to delete a report template
	 * @param vo
	 * @return vo
	 */
	public String deleteReportTemplate(ReportVO vo);

	/**
	 * This method is used to generate XSSFWorkbook from diff role based tables using report type
	 * @param vo
	 * @return XSSFWorkbook
	 */
	XSSFWorkbook getXSSFWorkbookForAdministrativeReport(CommonVO vo);

	/**
	 * This method is used to get all audit report type
	 *
	 * @return stringified audit report types list
	 */
    String getAuditReportTypes();

	/**
	 *  This method is used to send daily report from audit report table - scheduler
	 */
	public void roleRightAuditReport();

	/**
	 * This method is used to generate the report using BIRT engine
	 * @param vo
	 * @param request
	 * @param response
	 */
	public void generateReportFromBirt(ReportVO vo, HttpServletRequest request, HttpServletResponse response);

	/**
	 * This method is used to get the BIRT parameter details based on the report type
	 * @param reportTypeId
	 * @return details of the input parameters
	 */
	public List<Field> getParameterDetails(String reportTypeId, String token);

}
