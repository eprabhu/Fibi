package com.polus.fibicomp.print.service;

import javax.servlet.http.HttpServletResponse;

import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.http.ResponseEntity;

import com.polus.fibicomp.progressreport.pojo.AwardProgressReport;

public interface ProgressReportPrintService {

	/**
	 * This method is user to generate progress report as excel file
	 * @param response
	 * @param progressReportId
	 * @return
	 */
	public ResponseEntity<byte[]> generateProgressReport(HttpServletResponse response, Integer progressReportId);

	/**
	 * @param progressReportId
	 * @param awardId
	 * @param awardLeadUnitNumber
	 * @param response
	 */
	public void printEntireProgressReport(Integer progressReportId, Integer awardId, String awardLeadUnitNumber, HttpServletResponse response);

}
