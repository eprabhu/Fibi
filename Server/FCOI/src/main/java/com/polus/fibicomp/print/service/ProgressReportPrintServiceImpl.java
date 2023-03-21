package com.polus.fibicomp.print.service;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletResponse;
import javax.transaction.Transactional;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.poi.ss.usermodel.BorderStyle;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.HorizontalAlignment;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.util.CellRangeAddress;
import org.apache.poi.ss.util.RegionUtil;
import org.apache.poi.xssf.usermodel.XSSFCellStyle;
import org.apache.poi.xssf.usermodel.XSSFFont;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;

import com.polus.fibicomp.award.pojo.AwardMileStone;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.pojo.FileData;
import com.polus.fibicomp.pojo.Sponsor;
import com.polus.fibicomp.print.dto.QuestionAndAnswer;
import com.polus.fibicomp.print.dto.QuestionnairePrintParameter;
import com.polus.fibicomp.progressreport.dao.ProgressReportDao;
import com.polus.fibicomp.progressreport.pojo.AwardProgressReport;
import com.polus.fibicomp.progressreport.pojo.AwardProgressReportAttachment;
import com.polus.fibicomp.progressreport.pojo.AwardProgressReportKPISummary;
import com.polus.fibicomp.progressreport.pojo.AwardProgressReportMilestone;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPICashFunding;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPICollaborationProjects;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPICompetitiveGrants;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPIConferencePresentation;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPIGrantSpecific;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPIHealthSpecificOutcomes;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPIImpactPublications;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPIInkindContributions;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPILicenses;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPIManpowerDevelopment;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPIPatents;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPIPostDocsEmployed;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPISuccessfulStartups;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPITechnologiesDeployed;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPITechnologyDisclosure;
import com.polus.fibicomp.progressreport.pojo.ProgressReportKPIUndergraduateStudent;
import com.polus.fibicomp.questionnaire.dao.QuestionnaireDAO;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.vo.CommonVO;

import fr.opensagres.xdocreport.core.io.internal.ByteArrayOutputStream;

@Transactional
@Service(value = "progressReportPrintService")
public class ProgressReportPrintServiceImpl implements ProgressReportPrintService {

	protected static Logger logger = LogManager.getLogger(ProgressReportPrintServiceImpl.class.getName());

	private static final String PUBLIC = "public";
	private static final String CONTENT_TYPE = "application/octet-stream";
	private static final String CACHE_CONTROL = "must-revalidate, post-check=0, pre-check=0";
	private static final String SUMMARY_SECTION_TYPE = "S";
	private static final String FUTURE_SECTION_TYPE = "F";

	@Autowired
	private ProgressReportDao progressReportDao;

	@Autowired
	private CommonService commonService;

	@Autowired
	private PersonDao personDao;

	@Autowired
	private PrintService printService;

	@Autowired
	public QuestionnaireDAO questionnaireDAO;

	@Autowired
	public CommonDao commonDao;

	@Override
	public ResponseEntity<byte[]> generateProgressReport(HttpServletResponse response, Integer progressReportId) {
		ResponseEntity<byte[]> attachmentData = null;
		try {
			ByteArrayOutputStream bos = prepareProgressReportExcelData(progressReportId);
			attachmentData = getResponseEntityForExcelDownload(bos.toByteArray());
		} catch (Exception e) {
			logger.error("Exception in generateProgressReport : {} ", e.getMessage());
			e.printStackTrace();
		}
		return attachmentData;	
	}

	private ByteArrayOutputStream prepareProgressReportExcelData(Integer progressReportId) {
		ByteArrayOutputStream bos = new ByteArrayOutputStream();
		try {
			XSSFWorkbook workbook = new XSSFWorkbook();
			String documentHeading = null;
			AwardProgressReport awardProgressReport = progressReportDao.loadAwardProgressReport(progressReportId);
			if (awardProgressReport.getReportClassCode().equals(Constants.FINAL_REPORT_CLASS_CODE) ) {
				documentHeading = "Final Report";
			} else if(awardProgressReport.getReportClassCode().equals(Constants.PROGRESS_REPORT_CLASS_CODE)) {
				documentHeading = "Progress Report";
			}
			XSSFSheet sheet = workbook.createSheet(documentHeading);
			commonService.addDetailsInHeader(workbook,sheet);
			prepareExcelSheetForProgressReport(documentHeading, awardProgressReport, sheet, workbook);
			workbook.write(bos);
		} catch (Exception e) {
			e.printStackTrace();
		}		
		return bos;
		
	}

	private ResponseEntity<byte[]> getResponseEntityForExcelDownload(byte[] bytes) {
		ResponseEntity<byte[]> attachmentData = null;
		try {
			HttpHeaders headers = new HttpHeaders();
			headers.setContentType(MediaType.parseMediaType(CONTENT_TYPE));
			headers.setContentLength(bytes.length);
			headers.setCacheControl(CACHE_CONTROL);
			headers.setPragma(PUBLIC);
			attachmentData = new ResponseEntity<>(bytes, headers, HttpStatus.OK);
		} catch (Exception e) {
			logger.error("Error in method getResponseEntity", e);
		}
		return attachmentData;
	}

	public Integer prepareProgressReportGeneralInformationForExcel(String documentHeading, XSSFSheet sheet, int rowNumber, AwardProgressReport awardProgressReport, XSSFWorkbook workbook) {
		// Excel sheet heading style and font creation code.
		Integer count = 0;
		sheet.addMergedRegion(new CellRangeAddress(rowNumber, rowNumber, 0, 8));
		setBordersToMergedCells(sheet, new CellRangeAddress(rowNumber, rowNumber, 0, 8));
		Row headerRow = sheet.createRow(rowNumber++);
		Cell headingCell = headerRow.createCell(0);
		headingCell.setCellValue(documentHeading);
		XSSFFont headerFont = workbook.createFont();
		headerFont.setBold(true);
		headerFont.setFontHeightInPoints((short) 15);
		XSSFCellStyle headerStyle = workbook.createCellStyle();
		headerStyle.setAlignment(HorizontalAlignment.CENTER);
		headerStyle.setFont(headerFont);
		headingCell.setCellStyle(headerStyle);
		// Table head style and font creation code.
		XSSFCellStyle tableHeadStyle = workbook.createCellStyle();
		tableHeadStyle.setBorderTop(BorderStyle.HAIR);
		tableHeadStyle.setBorderBottom(BorderStyle.HAIR);
		tableHeadStyle.setBorderLeft(BorderStyle.HAIR);
		tableHeadStyle.setBorderRight(BorderStyle.HAIR);
		XSSFFont tableHeadFont = workbook.createFont();
		tableHeadFont.setBold(true);
		tableHeadFont.setFontHeightInPoints((short) 12);
		tableHeadStyle.setFont(tableHeadFont);
		// Table body style and font creation code.
		XSSFCellStyle tableBodyStyle = workbook.createCellStyle();
		tableBodyStyle.setBorderTop(BorderStyle.HAIR);
		tableBodyStyle.setBorderBottom(BorderStyle.HAIR);
		tableBodyStyle.setBorderLeft(BorderStyle.HAIR);
		tableBodyStyle.setBorderRight(BorderStyle.HAIR);
		XSSFFont tableBodyFont = workbook.createFont();
		tableBodyFont.setFontHeightInPoints((short) 12);
		tableBodyStyle.setFont(tableBodyFont);

		Row progressReportTitleRow = sheet.createRow(rowNumber++);
		Cell progressReportHeadingCell = progressReportTitleRow.createCell(0);
		sheet.addMergedRegion(new CellRangeAddress(progressReportTitleRow.getRowNum(), progressReportTitleRow.getRowNum(), progressReportHeadingCell.getColumnIndex(), progressReportHeadingCell.getColumnIndex()+2));
		progressReportHeadingCell.setCellStyle(tableBodyStyle);
		progressReportHeadingCell.setCellValue(documentHeading);
		setBordersToMergedCells(sheet, new CellRangeAddress(progressReportTitleRow.getRowNum(), progressReportTitleRow.getRowNum(), progressReportHeadingCell.getColumnIndex(), progressReportHeadingCell.getColumnIndex() + 2));
		
		Cell progressReportValueCell = progressReportTitleRow.createCell(3);
		sheet.addMergedRegion(new CellRangeAddress(progressReportTitleRow.getRowNum(), progressReportTitleRow.getRowNum(), progressReportValueCell.getColumnIndex(), progressReportValueCell.getColumnIndex()+5));
		progressReportValueCell.setCellStyle(tableHeadStyle);
		progressReportValueCell.setCellValue(awardProgressReport.getProgressReportNumber());
		setBordersToMergedCells(sheet, new CellRangeAddress(progressReportTitleRow.getRowNum(), progressReportTitleRow.getRowNum(), progressReportValueCell.getColumnIndex(), progressReportValueCell.getColumnIndex() + 5));

		Row prTitleRow = sheet.createRow(rowNumber++);
		Cell prTitleHeadingCell = prTitleRow.createCell(0);
		sheet.addMergedRegion(new CellRangeAddress(prTitleRow.getRowNum(), prTitleRow.getRowNum(), prTitleHeadingCell.getColumnIndex(), prTitleHeadingCell.getColumnIndex()+2));
		prTitleHeadingCell.setCellStyle(tableBodyStyle);
		prTitleHeadingCell.setCellValue(documentHeading + " Title");
		setBordersToMergedCells(sheet, new CellRangeAddress(prTitleRow.getRowNum(), prTitleRow.getRowNum(), prTitleHeadingCell.getColumnIndex(), prTitleHeadingCell.getColumnIndex() + 2));
		
		Cell prTitleValueCell = prTitleRow.createCell(3);
		sheet.addMergedRegion(new CellRangeAddress(prTitleRow.getRowNum(), prTitleRow.getRowNum(), prTitleValueCell.getColumnIndex(), prTitleValueCell.getColumnIndex()+5));
		prTitleValueCell.setCellStyle(tableHeadStyle);
		prTitleValueCell.setCellValue(awardProgressReport.getTitle());
		setBordersToMergedCells(sheet, new CellRangeAddress(prTitleRow.getRowNum(), prTitleRow.getRowNum(), prTitleValueCell.getColumnIndex(), prTitleValueCell.getColumnIndex() + 5));
		
		Row managingAgencyRow = sheet.createRow(rowNumber++);
		Cell managingAgencyHeadingCell = managingAgencyRow.createCell(0);
		sheet.addMergedRegion(new CellRangeAddress(managingAgencyRow.getRowNum(), managingAgencyRow.getRowNum(), managingAgencyHeadingCell.getColumnIndex(), managingAgencyHeadingCell.getColumnIndex()+2));
		managingAgencyHeadingCell.setCellStyle(tableBodyStyle);
		managingAgencyHeadingCell.setCellValue("Sponsor Name");
		setBordersToMergedCells(sheet, new CellRangeAddress(managingAgencyRow.getRowNum(), managingAgencyRow.getRowNum(), managingAgencyHeadingCell.getColumnIndex(), managingAgencyHeadingCell.getColumnIndex() + 2));

		Cell managingAgencyValueCell = managingAgencyRow.createCell(3);
		sheet.addMergedRegion(new CellRangeAddress(managingAgencyRow.getRowNum(), managingAgencyRow.getRowNum(), managingAgencyValueCell.getColumnIndex(), managingAgencyValueCell.getColumnIndex()+5));
		managingAgencyValueCell.setCellStyle(tableHeadStyle);
		Sponsor sponsor = awardProgressReport.getAward().getSponsor();
		if (sponsor != null) {
			managingAgencyValueCell.setCellValue(commonService.getSponsorFormatBySponsorDetail(sponsor.getSponsorCode(), sponsor.getSponsorName(), sponsor.getAcronym()));
		}
		else 
			managingAgencyValueCell.setCellValue("");
    	setBordersToMergedCells(sheet, new CellRangeAddress(managingAgencyRow.getRowNum(), managingAgencyRow.getRowNum(), managingAgencyValueCell.getColumnIndex(), managingAgencyValueCell.getColumnIndex() + 5));

		Row awardIdRow = sheet.createRow(rowNumber++);
		Cell awardIdHeadingCell = awardIdRow.createCell(0);
		sheet.addMergedRegion(new CellRangeAddress(awardIdRow.getRowNum(), awardIdRow.getRowNum(), awardIdHeadingCell.getColumnIndex(), awardIdHeadingCell.getColumnIndex()+2));
		awardIdHeadingCell.setCellStyle(tableBodyStyle);
		awardIdHeadingCell.setCellValue("Award Number");
		setBordersToMergedCells(sheet, new CellRangeAddress(awardIdRow.getRowNum(), awardIdRow.getRowNum(), awardIdHeadingCell.getColumnIndex(), awardIdHeadingCell.getColumnIndex() + 2));

		Cell awardIdValueCell = awardIdRow.createCell(3);
		sheet.addMergedRegion(new CellRangeAddress(awardIdRow.getRowNum(), awardIdRow.getRowNum(), awardIdValueCell.getColumnIndex(), awardIdValueCell.getColumnIndex()+5));
		awardIdValueCell.setCellStyle(tableHeadStyle);
		awardIdValueCell.setCellValue(awardProgressReport.getAwardNumber());
		setBordersToMergedCells(sheet, new CellRangeAddress(awardIdRow.getRowNum(), awardIdRow.getRowNum(), awardIdValueCell.getColumnIndex(), awardIdValueCell.getColumnIndex() + 5));

		Row titleRow = sheet.createRow(rowNumber++);
		Cell titleHeadingCell = titleRow.createCell(0);
		sheet.addMergedRegion(new CellRangeAddress(titleRow.getRowNum(), titleRow.getRowNum(), titleHeadingCell.getColumnIndex(), titleHeadingCell.getColumnIndex()+2));
		titleHeadingCell.setCellStyle(tableBodyStyle);
		titleHeadingCell.setCellValue("Award Title");
		setBordersToMergedCells(sheet, new CellRangeAddress(titleRow.getRowNum(), titleRow.getRowNum(), titleHeadingCell.getColumnIndex(), titleHeadingCell.getColumnIndex() + 2));

		Cell titleValueCell = titleRow.createCell(3);
		sheet.addMergedRegion(new CellRangeAddress(titleRow.getRowNum(), titleRow.getRowNum(), titleValueCell.getColumnIndex(), titleValueCell.getColumnIndex()+5));
		titleValueCell.setCellStyle(tableHeadStyle);
		count = convertStringToParagraph(titleValueCell, count, awardProgressReport.getAward().getTitle());
		setBordersToMergedCells(sheet, new CellRangeAddress(titleRow.getRowNum(), titleRow.getRowNum(), titleValueCell.getColumnIndex(), titleValueCell.getColumnIndex() + 5));

		Row externalProjIdRow = sheet.createRow(rowNumber++);
		Cell externalProjIdHeadingCell = externalProjIdRow.createCell(0);
		sheet.addMergedRegion(new CellRangeAddress(externalProjIdRow.getRowNum(), externalProjIdRow.getRowNum(), externalProjIdHeadingCell.getColumnIndex(), externalProjIdHeadingCell.getColumnIndex()+2));
		externalProjIdHeadingCell.setCellStyle(tableBodyStyle);
		externalProjIdHeadingCell.setCellValue("Funder reference number");
		setBordersToMergedCells(sheet, new CellRangeAddress(externalProjIdRow.getRowNum(), externalProjIdRow.getRowNum(), externalProjIdHeadingCell.getColumnIndex(), externalProjIdHeadingCell.getColumnIndex() + 2));

		Cell externalProjIdValueCell = externalProjIdRow.createCell(3);
		sheet.addMergedRegion(new CellRangeAddress(externalProjIdRow.getRowNum(), externalProjIdRow.getRowNum(), externalProjIdValueCell.getColumnIndex(), externalProjIdValueCell.getColumnIndex()+5));
		externalProjIdValueCell.setCellStyle(tableHeadStyle);
		externalProjIdValueCell.setCellValue(awardProgressReport.getAward() != null ? awardProgressReport.getAward().getSponsorAwardNumber() : "");
		setBordersToMergedCells(sheet, new CellRangeAddress(externalProjIdRow.getRowNum(), externalProjIdRow.getRowNum(), externalProjIdValueCell.getColumnIndex(), externalProjIdValueCell.getColumnIndex() +5));

		Row periodRow = sheet.createRow(rowNumber++);
		Cell periodHeadingCell = periodRow.createCell(0);
		sheet.addMergedRegion(new CellRangeAddress(periodRow.getRowNum(), periodRow.getRowNum(), periodHeadingCell.getColumnIndex(), periodHeadingCell.getColumnIndex()+2));
		periodHeadingCell.setCellStyle(tableBodyStyle);
		periodHeadingCell.setCellValue("Reporting Period");
		setBordersToMergedCells(sheet, new CellRangeAddress(periodRow.getRowNum(), periodRow.getRowNum(), periodHeadingCell.getColumnIndex(), periodHeadingCell.getColumnIndex() + 2));

		Cell periodValueCell = periodRow.createCell(3);
		sheet.addMergedRegion(new CellRangeAddress(periodRow.getRowNum(), periodRow.getRowNum(), periodValueCell.getColumnIndex(), periodValueCell.getColumnIndex()+5));
		periodValueCell.setCellStyle(tableHeadStyle);
		String startDate = commonService.convertDateFormatBasedOnTimeZone(awardProgressReport.getReportStartDate().getTime(),
					Constants.DEFAULT_DATE_FORMAT);
		String endDate = commonService.convertDateFormatBasedOnTimeZone(awardProgressReport.getReportEndDate().getTime(),
					Constants.DEFAULT_DATE_FORMAT);
		periodValueCell.setCellValue(startDate+" - "+endDate);
		setBordersToMergedCells(sheet, new CellRangeAddress(periodRow.getRowNum(), periodRow.getRowNum(), periodValueCell.getColumnIndex(), periodValueCell.getColumnIndex() + 5));
		
		Row statusRow = sheet.createRow(rowNumber++);
		Cell statusHeadingCell = statusRow.createCell(0);
		sheet.addMergedRegion(new CellRangeAddress(statusRow.getRowNum(), statusRow.getRowNum(), statusHeadingCell.getColumnIndex(), statusHeadingCell.getColumnIndex()+2));
		statusHeadingCell.setCellStyle(tableBodyStyle);
		statusHeadingCell.setCellValue("Progress Status");
		setBordersToMergedCells(sheet, new CellRangeAddress(statusRow.getRowNum(), statusRow.getRowNum(), statusHeadingCell.getColumnIndex(), statusHeadingCell.getColumnIndex() + 2));

		Cell statusValueCell = statusRow.createCell(3);
		statusValueCell.setCellStyle(tableHeadStyle);
		sheet.addMergedRegion(new CellRangeAddress(statusRow.getRowNum(), statusRow.getRowNum(), statusValueCell.getColumnIndex(), statusValueCell.getColumnIndex()+5));
		statusValueCell.setCellValue(awardProgressReport.getProgressReportStatus() != null ? awardProgressReport.getProgressReportStatus().getDescription() : "");
		setBordersToMergedCells(sheet, new CellRangeAddress(statusRow.getRowNum(), statusRow.getRowNum(), statusValueCell.getColumnIndex(), statusValueCell.getColumnIndex() + 5));

		Row dueDateRow = sheet.createRow(rowNumber++);
		Cell dueDateHeadingCell = dueDateRow.createCell(0);
		sheet.addMergedRegion(new CellRangeAddress(dueDateRow.getRowNum(), dueDateRow.getRowNum(), dueDateHeadingCell.getColumnIndex(), dueDateHeadingCell.getColumnIndex()+2));
		dueDateHeadingCell.setCellStyle(tableBodyStyle);
		dueDateHeadingCell.setCellValue("Due Date");
		setBordersToMergedCells(sheet, new CellRangeAddress(dueDateRow.getRowNum(), dueDateRow.getRowNum(), dueDateHeadingCell.getColumnIndex(), dueDateHeadingCell.getColumnIndex() + 2));

		Cell dueDateValueCell = dueDateRow.createCell(3);
		sheet.addMergedRegion(new CellRangeAddress(dueDateRow.getRowNum(), dueDateRow.getRowNum(), dueDateValueCell.getColumnIndex(), dueDateValueCell.getColumnIndex()+5));
		dueDateValueCell.setCellStyle(tableHeadStyle);
		String dueDate;
		if (awardProgressReport.getDueDate() != null) {
			Date reportDuedate = (Date) awardProgressReport.getDueDate();
			dueDate = commonService.convertDateFormatBasedOnTimeZone(reportDuedate.getTime(),
					Constants.DEFAULT_DATE_FORMAT);
		} else {
			dueDate = " ";
		}
		dueDateValueCell.setCellValue(dueDate);
		setBordersToMergedCells(sheet, new CellRangeAddress(dueDateRow.getRowNum(), dueDateRow.getRowNum(), dueDateValueCell.getColumnIndex(), dueDateValueCell.getColumnIndex() + 5));

		Row ceatedByRow = sheet.createRow(rowNumber++);
		Cell ceatedByHeadingCell = ceatedByRow.createCell(0);
		sheet.addMergedRegion(new CellRangeAddress(ceatedByRow.getRowNum(), ceatedByRow.getRowNum(), ceatedByHeadingCell.getColumnIndex(), ceatedByHeadingCell.getColumnIndex()+2));
		ceatedByHeadingCell.setCellStyle(tableBodyStyle);
		ceatedByHeadingCell.setCellValue("Created By");
		setBordersToMergedCells(sheet, new CellRangeAddress(ceatedByRow.getRowNum(), ceatedByRow.getRowNum(), ceatedByHeadingCell.getColumnIndex(), ceatedByHeadingCell.getColumnIndex() + 2));

		Cell ceatedByValueCell = ceatedByRow.createCell(3);
		sheet.addMergedRegion(new CellRangeAddress(ceatedByRow.getRowNum(), ceatedByRow.getRowNum(), ceatedByValueCell.getColumnIndex(), ceatedByValueCell.getColumnIndex()+5));
		ceatedByValueCell.setCellStyle(tableHeadStyle);
		ceatedByValueCell.setCellValue(personDao.getUserFullNameByUserName(awardProgressReport.getCreateUser()));
		setBordersToMergedCells(sheet, new CellRangeAddress(ceatedByRow.getRowNum(), ceatedByRow.getRowNum(), ceatedByValueCell.getColumnIndex(), ceatedByValueCell.getColumnIndex() + 5));

		Row lastModifiedRow = sheet.createRow(rowNumber++);
		Cell lastModifiedHeadingCell = lastModifiedRow.createCell(0);
		sheet.addMergedRegion(new CellRangeAddress(lastModifiedRow.getRowNum(), lastModifiedRow.getRowNum(), lastModifiedHeadingCell.getColumnIndex(), lastModifiedHeadingCell.getColumnIndex()+2));
		lastModifiedHeadingCell.setCellStyle(tableBodyStyle);
		lastModifiedHeadingCell.setCellValue("Last Modified By");
		setBordersToMergedCells(sheet, new CellRangeAddress(lastModifiedRow.getRowNum(), lastModifiedRow.getRowNum(), lastModifiedHeadingCell.getColumnIndex(), lastModifiedHeadingCell.getColumnIndex() + 2));

		Cell lastModifiedValueCell = lastModifiedRow.createCell(3);
		sheet.addMergedRegion(new CellRangeAddress(lastModifiedRow.getRowNum(), lastModifiedRow.getRowNum(), lastModifiedValueCell.getColumnIndex(), lastModifiedValueCell.getColumnIndex()+5));
		lastModifiedValueCell.setCellStyle(tableHeadStyle);
		lastModifiedValueCell.setCellValue(personDao.getUserFullNameByUserName(awardProgressReport.getUpdateUser()));
		setBordersToMergedCells(sheet, new CellRangeAddress(lastModifiedRow.getRowNum(), lastModifiedRow.getRowNum(), lastModifiedValueCell.getColumnIndex(), lastModifiedValueCell.getColumnIndex() + 5));

		if (count > 0) {
			titleRow.setHeightInPoints(titleValueCell.getStringCellValue().split("\n").length*sheet.getDefaultRowHeightInPoints());
		}

		return rowNumber;
	}

	public void setBordersToMergedCells(XSSFSheet sheet, CellRangeAddress rangeAddress) {
        RegionUtil.setBorderTop(BorderStyle.HAIR, rangeAddress, sheet);
        RegionUtil.setBorderLeft(BorderStyle.HAIR, rangeAddress, sheet);
        RegionUtil.setBorderRight(BorderStyle.HAIR, rangeAddress, sheet);
        RegionUtil.setBorderBottom(BorderStyle.HAIR, rangeAddress, sheet);
    }

	private void prepareExcelSheetForProgressReport(String documentHeading, AwardProgressReport awardProgressReport, XSSFSheet sheet, XSSFWorkbook workbook) throws Exception {
		XSSFCellStyle tableBodyStyle = workbook.createCellStyle();
		int rowNumber = 0;
		rowNumber = prepareProgressReportGeneralInformationForExcel(documentHeading, sheet, rowNumber, awardProgressReport, workbook);
		rowNumber = prepareSummaryReportDetail(sheet, ++rowNumber, awardProgressReport, workbook);
		rowNumber = prepareResearchMilestoneDetail(sheet, ++rowNumber, awardProgressReport.getProgressReportId(), tableBodyStyle, workbook);
		rowNumber = prepareRecordOfEquipment(sheet, ++rowNumber, awardProgressReport.getProgressReportId(), tableBodyStyle, workbook);
		rowNumber = prepareFutureReportDetail(sheet, ++rowNumber, awardProgressReport, workbook);
		rowNumber = preparePerformanceOfIndicatorDetail(sheet, ++rowNumber, awardProgressReport.getProgressReportId(), tableBodyStyle, workbook);
		rowNumber = prepareKPIDetail(sheet, ++rowNumber, awardProgressReport.getProgressReportId(), tableBodyStyle, workbook);
	}

	public Integer prepareSummaryReportDetail(XSSFSheet sheet, int rowNumber, AwardProgressReport awardProgressReport, XSSFWorkbook workbook) {
		sheet.addMergedRegion(new CellRangeAddress(rowNumber, rowNumber, 0, 8));
		setBordersToMergedCells(sheet, new CellRangeAddress(rowNumber, rowNumber, 0, 8));
		Row headerRow = sheet.createRow(rowNumber++);
		Cell headingCell = headerRow.createCell(0);
		headingCell.setCellValue("SECTION 1: SUMMARY OF PROGRESS");
		XSSFFont headerFont = workbook.createFont();
		headerFont.setBold(true);
		headerFont.setFontHeightInPoints((short) 12);
		XSSFCellStyle headerStyle = workbook.createCellStyle();
		headerStyle.setAlignment(HorizontalAlignment.CENTER);
		headerStyle.setFont(headerFont);
		headingCell.setCellStyle(headerStyle);
		// Table head style and font creation code.
		XSSFCellStyle tableHeadStyle = workbook.createCellStyle();
		tableHeadStyle.setBorderTop(BorderStyle.HAIR);
		tableHeadStyle.setBorderBottom(BorderStyle.HAIR);
		tableHeadStyle.setBorderLeft(BorderStyle.HAIR);
		tableHeadStyle.setBorderRight(BorderStyle.HAIR);
		XSSFFont tableHeadFont = workbook.createFont();
		tableHeadFont.setBold(true);
		tableHeadFont.setFontHeightInPoints((short) 12);
		tableHeadStyle.setFont(tableHeadFont);
		// Table body style and font creation code.
		XSSFCellStyle tableBodyStyle = workbook.createCellStyle();
		tableBodyStyle.setBorderTop(BorderStyle.HAIR);
		tableBodyStyle.setBorderBottom(BorderStyle.HAIR);
		tableBodyStyle.setBorderLeft(BorderStyle.HAIR);
		tableBodyStyle.setBorderRight(BorderStyle.HAIR);
		tableBodyStyle.setWrapText(true);
		XSSFFont tableBodyFont = workbook.createFont();
		tableBodyFont.setFontHeightInPoints((short) 12);
		tableBodyStyle.setFont(tableBodyFont);
		tableHeadStyle.setWrapText(true);
		
		AtomicInteger rowNumberCount = new AtomicInteger(rowNumber);
		awardProgressReport.getAwardProgressReportAchievements().stream().filter(achievement -> 
			achievement.getProgressReportAchievementType().getSectionType().equals(SUMMARY_SECTION_TYPE)).collect(Collectors.toList()).stream().
			forEach(achievement -> {
				Integer count =0;
				Row summaryTitleRow = sheet.createRow(rowNumberCount.getAndIncrement());
				Cell summaryHeadingCell = summaryTitleRow.createCell(0);
				summaryHeadingCell.setCellStyle(tableHeadStyle);
				sheet.addMergedRegion(new CellRangeAddress(summaryTitleRow.getRowNum(), summaryTitleRow.getRowNum(), summaryHeadingCell.getColumnIndex(),  summaryHeadingCell.getColumnIndex()+2));
				count = convertStringToParagraph(summaryHeadingCell, count, achievement.getProgressReportAchievementType().getDescription());
				setBordersToMergedCells(sheet, new CellRangeAddress(summaryTitleRow.getRowNum(), summaryTitleRow.getRowNum(), summaryHeadingCell.getColumnIndex(), summaryHeadingCell.getColumnIndex() + 2));
				summaryTitleRow.setHeightInPoints(summaryHeadingCell.getStringCellValue().split("\n").length*sheet.getDefaultRowHeightInPoints());
				
				Cell summaryValueCell = summaryTitleRow.createCell(3);
				summaryValueCell.setCellStyle(tableBodyStyle);
				sheet.addMergedRegion(new CellRangeAddress(summaryTitleRow.getRowNum(), summaryTitleRow.getRowNum(), summaryValueCell.getColumnIndex(), summaryValueCell.getColumnIndex()+5));
				setBordersToMergedCells(sheet, new CellRangeAddress(summaryTitleRow.getRowNum(), summaryTitleRow.getRowNum(), summaryValueCell.getColumnIndex(), summaryValueCell.getColumnIndex() + 5));
				count = convertStringToParagraph(summaryValueCell, count, achievement.getDescription());
				if (count > 0) {
					summaryTitleRow.setHeightInPoints(summaryTitleRow.getCell(count).getStringCellValue().split("\n").length*sheet.getDefaultRowHeightInPoints());
				}
				sheet.setColumnWidth(summaryHeadingCell.getColumnIndex() , 40 * 256);
				sheet.setColumnWidth(summaryValueCell.getColumnIndex() , 40 * 256);
			});
		return rowNumberCount.get();
	}

	public Integer prepareFutureReportDetail(XSSFSheet sheet, int rowNumber, AwardProgressReport awardProgressReport, XSSFWorkbook workbook) {
		sheet.addMergedRegion(new CellRangeAddress(rowNumber, rowNumber, 0, 8));
		setBordersToMergedCells(sheet, new CellRangeAddress(rowNumber, rowNumber, 0, 8));
		Row headerRow = sheet.createRow(rowNumber++);
		Cell headingCell = headerRow.createCell(0);
		headingCell.setCellValue("SECTION 4: FUTURE PLANS");
		XSSFFont headerFont = workbook.createFont();
		headerFont.setBold(true);
		headerFont.setFontHeightInPoints((short) 12);
		XSSFCellStyle headerStyle = workbook.createCellStyle();
		headerStyle.setAlignment(HorizontalAlignment.CENTER);
		headerStyle.setFont(headerFont);
		headingCell.setCellStyle(headerStyle);
		// Table head style and font creation code.
		XSSFCellStyle tableHeadStyle = workbook.createCellStyle();
		tableHeadStyle.setBorderTop(BorderStyle.HAIR);
		tableHeadStyle.setBorderBottom(BorderStyle.HAIR);
		tableHeadStyle.setBorderLeft(BorderStyle.HAIR);
		tableHeadStyle.setBorderRight(BorderStyle.HAIR);
		XSSFFont tableHeadFont = workbook.createFont();
		tableHeadFont.setBold(true);
		tableHeadFont.setFontHeightInPoints((short) 12);
		tableHeadStyle.setFont(tableHeadFont);
		// Table body style and font creation code.
		XSSFCellStyle tableBodyStyle = workbook.createCellStyle();
		tableBodyStyle.setBorderTop(BorderStyle.HAIR);
		tableBodyStyle.setBorderBottom(BorderStyle.HAIR);
		tableBodyStyle.setBorderLeft(BorderStyle.HAIR);
		tableBodyStyle.setBorderRight(BorderStyle.HAIR);
		tableBodyStyle.setWrapText(true);
		XSSFFont tableBodyFont = workbook.createFont();
		tableBodyFont.setFontHeightInPoints((short) 12);
		tableBodyStyle.setFont(tableBodyFont);
		tableHeadStyle.setWrapText(true);

		AtomicInteger rowNumberCount = new AtomicInteger(rowNumber);
		awardProgressReport.getAwardProgressReportAchievements().stream().filter(achievement -> 
			achievement.getProgressReportAchievementType().getSectionType().equals(FUTURE_SECTION_TYPE)).collect(Collectors.toList()).stream().
			forEach(achievement -> {
				Integer count = 0;
				Row summaryTitleRow = sheet.createRow(rowNumberCount.getAndIncrement());
				sheet.addMergedRegion(new CellRangeAddress(summaryTitleRow.getRowNum(), summaryTitleRow.getRowNum(),0, 2));
				Cell summaryHeadingCell = summaryTitleRow.createCell(0);
				summaryHeadingCell.setCellStyle(tableHeadStyle);
				count = convertStringToParagraph(summaryHeadingCell, count, achievement.getProgressReportAchievementType().getDescription());
				summaryTitleRow.setHeightInPoints(summaryHeadingCell.getStringCellValue().split("\n").length*sheet.getDefaultRowHeightInPoints());
				setBordersToMergedCells(sheet, new CellRangeAddress(summaryTitleRow.getRowNum(), summaryTitleRow.getRowNum(), 0, 2));

				Cell summaryValueCell = summaryTitleRow.createCell(3);
				sheet.addMergedRegion(new CellRangeAddress(summaryTitleRow.getRowNum(), summaryTitleRow.getRowNum(),summaryValueCell.getColumnIndex(), summaryValueCell.getColumnIndex()+5));
				summaryValueCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(summaryValueCell, count, achievement.getDescription());
				setBordersToMergedCells(sheet, new CellRangeAddress(summaryTitleRow.getRowNum(), summaryTitleRow.getRowNum(),summaryValueCell.getColumnIndex(), summaryValueCell.getColumnIndex()+5));
				if (count > 0) {
					summaryTitleRow.setHeightInPoints(summaryTitleRow.getCell(count).getStringCellValue().split("\n").length*sheet.getDefaultRowHeightInPoints());
				}
				sheet.setColumnWidth(summaryHeadingCell.getColumnIndex() , 40 * 256);
				sheet.setColumnWidth(summaryValueCell.getColumnIndex() , 40 * 256);
			});
		return rowNumberCount.get();
	}

	public Integer prepareResearchMilestoneDetail(XSSFSheet sheet, int rowNumber, Integer progressReportId, XSSFCellStyle tableBodyStyle, XSSFWorkbook workbook) {
		sheet.addMergedRegion(new CellRangeAddress(rowNumber, rowNumber, 0, 8));
		setBordersToMergedCells(sheet, new CellRangeAddress(rowNumber, rowNumber, 0, 8));
		String isHeaderChange = "ResearchMilestone";
		Row headerRow = sheet.createRow(rowNumber++);
		Cell headingCell = headerRow.createCell(0);
		headingCell.setCellValue("SECTION 2: RESEARCH MILESTONE");
		XSSFFont headerFont = workbook.createFont();
		headerFont.setBold(true);
		headerFont.setFontHeightInPoints((short) 12);
		XSSFCellStyle headerStyle = workbook.createCellStyle();
		headerStyle.setAlignment(HorizontalAlignment.CENTER);
		headerStyle.setFont(headerFont);
		headingCell.setCellStyle(headerStyle);

		Object[] researchMilestoneHeading = { "No", "Research Milestone", "Start Date", "End Date", "Actual Start Date", "Actual End Date", "Status", "Remark" };
		prepareExcelSheetHeaderForPRogressReport(isHeaderChange, sheet, researchMilestoneHeading, workbook, tableBodyStyle, rowNumber++);
		
		List<AwardProgressReportMilestone> awardProgressMileStones = progressReportDao.loadProgressReportMilestone(progressReportId);
		List<String> mileStoneNumbers = awardProgressMileStones.stream().map(AwardProgressReportMilestone::getMilestoneNumber).collect(Collectors.toList());
		if (!mileStoneNumbers.isEmpty()) {
			List<AwardMileStone> awardMilestones = progressReportDao.getAwardMilestoneByParam(progressReportId);
			Map<String, List<AwardMileStone>> collect = awardMilestones.stream().collect(Collectors.groupingBy(AwardMileStone::getMilestoneNumber));
			awardProgressMileStones.stream().filter(item -> collect.containsKey(item.getMilestoneNumber())).forEach(item -> item.setAwardMileStone(collect.get(item.getMilestoneNumber()).get(0)));
			Comparator<AwardProgressReportMilestone> comparator = Comparator.comparing(milestone -> milestone.getAwardMileStone().getStartDate());
		    Comparator<AwardProgressReportMilestone> reverseComparator = comparator.reversed().thenComparing(milestone -> milestone.getAwardMileStone().getMilestone());
			awardProgressMileStones = awardProgressMileStones.stream().filter(milestone -> milestone.getAwardMileStone() != null && milestone.getAwardMileStone().getStartDate() != null)
					.sorted(reverseComparator).collect(Collectors.toList());
		}
		if (!awardProgressMileStones.isEmpty()) {
			int serialNumber = 0;
			XSSFCellStyle cellStyleSlno = workbook.createCellStyle();
			cellStyleSlno.setBorderTop(BorderStyle.HAIR);
			cellStyleSlno.setBorderBottom(BorderStyle.HAIR);
			cellStyleSlno.setBorderLeft(BorderStyle.HAIR);
			cellStyleSlno.setBorderRight(BorderStyle.HAIR);
			cellStyleSlno.setAlignment(HorizontalAlignment.LEFT);
			tableBodyStyle.setWrapText(true);
			for (AwardProgressReportMilestone awardProgressMileStone : awardProgressMileStones) {
				int cellNumber = 0;
				Integer count = 0;
				Row researchMilestone = sheet.createRow(rowNumber++);
				Cell serialNumberCell = researchMilestone.createCell(cellNumber);
				serialNumberCell.setCellStyle(cellStyleSlno);
				serialNumberCell.setCellValue(++serialNumber);
				cellNumber++;
				
				Cell researchMilestoneCell = researchMilestone.createCell(cellNumber);
				researchMilestoneCell.setCellStyle(tableBodyStyle);
				sheet.addMergedRegion(new CellRangeAddress(researchMilestone.getRowNum(), researchMilestone.getRowNum(), researchMilestoneCell.getColumnIndex(), researchMilestoneCell.getColumnIndex()+1));
				setBordersToMergedCells(sheet, new CellRangeAddress(researchMilestone.getRowNum(), researchMilestone.getRowNum(), researchMilestoneCell.getColumnIndex(), researchMilestoneCell.getColumnIndex() + 1));
				count = convertStringToParagraph(researchMilestoneCell, count, awardProgressMileStone.getAwardMileStone() != null ? awardProgressMileStone.getAwardMileStone().getMilestone() : "");
				cellNumber = researchMilestoneCell.getColumnIndex()+1;
				cellNumber++;

				Cell committedStartCell = researchMilestone.createCell(cellNumber);
				committedStartCell.setCellStyle(tableBodyStyle);
				if (awardProgressMileStone.getAwardMileStone() != null && awardProgressMileStone.getAwardMileStone().getStartDate() != null) {
					Date committedStart = (Date) awardProgressMileStone.getAwardMileStone().getStartDate();
					String committedStartDate = commonService.convertDateFormatBasedOnTimeZone(committedStart.getTime(),
							Constants.DEFAULT_DATE_FORMAT);
					committedStartCell.setCellValue(committedStartDate);
				} else {
					committedStartCell.setCellValue("");
				}
				sheet.setColumnWidth(committedStartCell.getColumnIndex() , 10 * 256);
				cellNumber++;
				
				Cell committedEndCell = researchMilestone.createCell(cellNumber);
				committedEndCell.setCellStyle(tableBodyStyle);
				if (awardProgressMileStone.getAwardMileStone() != null && awardProgressMileStone.getAwardMileStone().getEndDate() != null) {
					Date committedEnd = (Date) awardProgressMileStone.getAwardMileStone().getEndDate();
					String committedEndDate = commonService.convertDateFormatBasedOnTimeZone(committedEnd.getTime(),
							Constants.DEFAULT_DATE_FORMAT);
					committedEndCell.setCellValue(committedEndDate);
				} else {
					committedEndCell.setCellValue("");
				}
				sheet.setColumnWidth(committedEndCell.getColumnIndex() , 10 * 256);
				cellNumber++;

				Cell reportedStartCell = researchMilestone.createCell(cellNumber);
				reportedStartCell.setCellStyle(tableBodyStyle);
				if (awardProgressMileStone.getActualStartMonth() != null) {
					String reportedStartDate = commonService.convertDateFormatBasedOnTimeZone(awardProgressMileStone.getActualStartMonth().getTime(),
							Constants.DEFAULT_DATE_FORMAT);
					reportedStartCell.setCellValue(reportedStartDate);
				} else {
					reportedStartCell.setCellValue("");
				}
				sheet.setColumnWidth(reportedStartCell.getColumnIndex() , 15 * 256);
				researchMilestone.setHeightInPoints(reportedStartCell.getStringCellValue().split("\n").length*sheet.getDefaultRowHeightInPoints());
				cellNumber++;
				
				Cell reportedEndCell = researchMilestone.createCell(cellNumber);
				reportedEndCell.setCellStyle(tableBodyStyle);
				if (awardProgressMileStone.getActualEndMonth() != null) {
					String reportedEndDate = commonService.convertDateFormatBasedOnTimeZone(awardProgressMileStone.getActualEndMonth().getTime(),
							Constants.DEFAULT_DATE_FORMAT);
					reportedEndCell.setCellValue(reportedEndDate);
				} else {
					reportedEndCell.setCellValue("");
				}
				sheet.setColumnWidth(reportedEndCell.getColumnIndex() , 15 * 256);
				researchMilestone.setHeightInPoints(reportedEndCell.getStringCellValue().split("\n").length*sheet.getDefaultRowHeightInPoints());
				cellNumber++;

				Cell statusCell = researchMilestone.createCell(cellNumber);
				statusCell.setCellStyle(tableBodyStyle);
				statusCell.setCellValue(awardProgressMileStone.getMilestoneStatus() != null ? awardProgressMileStone.getMilestoneStatus().getDescription() : "");
				sheet.setColumnWidth(statusCell.getColumnIndex() , 10 * 256);
				researchMilestone.setHeightInPoints(statusCell.getStringCellValue().split("\n").length*sheet.getDefaultRowHeightInPoints());
				cellNumber++;
				
				Cell remarkCell = researchMilestone.createCell(cellNumber);
				remarkCell.setCellStyle(tableBodyStyle);
				sheet.addMergedRegion(new CellRangeAddress(researchMilestone.getRowNum(), researchMilestone.getRowNum(), remarkCell.getColumnIndex(), remarkCell.getColumnIndex()+1));
				setBordersToMergedCells(sheet, new CellRangeAddress(researchMilestone.getRowNum(), researchMilestone.getRowNum(), remarkCell.getColumnIndex(), remarkCell.getColumnIndex() + 1));
				count = convertStringToParagraph(remarkCell, count, awardProgressMileStone.getRemark()!= null ? awardProgressMileStone.getRemark() : "");
				cellNumber++;
				if (count > 0) {
					researchMilestone.setHeightInPoints(researchMilestone.getCell(count).getStringCellValue().split("\n").length*sheet.getDefaultRowHeightInPoints());
				}
			}
		}
		return rowNumber;
	}

	public Integer preparePerformanceOfIndicatorDetail(XSSFSheet sheet, int rowNumber, Integer progressReportId, XSSFCellStyle tableBodyStyle, XSSFWorkbook workbook) {
		sheet.addMergedRegion(new CellRangeAddress(rowNumber, rowNumber, 0, 8));
		setBordersToMergedCells(sheet, new CellRangeAddress(rowNumber, rowNumber, 0, 8));
		String isHeaderChange = "performanceOfIndicator";
		Row headerRow = sheet.createRow(rowNumber++);
		Cell headingCell = headerRow.createCell(0);
		headingCell.setCellValue("SECTION 5: PERFORMANCE INDICATORS");
		XSSFFont headerFont = workbook.createFont();
		headerFont.setBold(true);
		headerFont.setFontHeightInPoints((short) 12);
		XSSFCellStyle headerStyle = workbook.createCellStyle();
		headerStyle.setAlignment(HorizontalAlignment.CENTER);
		headerStyle.setFont(headerFont);
		headingCell.setCellStyle(headerStyle);

		List<AwardProgressReportKPISummary> awardProgressReportKPISummaryList = progressReportDao.loadProgressReportKPISummary(progressReportId);
		awardProgressReportKPISummaryList = awardProgressReportKPISummaryList.stream()
				.filter(kpi -> kpi.getOriginatingProgressReportId().equals(progressReportId))
				.sorted(Comparator.comparing(AwardProgressReportKPISummary::getKpiCategoryTypeCode)
				.thenComparing(summary -> summary.getKpiCriteriaType().getDescription())).collect(Collectors.toList());
		awardProgressReportKPISummaryList.forEach(kpiSummary -> {
			commonDao.detachEntityFromSession(kpiSummary.getKpiType());
			getKPICriteriaAchievedValueHistory(kpiSummary,progressReportId);
			kpiSummary.getKpiType().setKpiCriteriaType(null);
		});
		List<Map<Object, Object>> progressReportNumbers = new ArrayList<>();
		if(!awardProgressReportKPISummaryList.isEmpty())
			progressReportNumbers = progressReportDao.getAllProgressReportNumberAndDueDateOfAward(awardProgressReportKPISummaryList.get(0).getAwardProgressReport().getProgressReportId());
		Object[] performanceIndicatorHeading = { "No", "KPI Category", "KPI Criteria", "Target" };
		for (Map<Object, Object> reportNumber : progressReportNumbers) {			
			String dueDate = reportNumber.get("dueDate").toString();
			try {
				String dueDateUtil = commonService.convertDateFormatBasedOnTimeZone(new SimpleDateFormat("yyyy/MM/dd").parse(dueDate.substring(0,10).replace("-","/")).getTime(),
						Constants.DEFAULT_DATE_FORMAT);
				String achievedYear = commonService.convertDateFormatBasedOnTimeZone(new SimpleDateFormat("yyyy/MM/dd").parse(dueDate.substring(0,10).replace("-","/")).getTime(),
						Constants.PROGRESS_REPORT_DATE_FORMAT);
				performanceIndicatorHeading = appendHeader(performanceIndicatorHeading, "Achieved as at " + achievedYear + " (Due date:" + dueDateUtil +")") ;
			} catch (ParseException e) {			
				e.printStackTrace();
			}
		}
		performanceIndicatorHeading = appendHeader(performanceIndicatorHeading, "Total Achieved");	
		prepareExcelSheetHeaderForPRogressReport(isHeaderChange, sheet, performanceIndicatorHeading, workbook, tableBodyStyle, rowNumber++);

		if (!awardProgressReportKPISummaryList.isEmpty()) {
			int serialNumber = 0;
			XSSFCellStyle cellStyleSlno = workbook.createCellStyle();
			cellStyleSlno.setBorderTop(BorderStyle.HAIR);
			cellStyleSlno.setBorderBottom(BorderStyle.HAIR);
			cellStyleSlno.setBorderLeft(BorderStyle.HAIR);
			cellStyleSlno.setBorderRight(BorderStyle.HAIR);
			cellStyleSlno.setAlignment(HorizontalAlignment.LEFT);
			tableBodyStyle.setWrapText(true);
			for (AwardProgressReportKPISummary awardProgressReportKPISummary : awardProgressReportKPISummaryList) {
				int cellNumber = 0;
				Integer count = 0;
				Row performanceIndicatorRow = sheet.createRow(rowNumber++);
				Cell serialNumberCell = performanceIndicatorRow.createCell(cellNumber);
				serialNumberCell.setCellStyle(cellStyleSlno);
				serialNumberCell.setCellValue(++serialNumber);
				cellNumber++;

				Cell kpiCategoryCell = performanceIndicatorRow.createCell(cellNumber);
				kpiCategoryCell.setCellStyle(tableBodyStyle);
				sheet.addMergedRegion(new CellRangeAddress(performanceIndicatorRow.getRowNum(), performanceIndicatorRow.getRowNum(), kpiCategoryCell.getColumnIndex(),  kpiCategoryCell.getColumnIndex()+1));
				setBordersToMergedCells(sheet, new CellRangeAddress(performanceIndicatorRow.getRowNum(), performanceIndicatorRow.getRowNum(), kpiCategoryCell.getColumnIndex(), kpiCategoryCell.getColumnIndex() + 1));
				count = convertStringToParagraph(kpiCategoryCell, count, awardProgressReportKPISummary.getKpiType() != null ? awardProgressReportKPISummary.getKpiType().getDescription() : "");
				cellNumber = kpiCategoryCell.getColumnIndex()+1;
				cellNumber++;

				Cell kpiCriteriaCell = performanceIndicatorRow.createCell(cellNumber);
				kpiCriteriaCell.setCellStyle(tableBodyStyle);
				sheet.addMergedRegion(new CellRangeAddress(performanceIndicatorRow.getRowNum(), performanceIndicatorRow.getRowNum(), kpiCriteriaCell.getColumnIndex(),  kpiCriteriaCell.getColumnIndex()+1));
				setBordersToMergedCells(sheet, new CellRangeAddress(performanceIndicatorRow.getRowNum(), performanceIndicatorRow.getRowNum(), kpiCriteriaCell.getColumnIndex(), kpiCriteriaCell.getColumnIndex() + 1));
				count = convertStringToParagraph(kpiCriteriaCell, count, awardProgressReportKPISummary.getKpiCriteriaType() != null ? awardProgressReportKPISummary.getKpiCriteriaType().getDescription() : "");
				cellNumber = kpiCriteriaCell.getColumnIndex()+1;
				cellNumber++;

				Cell targetCell = performanceIndicatorRow.createCell(cellNumber);
				targetCell.setCellStyle(tableBodyStyle);
				targetCell.setCellValue(awardProgressReportKPISummary.getTarget() != null ? awardProgressReportKPISummary.getTarget().toString() : "");
				cellNumber++;

				if (awardProgressReportKPISummary.getAchievedValues() != null) {	
					for (Map.Entry<Object, Object> achievedValues : awardProgressReportKPISummary.getAchievedValues().entrySet()) {
						Cell achievedCell = performanceIndicatorRow.createCell(cellNumber);
						achievedCell.setCellStyle(tableBodyStyle);
						achievedCell.setCellValue(achievedValues.getValue().toString());
						cellNumber++;
					}
				}

				Cell totalAchievedCell = performanceIndicatorRow.createCell(cellNumber);
				totalAchievedCell.setCellStyle(tableBodyStyle);
				totalAchievedCell.setCellValue(awardProgressReportKPISummary.getTotalAchieved() != null ? awardProgressReportKPISummary.getTotalAchieved().toString() : "");
				if (count > 0) {
					performanceIndicatorRow.setHeightInPoints(performanceIndicatorRow.getCell(count).getStringCellValue().split("\n").length*sheet.getDefaultRowHeightInPoints());
				}
			}
		}
		return rowNumber;
	}

	private void getKPICriteriaAchievedValueHistory(AwardProgressReportKPISummary summary, Integer progressReportId) {
		double count = 0.00;
		List<Object[]> achievedValueHistory = progressReportDao.getKPICriteriaAchievedValueHistory(summary.getKpiCriteriaTypeCode(), progressReportId);
		count = achievedValueHistory.stream().filter(history -> history[1] != null).mapToDouble(history -> Double.parseDouble(history[1].toString())).sum();
		Map<Object, Object> achievedValues = achievedValueHistory.stream().collect(Collectors.toMap(history -> history[0], history -> history[1] == null ? 0.00 : history[1]));
		summary.setTotalAchieved(count);
		summary.setAchievedValues(achievedValues);
	}

	private Object[] appendHeader(Object[] headers, Object header) {
		ArrayList<Object> newHeader = new ArrayList<>(Arrays.asList(headers));
		newHeader.add(header);
		return newHeader.toArray();
	}

	public Integer prepareRecordOfEquipment(XSSFSheet sheet, int rowNumber, Integer progressReportId, XSSFCellStyle tableBodyStyle, XSSFWorkbook workbook) throws Exception {
		sheet.addMergedRegion(new CellRangeAddress(rowNumber, rowNumber, 0, 8));
		setBordersToMergedCells(sheet, new CellRangeAddress(rowNumber, rowNumber, 0, 8));
		Row headerRow = sheet.createRow(rowNumber++);
		Cell headingCell = headerRow.createCell(0);
		headingCell.setCellValue("SECTION 3: QUESTIONNAIRE");
		XSSFFont headerFont = workbook.createFont();
		headerFont.setBold(true);
		headerFont.setFontHeightInPoints((short) 12);
		XSSFCellStyle headerStyle = workbook.createCellStyle();
		headerStyle.setAlignment(HorizontalAlignment.CENTER);
		headerStyle.setFont(headerFont);
		headingCell.setCellStyle(headerStyle);

		String logginPersonId = AuthenticatedUser.getLoginPersonId();
		String updateUser = AuthenticatedUser.getLoginUserName();
		List<HashMap<String, Object>> mapList = questionnaireDAO.getApplicableQuestionnaireData(progressReportId.toString(), Constants.SUBMODULE_ITEM_KEY, Constants.PROGRESS_REPORT_MODULE_CODE, Constants.PROGRESS_REPORT_SUBMODULE_CODE, logginPersonId, updateUser);
		CommonVO vo = new CommonVO();
		vo.setPersonId(logginPersonId);
		vo.setUserName(updateUser);
		vo.setModuleCode(Constants.PROGRESS_REPORT_MODULE_CODE);
		vo.setModuleItemKey(progressReportId.toString());
		vo.setSubModuleCode(Constants.PROGRESS_REPORT_SUBMODULE_CODE);
		vo.setSubModuleItemKey(Constants.SUBMODULE_ITEM_KEY);
		for (HashMap<String, Object> map : mapList) {
			vo.setQuestionnaireId(Integer.parseInt(map.get("QUESTIONNAIRE_ID").toString()));
			List<QuestionnairePrintParameter> questionnairePrintParameters = printService.setQuestionnaireDataBasedOnQuestionnaireId(vo);
			if (!questionnairePrintParameters.isEmpty()) {
				QuestionnairePrintParameter questionnairePrintParameter = questionnairePrintParameters.get(0);
				rowNumber = prepareExcelSheetForQuestionnaire(questionnairePrintParameter, rowNumber, sheet, workbook, tableBodyStyle);
			}
		}
		return rowNumber;
	}

	public Integer prepareExcelSheetForQuestionnaire(QuestionnairePrintParameter questionnaire, int rowNumber, XSSFSheet sheet, XSSFWorkbook workbook, XSSFCellStyle tableBodyStyle) {
		// Excel sheet heading style and font creation code.
		String isHeaderChange = "Questionnaire";
		sheet.addMergedRegion(new CellRangeAddress(rowNumber, rowNumber, 0, 5));
		setBordersToMergedCells(sheet, new CellRangeAddress(rowNumber, rowNumber, 0, 5));
		Row headerRow = sheet.createRow(rowNumber++);
		Cell headingCell = headerRow.createCell(0);
		headingCell.setCellValue(questionnaire.getQuestionnaireName());
		XSSFFont headerFont = workbook.createFont();
		headerFont.setBold(true);
		headerFont.setFontHeightInPoints((short) 12);
		XSSFCellStyle headerStyle = workbook.createCellStyle();
		headerStyle.setAlignment(HorizontalAlignment.LEFT);
		headerStyle.setFont(headerFont);
		headingCell.setCellStyle(headerStyle);
		// Set table body data to each column.
		int questionNumber = 1;
		Object[] tableHeadingRowData = null;
		if (questionnaire.getMaximumNumberOfAnswers() > 0) {
			tableHeadingRowData = new Object[questionnaire.getMaximumNumberOfAnswers() + 2];
			tableHeadingRowData[0] = "Question No";
			tableHeadingRowData[1] = "Question";
			int answerNumber = 1;
			for (int index = 2; index < tableHeadingRowData.length; index++) {
				tableHeadingRowData[index] = "Answer" + answerNumber;
				answerNumber++;
			}
		} else {
			tableHeadingRowData = new Object[3];
			tableHeadingRowData[0] = "Question No";
			tableHeadingRowData[1] = "Question";
			tableHeadingRowData[2] = "Answer";
		}
		prepareExcelSheetHeaderForPRogressReport(isHeaderChange, sheet, tableHeadingRowData, workbook, tableBodyStyle, rowNumber++);

		XSSFCellStyle cellStyleSlno = workbook.createCellStyle();
		cellStyleSlno.setBorderTop(BorderStyle.HAIR);
		cellStyleSlno.setBorderBottom(BorderStyle.HAIR);
		cellStyleSlno.setBorderLeft(BorderStyle.HAIR);
		cellStyleSlno.setBorderRight(BorderStyle.HAIR);
		cellStyleSlno.setAlignment(HorizontalAlignment.LEFT);
		tableBodyStyle.setWrapText(true);
		for (QuestionAndAnswer questionAndAnswer : questionnaire.getQuestionAndAnswerList()) {
			Row row = sheet.createRow(rowNumber++);
			int cellNumber = 0;
			Integer count = 0;
			Cell questionNumberCell = row.createCell(cellNumber++);			
			questionNumberCell.setCellStyle(cellStyleSlno);
			questionNumberCell.setCellValue(questionNumber);
			questionNumber++;

			Cell questionCell = row.createCell(cellNumber++);
			questionCell.setCellStyle(tableBodyStyle);
			sheet.addMergedRegion(new CellRangeAddress(row.getRowNum(), row.getRowNum(), questionCell.getColumnIndex(),  questionCell.getColumnIndex()+1));
			count = convertStringToParagraph(questionCell, count, questionAndAnswer.getQuestion());
			setBordersToMergedCells(sheet, new CellRangeAddress(row.getRowNum(), row.getRowNum(), questionCell.getColumnIndex(), questionCell.getColumnIndex() + 1));
			cellNumber = questionCell.getColumnIndex()+2;
			row.setHeightInPoints(questionCell.getStringCellValue().split("\n").length*sheet.getDefaultRowHeightInPoints());
			sheet.setColumnWidth(questionCell.getColumnIndex() , 40 * 256);

			for (String answer : questionAndAnswer.getAnswersList()) {
				Cell answerCell = row.createCell(cellNumber++);
				answerCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(answerCell, count, answer);
				sheet.setColumnWidth(answerCell.getColumnIndex() , 30 * 256);
			}
		}
		return rowNumber;
	}

	private XSSFWorkbook prepareExcelSheetHeaderForPRogressReport(String isHeaderChange, XSSFSheet sheet, Object[] tableHeadingRow, XSSFWorkbook workbook,
			XSSFCellStyle tableBodyStyle, int rowNumber) {
		int headingCellNumber = 0;
		Row tableHeadRow = sheet.createRow(rowNumber);
		XSSFCellStyle tableHeadStyle = workbook.createCellStyle();
		tableHeadStyle.setBorderTop(BorderStyle.HAIR);
		tableHeadStyle.setBorderBottom(BorderStyle.HAIR);
		tableHeadStyle.setBorderLeft(BorderStyle.HAIR);
		tableHeadStyle.setBorderRight(BorderStyle.HAIR);
		XSSFFont tableHeadFont = workbook.createFont();
		tableHeadFont.setBold(true);
		tableHeadFont.setFontHeightInPoints((short) 12);
		tableHeadStyle.setFont(tableHeadFont);
		// Table body style and font creation code.
		tableBodyStyle.setBorderTop(BorderStyle.HAIR);
		tableBodyStyle.setBorderBottom(BorderStyle.HAIR);
		tableBodyStyle.setBorderLeft(BorderStyle.HAIR);
		tableBodyStyle.setBorderRight(BorderStyle.HAIR);
		XSSFFont tableBodyFont = workbook.createFont();
		tableBodyFont.setFontHeightInPoints((short) 12);
		tableBodyStyle.setFont(tableBodyFont);
		// Set table head data to each column.
		if (isHeaderChange != null) {
			prepareHeaderForProgressReport(workbook, headingCellNumber, tableHeadingRow, isHeaderChange, tableHeadRow, tableHeadStyle, sheet);
		} else {
			for (Object heading : tableHeadingRow) {
				Cell cell = tableHeadRow.createCell(headingCellNumber++);
				cell.setCellValue((String) heading);
				cell.setCellStyle(tableHeadStyle);
			}
		}
		return workbook;
	}

	private XSSFWorkbook prepareHeaderForProgressReport(XSSFWorkbook workbook, int headingCellNumber, Object[] tableHeadingRow, String isHeaderChange, Row tableHeadRow, XSSFCellStyle tableHeadStyle, XSSFSheet sheet) {
		for (Object heading : tableHeadingRow) {
			Cell cell = null;
			if (isHeaderChange.equals("Questionnaire")) {
				if (heading.equals("Question")) {
					cell = tableHeadRow.createCell(headingCellNumber++);
					sheet.addMergedRegion(new CellRangeAddress(tableHeadRow.getRowNum(), tableHeadRow.getRowNum(), cell.getColumnIndex(),  cell.getColumnIndex()+1));
					headingCellNumber = cell.getColumnIndex() + 2;
					setBordersToMergedCells(sheet, new CellRangeAddress(tableHeadRow.getRowNum(), tableHeadRow.getRowNum(), cell.getColumnIndex(), cell.getColumnIndex() + 1));
				} else {
					cell = tableHeadRow.createCell(headingCellNumber++);
				}
			} else if (isHeaderChange.equals("performanceOfIndicator")) {
				if(heading.equals("KPI Category") || heading.equals("KPI Criteria")) {
					cell = tableHeadRow.createCell(headingCellNumber++);
					sheet.addMergedRegion(new CellRangeAddress(tableHeadRow.getRowNum(), tableHeadRow.getRowNum(), cell.getColumnIndex(),  cell.getColumnIndex()+1));
					headingCellNumber = cell.getColumnIndex() + 2;
				} else {
					cell = tableHeadRow.createCell(headingCellNumber++);
				}
			} else if(isHeaderChange.equals("ResearchMilestone")){
				if(heading.equals("Research Milestone")) {
					cell = tableHeadRow.createCell(headingCellNumber++);
					sheet.addMergedRegion(new CellRangeAddress(tableHeadRow.getRowNum(), tableHeadRow.getRowNum(), cell.getColumnIndex(), cell.getColumnIndex()+1));
					headingCellNumber = cell.getColumnIndex() + 2;
					setBordersToMergedCells(sheet, new CellRangeAddress(tableHeadRow.getRowNum(), tableHeadRow.getRowNum(), cell.getColumnIndex(), cell.getColumnIndex()+1));
				} else {
					cell = tableHeadRow.createCell(headingCellNumber++);
				}
			}
			if (cell != null) {
				if(heading.equals("No") || heading.equals("Question No")) {
					sheet.setColumnWidth(cell.getColumnIndex() , 9*256);
				}
				cell.setCellValue((String) heading);
				cell.setCellStyle(tableHeadStyle);
			}
		}
		return workbook;
	}

	public Integer prepareKPIDetail(XSSFSheet sheet, int rowNumber, Integer progressReportId, XSSFCellStyle tableBodyStyle, XSSFWorkbook workbook) {
		List<AwardProgressReportKPISummary> awardProgressReportKPISummaryList = progressReportDao.loadProgressReportKPISummary(progressReportId);
		awardProgressReportKPISummaryList.forEach(kpiSummary -> {
			commonDao.detachEntityFromSession(kpiSummary.getKpiType());
			getKPICriteriaAchievedValueHistory(kpiSummary, progressReportId);
			kpiSummary.getKpiType().setKpiCriteriaType(null);
		});
		List<AwardProgressReportKPISummary> awardProgressReportKPISummaryDetails = awardProgressReportKPISummaryList.stream()
				.filter(kpi -> kpi.getOriginatingProgressReportId().equals(progressReportId))
				.sorted(Comparator.comparing(AwardProgressReportKPISummary :: getKpiCategoryTypeCode).thenComparing(summary -> summary.getKpiCriteriaType().getDescription()))
				.collect(Collectors.toList());
		int serialNumber = 1;
		for (AwardProgressReportKPISummary awardProgressReportKPISummary : awardProgressReportKPISummaryDetails) {
			Integer headCount = 0;
			Row headerRow = sheet.createRow(rowNumber++);
			XSSFCellStyle tableHeadStyle = workbook.createCellStyle();
			tableHeadStyle.setBorderTop(BorderStyle.HAIR);
			tableHeadStyle.setBorderBottom(BorderStyle.HAIR);
			tableHeadStyle.setBorderLeft(BorderStyle.HAIR);
			tableHeadStyle.setBorderRight(BorderStyle.HAIR);
			XSSFFont tableHeadFont = workbook.createFont();
			tableHeadFont.setBold(true);
			tableHeadFont.setFontHeightInPoints((short) 12);
			tableHeadStyle.setFont(tableHeadFont);
			for(int i = 0; i < awardProgressReportKPISummary.getKpiCriteriaType().getDescription().length(); i++) {
				if (i % 30 == 0) {
					headCount ++;
				}
			}
			Cell headingCell = headerRow.createCell(0);
			if (headCount > 0) {
				sheet.addMergedRegion(new CellRangeAddress(headerRow.getRowNum(), headerRow.getRowNum(), headingCell.getColumnIndex(), headingCell.getColumnIndex()+8));
			}
			headingCell.setCellValue("KPI No. "+ serialNumber++ +": "+ awardProgressReportKPISummary.getKpiCriteriaType().getDescription());
			headingCell.setCellStyle(tableHeadStyle);
			tableBodyStyle.setWrapText(true);
			switch (awardProgressReportKPISummary.getSectionCode()) {
				case "1":
					List<ProgressReportKPIImpactPublications> progressReportKPIImpactPublications = progressReportDao.getProgressReportKPIImpactPublicationsBasedOnCriteria(awardProgressReportKPISummary.getKpiSummaryId());
					rowNumber = prepareFirstKPITypeDetails(sheet, rowNumber, progressReportKPIImpactPublications, tableBodyStyle, workbook);
					++rowNumber;		
				break;
				case "2":
					List<ProgressReportKPICollaborationProjects> progressReportKPICollaborationProjects = progressReportDao.getProgressReportKPICollaborationProjectsBasedOnCriteria(awardProgressReportKPISummary.getKpiSummaryId());
					rowNumber = prepareSecondKPITypeDetails(sheet, rowNumber, progressReportKPICollaborationProjects, tableBodyStyle, workbook);
					++rowNumber;
					break;
				case "3":
					List<ProgressReportKPITechnologyDisclosure> progressReportKPITechnologyDisclosure = progressReportDao.getProgressReportKPITechnologyDisclosureBasedOnCriteria(awardProgressReportKPISummary.getKpiSummaryId());
					rowNumber = prepareThirdKPITypeDetails(sheet, rowNumber, progressReportKPITechnologyDisclosure, tableBodyStyle, workbook);
					++rowNumber;
					break;
				case "4":
					List<ProgressReportKPIManpowerDevelopment> progressReportKPIManpowerDevelopment = progressReportDao.getProgressReportKPIManpowerDevelopmentBasedOnCriteria(awardProgressReportKPISummary.getKpiSummaryId());
					rowNumber = prepareFourthKPITypeDetails(sheet, rowNumber, progressReportKPIManpowerDevelopment, tableBodyStyle, workbook);
					++rowNumber;
					break;
				case "5":
					List<ProgressReportKPIUndergraduateStudent> progressReportKPIUndergraduateStudent = progressReportDao.getProgressReportKPIUndergraduateStudentBasedOnCriteria(awardProgressReportKPISummary.getKpiSummaryId());
					rowNumber = prepareFifthKPITypeDetails(sheet, rowNumber, progressReportKPIUndergraduateStudent, tableBodyStyle, workbook);
					++rowNumber;
					break;
				case "6":
					List<ProgressReportKPIConferencePresentation> progressReportKPIConferencePresentation = progressReportDao.getProgressReportKPIConferencePresentationBasedOnCriteria(awardProgressReportKPISummary.getKpiSummaryId());
					rowNumber = prepareSixthKPITypeDetails(sheet, rowNumber, progressReportKPIConferencePresentation, tableBodyStyle, workbook);
					++rowNumber;
					break;
				case "7":
					List<ProgressReportKPICompetitiveGrants> progressReportKPICompetitiveGrants = progressReportDao.getProgressReportKPICompetitiveGrantsBasedOnCriteria(awardProgressReportKPISummary.getKpiSummaryId());
					rowNumber = prepareSeventhKPITypeDetails(sheet, rowNumber, progressReportKPICompetitiveGrants, tableBodyStyle, workbook);
					++rowNumber;
					break;
				case "8":
					List<ProgressReportKPIPatents> progressReportKPIPatents = progressReportDao.getProgressReportKPIPatentsBasedOnCriteria(awardProgressReportKPISummary.getKpiSummaryId());
					rowNumber = prepareEighthKPITypeDetails(sheet, rowNumber, progressReportKPIPatents, tableBodyStyle, workbook);
					++rowNumber;
					break;
				case "9":
					List<ProgressReportKPILicenses> progressReportKPILicenses = progressReportDao.getProgressReportKPILicensesBasedOnCriteria(awardProgressReportKPISummary.getKpiSummaryId());
					rowNumber = prepareNinthKPITypeDetails(sheet, rowNumber, progressReportKPILicenses, tableBodyStyle, workbook);
					++rowNumber;
					break;
				case "10":
					List<ProgressReportKPISuccessfulStartups> progressReportKPISuccessfulStartups = progressReportDao.getProgressReportKPISuccessfulStartupsBasedOnCriteria(awardProgressReportKPISummary.getKpiSummaryId());
					rowNumber = prepareTenthKPITypeDetails(sheet, rowNumber, progressReportKPISuccessfulStartups, tableBodyStyle, workbook);
					++rowNumber;
					break;
				case "11":
					List<ProgressReportKPIHealthSpecificOutcomes> progressReportKPIHealthSpecificOutcomes = progressReportDao.getProgressReportKPIHealthSpecificOutcomesBasedOnCriteria(awardProgressReportKPISummary.getKpiSummaryId());
					rowNumber = prepareEleventhKPITypeDetails(sheet, rowNumber, progressReportKPIHealthSpecificOutcomes, tableBodyStyle, workbook);
					++rowNumber;
					break;
				case "12":
					List<ProgressReportKPIPostDocsEmployed> progressReportKPIPostDocsEmployed = progressReportDao.getProgressReportKPIPostDocsEmployedBasedOnCriteria(awardProgressReportKPISummary.getKpiSummaryId());
					rowNumber = prepareTwelfthKPITypeDetails(sheet, rowNumber, progressReportKPIPostDocsEmployed, tableBodyStyle, workbook);
					++rowNumber;
					break;
				case "13":
					List<ProgressReportKPIGrantSpecific> progressReportKPIGrantSpecifics = progressReportDao.getProgressReportKPIGrantSpecificBasedOnCriteriaBasedOnCriteria(awardProgressReportKPISummary.getKpiSummaryId());
					rowNumber = prepareThirteenthKPITypeDetails(sheet, rowNumber, progressReportKPIGrantSpecifics, tableBodyStyle, workbook);
					++rowNumber;
					break;
				case "14":
					List<ProgressReportKPICashFunding> progressReportKPICashFunding = progressReportDao.getProgressReportKPICashFundingBasedOnCriteria(awardProgressReportKPISummary.getKpiSummaryId());
					rowNumber = prepareFourteenthKPITypeDetails(sheet, rowNumber, progressReportKPICashFunding, tableBodyStyle, workbook);
					++rowNumber;
					break;
				case "15":
					List<ProgressReportKPIInkindContributions> progressReportKPIInkindContributions = progressReportDao.getProgressReportKPIInkindContributionsBasedOnCriteria(awardProgressReportKPISummary.getKpiSummaryId());
					rowNumber = prepareFifteenthKPITypeDetails(sheet, rowNumber, progressReportKPIInkindContributions, tableBodyStyle, workbook);
					++rowNumber;
					break;
				case "16":
					List<ProgressReportKPITechnologiesDeployed> progressReportKPITechnologiesDeployed = progressReportDao.getProgressReportKPITechnologiesDeployedBasedOnCriteria(awardProgressReportKPISummary.getKpiSummaryId());
					rowNumber = prepareSixteenthKPITypeDetails(sheet, rowNumber, progressReportKPITechnologiesDeployed, tableBodyStyle, workbook);
					++rowNumber;
					break;
				default:
					break;
			}
		}
		return rowNumber;
	}

	private Integer convertStringToParagraph(Cell cell, Integer count, String description) {
    	StringBuilder descriptionDetail = new StringBuilder("");
    	if (description != null) {
    		String[] words = description.split("\\s+");
            Integer lineSperator = 8;
            for(int i = 0; i < words.length; i++) {
            	if(i < lineSperator) {
            		descriptionDetail = descriptionDetail.append(words[i] + " ");
              	 }
            	if (i == lineSperator) {
            		descriptionDetail = descriptionDetail.append(words[i]).append("\n");
          			lineSperator = lineSperator + 10;
          		 }
            	cell.setCellValue(descriptionDetail.toString());
            	count = cell.getColumnIndex();
    		}
    	}
    	return count;
	}

	private Row setCellHeight(Integer cellNum, XSSFSheet sheet, Row headerRow) {
		String[] value = headerRow.getCell(cellNum).getStringCellValue().split("\\s+");
		if( value.length > 8) {
			headerRow.setHeightInPoints(headerRow.getCell(cellNum).getStringCellValue().split("\n").length*sheet.getDefaultRowHeightInPoints());
			sheet.setColumnWidth(headerRow.getCell(cellNum).getColumnIndex(), 30 * 256);
			headerRow.setHeight((short) 2500);
		} 
		return headerRow;
	}

	public Integer prepareFirstKPITypeDetails(XSSFSheet sheet, int rowNumber, List<ProgressReportKPIImpactPublications> progressReportKPIImpactPublications, XSSFCellStyle tableBodyStyle, XSSFWorkbook workbook) {
		Object[] kpiCategoryHeading = { "Status", "Author Names", "Title of Article", "Journal Name", "Publisher", "Year/Issue/No.", "Page No.", "Impact Factor", "Acknowledgement of funding (Yes/No) If no, please provide reason.", "Comments" };
		prepareExcelSheetHeaderForPRogressReport(null, sheet, kpiCategoryHeading, workbook, tableBodyStyle, rowNumber++);
		if(!progressReportKPIImpactPublications.isEmpty()) {
			for (ProgressReportKPIImpactPublications progressReportKPIImpactPublication : progressReportKPIImpactPublications) {
				Integer count = 0;
				int cellNumber = 0;
				Row performanceIndicatorRow = sheet.createRow(rowNumber++);
				Cell titleCell = performanceIndicatorRow.createCell(cellNumber);
				titleCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(titleCell, count, progressReportKPIImpactPublication.getKpiPublicationStatus() != null ? progressReportKPIImpactPublication.getKpiPublicationStatus().getDescription() : "");
				setCellHeight(titleCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell nameOfJournalCell = performanceIndicatorRow.createCell(cellNumber);
				nameOfJournalCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(nameOfJournalCell, count, progressReportKPIImpactPublication.getAuthorName());
				setCellHeight(nameOfJournalCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell publishedDateCell = performanceIndicatorRow.createCell(cellNumber);
				publishedDateCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(publishedDateCell, count, progressReportKPIImpactPublication.getTitleOfArticle());
				setCellHeight(publishedDateCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell publicationDoiCell = performanceIndicatorRow.createCell(cellNumber);
				publicationDoiCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(publicationDoiCell, count, progressReportKPIImpactPublication.getJournalName());
				setCellHeight(publicationDoiCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell authorNameCell = performanceIndicatorRow.createCell(cellNumber);
				authorNameCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(authorNameCell, count, progressReportKPIImpactPublication.getPublisher());
				setCellHeight(authorNameCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell allAuthorCell = performanceIndicatorRow.createCell(cellNumber);
				allAuthorCell.setCellStyle(tableBodyStyle);
				allAuthorCell.setCellValue(progressReportKPIImpactPublication.getYear());
				cellNumber++;

				Cell pageNoCell = performanceIndicatorRow.createCell(cellNumber);
				pageNoCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(pageNoCell, count, progressReportKPIImpactPublication.getPageNo());
				setCellHeight(pageNoCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell impactCell = performanceIndicatorRow.createCell(cellNumber);
				impactCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(impactCell, count, progressReportKPIImpactPublication.getImpactFactor());
				setCellHeight(impactCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell acknowledgementCell = performanceIndicatorRow.createCell(cellNumber);
				acknowledgementCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(acknowledgementCell, count, progressReportKPIImpactPublication.getFundingAcknowledgement());
				setCellHeight(acknowledgementCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell commentCell = performanceIndicatorRow.createCell(cellNumber);
				commentCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(commentCell, count, progressReportKPIImpactPublication.getComments());
				setCellHeight(commentCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;
				sheet.setColumnWidth(titleCell.getColumnIndex() , 15 * 256);
			}
		}
		return rowNumber;
	}

	public Integer prepareSecondKPITypeDetails(XSSFSheet sheet, int rowNumber, List<ProgressReportKPICollaborationProjects> progressReportKPICollaborationProjects, XSSFCellStyle tableBodyStyle, XSSFWorkbook workbook) {
		Object[] kpiCategoryHeading = { "Project Title", "Project Description", "Project Start Date", "Project End Date", "Name of Collaborating Organization", "Country", "Company UEN", "Comment" };
		prepareExcelSheetHeaderForPRogressReport(null, sheet, kpiCategoryHeading, workbook, tableBodyStyle, rowNumber++);
		if(!progressReportKPICollaborationProjects.isEmpty()) {
			for (ProgressReportKPICollaborationProjects collaborationProject : progressReportKPICollaborationProjects) {
				Integer count = 0;
				int cellNumber = 0;
				Row performanceIndicatorRow = sheet.createRow(rowNumber++);
				Cell titleCell = performanceIndicatorRow.createCell(cellNumber);
				titleCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(titleCell, count, collaborationProject.getProjectTitle());
				setCellHeight(titleCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell nameOfJournalCell = performanceIndicatorRow.createCell(cellNumber);
				nameOfJournalCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(nameOfJournalCell, count, collaborationProject.getProjectDescription());
				setCellHeight(nameOfJournalCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell startDateCell = performanceIndicatorRow.createCell(cellNumber);
				startDateCell.setCellStyle(tableBodyStyle);
				if (collaborationProject.getProjectStartDate() != null) {
					String projectStartDate = commonService.convertDateFormatBasedOnTimeZone(collaborationProject.getProjectStartDate().getTime(),
							Constants.DEFAULT_DATE_FORMAT);
					startDateCell.setCellValue(projectStartDate);
				} else {
					startDateCell.setCellValue("");
				}			
				cellNumber++;

				Cell endDateCell = performanceIndicatorRow.createCell(cellNumber);
				endDateCell.setCellStyle(tableBodyStyle);
				if (collaborationProject.getProjectEndDate() != null) {
					String projectEndDate = commonService.convertDateFormatBasedOnTimeZone(collaborationProject.getProjectEndDate().getTime(),
							Constants.DEFAULT_DATE_FORMAT);
					endDateCell.setCellValue(projectEndDate);
				} else {
					endDateCell.setCellValue("");
				}
				cellNumber++;

				Cell authorNameCell = performanceIndicatorRow.createCell(cellNumber);
				authorNameCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(authorNameCell, count, collaborationProject.getCollaboratingOrganization());
				setCellHeight(authorNameCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell allAuthorCell = performanceIndicatorRow.createCell(cellNumber);
				allAuthorCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(allAuthorCell, count, collaborationProject.getCountry() != null ? collaborationProject.getCountry().getCountryName() : "");
				setCellHeight(allAuthorCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell typeCell = performanceIndicatorRow.createCell(cellNumber);
				typeCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(typeCell, count, collaborationProject.getComments());
				setCellHeight(typeCell.getColumnIndex(), sheet, performanceIndicatorRow);
			}
		}
		return rowNumber;
	}

	public Integer prepareThirdKPITypeDetails(XSSFSheet sheet, int rowNumber, List<ProgressReportKPITechnologyDisclosure> progressReportKPITechnologyDisclosure, XSSFCellStyle tableBodyStyle, XSSFWorkbook workbook) {
		Object[] kpiCategoryHeading = { "Status", "Author Names", "Title of Patent", "Covering Countries", "Filing Office", "Date of Filing", "Date of Award (if applicable)", "Comments" };
		prepareExcelSheetHeaderForPRogressReport(null, sheet, kpiCategoryHeading, workbook, tableBodyStyle, rowNumber++);
		if(!progressReportKPITechnologyDisclosure.isEmpty()) {
			for (ProgressReportKPITechnologyDisclosure technologyDisclosure : progressReportKPITechnologyDisclosure) {
				Integer count = 0;
				int cellNumber = 0;
				Row performanceIndicatorRow = sheet.createRow(rowNumber++);
				Cell titleCell = performanceIndicatorRow.createCell(cellNumber);
				titleCell.setCellStyle(tableBodyStyle);
				titleCell.setCellValue(technologyDisclosure.getKpiTechnologyDisclosureStatus() != null ? technologyDisclosure.getKpiTechnologyDisclosureStatus().getDescription() : "");
				cellNumber++;

				Cell nameOfJournalCell = performanceIndicatorRow.createCell(cellNumber);
				nameOfJournalCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(nameOfJournalCell, count, technologyDisclosure.getAuthorName());
				setCellHeight(nameOfJournalCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell publishedDateCell = performanceIndicatorRow.createCell(cellNumber);
				publishedDateCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(publishedDateCell, count, technologyDisclosure.getTitleOfPatent());
				setCellHeight(publishedDateCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell publicationDoiCell = performanceIndicatorRow.createCell(cellNumber);
				publicationDoiCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(publicationDoiCell, count, technologyDisclosure.getCoveringCountries());
				setCellHeight(publicationDoiCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell authorNameCell = performanceIndicatorRow.createCell(cellNumber);
				authorNameCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(authorNameCell, count, technologyDisclosure.getFillingOffice());
				setCellHeight(authorNameCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell dateOffilingCell = performanceIndicatorRow.createCell(cellNumber);
				dateOffilingCell.setCellStyle(tableBodyStyle);
				if (technologyDisclosure.getDateOffilling() != null) {
					String filingDate = commonService.convertDateFormatBasedOnTimeZone(technologyDisclosure.getDateOffilling().getTime(),
							Constants.DEFAULT_DATE_FORMAT);
					dateOffilingCell.setCellValue(filingDate);
				} else {
					dateOffilingCell.setCellValue("");
				}
				cellNumber++;

				Cell awardDateCell = performanceIndicatorRow.createCell(cellNumber);
				awardDateCell.setCellStyle(tableBodyStyle);
				if (technologyDisclosure.getDateOfAward() != null) {
					String awardDate = commonService.convertDateFormatBasedOnTimeZone(technologyDisclosure.getDateOfAward().getTime(),
							Constants.DEFAULT_DATE_FORMAT);
					awardDateCell.setCellValue(awardDate);
				} else {
					awardDateCell.setCellValue("");
				}
				cellNumber++;

				Cell commentCell = performanceIndicatorRow.createCell(cellNumber);
				commentCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(commentCell, count, technologyDisclosure.getComments());
				setCellHeight(commentCell.getColumnIndex(), sheet, performanceIndicatorRow);
				sheet.setColumnWidth(titleCell.getColumnIndex() , 10 * 256);
			}
		}
		return rowNumber;
	}

	public Integer prepareFourthKPITypeDetails(XSSFSheet sheet, int rowNumber, List<ProgressReportKPIManpowerDevelopment> progressReportKPIManpowerDevelopment, XSSFCellStyle tableBodyStyle, XSSFWorkbook workbook) {
		Object[] kpiCategoryHeading = { "Name of Student", "Citizenship", "Current Status", "Date Enrolled", "Date Graduated / Resigned", "Date of Joining This Programme", "Date of Leaving This Programme", "Comments" };
		prepareExcelSheetHeaderForPRogressReport(null, sheet, kpiCategoryHeading, workbook, tableBodyStyle, rowNumber++);
		if(!progressReportKPIManpowerDevelopment.isEmpty()) {
			for (ProgressReportKPIManpowerDevelopment manpowerDevelopment : progressReportKPIManpowerDevelopment) {
				Integer count = 0;
				int cellNumber = 0;
				Row performanceIndicatorRow = sheet.createRow(rowNumber++);
				Cell titleCell = performanceIndicatorRow.createCell(cellNumber);
				titleCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(titleCell, count, manpowerDevelopment.getNameOfStudent());
				setCellHeight(titleCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell nameOfJournalCell = performanceIndicatorRow.createCell(cellNumber);
				nameOfJournalCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(nameOfJournalCell, count, manpowerDevelopment.getCitizenship());
				setCellHeight(nameOfJournalCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell publishedDateCell = performanceIndicatorRow.createCell(cellNumber);
				publishedDateCell.setCellStyle(tableBodyStyle);
				publishedDateCell.setCellValue(manpowerDevelopment.getKpiManpowerDevelopmentCurrentStatus() != null ? manpowerDevelopment.getKpiManpowerDevelopmentCurrentStatus().getDescription() : null);
				cellNumber++;

				Cell dateEnrolledCell = performanceIndicatorRow.createCell(cellNumber);
				dateEnrolledCell.setCellStyle(tableBodyStyle);
				if (manpowerDevelopment.getDateEnrolled() != null) {
					String enrolledDate = commonService.convertDateFormatBasedOnTimeZone(manpowerDevelopment.getDateEnrolled().getTime(),
							Constants.DEFAULT_DATE_FORMAT);
					dateEnrolledCell.setCellValue(enrolledDate);
				} else {
					dateEnrolledCell.setCellValue("");
				}
				cellNumber++;

				Cell graduatedDateCell = performanceIndicatorRow.createCell(cellNumber);
				graduatedDateCell.setCellStyle(tableBodyStyle);
				if (manpowerDevelopment.getDateGraduated() != null) {
					String graduatedDate = commonService.convertDateFormatBasedOnTimeZone(manpowerDevelopment.getDateGraduated().getTime(),
							Constants.DEFAULT_DATE_FORMAT);
					graduatedDateCell.setCellValue(graduatedDate);
				} else {
					graduatedDateCell.setCellValue("");
				}
				cellNumber++;

				Cell joiningDateCell = performanceIndicatorRow.createCell(cellNumber);
				joiningDateCell.setCellStyle(tableBodyStyle);
				if (manpowerDevelopment.getDateOfJoining() != null) {
					String joiningDate = commonService.convertDateFormatBasedOnTimeZone(manpowerDevelopment.getDateOfJoining().getTime(),
							Constants.DEFAULT_DATE_FORMAT);
					joiningDateCell.setCellValue(joiningDate);
				} else {
					joiningDateCell.setCellValue("");
				}
				cellNumber++;

				Cell leavingDateCell = performanceIndicatorRow.createCell(cellNumber);
				leavingDateCell.setCellStyle(tableBodyStyle);
				if (manpowerDevelopment.getDateOfLeaving() != null) {
					String leavingDate = commonService.convertDateFormatBasedOnTimeZone(manpowerDevelopment.getDateOfLeaving().getTime(),
							Constants.DEFAULT_DATE_FORMAT);
					leavingDateCell.setCellValue(leavingDate);
				} else {
					leavingDateCell.setCellValue("");
				}
				cellNumber++;

				Cell projectTitleCell = performanceIndicatorRow.createCell(cellNumber);
				projectTitleCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(projectTitleCell, count, manpowerDevelopment.getComments());
				setCellHeight(projectTitleCell.getColumnIndex(), sheet, performanceIndicatorRow);
			}
		}
		return rowNumber;
	}

	public Integer prepareFifthKPITypeDetails(XSSFSheet sheet, int rowNumber, List<ProgressReportKPIUndergraduateStudent> progressReportKPIUndergraduateStudent, XSSFCellStyle tableBodyStyle, XSSFWorkbook workbook) {
		Object[] kpiCategoryHeading = { "Name of Student", "Citizenship", "Current Status (Active / Graduated)", "Date (MM/YY) of Joining This Project", "Date (MM/YY) of Leaving This Project", "Comments" };
		prepareExcelSheetHeaderForPRogressReport(null, sheet, kpiCategoryHeading, workbook, tableBodyStyle, rowNumber++);
		if(!progressReportKPIUndergraduateStudent.isEmpty()) {
			for (ProgressReportKPIUndergraduateStudent undergraduateStudent : progressReportKPIUndergraduateStudent) {
				Integer count = 0;
				int cellNumber = 0;
				Row performanceIndicatorRow = sheet.createRow(rowNumber++);
				Cell titleCell = performanceIndicatorRow.createCell(cellNumber);
				titleCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(titleCell, count, undergraduateStudent.getNameOfStudent());
				setCellHeight(titleCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell nameOfJournalCell = performanceIndicatorRow.createCell(cellNumber);
				nameOfJournalCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(nameOfJournalCell, count, undergraduateStudent.getCitizenship());
				setCellHeight(nameOfJournalCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell publishedDateCell = performanceIndicatorRow.createCell(cellNumber);
				publishedDateCell.setCellStyle(tableBodyStyle);
				publishedDateCell.setCellValue(undergraduateStudent.getKpiManpowerDevelopmentCurrentStatus() != null ? undergraduateStudent.getKpiManpowerDevelopmentCurrentStatus().getDescription() : null);
				cellNumber++;

				Cell joiningDateCell = performanceIndicatorRow.createCell(cellNumber);
				joiningDateCell.setCellStyle(tableBodyStyle);
				if (undergraduateStudent.getDateOfJoining() != null) {
					String joiningDate = commonService.convertDateFormatBasedOnTimeZone(undergraduateStudent.getDateOfJoining().getTime(),
							Constants.DEFAULT_DATE_FORMAT);
					joiningDateCell.setCellValue(joiningDate);
				} else {
					joiningDateCell.setCellValue("");
				}
				cellNumber++;

				Cell leavingDateCell = performanceIndicatorRow.createCell(cellNumber);
				leavingDateCell.setCellStyle(tableBodyStyle);
				if (undergraduateStudent.getDateOfLeaving() != null) {
					String leavingDate = commonService.convertDateFormatBasedOnTimeZone(undergraduateStudent.getDateOfLeaving().getTime(),
							Constants.DEFAULT_DATE_FORMAT);
					leavingDateCell.setCellValue(leavingDate);
				} else {
					leavingDateCell.setCellValue("");
				}
				cellNumber++;

				Cell projectTitleCell = performanceIndicatorRow.createCell(cellNumber);
				projectTitleCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(projectTitleCell, count, undergraduateStudent.getComments());
				setCellHeight(projectTitleCell.getColumnIndex(), sheet, performanceIndicatorRow);
			}
		}
		return rowNumber;
	}

	public Integer prepareSixthKPITypeDetails(XSSFSheet sheet, int rowNumber, List<ProgressReportKPIConferencePresentation> progressReportKPIConferencePresentation, XSSFCellStyle tableBodyStyle, XSSFWorkbook workbook) {
		Object[] kpiCategoryHeading = { "Name of Presenter(s) / Researcher(s)", "Title", "Conference Title / Award Title", "Organiser", "Conference Location (Country/State)", "Date", "Comments" };
		prepareExcelSheetHeaderForPRogressReport(null, sheet, kpiCategoryHeading, workbook, tableBodyStyle, rowNumber++);
		if(!progressReportKPIConferencePresentation.isEmpty()) {
			for (ProgressReportKPIConferencePresentation conferencePresentation : progressReportKPIConferencePresentation) {
				Integer count = 0;
				int cellNumber = 0;
				Row performanceIndicatorRow = sheet.createRow(rowNumber++);
				Cell titleCell = performanceIndicatorRow.createCell(cellNumber);
				titleCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(titleCell, count, conferencePresentation.getNameOfPresenter());
				setCellHeight(titleCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell nameOfJournalCell = performanceIndicatorRow.createCell(cellNumber);
				nameOfJournalCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(nameOfJournalCell, count, conferencePresentation.getTitle());
				setCellHeight(nameOfJournalCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell publishedDateCell = performanceIndicatorRow.createCell(cellNumber);
				publishedDateCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(publishedDateCell, count, conferencePresentation.getConferenceTitle());
				setCellHeight(publishedDateCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell organiserCell = performanceIndicatorRow.createCell(cellNumber);
				organiserCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(organiserCell, count, conferencePresentation.getOrganiser());
				setCellHeight(organiserCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell allAuthorCell = performanceIndicatorRow.createCell(cellNumber);
				allAuthorCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(allAuthorCell, count, conferencePresentation.getConferenceLocation());
				setCellHeight(allAuthorCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell dateCell = performanceIndicatorRow.createCell(cellNumber);
				dateCell.setCellStyle(tableBodyStyle);
				if (conferencePresentation.getDate() != null) {
					String date = commonService.convertDateFormatBasedOnTimeZone(conferencePresentation.getDate().getTime(),
							Constants.DEFAULT_DATE_FORMAT);
					dateCell.setCellValue(date);
				} else {
					dateCell.setCellValue("");
				}
				cellNumber++;

				Cell commentsCell = performanceIndicatorRow.createCell(cellNumber);
				commentsCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(commentsCell, count, conferencePresentation.getComments());
				setCellHeight(commentsCell.getColumnIndex(), sheet, performanceIndicatorRow);
			}
		}
		return rowNumber;
	}

	public Integer prepareSeventhKPITypeDetails(XSSFSheet sheet, int rowNumber, List<ProgressReportKPICompetitiveGrants> progressReportKPICompetitiveGrants, XSSFCellStyle tableBodyStyle, XSSFWorkbook workbook) {
		Object[] kpiCategoryHeading = { "Name of Grant Received", "Project Reference No", "Funding Agency", "Recipient of Grant", "Host Institution", "Direct Cost", "Indirect Cost", "Project Start Date", "Project End Date", "Project Title", "Comments" };
		prepareExcelSheetHeaderForPRogressReport(null, sheet, kpiCategoryHeading, workbook, tableBodyStyle, rowNumber++);
		if(!progressReportKPICompetitiveGrants.isEmpty()) {
			for (ProgressReportKPICompetitiveGrants competitiveGrants : progressReportKPICompetitiveGrants) {
				int cellNumber = 0;
				Integer count = 0;
				Row performanceIndicatorRow = sheet.createRow(rowNumber++);
				Cell grantNameCell = performanceIndicatorRow.createCell(cellNumber);
				grantNameCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(grantNameCell, count, competitiveGrants.getNameOfGrantReceived());
				setCellHeight(grantNameCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell nameOfJournalCell = performanceIndicatorRow.createCell(cellNumber);
				nameOfJournalCell.setCellStyle(tableBodyStyle);
				nameOfJournalCell.setCellValue(competitiveGrants.getProjectReferenceNo());
				cellNumber++;

				Cell sponsorCell = performanceIndicatorRow.createCell(cellNumber);
				sponsorCell.setCellStyle(tableBodyStyle);
				if (competitiveGrants.getSponsor() != null) {
					String sponsorAcronym = competitiveGrants.getSponsor().getAcronym() != null ? " (" + competitiveGrants.getSponsor().getAcronym() + ")" : "";
					sponsorCell.setCellValue(competitiveGrants.getSponsor() != null ? competitiveGrants.getSponsor().getSponsorCode() + " - " + competitiveGrants.getSponsor().getSponsorName() + sponsorAcronym : "");
				}
				else 
					sponsorCell.setCellValue("");
				cellNumber++;

				Cell organiserCell = performanceIndicatorRow.createCell(cellNumber);
				organiserCell.setCellStyle(tableBodyStyle);
				organiserCell.setCellValue(competitiveGrants.getRecipientOfGrant());
				cellNumber++;

				Cell allAuthorCell = performanceIndicatorRow.createCell(cellNumber);
				allAuthorCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(allAuthorCell, count, competitiveGrants.getHostInsitution());
				setCellHeight(allAuthorCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell typeCell = performanceIndicatorRow.createCell(cellNumber);
				typeCell.setCellStyle(tableBodyStyle);
				typeCell.setCellValue(competitiveGrants.getDirectCost().toString());
				cellNumber++;

				Cell projectTitleCell = performanceIndicatorRow.createCell(cellNumber);
				projectTitleCell.setCellStyle(tableBodyStyle);
				projectTitleCell.setCellValue(competitiveGrants.getIndirectCost().toString());
				cellNumber++;

				Cell startDateCell = performanceIndicatorRow.createCell(cellNumber);
				startDateCell.setCellStyle(tableBodyStyle);
				if (competitiveGrants.getProjectStartDate() != null) {
					String startDate = commonService.convertDateFormatBasedOnTimeZone(competitiveGrants.getProjectStartDate().getTime(),
							Constants.DEFAULT_DATE_FORMAT);
					startDateCell.setCellValue(startDate);
				} else {
					startDateCell.setCellValue("");
				}
				cellNumber++;

				Cell endDateCell = performanceIndicatorRow.createCell(cellNumber);
				endDateCell.setCellStyle(tableBodyStyle);
				if (competitiveGrants.getProjectEndDate() != null) {
					String endDate = commonService.convertDateFormatBasedOnTimeZone(competitiveGrants.getProjectEndDate().getTime(),
							Constants.DEFAULT_DATE_FORMAT);
					endDateCell.setCellValue(endDate);
				} else {
					endDateCell.setCellValue("");
				}
				cellNumber++;

				Cell titleCell = performanceIndicatorRow.createCell(cellNumber);
				titleCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(titleCell, count, competitiveGrants.getProjectTitle());
				setCellHeight(titleCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell commentsCell = performanceIndicatorRow.createCell(cellNumber);
				commentsCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(commentsCell, count, competitiveGrants.getComments());
				setCellHeight(commentsCell.getColumnIndex(), sheet, performanceIndicatorRow);
			}
		}
		return rowNumber;
	}

	public Integer prepareEighthKPITypeDetails(XSSFSheet sheet, int rowNumber, List<ProgressReportKPIPatents> progressReportKPIPatents, XSSFCellStyle tableBodyStyle, XSSFWorkbook workbook) {
		Object[] kpiCategoryHeading = { "Date Filed", "Date Granted", "Title", "Description", "Ownership", "Patent Number", "Comments" };
		prepareExcelSheetHeaderForPRogressReport(null, sheet, kpiCategoryHeading, workbook, tableBodyStyle, rowNumber++);
		if(!progressReportKPIPatents.isEmpty()) {
			for (ProgressReportKPIPatents patents : progressReportKPIPatents) {
				Integer count = 0;
				int cellNumber = 0;
				Row performanceIndicatorRow = sheet.createRow(rowNumber++);
				Cell filedDateCell = performanceIndicatorRow.createCell(cellNumber);
				filedDateCell.setCellStyle(tableBodyStyle);
				if (patents.getDateFiled() != null) {
					String filedDate = commonService.convertDateFormatBasedOnTimeZone(patents.getDateFiled().getTime(),
							Constants.DEFAULT_DATE_FORMAT);
					filedDateCell.setCellValue(filedDate);
				} else {
					filedDateCell.setCellValue("");
				}
				cellNumber++;

				Cell grantedDateCell = performanceIndicatorRow.createCell(cellNumber);
				grantedDateCell.setCellStyle(tableBodyStyle);
				if (patents.getDateGranted() != null) {
					String grantedDate = commonService.convertDateFormatBasedOnTimeZone(patents.getDateGranted().getTime(),
							Constants.DEFAULT_DATE_FORMAT);
					grantedDateCell.setCellValue(grantedDate);
				} else {
					grantedDateCell.setCellValue("");
				}
				cellNumber++;

				Cell publishedDateCell = performanceIndicatorRow.createCell(cellNumber);
				publishedDateCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(publishedDateCell, count, patents.getTitle());
				setCellHeight(publishedDateCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell organiserCell = performanceIndicatorRow.createCell(cellNumber);
				organiserCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(organiserCell, count, patents.getDescription());
				setCellHeight(organiserCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell allAuthorCell = performanceIndicatorRow.createCell(cellNumber);
				allAuthorCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(allAuthorCell, count, patents.getOwnership());
				setCellHeight(allAuthorCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell typeCell = performanceIndicatorRow.createCell(cellNumber);
				typeCell.setCellStyle(tableBodyStyle);
				typeCell.setCellValue(patents.getPatentNumber());
				cellNumber++;

				Cell commentsCell = performanceIndicatorRow.createCell(cellNumber);
				commentsCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(commentsCell, count, patents.getComments());
				setCellHeight(commentsCell.getColumnIndex(), sheet, performanceIndicatorRow);
				sheet.setColumnWidth(filedDateCell.getColumnIndex() , 15 * 256);
			}
		}
		return rowNumber;
	}

	public Integer prepareNinthKPITypeDetails(XSSFSheet sheet, int rowNumber, List<ProgressReportKPILicenses> progressReportKPILicenses, XSSFCellStyle tableBodyStyle, XSSFWorkbook workbook) {
		Object[] kpiCategoryHeading = { "Start Date Of Licensing", "Name of Licensee/Assignee", "Company UEN", "Licensing Period (Number of Months)", "Details of the IP Licensed/Assigned (e.g. Patent, Assignment/Exclusive/Non-Exclusive, Field of Use, Revenue Arrangement)", "Comments" };
		prepareExcelSheetHeaderForPRogressReport(null, sheet, kpiCategoryHeading, workbook, tableBodyStyle, rowNumber++);
		if(!progressReportKPILicenses.isEmpty()) {
			for (ProgressReportKPILicenses license : progressReportKPILicenses) {
				int cellNumber = 0;
				Integer count = 0;
				Row performanceIndicatorRow = sheet.createRow(rowNumber++);
				Cell startDateCell = performanceIndicatorRow.createCell(cellNumber);
				startDateCell.setCellStyle(tableBodyStyle);
				if (license.getStartDate() != null) {
					String startDate = commonService.convertDateFormatBasedOnTimeZone(license.getStartDate().getTime(),
							Constants.DEFAULT_DATE_FORMAT);
					startDateCell.setCellValue(startDate);
				} else {
					startDateCell.setCellValue("");
				}
				cellNumber++;

				Cell nameOfJournalCell = performanceIndicatorRow.createCell(cellNumber);
				nameOfJournalCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(nameOfJournalCell, count, license.getNameOfLicense());
				setCellHeight(nameOfJournalCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell publishedDateCell = performanceIndicatorRow.createCell(cellNumber);
				publishedDateCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(publishedDateCell, count, license.getCompanyUen());
				setCellHeight(publishedDateCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell organiserCell = performanceIndicatorRow.createCell(cellNumber);
				organiserCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(organiserCell, count, license.getLicensingPeriod());
				setCellHeight(organiserCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell allAuthorCell = performanceIndicatorRow.createCell(cellNumber);
				allAuthorCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(allAuthorCell, count, license.getDetailsOfLicense());
				setCellHeight(allAuthorCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell commentsCell = performanceIndicatorRow.createCell(cellNumber);
				commentsCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(commentsCell, count, license.getComments());
				setCellHeight(commentsCell.getColumnIndex(), sheet, performanceIndicatorRow);
				sheet.setColumnWidth(startDateCell.getColumnIndex() , 15 * 256);
			}
		}
		return rowNumber;
	}

	public Integer prepareTenthKPITypeDetails(XSSFSheet sheet, int rowNumber, List<ProgressReportKPISuccessfulStartups> progressReportKPISuccessfulStartups, XSSFCellStyle tableBodyStyle, XSSFWorkbook workbook) {
		Object[] kpiCategoryHeading = { "Date of Establishment", "Date Established", "Name of Company", "Company UEN", "Meet External Funding Criteria?", "Meet Valuation Criteria?", "Met Annual Revenue Criteria?", "Comments" };
		prepareExcelSheetHeaderForPRogressReport(null, sheet, kpiCategoryHeading, workbook, tableBodyStyle, rowNumber++);
		if(!progressReportKPISuccessfulStartups.isEmpty()) {
			for (ProgressReportKPISuccessfulStartups startups : progressReportKPISuccessfulStartups) {
				int cellNumber = 0;
				Integer count = 0;
				Row performanceIndicatorRow = sheet.createRow(rowNumber++);
				Cell dateCell = performanceIndicatorRow.createCell(cellNumber);
				dateCell.setCellStyle(tableBodyStyle);
				if (startups.getDateOfEstablishment() != null) {
					String date = commonService.convertDateFormatBasedOnTimeZone(startups.getDateOfEstablishment().getTime(),
							Constants.DEFAULT_DATE_FORMAT);
					dateCell.setCellValue(date);
				} else {
					dateCell.setCellValue("");
				}
				cellNumber++;

				Cell establishedDateCell = performanceIndicatorRow.createCell(cellNumber);
				establishedDateCell.setCellStyle(tableBodyStyle);
				if (startups.getDateEstablished() != null) {
					String establishedDate = commonService.convertDateFormatBasedOnTimeZone(startups.getDateEstablished().getTime(),
							Constants.DEFAULT_DATE_FORMAT);
					establishedDateCell.setCellValue(establishedDate);
				} else {
					establishedDateCell.setCellValue("");
				}
				cellNumber++;

				Cell publishedDateCell = performanceIndicatorRow.createCell(cellNumber);
				publishedDateCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(publishedDateCell, count, startups.getNameOfCompany());
				setCellHeight(publishedDateCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell organiserCell = performanceIndicatorRow.createCell(cellNumber);
				organiserCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(organiserCell, count, startups.getCompanyUen());
				setCellHeight(organiserCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell allAuthorCell = performanceIndicatorRow.createCell(cellNumber);
				allAuthorCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(allAuthorCell, count, startups.getExternalFundingCriteria());
				setCellHeight(allAuthorCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell validationCell = performanceIndicatorRow.createCell(cellNumber);
				validationCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(validationCell, count, startups.getValuationCriteria());
				setCellHeight(validationCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell revenueCell = performanceIndicatorRow.createCell(cellNumber);
				revenueCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(revenueCell, count, startups.getAnnualRevenueCriteria());
				setCellHeight(revenueCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell commentsCell = performanceIndicatorRow.createCell(cellNumber);
				commentsCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(commentsCell, count, startups.getComments());
				setCellHeight(commentsCell.getColumnIndex(), sheet, performanceIndicatorRow);
				sheet.setColumnWidth(dateCell.getColumnIndex() , 10 * 256);
			}
		}
		return rowNumber;
	}

	public Integer prepareEleventhKPITypeDetails(XSSFSheet sheet, int rowNumber, List<ProgressReportKPIHealthSpecificOutcomes> progressReportKPIHealthSpecificOutcomes, XSSFCellStyle tableBodyStyle, XSSFWorkbook workbook) {
		Object[] kpiCategoryHeading = { "Date", "Number of Life years Saved", "Description/Title", "Remarks" };
		prepareExcelSheetHeaderForPRogressReport(null, sheet, kpiCategoryHeading, workbook, tableBodyStyle, rowNumber++);
		if(!progressReportKPIHealthSpecificOutcomes.isEmpty()) {
			for (ProgressReportKPIHealthSpecificOutcomes outcomes : progressReportKPIHealthSpecificOutcomes) {
				int cellNumber = 0;
				Integer count = 0;
				Row performanceIndicatorRow = sheet.createRow(rowNumber++);
				Cell establishedDateCell = performanceIndicatorRow.createCell(cellNumber);
				establishedDateCell.setCellStyle(tableBodyStyle);
				if (outcomes.getDateEstablished() != null) {
					String establishedDate = commonService.convertDateFormatBasedOnTimeZone(outcomes.getDateEstablished().getTime(),
							Constants.DEFAULT_DATE_FORMAT);
					establishedDateCell.setCellValue(establishedDate);
				} else {
					establishedDateCell.setCellValue("");
				}
				cellNumber++;

				Cell nameOfJournalCell = performanceIndicatorRow.createCell(cellNumber);
				nameOfJournalCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(nameOfJournalCell, count, outcomes.getNumberOfLifeYears());
				setCellHeight(nameOfJournalCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell publishedDateCell = performanceIndicatorRow.createCell(cellNumber);
				publishedDateCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(publishedDateCell, count, outcomes.getTitle());
				setCellHeight(publishedDateCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell commentsCell = performanceIndicatorRow.createCell(cellNumber);
				commentsCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(commentsCell, count, outcomes.getComments());
				setCellHeight(commentsCell.getColumnIndex(), sheet, performanceIndicatorRow);
				sheet.setColumnWidth(establishedDateCell.getColumnIndex() , 10 * 256);
			}
		}
		return rowNumber;
	}

	public Integer prepareTwelfthKPITypeDetails(XSSFSheet sheet, int rowNumber, List<ProgressReportKPIPostDocsEmployed> progressReportKPIPostDocsEmployed, XSSFCellStyle tableBodyStyle, XSSFWorkbook workbook) {
		Object[] kpiCategoryHeading = { "Employment Start Date", "Name", "Nationality", "Singapore Permanent Residence", "Comments" };
		prepareExcelSheetHeaderForPRogressReport(null, sheet, kpiCategoryHeading, workbook, tableBodyStyle, rowNumber++);
		if(!progressReportKPIPostDocsEmployed.isEmpty()) {
			for (ProgressReportKPIPostDocsEmployed postDocsEmployed : progressReportKPIPostDocsEmployed) {
				int cellNumber = 0;
				Integer count = 0;
				Row performanceIndicatorRow = sheet.createRow(rowNumber++);
				Cell startDateCell = performanceIndicatorRow.createCell(cellNumber);
				startDateCell.setCellStyle(tableBodyStyle);
				if (postDocsEmployed.getEmploymentStartDate() != null) {
					String startDate = commonService.convertDateFormatBasedOnTimeZone(postDocsEmployed.getEmploymentStartDate().getTime(),
							Constants.DEFAULT_DATE_FORMAT);
					startDateCell.setCellValue(startDate);
				} else {
					startDateCell.setCellValue("");
				}
				cellNumber++;

				Cell nameOfJournalCell = performanceIndicatorRow.createCell(cellNumber);
				nameOfJournalCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(nameOfJournalCell, count, postDocsEmployed.getEmployeeName());
				setCellHeight(nameOfJournalCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell publishedDateCell = performanceIndicatorRow.createCell(cellNumber);
				publishedDateCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(publishedDateCell, count, postDocsEmployed.getNationality());
				setCellHeight(publishedDateCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell residenceCell = performanceIndicatorRow.createCell(cellNumber);
				residenceCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(residenceCell, count, postDocsEmployed.getPermanentResidence());
				setCellHeight(residenceCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell commentsCell = performanceIndicatorRow.createCell(cellNumber);
				commentsCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(commentsCell, count, postDocsEmployed.getComments());
				setCellHeight(commentsCell.getColumnIndex(), sheet, performanceIndicatorRow);
				sheet.setColumnWidth(startDateCell.getColumnIndex() , 10 * 256);
			}
		}
		return rowNumber;
	}

	public Integer prepareThirteenthKPITypeDetails(XSSFSheet sheet, int rowNumber, List<ProgressReportKPIGrantSpecific> progressReportKPIGrantSpecifics, XSSFCellStyle tableBodyStyle, XSSFWorkbook workbook) {
		Object[] kpiCategoryHeading = { "Type", "Comment" };
		prepareExcelSheetHeaderForPRogressReport(null, sheet, kpiCategoryHeading, workbook, tableBodyStyle, rowNumber++);
		if(!progressReportKPIGrantSpecifics.isEmpty()) {
			for (ProgressReportKPIGrantSpecific grantSpecific : progressReportKPIGrantSpecifics) {
				int cellNumber = 0;
				Integer count = 0;
				Row performanceIndicatorRow = sheet.createRow(rowNumber++);
				Cell numberCell = performanceIndicatorRow.createCell(cellNumber);
				numberCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(numberCell, count,grantSpecific.getType());
				setCellHeight(numberCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell commentsCell = performanceIndicatorRow.createCell(cellNumber);
				commentsCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(commentsCell, count, grantSpecific.getComments());
				setCellHeight(commentsCell.getColumnIndex(), sheet, performanceIndicatorRow);
			}
		}
		return rowNumber;
	}

	public Integer prepareFourteenthKPITypeDetails(XSSFSheet sheet, int rowNumber, List<ProgressReportKPICashFunding> progressReportKPICashFunding, XSSFCellStyle tableBodyStyle, XSSFWorkbook workbook) {
		Object[] kpiCategoryHeading = { "Name of Company Contributing", "Country of Company", "Company UEN", "Date of Contribution", "Amount of Cash Funding ("+ Constants.DOLLAR_SYMBOL +"), up to 2 decimal places", "Comments" };
		prepareExcelSheetHeaderForPRogressReport(null, sheet, kpiCategoryHeading, workbook, tableBodyStyle, rowNumber++);
		if(!progressReportKPICashFunding.isEmpty()) {
			for (ProgressReportKPICashFunding cashFunding : progressReportKPICashFunding) {
				int cellNumber = 0;
				Integer count = 0;
				Row performanceIndicatorRow = sheet.createRow(rowNumber++);
				Cell grantNameCell = performanceIndicatorRow.createCell(cellNumber);
				grantNameCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(grantNameCell, count, cashFunding.getNameOfCompany());
				setCellHeight(grantNameCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell nameOfJournalCell = performanceIndicatorRow.createCell(cellNumber);
				nameOfJournalCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(nameOfJournalCell, count, cashFunding.getCountry() != null ? cashFunding.getCountry().getCountryName() : "");
				setCellHeight(nameOfJournalCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell publishedDateCell = performanceIndicatorRow.createCell(cellNumber);
				publishedDateCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(publishedDateCell, count, cashFunding.getCompanyUen());
				setCellHeight(publishedDateCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell dateCell = performanceIndicatorRow.createCell(cellNumber);
				dateCell.setCellStyle(tableBodyStyle);
				if (cashFunding.getDateOfContribution() != null) {
					String date = commonService.convertDateFormatBasedOnTimeZone(cashFunding.getDateOfContribution().getTime(),
							Constants.DEFAULT_DATE_FORMAT);
					dateCell.setCellValue(date);
				} else {
					dateCell.setCellValue("");
				}
				cellNumber++;

				Cell numberCell = performanceIndicatorRow.createCell(cellNumber);
				numberCell.setCellStyle(tableBodyStyle);
				numberCell.setCellValue(cashFunding.getAmount().toString());
				cellNumber++;

				Cell commentsCell = performanceIndicatorRow.createCell(cellNumber);
				commentsCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(commentsCell, count, cashFunding.getComments());
				setCellHeight(commentsCell.getColumnIndex(), sheet, performanceIndicatorRow);
			}
		}
		return rowNumber;
	}

	public Integer prepareFifteenthKPITypeDetails(XSSFSheet sheet, int rowNumber, List<ProgressReportKPIInkindContributions> progressReportKPIInkindContributions, XSSFCellStyle tableBodyStyle, XSSFWorkbook workbook) {
		Object[] kpiCategoryHeading = { "Date of Contribution", "Name of Company Contributing", "Country of Company", "Company UEN", "Amount of in kind contributions ("+ Constants.DOLLAR_SYMBOL +"), up to 2 decimal places", "Comments" };
		prepareExcelSheetHeaderForPRogressReport(null, sheet, kpiCategoryHeading, workbook, tableBodyStyle, rowNumber++);
		if(!progressReportKPIInkindContributions.isEmpty()) {
			for (ProgressReportKPIInkindContributions inkind : progressReportKPIInkindContributions) {
				int cellNumber = 0;
				Integer count = 0;
				Row performanceIndicatorRow = sheet.createRow(rowNumber++);
				Cell dateCell = performanceIndicatorRow.createCell(cellNumber);
				dateCell.setCellStyle(tableBodyStyle);
				if (inkind.getDateOfContribution() != null) {
					String date = commonService.convertDateFormatBasedOnTimeZone(inkind.getDateOfContribution().getTime(),
							Constants.DEFAULT_DATE_FORMAT);
					dateCell.setCellValue(date);
				} else {
					dateCell.setCellValue("");
				}
				cellNumber++;

				Cell nameOfJournalCell = performanceIndicatorRow.createCell(cellNumber);
				nameOfJournalCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(nameOfJournalCell, count, inkind.getNameOfCompany());
				setCellHeight(nameOfJournalCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell publishedDateCell = performanceIndicatorRow.createCell(cellNumber);
				publishedDateCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(publishedDateCell, count, inkind.getCountry() != null ? inkind.getCountry().getCountryName() : "");
				setCellHeight(publishedDateCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell residenceCell = performanceIndicatorRow.createCell(cellNumber);
				residenceCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(residenceCell, count, inkind.getCompanyUen());
				setCellHeight(residenceCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell numberCell = performanceIndicatorRow.createCell(cellNumber);
				numberCell.setCellStyle(tableBodyStyle);
				numberCell.setCellValue(inkind.getAmount() != null ? inkind.getAmount().toString() : null);
				cellNumber++;

				Cell commentsCell = performanceIndicatorRow.createCell(cellNumber);
				commentsCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(commentsCell, count, inkind.getComments());
				setCellHeight(commentsCell.getColumnIndex(), sheet, performanceIndicatorRow);
				sheet.setColumnWidth(dateCell.getColumnIndex() , 10 * 256);
			}
		}
		return rowNumber;
	}

	public Integer prepareSixteenthKPITypeDetails(XSSFSheet sheet, int rowNumber, List<ProgressReportKPITechnologiesDeployed> progressReportKPITechnologiesDeployed, XSSFCellStyle tableBodyStyle, XSSFWorkbook workbook) {
		Object[] kpiCategoryHeading = { "Date of deploying", "Name of company deploying", "Country of Company", "Company UEN", "Details of Technologies Deployed (e.g. new products or services introduced, process improvement). One row for each case of deployment.", "Comments" };
		prepareExcelSheetHeaderForPRogressReport(null, sheet, kpiCategoryHeading, workbook, tableBodyStyle, rowNumber++);
		if(!progressReportKPITechnologiesDeployed.isEmpty()) {
			for (ProgressReportKPITechnologiesDeployed deployed : progressReportKPITechnologiesDeployed) {
				int cellNumber = 0;
				Integer count = 0;
				Row performanceIndicatorRow = sheet.createRow(rowNumber++);
				Cell deployingDateCell = performanceIndicatorRow.createCell(cellNumber);
				deployingDateCell.setCellStyle(tableBodyStyle);
				if (deployed.getDateOfDeploying() != null) {
					String deployingDate = commonService.convertDateFormatBasedOnTimeZone(deployed.getDateOfDeploying().getTime(),
							Constants.DEFAULT_DATE_FORMAT);
					deployingDateCell.setCellValue(deployingDate);
				} else {
					deployingDateCell.setCellValue("");
				}
				cellNumber++;

				Cell nameOfJournalCell = performanceIndicatorRow.createCell(cellNumber);
				nameOfJournalCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(nameOfJournalCell, count, deployed.getNameOfCompany());
				setCellHeight(nameOfJournalCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell publishedDateCell = performanceIndicatorRow.createCell(cellNumber);
				publishedDateCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(publishedDateCell, count, deployed.getCountry() != null ? deployed.getCountry().getCountryName() : "");
				setCellHeight(publishedDateCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell residenceCell = performanceIndicatorRow.createCell(cellNumber);
				residenceCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(residenceCell, count, deployed.getCompanyUen());
				setCellHeight(residenceCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell numberCell = performanceIndicatorRow.createCell(cellNumber);
				numberCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(numberCell, count, deployed.getDetailsOfTechnology());
				setCellHeight(numberCell.getColumnIndex(), sheet, performanceIndicatorRow);
				cellNumber++;

				Cell commentsCell = performanceIndicatorRow.createCell(cellNumber);
				commentsCell.setCellStyle(tableBodyStyle);
				count = convertStringToParagraph(commentsCell, count, deployed.getComments());
				setCellHeight(commentsCell.getColumnIndex(), sheet, performanceIndicatorRow);
				sheet.setColumnWidth(deployingDateCell.getColumnIndex() , 10 * 256);
			}
		}
		return rowNumber;
	}

	@Override
	public void printEntireProgressReport(Integer progressReportId, Integer awardId, String awardLeadUnitNumber, HttpServletResponse response) {
		response.setContentType("application/zip");
		String documentHeading = null;
		String reportClassCode = progressReportDao.getReportClassCodeByProgressReportId(progressReportId);
		if (reportClassCode.equals(Constants.FINAL_REPORT_CLASS_CODE) ) {
			documentHeading = "Final Report";
		} else if(reportClassCode.equals(Constants.PROGRESS_REPORT_CLASS_CODE)) {
			documentHeading = "Progress Report";
		}
		String fileName = documentHeading+"_" + progressReportId;
		response.setHeader("Content-Disposition", "attachment;filename=\"" + fileName + ".zip" + "\"");
		List<String> rightNames = new ArrayList<>();
		rightNames.add(Constants.VIEW_CONFIDENTIAL_PROGRESS_REPORT_ATTACHMENTS_RIGHT);
		Boolean isPersonHasPermission = commonDao.checkPersonHasRightInModule(Constants.AWARD_MODULE_CODE, awardId, rightNames, AuthenticatedUser.getLoginPersonId());
		if(Boolean.FALSE.equals(isPersonHasPermission)) {
			isPersonHasPermission = personDao.isPersonHasPermission(AuthenticatedUser.getLoginPersonId(), Constants.VIEW_CONFIDENTIAL_PROGRESS_REPORT_ATTACHMENTS_RIGHT, awardLeadUnitNumber);
		}
		List<AwardProgressReportAttachment> attachments = progressReportDao.loadProgressReportAttachments(progressReportId, isPersonHasPermission);
		try {
			ByteArrayOutputStream baos = new ByteArrayOutputStream();
			ZipOutputStream zos = new ZipOutputStream(baos);
			if (attachments != null && !attachments.isEmpty()) {
				Integer index = 0;
				for (AwardProgressReportAttachment attachment : attachments) {
					index = commonService.addFilesToZipFolder(index, attachment.getFileName(), zos);
					FileData fileData = commonDao.getFileDataById(attachment.getFileDataId());
					byte[] data = fileData.getAttachment();
					zos.write(data);
				}
			}
			ByteArrayOutputStream bos = prepareProgressReportExcelData(progressReportId);
			zos.putNextEntry(new ZipEntry(new StringBuilder(documentHeading).append("_").append(progressReportId).append(".xlsx").toString()));
			zos.write(bos.toByteArray());
			zos.closeEntry();
			zos.flush();
			baos.flush();
			zos.close();
			baos.close();
			ServletOutputStream op = response.getOutputStream();
			op.write(baos.toByteArray());
			op.flush();
			op.close();
		} catch (Exception e) {
			logger.error("exception in printEntireProgressReport: {} ", e.getMessage());
			e.printStackTrace();
		}
	}

}