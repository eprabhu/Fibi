package com.polus.fibicomp.awardbudgetapiintegration.service;

import java.io.BufferedReader;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.InputStreamReader;
import java.math.BigDecimal;
import java.net.HttpURLConnection;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.text.DecimalFormat;
import java.util.Arrays;
import java.util.List;

import javax.servlet.http.HttpServletResponse;

import org.apache.commons.io.IOUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.poi.ss.usermodel.BorderStyle;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.util.CellRangeAddress;
import org.apache.poi.ss.util.RegionUtil;
import org.apache.poi.xssf.usermodel.XSSFCellStyle;
import org.apache.poi.xssf.usermodel.XSSFFont;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Configuration;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.FileCopyUtils;

import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.itextpdf.text.BaseColor;
import com.itextpdf.text.Document;
import com.itextpdf.text.DocumentException;
import com.itextpdf.text.Element;
import com.itextpdf.text.Font;
import com.itextpdf.text.FontFactory;
import com.itextpdf.text.PageSize;
import com.itextpdf.text.Paragraph;
import com.itextpdf.text.Phrase;
import com.itextpdf.text.pdf.PdfPCell;
import com.itextpdf.text.pdf.PdfPTable;
import com.itextpdf.text.pdf.PdfWriter;
import com.polus.fibicomp.awardbudgetapiintegration.dao.BudgetAPIDao;
import com.polus.fibicomp.awardbudgetapiintegration.pojo.BudgetAPIResponse;
import com.polus.fibicomp.awardbudgetapiintegration.vo.AwardBudgetPrintVO;
import com.polus.fibicomp.awardbudgetapiintegration.vo.BudgetAPIVO;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.print.service.PrintService;

@Configuration
@Service(value = "budgetAPIService")
public class BudgetAPIServiceImpl implements BudgetAPIService {

	protected static Logger logger = LogManager.getLogger(BudgetAPIServiceImpl.class.getName());

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private BudgetAPIDao budgetAPIDao;
	
	@Autowired
    private CommonService commonService;

	@Autowired
	private PrintService printService;

	private String budgetAPI;
	
	private String authorizationCode;
	
	private String budgetYearToFetch;

	DecimalFormat decimalFormat = new DecimalFormat(Constants.NUMBER_FORMAT_WITH_DECIMAL);

	private static final String PUBLIC = "public";
	 
    private static final String RESULT = "Result";
	 
	private static final String CONTENT_TYPE = "application/octet-stream";
	 
	private static final String CACHE_CONTROL = "must-revalidate, post-check=0, pre-check=0";
	 
	private static final String CONTENT_DISPOSITION = "Content-Disposition";
	 
	private static final String ATTACHMENT_FILENAME = "attachment; filename=\"";

	@Override
	public void fetchBudgetAPIResponse() {
		try {
			getAwardBudgetConfiguration();
			budgetAPI = budgetAPI.concat(budgetYearToFetch) ;
			logger.info("Award Budget API call for Year: {}", budgetYearToFetch);
			URL url = new URL(budgetAPI);
			HttpURLConnection conn = (HttpURLConnection) url.openConnection();
			conn.setRequestMethod("GET");
			conn.setRequestProperty("Accept", "application/json");
			conn.setRequestProperty("Authorization", authorizationCode);
			if (conn.getResponseCode() != 200) {
				throw new Exception("Failed : HTTP error code : " + conn.getResponseCode());
			}
			BufferedReader br = new BufferedReader(new InputStreamReader((conn.getInputStream())));
			String output = br.readLine();
			ObjectMapper mapper = new ObjectMapper();
			mapper.disable(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES);
			List<BudgetAPIResponse> budgetAPIResponses = Arrays.asList(mapper.readValue(output, BudgetAPIResponse[].class));
			if (budgetAPIResponses != null && !budgetAPIResponses.isEmpty()) {
				int budgetDataCount = budgetAPIResponses.size();
				logger.info("Award Budget API data count: {}", budgetDataCount);
				for (BudgetAPIResponse apiResponse : budgetAPIResponses) {
					apiResponse.setUpdateUser(Constants.QUICKSTART);
					budgetAPIDao.saveBudgetAPIResponse(apiResponse);
				}
			}
			logger.info("Award Budget API service execution completed at : {}", commonDao.getCurrentTimestamp());
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	private void getAwardBudgetConfiguration() {
		budgetAPI = budgetAPIDao.getBudgetIntegrationConfigurationValue(Constants.AWARD_BUDGET_API);
		authorizationCode = budgetAPIDao.getBudgetIntegrationConfigurationValue(Constants.BUDGET_API_AUTHORIZATION);
		budgetYearToFetch = commonDao.getParameterValueAsString(Constants.YEAR_TO_FETCH);	
	}

	@Override
	public String fetchBudgetAPIResponseBasedOnParams(BudgetAPIVO budgetAPIVO) {
		List<BudgetAPIResponse> budgetAPIresponses = budgetAPIDao.fetchBudgetAPIResponse(budgetAPIVO.getProjectNumber(), Integer.parseInt(budgetAPIVO.getYear()));
		if (budgetAPIresponses != null && !budgetAPIresponses.isEmpty()) {
			BudgetAPIResponse response = budgetAPIresponses.get(0);
			if (response != null && response.getLongName() != null && response.getProjectName() != null) {
				budgetAPIVO.setProjectName(response.getProjectName());
				budgetAPIVO.setLongName(response.getLongName());
			}
			budgetAPIVO.setBudgetAPIResponses(budgetAPIresponses);
		}
		return commonDao.convertObjectToJSON(budgetAPIVO);
	}

	@Override
	public ResponseEntity<byte[]> generateAwardBudgetIntegrationReport(HttpServletResponse response, AwardBudgetPrintVO vo) {
		ResponseEntity<byte[]> attachmentData = null;
		try {
			List<BudgetAPIResponse> awardBudgetresponses = budgetAPIDao.fetchBudgetAPIResponse(vo.getProjectNumber(), vo.getYear());
			if (vo.getExportType().equals("pdf")) {
				attachmentData = generateAwardBudgetPDFReport(awardBudgetresponses, response, attachmentData, vo);
			} else {
				attachmentData = generateAwardBudgetExcelReport(awardBudgetresponses, attachmentData, vo);
			}
		} catch (Exception e) {
			logger.error("Exception in generateAwardBudgetIntegrationReport : {} ", e.getMessage());
		}
		return attachmentData;
	}

	public ResponseEntity<byte[]> generateAwardBudgetPDFReport(List<BudgetAPIResponse> awardBudgetresponses, HttpServletResponse response, ResponseEntity<byte[]> attachmentData, AwardBudgetPrintVO vo) {
		try {
			ByteArrayInputStream bis = generateAwardBudgetPDF(vo, awardBudgetresponses);
			byte[] mergedOutput = IOUtils.toByteArray(bis);
			String generatedFileName = RESULT + System.nanoTime() + ".pdf";
			HttpHeaders headers = new HttpHeaders();
			headers.setContentType(MediaType.parseMediaType(CONTENT_TYPE));
			headers.setContentDispositionFormData(generatedFileName, generatedFileName);
			headers.setContentLength(mergedOutput.length);
			headers.setCacheControl(CACHE_CONTROL);
			headers.setPragma(PUBLIC);
			attachmentData = new ResponseEntity<>(mergedOutput, headers, HttpStatus.OK);
			response.setCharacterEncoding(StandardCharsets.UTF_8.name());
			response.setContentType(CONTENT_TYPE);
			response.setContentLength(mergedOutput.length);
			response.setHeader(CONTENT_DISPOSITION, ATTACHMENT_FILENAME + generatedFileName + "\"");
			FileCopyUtils.copy(mergedOutput, response.getOutputStream());
		} catch (Exception e) {
			logger.error("Exception in generateAwardBudgetReport : {}", e.getMessage());
		}
		return attachmentData;
	}

	private ByteArrayInputStream generateAwardBudgetPDF(AwardBudgetPrintVO vo, List<BudgetAPIResponse> awardBudgetresponses) throws DocumentException {
		Document document = new Document();
		document.setPageSize(PageSize.A4.rotate());
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		document.setMargins(40, 40, 80, 40);
		PdfWriter writer = PdfWriter.getInstance(document, out);
		commonService.addPdfHeaderAndFooter(writer);
		document.open();
		Font headFont = FontFactory.getFont(FontFactory.TIMES_BOLD);
		headFont.setSize(16);
		Font bodyFont = FontFactory.getFont(FontFactory.HELVETICA);
		bodyFont.setSize(10);
		Font bodyTextFont = FontFactory.getFont(FontFactory.HELVETICA_BOLD);
		bodyFont.setSize(10);
		Font tableLabelFont = FontFactory.getFont(FontFactory.TIMES_BOLD);
		bodyFont.setSize(10);
		Font subHeadFont = FontFactory.getFont(FontFactory.TIMES_BOLDITALIC);
		subHeadFont.setColor(255, 255, 255);
		subHeadFont.setSize(14);
		try {
			Paragraph pdfHeading = new Paragraph("Award Budget Summary", headFont);
			pdfHeading.setAlignment(Element.ALIGN_CENTER);
			pdfHeading.setSpacingBefore(5f);
			pdfHeading.setSpacingAfter(7f);
			document.add(pdfHeading);
			printService.prepareAwardGeneralInfo(document, subHeadFont, bodyFont, bodyTextFont, null, Boolean.FALSE, null, vo.getAwardId().toString());
			prepareAwardBudgetOverviewDetails(document, subHeadFont, bodyFont, bodyTextFont, awardBudgetresponses);
			if (awardBudgetresponses != null && !awardBudgetresponses.isEmpty()) {
				prepareAwardBudgetSummaryDetail(document, subHeadFont, bodyFont, tableLabelFont, awardBudgetresponses);
			}
			document.close();
		} catch (DocumentException ex) {
			logger.error("error in generateAwardBudgetPDF {}", ex.getMessage());
		}
		return new ByteArrayInputStream(out.toByteArray());
	}

	private void prepareAwardBudgetSummaryDetail(Document document, Font subHeadFont, Font bodyFont, Font tableLabelFont, List<BudgetAPIResponse> awardBudgetresponses) {
		try {
			PdfPTable budgetHeading = new PdfPTable(1);
			budgetHeading.setWidthPercentage(100);
			budgetHeading.setSpacingBefore(120f);
			budgetHeading.addCell(getSubHeadingCell("Budget Detail", Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, subHeadFont));
			document.add(budgetHeading);
			PdfPTable budgetDetail = new PdfPTable(7);
			budgetDetail.setWidthPercentage(100);
			budgetDetail.addCell(getTableHeadingCell("Task Name", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, tableLabelFont));
			budgetDetail.addCell(getTableHeadingCell("Task Number", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, tableLabelFont));
			budgetDetail.addCell(getTableHeadingCell("Expenditure Type", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, tableLabelFont));
			budgetDetail.addCell(getTableHeadingCell("Budget (" + Constants.DOLLAR_SYMBOL + ")", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, tableLabelFont));
			budgetDetail.addCell(getTableHeadingCell("Actual (" + Constants.DOLLAR_SYMBOL + ")", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, tableLabelFont));
			budgetDetail.addCell(getTableHeadingCell("Commitment (" + Constants.DOLLAR_SYMBOL + ")", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, tableLabelFont));
			budgetDetail.addCell(getTableHeadingCell("Fund Available (" + Constants.DOLLAR_SYMBOL + ")", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, tableLabelFont));

			setAwardBudgetDetail(tableLabelFont, bodyFont, budgetDetail, awardBudgetresponses);
			document.add(budgetDetail);
		} catch (Exception e) {
			logger.error("error in prepareAwardBudgetSummaryDetail {}", e.getMessage());
		}
	}

	private PdfPTable setAwardBudgetDetail(Font tableLabelFont, Font bodyFont, PdfPTable budgetDetail, List<BudgetAPIResponse> awardBudgetresponses) {
		BigDecimal totalBudget = BigDecimal.ZERO;
		BigDecimal totalActualAmount = BigDecimal.ZERO;
		BigDecimal totalCommitmentAmount = BigDecimal.ZERO;
		BigDecimal totalFundAvailable = BigDecimal.ZERO;
		for (BudgetAPIResponse awardBudgetresponse : awardBudgetresponses) {
			budgetDetail.addCell(getTableCell(awardBudgetresponse.getTaskName() != null ? awardBudgetresponse.getTaskName() : "", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
			budgetDetail.addCell(getTableCell(awardBudgetresponse.getTaskNumber() != null ? awardBudgetresponse.getTaskNumber() : "", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
			budgetDetail.addCell(getTableCell(awardBudgetresponse.getExpenditureType() != null ? awardBudgetresponse.getExpenditureType() : "", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
			totalBudget = totalBudget.add(awardBudgetresponse.getBudget() != null ? awardBudgetresponse.getBudget() : BigDecimal.ZERO);
			budgetDetail.addCell(getTableCell(awardBudgetresponse.getBudget() != null ? awardBudgetresponse.getBudget().toString()	: "0.00", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
			totalActualAmount = totalActualAmount.add(awardBudgetresponse.getActual() != null ? awardBudgetresponse.getActual() : BigDecimal.ZERO);
			budgetDetail.addCell(getTableCell(awardBudgetresponse.getActual() != null ? awardBudgetresponse.getActual().toString() : "0.00", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
			totalCommitmentAmount = totalCommitmentAmount.add(awardBudgetresponse.getCommitment() != null ? awardBudgetresponse.getCommitment()	: BigDecimal.ZERO);
			budgetDetail.addCell(getTableCell(awardBudgetresponse.getCommitment() != null ? awardBudgetresponse.getCommitment().toString() : "0.00", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
			totalFundAvailable = totalFundAvailable.add(awardBudgetresponse.getFundAvailable() != null ? awardBudgetresponse.getFundAvailable()	: BigDecimal.ZERO);
			budgetDetail.addCell(getTableCell(awardBudgetresponse.getFundAvailable() != null ? awardBudgetresponse.getFundAvailable().toString() : "0.00", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
		}
		setBudgetTotalDetail(budgetDetail, tableLabelFont, totalBudget, totalActualAmount, totalCommitmentAmount, totalFundAvailable);
		return budgetDetail;
	}

	private PdfPTable setBudgetTotalDetail(PdfPTable budgetDetail, Font tableLabelFont, BigDecimal totalBudget, BigDecimal totalActualAmount, BigDecimal totalCommitmentAmount, BigDecimal totalFundAvailable) {
		PdfPCell mergeCell = new PdfPCell(new Phrase(" "));
		mergeCell.setColspan(2);
		mergeCell.setRowspan(2);
		budgetDetail.addCell(mergeCell);
		budgetDetail.addCell(getTableCell("TOTAL", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, tableLabelFont));
		budgetDetail.addCell(getTableCell(decimalFormat.format(totalBudget), Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, tableLabelFont));
		budgetDetail.addCell(getTableCell(decimalFormat.format(totalActualAmount), Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, tableLabelFont));
		budgetDetail.addCell(getTableCell(decimalFormat.format(totalCommitmentAmount), Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, tableLabelFont));
		budgetDetail.addCell(getTableCell(decimalFormat.format(totalFundAvailable), Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, tableLabelFont));
		return budgetDetail;
	}

	public ResponseEntity<byte[]> generateAwardBudgetExcelReport(List<BudgetAPIResponse> awardBudgetresponses, ResponseEntity<byte[]> attachmentData, AwardBudgetPrintVO vo) {
		XSSFWorkbook workbook = new XSSFWorkbook();
		try {
			XSSFSheet sheet = workbook.createSheet("Award Budget Detail");
			commonService.addDetailsInHeader(workbook, sheet);
			prepareExcelSheetForAwardBudgetDetail(awardBudgetresponses, vo.getAwardId().toString(), sheet, workbook);
			ByteArrayOutputStream bos = new ByteArrayOutputStream();
			workbook.write(bos);
			attachmentData = getResponseEntityForExcelDownload(bos.toByteArray());
		} catch (Exception e) {
			logger.error("error in generateAwardBudgetExcelReport {}", e.getMessage());
		} finally {
			try {
				workbook.close();
			} catch (Exception e) {
				logger.error("error in generateAwardBudgetExcelReport {}", e.getMessage());
			}
		}
		return attachmentData;
	}

	private void prepareExcelSheetForAwardBudgetDetail(List<BudgetAPIResponse> awardBudgetresponses, String awardId, XSSFSheet sheet, XSSFWorkbook workbook) {
		try {
			XSSFCellStyle tableBodyStyle = workbook.createCellStyle();
			sheet.addMergedRegion(new CellRangeAddress(0, 0, 0, 4));
			int rowNumber = 0;
			rowNumber = printService.prepareAwardGeneralInformationForExcel(sheet, rowNumber, awardId, workbook, null, Boolean.FALSE);
			String heading = "Award Budget Summary";
			int totalColumnCount = 7;
			rowNumber = rowNumber + 1;
			printService.prepareExcelSheetHeading(sheet, heading, workbook, totalColumnCount, rowNumber);
			rowNumber = rowNumber + 1;
			Object[] tableOverviewHeading = { "Year", "Project Number", "Project Code", "Project Name" };
			prepareExcelSheetHeader(sheet, tableOverviewHeading, workbook, tableBodyStyle, rowNumber++);
			if (awardBudgetresponses != null && !awardBudgetresponses.isEmpty()) {
				rowNumber = prepareAwardBudgetOverviewDetail(awardBudgetresponses, rowNumber, sheet, tableBodyStyle);
			}
			rowNumber = rowNumber + 2;
			prepareAwardBudgetDetailForExcel(awardBudgetresponses, sheet, workbook, tableBodyStyle, rowNumber);
		} catch (Exception e) {
			logger.error("error in prepareExcelSheetForAwardBudgetDetail {}", e.getMessage());
		}
	}

	private XSSFWorkbook prepareAwardBudgetDetailForExcel(List<BudgetAPIResponse> awardBudgetresponses, XSSFSheet sheet, XSSFWorkbook workbook, XSSFCellStyle tableBodyStyle, Integer rowNumber) {
		BigDecimal totalBudget = BigDecimal.ZERO;
		BigDecimal totalActualAmount = BigDecimal.ZERO;
		BigDecimal totalCommitmentAmount = BigDecimal.ZERO;
		BigDecimal totalFundAvailable = BigDecimal.ZERO;
		Object[] tableHeading = { "Task Name", "Task Number", "Expenditure Type",
				"Budget(" + Constants.DOLLAR_SYMBOL + ")", "Actual(" + Constants.DOLLAR_SYMBOL + ")",
				"Commitment(" + Constants.DOLLAR_SYMBOL + ")", "Fund Available(" + Constants.DOLLAR_SYMBOL + ")" };
		printService.prepareExcelSheetHeader(sheet, tableHeading, workbook, tableBodyStyle, rowNumber++);
		if (awardBudgetresponses != null && !awardBudgetresponses.isEmpty()) {
			for (BudgetAPIResponse awardBudgetresponse : awardBudgetresponses) {
				totalBudget = totalBudget.add(awardBudgetresponse.getBudget() != null ? awardBudgetresponse.getBudget() : BigDecimal.ZERO);
				totalActualAmount = totalActualAmount.add(awardBudgetresponse.getActual() != null ? awardBudgetresponse.getActual() : BigDecimal.ZERO);
				totalCommitmentAmount = totalCommitmentAmount.add(awardBudgetresponse.getCommitment() != null ? awardBudgetresponse.getCommitment()	: BigDecimal.ZERO);
				totalFundAvailable = totalFundAvailable.add(awardBudgetresponse.getFundAvailable() != null ? awardBudgetresponse.getFundAvailable() : BigDecimal.ZERO);
				rowNumber = prepareAwardBudgetDetail(awardBudgetresponse, rowNumber, sheet, tableBodyStyle);
				++rowNumber;
			}
			prepareBudgetTotalDetail(workbook, totalBudget, totalActualAmount, totalCommitmentAmount, totalFundAvailable, rowNumber, sheet);
		}
		return workbook;
	}

	private void prepareBudgetTotalDetail(XSSFWorkbook workbook, BigDecimal totalBudget, BigDecimal totalActualAmount, BigDecimal totalCommitmentAmount, BigDecimal totalFundAvailable, int rowNumber, XSSFSheet sheet) {

		XSSFCellStyle tableCellStyle = workbook.createCellStyle();
		XSSFFont tableHeadFont = workbook.createFont();
		tableHeadFont.setBold(true);
		tableHeadFont.setFontHeightInPoints((short) 12);
		tableCellStyle.setFont(tableHeadFont);

		Row totalRow = sheet.createRow(rowNumber++);
		Cell toatalCell = totalRow.createCell(2);
		toatalCell.setCellStyle(tableCellStyle);
		toatalCell.setCellValue("Total");

		Cell totalBudgetCell = totalRow.createCell(3);
		totalBudgetCell.setCellStyle(tableCellStyle);
		totalBudgetCell.setCellValue(decimalFormat.format(totalBudget));

		Cell totalActualCell = totalRow.createCell(4);
		totalActualCell.setCellStyle(tableCellStyle);
		totalActualCell.setCellValue(decimalFormat.format(totalActualAmount));

		Cell totalCommitmentCell = totalRow.createCell(5);
		totalCommitmentCell.setCellStyle(tableCellStyle);
		totalCommitmentCell.setCellValue(decimalFormat.format(totalCommitmentAmount));

		Cell totalFundAvailableCell = totalRow.createCell(6);
		totalFundAvailableCell.setCellStyle(tableCellStyle);
		totalFundAvailableCell.setCellValue(decimalFormat.format(totalFundAvailable));
	}

	private int prepareAwardBudgetOverviewDetail(List<BudgetAPIResponse> awardBudgetresponses, int rowNumber, XSSFSheet sheet, XSSFCellStyle tableBodyStyle) {
		Row row = sheet.createRow(rowNumber);
		Integer cellNumber = 0;

		Cell yearCell = assignCell(cellNumber, tableBodyStyle, row);
		if (awardBudgetresponses.get(0).getYear() != null) {
			yearCell.setCellValue(awardBudgetresponses.get(0).getYear().toString());
		} else {
			yearCell.setCellValue(" ");
		}
		cellNumber++;

		Cell projectNumberCell = assignCell(cellNumber, tableBodyStyle, row);
		if (awardBudgetresponses.get(0).getProjectNumber() != null) {
			projectNumberCell.setCellValue(awardBudgetresponses.get(0).getProjectNumber());
		} else {
			projectNumberCell.setCellValue(" ");
		}
		cellNumber++;

		Cell projectCodeCell = assignCell(cellNumber, tableBodyStyle, row);
		if (awardBudgetresponses.get(0).getProjectName() != null) {
			projectCodeCell.setCellValue(awardBudgetresponses.get(0).getProjectName());
		} else {
			projectCodeCell.setCellValue(" ");
		}
		cellNumber++;

		Cell projectNameCell = assignCell(cellNumber, tableBodyStyle, row);
		if (awardBudgetresponses.get(0).getLongName() != null) {
			sheet.addMergedRegion(new CellRangeAddress(row.getRowNum(), row.getRowNum(), projectNameCell.getColumnIndex(), projectNameCell.getColumnIndex() + 3));
			setBordersToMergedCells(sheet, new CellRangeAddress(row.getRowNum(), row.getRowNum(), projectNameCell.getColumnIndex(), projectNameCell.getColumnIndex() + 3));
			projectNameCell.setCellValue(convertStringToParagraph(awardBudgetresponses.get(0).getLongName()));
			row.setHeightInPoints(projectNameCell.getStringCellValue().split("\n").length * sheet.getDefaultRowHeightInPoints());
		} else {
			projectNameCell.setCellValue(" ");
		}
		cellNumber++;
		return rowNumber;
	}

	private int prepareAwardBudgetDetail(BudgetAPIResponse awardBudgetresponse, int rowNumber, XSSFSheet sheet, XSSFCellStyle tableBodyStyle) {
		Row row = sheet.createRow(rowNumber);
		Integer cellNumber = 0;

		Cell taskNameCell = assignCell(cellNumber, tableBodyStyle, row);
		if (awardBudgetresponse.getTaskName() != null) {
			taskNameCell.setCellValue(awardBudgetresponse.getTaskName());
		} else {
			taskNameCell.setCellValue(" ");
		}
		cellNumber++;

		Cell taskNumberCell = assignCell(cellNumber, tableBodyStyle, row);
		if (awardBudgetresponse.getTaskNumber() != null) {
			taskNumberCell.setCellValue(awardBudgetresponse.getTaskNumber());
		} else {
			taskNumberCell.setCellValue(" ");
		}
		cellNumber++;

		Cell expenditureTypeCell = assignCell(cellNumber, tableBodyStyle, row);
		if (awardBudgetresponse.getExpenditureType() != null) {
			expenditureTypeCell.setCellValue(awardBudgetresponse.getExpenditureType());
		} else {
			expenditureTypeCell.setCellValue(" ");
		}
		cellNumber++;

		Cell budgetCell = assignCell(cellNumber, tableBodyStyle, row);
		if (awardBudgetresponse.getBudget() != null) {
			budgetCell.setCellValue(decimalFormat.format(awardBudgetresponse.getBudget()));
		} else {
			budgetCell.setCellValue("0.00");
		}
		cellNumber++;

		Cell actualCell = assignCell(cellNumber, tableBodyStyle, row);
		if (awardBudgetresponse.getActual() != null) {
			actualCell.setCellValue(decimalFormat.format(awardBudgetresponse.getActual()));
		} else {
			actualCell.setCellValue("0.00");
		}
		cellNumber++;

		Cell commitmentCell = assignCell(cellNumber, tableBodyStyle, row);
		if (awardBudgetresponse.getCommitment() != null) {
			commitmentCell.setCellValue(decimalFormat.format(awardBudgetresponse.getCommitment()));
		} else {
			commitmentCell.setCellValue("0.00");
		}
		cellNumber++;

		Cell fundAvailableCell = assignCell(cellNumber, tableBodyStyle, row);
		if (awardBudgetresponse.getFundAvailable() != null) {
			fundAvailableCell.setCellValue(decimalFormat.format(awardBudgetresponse.getFundAvailable()));
		} else {
			fundAvailableCell.setCellValue("0.00");
		}
		cellNumber++;
		return rowNumber;
	}

	private Cell assignCell(int cellNumber, XSSFCellStyle tableBodyStyle, Row row) {
		Cell cell = row.createCell(cellNumber);
		cell.setCellStyle(tableBodyStyle);
		return cell;
	}

	private XSSFWorkbook prepareExcelSheetHeader(XSSFSheet sheet, Object[] tableHeadingRow, XSSFWorkbook workbook, XSSFCellStyle tableBodyStyle, int rowNumber) {
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
		for (Object heading : tableHeadingRow) {
			Cell cell = tableHeadRow.createCell(headingCellNumber++);
			if (heading.equals("Project Name")) {
				sheet.addMergedRegion(new CellRangeAddress(tableHeadRow.getRowNum(), tableHeadRow.getRowNum(), cell.getColumnIndex(), cell.getColumnIndex() + 3));
			}
			cell.setCellValue((String) heading);
			cell.setCellStyle(tableHeadStyle);
		}
		printService.autoSizeColumns(workbook, rowNumber);
		return workbook;
	}

	private String convertStringToParagraph(String description) {
		StringBuilder descriptionDetail = new StringBuilder("");
		if (description != null) {
			String[] words = description.split("\\s+");
			Integer lineSperator = 10;
			for (int i = 0; i < words.length; i++) {
				if (i < lineSperator) {
					descriptionDetail = descriptionDetail.append(words[i] + " ");
				}
				if (i == lineSperator) {
					descriptionDetail = descriptionDetail.append(words[i]).append("\n");
					lineSperator = lineSperator + 10;
				}
			}
		}
		return descriptionDetail.toString();
	}

	public void setBordersToMergedCells(XSSFSheet sheet, CellRangeAddress rangeAddress) {
		RegionUtil.setBorderTop(BorderStyle.HAIR, rangeAddress, sheet);
		RegionUtil.setBorderLeft(BorderStyle.HAIR, rangeAddress, sheet);
		RegionUtil.setBorderRight(BorderStyle.HAIR, rangeAddress, sheet);
		RegionUtil.setBorderBottom(BorderStyle.HAIR, rangeAddress, sheet);
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

	public static PdfPCell getGeneralDetailsCell(String text, int verticalAlignment, int horizontalAlignmentt, Font bodyFont) {
		PdfPCell cell = new PdfPCell(new Phrase(text, bodyFont));
		cell.setPadding(5);
		cell.setVerticalAlignment(verticalAlignment);
		cell.setHorizontalAlignment(horizontalAlignmentt);
		cell.setBorder(PdfPCell.NO_BORDER);
		return cell;
	}

	public static PdfPCell getTableCell(String text, int verticalAlignment, int horizontalAlignmentt, Font bodyFont) {
		PdfPCell cell = new PdfPCell(new Phrase(text, bodyFont));
		cell.setPadding(10);
		cell.setVerticalAlignment(verticalAlignment);
		cell.setHorizontalAlignment(horizontalAlignmentt);
		return cell;
	}

	public static PdfPCell getTableHeadingCell(String text, int verticalAlignment, int horizontalAlignmentt, Font bodyFont) {
		PdfPCell cell = new PdfPCell(new Phrase(text, bodyFont));
		cell.setPadding(10);
		cell.setVerticalAlignment(verticalAlignment);
		cell.setHorizontalAlignment(horizontalAlignmentt);
		cell.setBackgroundColor(new BaseColor(176, 196, 222));
		return cell;
	}

	public static PdfPCell getSubHeadingCell(String text, int verticalAlignment, int horizontalAlignmentt, Font bodyFont) {
		PdfPCell cell = new PdfPCell(new Phrase(text, bodyFont));
		cell.setPadding(5);
		cell.setVerticalAlignment(verticalAlignment);
		cell.setHorizontalAlignment(horizontalAlignmentt);
		cell.setBackgroundColor(new BaseColor(0, 128, 128));
		return cell;
	}

	private void prepareAwardBudgetOverviewDetails(Document document, Font subHeadFont, Font bodyFont, Font bodyTextFont, List<BudgetAPIResponse> awardBudgetresponses) throws DocumentException {
		PdfPTable budgetOverviewHeading = new PdfPTable(1);
		budgetOverviewHeading.setWidthPercentage(100);
		budgetOverviewHeading.setWidths(new int[] { 12 });
		budgetOverviewHeading.setSpacingBefore(40f);
		budgetOverviewHeading.addCell(getSubHeadingCell("Budget Overview", Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, subHeadFont));
		document.add(budgetOverviewHeading);

		PdfPTable budgetOverviewDetails = new PdfPTable(6);
		budgetOverviewDetails.setWidthPercentage(100);
		budgetOverviewDetails.setWidths(new int[] { 3, 1, 5, 3, 1, 5 });
		if (awardBudgetresponses != null && !awardBudgetresponses.isEmpty()) {
			budgetOverviewDetails.setSpacingBefore(5f);
			budgetOverviewDetails.addCell(getGeneralDetailsCell("Year ", Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(awardBudgetresponses.get(0).getYear() != null ? awardBudgetresponses.get(0).getYear().toString() : "", Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyTextFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell("Project Number ", Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(awardBudgetresponses.get(0).getProjectNumber() != null	? awardBudgetresponses.get(0).getProjectNumber() : " ", Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyTextFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell("Project Code ", Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(awardBudgetresponses.get(0).getProjectName() != null ? awardBudgetresponses.get(0).getProjectName()	: " ", Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyTextFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell("Project Name ", Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(awardBudgetresponses.get(0).getLongName() != null ? awardBudgetresponses.get(0).getLongName() : "", Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyTextFont));
			document.add(budgetOverviewDetails);
		}
	}
}
