package com.polus.fibicomp.dashboard.service;

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Set;

import javax.validation.Valid;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.poi.ss.usermodel.BorderStyle;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.HorizontalAlignment;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.util.CellRangeAddress;
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
import org.springframework.transaction.annotation.Transactional;

import com.itextpdf.text.BaseColor;
import com.itextpdf.text.Chunk;
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
import com.polus.fibicomp.award.dao.AwardDao;
import com.polus.fibicomp.award.expense.service.AwardExpenseService;
import com.polus.fibicomp.award.expense.vo.AwardExpenseDetailVO;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardPerson;
import com.polus.fibicomp.agreements.dao.AgreementDao;
import com.polus.fibicomp.agreements.pojo.AgreementHeader;

import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.dashboard.dao.DashboardDao;
import com.polus.fibicomp.dashboard.vo.AgreementDashboardVO;
import com.polus.fibicomp.dashboard.vo.AwardDashboardVO;
import com.polus.fibicomp.dashboard.vo.ClaimDashboardVO;
import com.polus.fibicomp.dashboard.vo.CoiDashboardVO;
import com.polus.fibicomp.dashboard.vo.GrantCallDashboardVO;
import com.polus.fibicomp.dashboard.vo.InstituteProposalDashboardVO;
import com.polus.fibicomp.dashboard.vo.NegotiationDashboardVO;
import com.polus.fibicomp.dashboard.vo.ProgressReportDashboardVO;
import com.polus.fibicomp.dashboard.vo.ProposalDashboardVO;
import com.polus.fibicomp.dashboard.vo.ServiceRequestDashboardVO;
import com.polus.fibicomp.evaluation.dao.EvaluationDao;
import com.polus.fibicomp.evaluation.pojo.EvaluationStop;
import com.polus.fibicomp.grantcall.dao.GrantCallDao;
import com.polus.fibicomp.grantcall.pojo.GrantCall;
import com.polus.fibicomp.ip.pojo.InstituteProposal;
import com.polus.fibicomp.login.dao.LoginDao;
import com.polus.fibicomp.person.service.PersonService;
import com.polus.fibicomp.pojo.DashBoardProfile;
import com.polus.fibicomp.print.service.PrintService;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.proposal.pojo.ProposalPerson;
import com.polus.fibicomp.roles.pojo.Role;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequest;
import com.polus.fibicomp.view.AwardView;
import com.polus.fibicomp.vo.CommonVO;
import com.polus.fibicomp.vo.ParameterVO;

@Transactional
@Service(value = "dashboardService")
public class DashboardServiceImpl implements DashboardService {

	protected static Logger logger = LogManager.getLogger(DashboardServiceImpl.class.getName());

	@Autowired
	private DashboardDao dashboardDao;

	@Autowired
	private LoginDao loginDao;

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private EvaluationDao evaluationDao;
	
	@Autowired
	private CommonService commonService;

	@Autowired
	private GrantCallDao grantCallDao;

	@Autowired
	private AgreementDao agreementDao;

	@Autowired
	private PersonService personService;

	@Autowired
	private AwardExpenseService awardExpenseService;
	
	@Autowired
    private AwardDao awardDao;
	
	@Autowired
	private PrintService printService;
	
	DecimalFormat numberFormat = new DecimalFormat(Constants.NUMBER_FORMAT_WITHOUT_DECIMAL);
	DecimalFormat decimalFormat = new DecimalFormat(Constants.NUMBER_FORMAT_WITH_DECIMAL);
	private static final String DOCUMENT_HEADING = "documentHeading : {}";
	private static final String TITLE = "Title";
	private static final Integer PDF_WIDTH_PERCENTAGE = 95;

    @Override
    public String getDashBoardData(CommonVO vo) throws Exception {
        logger.info("-------- getDashBoardData ---------");
        DashBoardProfile dashBoardProfile = new DashBoardProfile();
        String requestType = vo.getTabIndex();
        logger.info("requestType : {}", requestType);
        try {
            if (requestType.equals("DISCLOSURE")) {
                dashBoardProfile = dashboardDao.getDashBoardDataForDisclosures(vo);
            }
            if (requestType.equals("COMMITTEE")) {
                dashBoardProfile = dashboardDao.getDashBoardDataForCommittee(vo);
            }
            if (requestType.equals("SCHEDULE")) {
                dashBoardProfile = dashboardDao.getDashBoardDataForCommitteeSchedule(vo);
            }
            dashBoardProfile.setUnitAdministrators(loginDao.isUnitAdmin(vo.getPersonId()));
        } catch (Exception e) {
            logger.error("Error in method getDashBoardData", e);
        }
        return commonDao.convertObjectToJSON(dashBoardProfile);
    }

    private int getColumnsCount(XSSFSheet xssfSheet) {
        int columnCount = 0;
        Iterator<Row> rowIterator = xssfSheet.iterator();
        while (rowIterator.hasNext()) {
            Row row = rowIterator.next();
            List<Cell> cells = new ArrayList<>();
            Iterator<Cell> cellIterator = row.cellIterator();
            while (cellIterator.hasNext()) {
                cells.add(cellIterator.next());
            }
            for (int cellIndex = cells.size(); cellIndex >= 1; cellIndex--) {
                Cell cell = cells.get(cellIndex - 1);
                if (cell.toString().trim().isEmpty()) {
                    cells.remove(cellIndex - 1);
                } else {
                    columnCount = cells.size() > columnCount ? cells.size() : columnCount;
                    break;
                }
            }
        }
        return columnCount;
    }

    @Override
    public byte[] generatePDFFileByteArray(String documentHeading, XSSFWorkbook workbook) {
        byte[] byteArray = null;
        try {
            ByteArrayOutputStream byteArrayOutputStream = new ByteArrayOutputStream();
            if (workbook.getNumberOfSheets() != 0) {
                XSSFSheet worksheet = workbook.getSheetAt(0);
                Iterator<Row> rowIterator = worksheet.iterator();
                Document document = new Document();
                document.setPageSize(PageSize.A4.rotate());
                document.setMargins(40, 40, 80, 40);
                PdfWriter writer = PdfWriter.getInstance(document, byteArrayOutputStream);
                commonService.addPdfHeaderAndFooter(writer);
                document.open();
                Font pdfTitleFont = FontFactory.getFont(FontFactory.HELVETICA_BOLD, 13);
                Paragraph paragraph = new Paragraph(documentHeading, pdfTitleFont);
                paragraph.setAlignment(Element.ALIGN_CENTER);
                document.add(paragraph);
                document.add(Chunk.NEWLINE);
                int columnCount = getColumnsCount(worksheet);
                PdfPTable table = new PdfPTable(columnCount);
                table.setWidthPercentage(PDF_WIDTH_PERCENTAGE);
                PdfPCell table_cell;
                Font tableHeadingFont = new Font(Font.FontFamily.TIMES_ROMAN, 12, Font.BOLD);
                Font tableBodyFont = new Font(Font.FontFamily.TIMES_ROMAN, 12, Font.NORMAL);
                while (rowIterator.hasNext()) {
                    Row row = rowIterator.next();
                    int rowIndex = row.getRowNum();
                    Iterator<Cell> cellIterator = row.cellIterator();
                    while (cellIterator.hasNext()) {
                        Cell cell = cellIterator.next();
                        switch (cell.getCellType()) {
                            case STRING:
                                if (rowIndex == 0) {
                                } else if (rowIndex == 1) {
                                    table_cell = new PdfPCell(new Phrase(cell.getStringCellValue(), tableHeadingFont));
                                    table.addCell(table_cell);
                                } else {
                                    table_cell = new PdfPCell(new Phrase(cell.getStringCellValue(), tableBodyFont));
                                    table.addCell(table_cell);
                                }
                                break;
                            case NUMERIC:
                                Double cellValueInDouble = cell.getNumericCellValue();
                                Integer cellValueInInteger = cellValueInDouble.intValue();
                                String cellValueInString = Integer.toString(cellValueInInteger);
                                table_cell = new PdfPCell(new Phrase(cellValueInString, tableBodyFont));
                                table.addCell(table_cell);
                                break;
                        }
                    }
                }
                document.add(table);
                document.close();
            }
            byteArray = byteArrayOutputStream.toByteArray();
        } catch (Exception e) {
            logger.error("Error in method generatePDFFileByteArray", e);
        }
        return byteArray;
    }

	@Override
	public ResponseEntity<byte[]> getResponseEntityForDownload(CommonVO vo, XSSFWorkbook workbook) throws Exception {
		logger.info("--------- getResponseEntityForExcelOrPDFDownload ---------");
		byte[] byteArray = null;
		String exportType = vo.getExportType();
		String documentHeading = vo.getDocumentHeading();
		logger.info("exportType : {}", exportType);
		logger.info("documentHeading : {}", documentHeading);
		if (exportType.equals("pdf")) {
			byteArray = generatePDFFileByteArray(documentHeading, workbook);
		} else {
			ByteArrayOutputStream bos = new ByteArrayOutputStream();
			workbook.write(bos);
			byteArray = bos.toByteArray();
		}
		return getResponseEntity(byteArray);
	}

	
	@Override
	public ResponseEntity<byte[]> getResponseEntityForDownloadExpense(CommonVO vo, XSSFWorkbook workbook) throws Exception {
		logger.info("--------- getResponseEntityForDownloadExpense ---------");
		byte[] byteArray = null;
		String exportType = vo.getExportType();
		String documentHeading = vo.getDocumentHeading();
		logger.info("exportType : {}", exportType);
		logger.info("documentHeading : {}", documentHeading);
		if (exportType.equals("pdf")) {
			List<AwardExpenseDetailVO> expenseDetails = awardExpenseService.prepareAwardExpenseDetails(vo.getAwardNumber(), vo.getAccountNumber(), vo.getType());
			byteArray = generatePDFFileForExpenseTracking(expenseDetails, vo);
		} else {
			ByteArrayOutputStream bos = new ByteArrayOutputStream();
			workbook.write(bos);
			byteArray = bos.toByteArray();
		}
		return getResponseEntity(byteArray);
	}

	private byte[] generatePDFFileForExpenseTracking(List<AwardExpenseDetailVO> expenseDetails, CommonVO vo) throws DocumentException {
		Document document = new Document();
		document.setPageSize(PageSize.A4_LANDSCAPE.rotate());
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		document.setMargins(40, 40, 80, 40);
		PdfWriter writer = PdfWriter.getInstance(document, out);
		commonService.addPdfHeaderAndFooter(writer);
		document.open();
		Font pdfTitleFont = FontFactory.getFont(FontFactory.TIMES_BOLD);
		pdfTitleFont.setSize(16);
		Font bodyFont = FontFactory.getFont(FontFactory.HELVETICA);
		bodyFont.setSize(10);
		Font bodyTextFont = FontFactory.getFont(FontFactory.HELVETICA_BOLD);
		bodyFont.setSize(10);
		Font tableLabelFont = FontFactory.getFont(FontFactory.TIMES_BOLD);
		bodyFont.setSize(10);
		Font subHeadFont = FontFactory.getFont(FontFactory.TIMES_BOLDITALIC);
		subHeadFont.setColor(255, 255, 255);
		subHeadFont.setSize(12);
		try {
			Paragraph paragraph = new Paragraph(vo.getDocumentHeading(), pdfTitleFont);
			paragraph.setAlignment(Paragraph.ALIGN_CENTER);
			document.add(paragraph);
			Award award = awardDao.fetchAwardByAwardId(vo.getAwardId());
			String awardPersonName = null;
			if (award.getAwardPersons() != null && !award.getAwardPersons().isEmpty()) {
				for (AwardPerson awardPerson : award.getAwardPersons()) {
					if (awardPerson.getPersonId() != null && vo.getPersonId() != null
							&& vo.getPersonId().equals(awardPerson.getPersonId())
							&& awardPerson.getPersonRoleId().equals(Constants.COI_ROLE_CODE)) {
						awardPersonName = awardPerson.getFullName();
					}
				}
			}
			printService.prepareAwardGeneralInfo(document, subHeadFont, bodyFont, bodyTextFont, award, Boolean.TRUE, awardPersonName, null);
			prepareExpenseTrackingPDFData(expenseDetails, document, subHeadFont, bodyFont, tableLabelFont);
			document.close();
		} catch (DocumentException ex) {
		}
		return out.toByteArray();
	}

	private void prepareExpenseTrackingPDFData(List<AwardExpenseDetailVO> expenseDetails, Document document, Font subHeadFont, Font bodyFont, Font tableLabelFont) {
		try {
			PdfPTable expenseTracking = new PdfPTable(1);
			expenseTracking.setWidthPercentage(100);
			expenseTracking.setWidths(new int[] { 12 });
			expenseTracking.setSpacingBefore(20f);
//        expenseTracking.addCell(getSubHeadingCell("Expense Details", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, subHeadFont));
			document.add(expenseTracking);
			PdfPTable tableHeader = null;
			PdfPTable tableValues = null;
			tableHeader = getTableHeaderForExpensePDF(subHeadFont);
			document.add(tableHeader);
			tableValues = getTableValuesForExpensePDF(bodyFont, expenseDetails);
			document.add(tableValues);
		} catch (Exception e) {
			logger.info("Exception in prepareExpenseTrackingPDFData : {}", e);
		}
	}

	private PdfPTable getTableValuesForExpensePDF(Font bodyFont, List<AwardExpenseDetailVO> expenseDetails) {
		PdfPTable tableValues = new PdfPTable(11);
		tableValues.setWidthPercentage(100);
		BigDecimal totalQuantity = BigDecimal.ZERO;
		BigDecimal totalOriginalApprovedBudget = BigDecimal.ZERO;
		BigDecimal totalLatestApprovedBudget = BigDecimal.ZERO;
		BigDecimal totalExpenditureToDate = BigDecimal.ZERO;
		BigDecimal totalBalance = BigDecimal.ZERO;
		BigDecimal totalUtilizationRate = BigDecimal.ZERO;
		BigDecimal totalCommittedAmount = BigDecimal.ZERO;
		BigDecimal totalBalanceLessCommittedAmount = BigDecimal.ZERO;
		BigDecimal totalUtilizationAmount = BigDecimal.ZERO;
		String percentage = "%";
		for (AwardExpenseDetailVO expenseData : expenseDetails) {
			if (expenseData.getBudgetCategory() != null) {
				tableValues.addCell(getTableCell(expenseData.getBudgetCategory(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
			} else {
				tableValues.addCell(getTableCell("", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
			}
			if (expenseData.getLineItem() != null) {
				tableValues.addCell(getTableCell(expenseData.getLineItem(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
			} else {
				tableValues.addCell(getTableCell("", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
			}
			if (expenseData.getInternalOrderCode() != null) {
				tableValues.addCell(getTableCell(expenseData.getInternalOrderCode(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
			} else {
				tableValues.addCell(getTableCell("", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
			}
			if (expenseData.getQuantity() != null) {
				totalQuantity = totalQuantity.add(expenseData.getQuantity());
				tableValues.addCell(getTableCell(numberFormat.format(expenseData.getQuantity()), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
			} else {
				tableValues.addCell(getTableCell("", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
			}
			if (expenseData.getOriginalApprovedBudget() != null) {
				totalOriginalApprovedBudget = totalOriginalApprovedBudget.add(expenseData.getOriginalApprovedBudget());
				tableValues.addCell(getTableCell(Constants.DOLLAR_SYMBOL + decimalFormat.format(expenseData.getOriginalApprovedBudget()),
						PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
			} else {
				tableValues.addCell(getTableCell("", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
			}
			if (expenseData.getLatestApprovedBudget() != null) {
				totalLatestApprovedBudget = totalLatestApprovedBudget.add(expenseData.getLatestApprovedBudget());
				tableValues.addCell(getTableCell(Constants.DOLLAR_SYMBOL + decimalFormat.format(expenseData.getLatestApprovedBudget()),
						PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
			} else {
				tableValues.addCell(getTableCell("", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
			}
			if (expenseData.getExpenditureToDate() != null) {
				totalExpenditureToDate = totalExpenditureToDate.add(expenseData.getExpenditureToDate());
				tableValues.addCell(getTableCell(Constants.DOLLAR_SYMBOL + decimalFormat.format(expenseData.getExpenditureToDate()),
								PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
			} else {
				tableValues.addCell(getTableCell("", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
			}
			if (expenseData.getBalance() != null) {
				totalBalance = totalBalance.add(expenseData.getBalance());
				tableValues.addCell(getTableCell(Constants.DOLLAR_SYMBOL + decimalFormat.format(expenseData.getBalance()),
								PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
			} else {
				tableValues.addCell(getTableCell("", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
			}
			if (expenseData.getUtilizationRate() != null) {
				totalUtilizationRate = totalUtilizationRate.add(expenseData.getUtilizationRate());
				tableValues.addCell(getTableCell(decimalFormat.format(expenseData.getUtilizationRate()) + percentage,
						PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
			} else {
				tableValues.addCell(
						getTableCell("0.0" + percentage, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
			}
			BigDecimal committedAmount = BigDecimal.ZERO;
			committedAmount = expenseData.getCommittedAmount();
			if (committedAmount != null) {
				totalCommittedAmount = totalCommittedAmount.add(committedAmount);
				tableValues.addCell(getTableCell(Constants.DOLLAR_SYMBOL + decimalFormat.format(committedAmount),
						PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
			} else {
				tableValues.addCell(getTableCell("", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
			}
			BigDecimal balanceLessCommittedAmount = BigDecimal.ZERO;
			balanceLessCommittedAmount = expenseData.getBalanceCommittedBudget();
			if (balanceLessCommittedAmount != null) {
				totalBalanceLessCommittedAmount = totalBalanceLessCommittedAmount.add(balanceLessCommittedAmount);
				tableValues.addCell(getTableCell(Constants.DOLLAR_SYMBOL + decimalFormat.format(balanceLessCommittedAmount),
								PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
			} else {
				tableValues.addCell(getTableCell("", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
			}
		}
		tableValues.addCell(getSubTableCell("TOTAL", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
		tableValues.addCell(getTableCell(numberFormat.format(totalQuantity), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
		tableValues.addCell(getTableCell(Constants.DOLLAR_SYMBOL + decimalFormat.format(totalOriginalApprovedBudget), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
		tableValues.addCell(getTableCell(Constants.DOLLAR_SYMBOL + decimalFormat.format(totalLatestApprovedBudget), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
		tableValues.addCell(getTableCell(Constants.DOLLAR_SYMBOL + decimalFormat.format(totalExpenditureToDate), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
		tableValues.addCell(getTableCell(Constants.DOLLAR_SYMBOL + decimalFormat.format(totalBalance), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
		if (totalExpenditureToDate != null && totalLatestApprovedBudget != null
				&& totalExpenditureToDate != BigDecimal.ZERO && totalLatestApprovedBudget != BigDecimal.ZERO) {
			totalUtilizationAmount = new BigDecimal(decimalFormat.format(((Double.valueOf(totalExpenditureToDate.toString())
							/ (Double.valueOf(totalLatestApprovedBudget.toString()))) * 100)));
		} else {
			totalUtilizationAmount = BigDecimal.ZERO;
		}
		tableValues.addCell(getTableCell(totalUtilizationAmount != null ? decimalFormat.format(totalUtilizationAmount) + percentage : "",
				PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
		tableValues.addCell(getTableCell(Constants.DOLLAR_SYMBOL + decimalFormat.format(totalCommittedAmount),
				PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
		tableValues.addCell(getTableCell(Constants.DOLLAR_SYMBOL + decimalFormat.format(totalBalanceLessCommittedAmount),
						PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
		return tableValues;
	}

	private PdfPCell getSubTableCell(String text, int verticalAlignment, int horizontalAlignmentt, Font bodyFont) {
		PdfPCell cell = new PdfPCell(new Phrase(text, bodyFont));
		cell.setPadding(10);
		cell.setVerticalAlignment(verticalAlignment);
		cell.setHorizontalAlignment(horizontalAlignmentt);
		cell.setColspan(3);
		return cell;
	}

	public static PdfPCell getTableCell(String text, int verticalAlignment, int horizontalAlignmentt, Font bodyFont) {
		PdfPCell cell = new PdfPCell(new Phrase(text, bodyFont));
		cell.setPadding(10);
		cell.setVerticalAlignment(verticalAlignment);
		cell.setHorizontalAlignment(horizontalAlignmentt);
		return cell;
	}

	private PdfPTable getTableHeaderForExpensePDF(Font subHeadFont) {
		PdfPTable expenseHeader = new PdfPTable(11);
		expenseHeader.setWidthPercentage(100);
		expenseHeader.addCell(getTableHeadingCell("BudgetCategory", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, subHeadFont));
		expenseHeader.addCell(getTableHeadingCell("Budget Line Item", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, subHeadFont));
		expenseHeader.addCell(getTableHeadingCell("WBS Number", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, subHeadFont));
		expenseHeader.addCell(getTableHeadingCell("Quantity", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, subHeadFont));
		expenseHeader.addCell(getTableHeadingCell("Original Approved Budget (" + Constants.DOLLAR_SYMBOL + ")", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, subHeadFont));
		expenseHeader.addCell(getTableHeadingCell("Latest Approved Budget (" + Constants.DOLLAR_SYMBOL + ")", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, subHeadFont));
		expenseHeader.addCell(getTableHeadingCell("Expenditure to Date (" + Constants.DOLLAR_SYMBOL + ")", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, subHeadFont));
		expenseHeader.addCell(getTableHeadingCell("Balance (" + Constants.DOLLAR_SYMBOL + ")", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, subHeadFont));
		expenseHeader.addCell(getTableHeadingCell("Utilization Rate", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, subHeadFont));
		expenseHeader.addCell(getTableHeadingCell("Committed Budget (" + Constants.DOLLAR_SYMBOL + ")", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, subHeadFont));
		expenseHeader.addCell(getTableHeadingCell("Balance Less Committed Budget (" + Constants.DOLLAR_SYMBOL + ")", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, subHeadFont));
		return expenseHeader;
	}

	private PdfPCell getTableHeadingCell(String text, int verticalAlignment, int horizontalAlignmentt, Font subHeadFont) {
		PdfPCell cell = new PdfPCell(new Phrase(text, subHeadFont));
		cell.setPadding(10);
		cell.setVerticalAlignment(verticalAlignment);
		cell.setHorizontalAlignment(horizontalAlignmentt);
		cell.setBackgroundColor(new BaseColor(176, 196, 222));
		return cell;
	}

	private ResponseEntity<byte[]> getResponseEntity(byte[] bytes) {
        ResponseEntity<byte[]> attachmentData = null;
        try {
            HttpHeaders headers = new HttpHeaders();
            headers.setContentType(MediaType.parseMediaType("application/octet-stream"));
            headers.setContentLength(bytes.length);
            headers.setCacheControl("must-revalidate, post-check=0, pre-check=0");
            headers.setPragma("public");
            attachmentData = new ResponseEntity<>(bytes, headers, HttpStatus.OK);
        } catch (Exception e) {
            logger.error("Error in method getResponseEntity", e);
        }
        return attachmentData;
    }

    private void autoSizeColumns(XSSFWorkbook workbook) {
        int numberOfSheets = workbook.getNumberOfSheets();
        for (int i = 0; i < numberOfSheets; i++) {
            XSSFSheet sheet = workbook.getSheetAt(i);
            if (sheet.getPhysicalNumberOfRows() > 0) {
                Row row = sheet.getRow(1);
                Iterator<Cell> cellIterator = row.cellIterator();
                while (cellIterator.hasNext()) {
                    Cell cell = cellIterator.next();
                    int columnIndex = cell.getColumnIndex();
                    sheet.autoSizeColumn(columnIndex);
                }
            }
        }
    }

    @Override
    public void prepareExcelSheet(List<Object[]> dashboardData, XSSFSheet sheet, Object[] tableHeadingRow, XSSFWorkbook workbook, CommonVO vo) {
        XSSFCellStyle tableBodyStyle = workbook.createCellStyle();
        String documentHeading = vo.getDocumentHeading();
        logger.info("documentHeading : {}", documentHeading);
        prepareExcelSheetHeader(sheet, tableHeadingRow, documentHeading, workbook, tableBodyStyle);
        int rowNumber = 2;
        for (Object[] objectArray : dashboardData) {
            Row row = sheet.createRow(rowNumber++);
            int cellNumber = 0;
            for (Object objectData : objectArray) {
                Cell cell = row.createCell(cellNumber++);
                cell.setCellStyle(tableBodyStyle);
                if (objectData instanceof String)
                    cell.setCellValue((String) objectData);
                else if (objectData instanceof Integer)
                    cell.setCellValue((Integer) objectData);
                else if (objectData instanceof BigInteger) {
                    String stringValue = ((BigInteger) objectData).toString();
                    cell.setCellValue((String) stringValue);
                } else if (objectData instanceof BigDecimal) {
                    DecimalFormat decimalFormat = new DecimalFormat(Constants.NUMBER_FORMAT_WITH_DECIMAL);
                    String stringValue = ((BigDecimal) objectData).toString();
                    XSSFCellStyle headStyle = workbook.createCellStyle();
                    headStyle.setAlignment(HorizontalAlignment.RIGHT);
                    cell.setCellStyle(headStyle);
                    cell.setCellValue(decimalFormat.format(new BigDecimal(stringValue)));
                } else if (objectData instanceof Date) {
                    if (objectData != null) {
                        Date date = (Date) objectData;
                        String dateValue = commonService.convertDateFormatBasedOnTimeZone(date.getTime(), Constants.DEFAULT_DATE_FORMAT);
                        cell.setCellValue((String) dateValue);
                    }
                } else if (objectData == null) {
                    cell.setCellValue((String) " ");
                }
            }
        }
        // Adjust size of table columns according to length of table data.
        autoSizeColumns(workbook);
    }

    @Override
    public XSSFWorkbook getXSSFWorkbookForDashboard(CommonVO vo) throws Exception {
        logger.info("--------- getXSSFWorkbookForDashboard ---------");
        XSSFWorkbook workbook = new XSSFWorkbook();
        List<Object[]> dashboardData = new ArrayList<>();
        String requestType = vo.getTabIndex();
        String tabName = vo.getTabName();
        logger.info("requestType : {}", requestType);
        logger.info("tabName : {}", tabName);
        if (requestType.equals("PERSON")) {
            dashboardData = dashboardDao.getDashBoardDataOfPersonForDownload(vo, dashboardData);
            XSSFSheet sheet = workbook.createSheet("Person Details");
            commonService.addDetailsInHeader(workbook, sheet);
            Object[] tableHeadingRow = {"Id#", "firstName"};
            prepareExcelSheet(dashboardData, sheet, tableHeadingRow, workbook, vo);
        } else if (requestType.equals("ROLODEX")) {
            dashboardData = dashboardDao.getDataOfRolodexForDownload(vo, dashboardData);
            XSSFSheet sheet = workbook.createSheet("Rolodex Details");
            commonService.addDetailsInHeader(workbook, sheet);
            Object[] tableHeadingRow = {"Id#", "Title"};
            prepareExcelSheet(dashboardData, sheet, tableHeadingRow, workbook, vo);
        }
        return workbook;
    }

    private void prepareExcelSheetForAward(List<AwardView> awardList, XSSFSheet sheet, Object[] tableHeadingRow, XSSFWorkbook workbook, AwardDashboardVO vo) {
        XSSFCellStyle tableBodyStyle = workbook.createCellStyle();
        String documentHeading = vo.getDocumentHeading();
        logger.info("documentHeading : {}", documentHeading);
        prepareExcelSheetHeader(sheet, tableHeadingRow, documentHeading, workbook, tableBodyStyle);
        if (commonDao.getParameterValueAsBoolean(Constants.IS_APPLICATION_ID_REQUIRED)) {
            if (vo.getTabName().equals("DRAFT_AWARDS") || vo.getTabName().equals("PENDING_AWARDS")) {
                int rowNumber = 2;
                for (AwardView awardData : awardList) {
                    Row row = sheet.createRow(rowNumber++);
                    int cellNumber = 0;
                    Cell cell1 = assignCell(cellNumber, tableBodyStyle, row);
                    if (awardData.getAwardNumber() != null)
                        cell1.setCellValue(awardData.getAwardNumber());
                    else
                        cell1.setCellValue(" ");
                    cellNumber++;

                    Cell cell2 = assignCell(cellNumber, tableBodyStyle, row);
                    if (awardData.getAccountNumber() != null)
                        cell2.setCellValue(awardData.getAccountNumber());
                    else
                        cell2.setCellValue(" ");
                    cellNumber++;

                    Cell cell3 = assignCell(cellNumber, tableBodyStyle, row);
                    if (awardData.getAwardDocumentType() != null && awardData.getAwardDocumentType().equals("1"))
                        cell3.setCellValue("Award Setup");
                    else if (awardData.getAwardDocumentType() != null && awardData.getAwardDocumentType().equals("2"))
                        cell3.setCellValue("Admin Correction");
                    else if (awardData.getAwardDocumentType() != null && awardData.getAwardDocumentType().equals("3"))
                        cell3.setCellValue(awardData.getAwardVariationType());
                    cellNumber++;

                    Cell cell4 = assignCell(cellNumber, tableBodyStyle, row);
                    if (awardData.getTitle() != null)
                        cell4.setCellValue(awardData.getTitle());
                    else
                        cell4.setCellValue(" ");
                    cellNumber++;

                    Cell cell5 = assignCell(cellNumber, tableBodyStyle, row);
                    if (awardData.getFullName() != null)
                        cell5.setCellValue(awardData.getFullName());
                    else
                        cell5.setCellValue(" ");
                    cellNumber++;

                    Cell cell6 = assignCell(cellNumber, tableBodyStyle, row);
                    if (awardData.getUnitName() != null)
                        cell6.setCellValue(commonService.getUnitFormatByUnitDetail(awardData.getUnitNumber(), awardData.getUnitName()));
                    else
                        cell6.setCellValue(" ");
                    cellNumber++;

                    Cell cell7 = assignCell(cellNumber, tableBodyStyle, row);
                    if (awardData.getSponsor() != null)
                        cell7.setCellValue(awardData.getSponsor());
                    else
                        cell7.setCellValue(" ");
                    cellNumber++;

                    Cell cell8 = assignCell(cellNumber, tableBodyStyle, row);
                    if (awardData.getAwardType() != null)
                        cell8.setCellValue(awardData.getAwardType());
                    else
                        cell8.setCellValue(" ");
                    cellNumber++;

                    Cell cell9 = assignCell(cellNumber, tableBodyStyle, row);
                    if (awardData.getStatus() != null)
                        cell9.setCellValue(awardData.getStatus());
                    else
                        cell9.setCellValue(" ");
                    cellNumber++;

                    Cell cell10 = assignCell(cellNumber, tableBodyStyle, row);
                    if (awardData.getAwardWorkflowStatus() != null)
                        cell10.setCellValue(awardData.getAwardWorkflowStatus());
                    else
                        cell10.setCellValue(" ");
                    cellNumber++;

                    Cell cell11 = assignCell(cellNumber, tableBodyStyle, row);
                    if (awardData.getGrantCallTitle() != null)
                        cell11.setCellValue(awardData.getGrantCallTitle());
                    else
                        cell11.setCellValue(" ");
                    cellNumber++;
                }
            } else if (vo.getTabName().equals("MY_AWARDS") || vo.getTabName().equals("ALL_AWARDS")) {
                int rowNumber = 2;
                for (AwardView awardData : awardList) {
                    Row row = sheet.createRow(rowNumber++);
                    int cellNumber = 0;
                    Cell cell1 = assignCell(cellNumber, tableBodyStyle, row);
                    if (awardData.getAwardNumber() != null)
                        cell1.setCellValue(awardData.getAwardNumber());
                    else
                        cell1.setCellValue(" ");
                    cellNumber++;

                    Cell cell2 = assignCell(cellNumber, tableBodyStyle, row);
                    if (awardData.getAccountNumber() != null)
                        cell2.setCellValue(awardData.getAccountNumber());
                    else
                        cell2.setCellValue(" ");
                    cellNumber++;

                    Cell cell3 = assignCell(cellNumber, tableBodyStyle, row);
                    if (awardData.getTitle() != null)
                        cell3.setCellValue(awardData.getTitle());
                    else
                        cell3.setCellValue(" ");
                    cellNumber++;

                    Cell cell4 = assignCell(cellNumber, tableBodyStyle, row);
                    if (awardData.getFullName() != null)
                        cell4.setCellValue(awardData.getFullName());
                    else
                        cell4.setCellValue(" ");
                    cellNumber++;

                    Cell cell5 = assignCell(cellNumber, tableBodyStyle, row);
                    if (awardData.getUnitName() != null)
                        cell5.setCellValue(commonService.getUnitFormatByUnitDetail(awardData.getUnitNumber(), awardData.getUnitName()));
                    else
                        cell5.setCellValue(" ");
                    cellNumber++;

                    Cell cell6 = assignCell(cellNumber, tableBodyStyle, row);
                    if (awardData.getSponsor() != null)
                        cell6.setCellValue(awardData.getSponsor());
                    else
                        cell6.setCellValue(" ");
                    cellNumber++;

                    Cell cell7 = assignCell(cellNumber, tableBodyStyle, row);
                    if (awardData.getAwardType() != null)
                        cell7.setCellValue(awardData.getAwardType());
                    else
                        cell7.setCellValue(" ");
                    cellNumber++;

                    Cell cell8 = assignCell(cellNumber, tableBodyStyle, row);
                    if (awardData.getStatus() != null)
                        cell8.setCellValue(awardData.getStatus());
                    else
                        cell8.setCellValue(" ");
                    cellNumber++;

                    Cell cell9 = assignCell(cellNumber, tableBodyStyle, row);
                    if (awardData.getGrantCallTitle() != null)
                        cell9.setCellValue(awardData.getGrantCallTitle());
                    else
                        cell9.setCellValue(" ");
                    cellNumber++;
                }
            }
        } else {
            if (vo.getTabName().equals("MY_AWARDS")) {
                int rowNumber = 2;
                for (AwardView awardData : awardList) {
                    Row row = sheet.createRow(rowNumber++);
                    int cellNumber = 0;
                    Cell cell1 = assignCell(cellNumber, tableBodyStyle, row);
                    if (awardData.getAwardNumber() != null)
                        cell1.setCellValue(awardData.getAwardNumber());
                    else
                        cell1.setCellValue(" ");
                    cellNumber++;

                    Cell cell9 = assignCell(cellNumber, tableBodyStyle, row);
	                if (awardData.getSponsorAwardNumber() != null)
	                    cell9.setCellValue(awardData.getSponsorAwardNumber());
	                else
	                    cell9.setCellValue(" ");
	                  cellNumber++;

                    Cell cell2 = assignCell(cellNumber, tableBodyStyle, row);
                    if (awardData.getTitle() != null)
                        cell2.setCellValue(awardData.getTitle());
                    else
                        cell2.setCellValue(" ");
                    cellNumber++;

                    Cell cell3 = assignCell(cellNumber, tableBodyStyle, row);
                    if (awardData.getFullName() != null)
                        cell3.setCellValue(awardData.getFullName());
                    else
                        cell3.setCellValue(" ");
                    cellNumber++;

                    Cell cell4 = assignCell(cellNumber, tableBodyStyle, row);
                    if (awardData.getUnitName() != null)
                        cell4.setCellValue(commonService.getUnitFormatByUnitDetail(awardData.getUnitNumber(), awardData.getUnitName()));
                    else
                        cell4.setCellValue(" ");
                    cellNumber++;

                    Cell cell5 = assignCell(cellNumber, tableBodyStyle, row);
                    if (awardData.getSponsor() != null)
                        cell5.setCellValue(awardData.getSponsor());
                    else
                        cell5.setCellValue(" ");
                    cellNumber++;

                    Cell cell6 = assignCell(cellNumber, tableBodyStyle, row);
                    if (awardData.getAwardType() != null)
                        cell6.setCellValue(awardData.getAwardType());
                    else
                        cell6.setCellValue(" ");
                    cellNumber++;

                    Cell cell7 = assignCell(cellNumber, tableBodyStyle, row);
                    if (awardData.getStatus() != null)
                        cell7.setCellValue(awardData.getStatus());
                    else
                        cell7.setCellValue(" ");
                    cellNumber++;
                }
            } else if (vo.getTabName().equals("DRAFT_AWARDS") || vo.getTabName().equals("PENDING_AWARDS") || vo.getTabName().equals("HOLD_AWARDS")  || vo.getTabName().equals("ALL_AWARDS")) {
                int rowNumber = 2;
                for (AwardView awardData : awardList) {
                    Row row = sheet.createRow(rowNumber++);
                    int cellNumber = 0;
                    Cell cell1 = assignCell(cellNumber, tableBodyStyle, row);
                    if (awardData.getAwardNumber() != null)
                        cell1.setCellValue(awardData.getAwardNumber());
                    else
                        cell1.setCellValue(" ");
                    cellNumber++;

                    Cell cell2 = assignCell(cellNumber, tableBodyStyle, row);
                    if (awardData.getAwardDocumentType().equals("1"))
                        cell2.setCellValue("Award Setup");
                    else if (awardData.getAwardDocumentType().equals("2"))
                        cell2.setCellValue("Admin Correction");
                    else if (awardData.getAwardDocumentType().equals("3"))
                        cell2.setCellValue(awardData.getAwardVariationType());
                    else 
                    	cell2.setCellValue(" ");
                    cellNumber++;

	                Cell cell10 = assignCell(cellNumber, tableBodyStyle, row);
	                if (awardData.getSponsorAwardNumber() != null)
	                    cell10.setCellValue(awardData.getSponsorAwardNumber());
	                else
	                    cell10.setCellValue(" ");
	                  cellNumber++;

                    Cell cell3 = assignCell(cellNumber, tableBodyStyle, row);
                    if (awardData.getTitle() != null)
                        cell3.setCellValue(awardData.getTitle());
                    else
                        cell3.setCellValue(" ");
                    cellNumber++;

                    Cell cell4 = assignCell(cellNumber, tableBodyStyle, row);
                    if (awardData.getFullName() != null)
                        cell4.setCellValue(awardData.getFullName());
                    else
                        cell4.setCellValue(" ");
                    cellNumber++;

                    Cell cell5 = assignCell(cellNumber, tableBodyStyle, row);
                    if (awardData.getUnitName() != null)
                        cell5.setCellValue(commonService.getUnitFormatByUnitDetail(awardData.getUnitNumber(), awardData.getUnitName()));
                    else
                        cell5.setCellValue(" ");
                    cellNumber++;

                    Cell cell6 = assignCell(cellNumber, tableBodyStyle, row);
                    if (awardData.getSponsor() != null)
                        cell6.setCellValue(awardData.getSponsor());
                    else
                        cell6.setCellValue(" ");
                    cellNumber++;

                    Cell cell7 = assignCell(cellNumber, tableBodyStyle, row);
                    if (awardData.getAwardType() != null)
                        cell7.setCellValue(awardData.getAwardType());
                    else
                        cell7.setCellValue(" ");
                    cellNumber++;

                    Cell cell8 = assignCell(cellNumber, tableBodyStyle, row);
                    if (awardData.getStatus() != null)
                        cell8.setCellValue(awardData.getStatus());
                    else
                        cell8.setCellValue(" ");
                    cellNumber++;

                    if (vo.getTabName().equals("ALL_AWARDS")) {
                    	 Cell cell9 = assignCell(cellNumber, tableBodyStyle, row);
                         if (awardData.getAwardSequenceStatus() != null)
                           cell9.setCellValue(awardData.getAwardSequenceStatus());
                         else
                            cell9.setCellValue(" ");
                    cellNumber++;
                    } 
                }
            }
        }
    }

    private void prepareExcelSheetForProposal(List<Proposal> dashboardData, XSSFSheet sheet, Object[] tableHeadingRow, XSSFWorkbook workbook, ProposalDashboardVO vo) {
        XSSFCellStyle tableBodyStyle = workbook.createCellStyle();
        String documentHeading = vo.getDocumentHeading();
        logger.info("documentHeading : {}", documentHeading);
        prepareExcelSheetHeader(sheet, tableHeadingRow, documentHeading, workbook, tableBodyStyle);
        int rowNumber = 2;
        if (commonDao.getParameterValueAsBoolean(Constants.IS_APPLICATION_ID_REQUIRED)) {
            for (Proposal proposalData : dashboardData) {
                Row row = sheet.createRow(rowNumber++);
                int cellNumber = 0;

                Cell cell1 = assignCell(cellNumber, tableBodyStyle, row);
                if (proposalData.getApplicationId() != null)
                    cell1.setCellValue(proposalData.getApplicationId());
                else
                    cell1.setCellValue(" ");
                cellNumber++;

                Cell cell2 = assignCell(cellNumber, tableBodyStyle, row);
                if (proposalData.getTitle() != null)
                    cell2.setCellValue(proposalData.getTitle());
                else
                    cell2.setCellValue(" ");
                cellNumber++;

                Cell cell3 = assignCell(cellNumber, tableBodyStyle, row);
                if (proposalData.getApplicationActivityType() != null)
                    cell3.setCellValue(proposalData.getApplicationActivityType());
                else
                    cell3.setCellValue(" ");
                cellNumber++;

                Cell cell4 = assignCell(cellNumber, tableBodyStyle, row);
                ProposalPerson investigator = proposalData.getInvestigator();
                if (investigator != null && investigator.getFullName() != null)
                    cell4.setCellValue(investigator.getFullName());
                else
                    cell4.setCellValue(" ");
                cellNumber++;

                Cell cell5 = assignCell(cellNumber, tableBodyStyle, row);
                if (proposalData.getHomeUnitName() != null)
                    cell5.setCellValue(commonService.getUnitFormatByUnitDetail(proposalData.getHomeUnitNumber(), proposalData.getHomeUnitName()));
                else
                    cell5.setCellValue(" ");
                cellNumber++;

                Cell cell6 = assignCell(cellNumber, tableBodyStyle, row);
                if (proposalData.getApplicationType() != null) {
                    cell6.setCellValue(proposalData.getApplicationType());
                } else {
                    cell6.setCellValue(" ");
                }
                cellNumber++;

                Cell cell7 = assignCell(cellNumber, tableBodyStyle, row);
                if (proposalData.getApplicationStatus() != null)
                    cell7.setCellValue(proposalData.getApplicationStatus());
                else
                    cell7.setCellValue(" ");
                cellNumber++;

                Cell cell8 = assignCell(cellNumber, tableBodyStyle, row);
                if (proposalData.getSponsorName() != null)
                    cell8.setCellValue(proposalData.getSponsorName());
                else
                    cell8.setCellValue(" ");
                cellNumber++;

                Cell cell9 = assignCell(cellNumber, tableBodyStyle, row);
                cellNumber++;

                if (proposalData.getGrantCallName() != null) {
                    cell9.setCellValue(proposalData.getGrantCallName());
                } else {
                    cell9.setCellValue(" ");
                }

                Cell cell10 = assignCell(cellNumber, tableBodyStyle, row);
                if (proposalData.getGrantCallClosingDate() != null) {
                    String dateValue = commonService.convertDateFormatBasedOnTimeZone(proposalData.getGrantCallClosingDate().getTime(), Constants.DEFAULT_DATE_FORMAT);
                    cell10.setCellValue(dateValue);
                } else {
                    cell10.setCellValue(" ");
                }
                cellNumber++;

                Cell cell11 = assignCell(cellNumber, tableBodyStyle, row);
                if (proposalData.getSponsorDeadlineDate() != null) {
                    String dateValue = commonService.convertDateFormatBasedOnTimeZone(proposalData.getSponsorDeadlineDate().getTime(), Constants.DEFAULT_DATE_FORMAT);
                    cell11.setCellValue(dateValue);
                } else {
                    cell11.setCellValue(" ");
                }
                cellNumber++;
            }
        } else {
            for (Proposal proposalData : dashboardData) {
                Row row = sheet.createRow(rowNumber++);
                int cellNumber = 0;

                Cell cell1 = assignCell(cellNumber, tableBodyStyle, row);
                if (proposalData.getProposalId() != null)
                    cell1.setCellValue(proposalData.getProposalId());
                else
                    cell1.setCellValue(" ");
                cellNumber++;

                Cell cell2 = assignCell(cellNumber, tableBodyStyle, row);
                if (proposalData.getTitle() != null)
                    cell2.setCellValue(proposalData.getTitle());
                else
                    cell2.setCellValue(" ");
                cellNumber++;

                Cell cell3 = assignCell(cellNumber, tableBodyStyle, row);
                ProposalPerson investigator = proposalData.getInvestigator();
                if (investigator != null && investigator.getFullName() != null)
                    cell3.setCellValue(investigator.getFullName());
                else
                    cell3.setCellValue(" ");
                cellNumber++;

                Cell cell4 = assignCell(cellNumber, tableBodyStyle, row);
                if (proposalData.getHomeUnitName() != null)
                    cell4.setCellValue(commonService.getUnitFormatByUnitDetail(proposalData.getHomeUnitNumber(), proposalData.getHomeUnitName()));
                else
                    cell4.setCellValue(" ");
                cellNumber++;

                if (commonDao.getParameterValueAsBoolean(Constants.IS_GRANTCALL_ABBREVATION_REQUIRED)) {
                    Cell cell7 = assignCell(cellNumber, tableBodyStyle, row);
                    if (proposalData.getAbbreviation() != null)
                        cell7.setCellValue(proposalData.getAbbreviation());
                    else
                        cell7.setCellValue(" ");
                    cellNumber++;
                }

                Cell cell5 = assignCell(cellNumber, tableBodyStyle, row);
                if (proposalData.getApplicationType() != null) {
                    cell5.setCellValue(proposalData.getApplicationType());
                } else {
                    cell5.setCellValue(" ");
                }
                cellNumber++;

                Cell cell6 = assignCell(cellNumber, tableBodyStyle, row);
                if (proposalData.getApplicationStatus() != null)
                    cell6.setCellValue(proposalData.getApplicationStatus());
                else
                    cell6.setCellValue(" ");
                cellNumber++;

                Cell cell7 = assignCell(cellNumber, tableBodyStyle, row);
                if (proposalData.getSponsorName() != null)
                    cell7.setCellValue(proposalData.getSponsorName());
                else
                    cell7.setCellValue(" ");
                cellNumber++;

                Cell cell9 = assignCell(cellNumber, tableBodyStyle, row);
                if (proposalData.getInternalDeadLineDate() != null) {
                    String dateValue = commonService.convertDateFormatBasedOnTimeZone(
                            proposalData.getInternalDeadLineDate().getTime(), Constants.DEFAULT_DATE_FORMAT);
                    cell9.setCellValue(dateValue);
                } else {
                    cell9.setCellValue(" ");
                }
                cellNumber++;

                Cell cell8 = assignCell(cellNumber, tableBodyStyle, row);
                if (proposalData.getSponsorDeadlineDate() != null) {
                    String dateValue = commonService.convertDateFormatBasedOnTimeZone(
                            proposalData.getSponsorDeadlineDate().getTime(), Constants.DEFAULT_DATE_FORMAT);
                    cell8.setCellValue(dateValue);
                } else {
                    cell8.setCellValue(" ");
                }
                cellNumber++;
            }
        }
    }

    private Cell assignCell(int cellNumber, XSSFCellStyle tableBodyStyle, Row row) {
        Cell cell = row.createCell(cellNumber);
        cell.setCellStyle(tableBodyStyle);
        return cell;
    }

    @Override
	public String fetchRequiredParams() throws Exception {
		ParameterVO vo = new ParameterVO();
		vo.setIsEvaluation(commonDao.getParameterValueAsString(Constants.WORKFLOW_TYPE).equals(Constants.EVALUATION));
		vo.setIsMapRouting(commonDao.getParameterValueAsString(Constants.WORKFLOW_TYPE).equals(Constants.MAP_ROUTING));
		vo.setIsEvaluationAndMapRouting(
				commonDao.getParameterValueAsString(Constants.WORKFLOW_TYPE).equals(Constants.EVALUATION_MAP_ROUTING));
		vo.setIsWafEnabled(commonDao.getParameterValueAsBoolean(Constants.IS_WAF_ENABLED));
		vo.setFileTypes(dashboardDao.getAllFileTypes());
		vo.setCanUserAddOrganization(commonDao.getParameterValueAsBoolean(Constants.ENABLE_USER_DEFINED_ORGANIZATION));
		vo.setIsOrcidWOrkEnabled(commonDao.getParameterValueAsBoolean(Constants.IS_ORCID_WORK_ENABLED));
		vo.setIsGrantCallStatusAutomated(
				commonDao.getParameterValueAsBoolean(Constants.AUTOMATE_GRANTCALL_INTERNAL_STATUS));
		vo.setIsEnableSpecialReview(commonDao.getParameterValueAsBoolean(Constants.ENABLE_SPECIAL_REVIEW));
		vo.setIsAwardManpowerActive(commonDao.getParameterValueAsBoolean(Constants.IS_MANPOWER_ENABLED));
		vo.setIsEnableAddtoAddressBook(commonDao.getParameterValueAsBoolean(Constants.ENABLE_ADD_TO_ADDRESS_BOOK));
		vo.setIsDevProposalVersioningEnabled(commonDao.getParameterValueAsBoolean(Constants.ENABLE_DEV_PROP_VERSIONS));
		vo.setIsProposalOrganizationEnabled(
				commonDao.getParameterValueAsBoolean(Constants.ENABLE_PROPOSAL_ORGANIZATION));
		vo.setTriageQuestionnaireRequired(commonDao.getParameterValueAsBoolean(Constants.IS_TRIAGE_QUESTIONNAIRE_REQUIRED));
		vo.setIsShowCreateAgreement(commonDao.getParameterValueAsBoolean(Constants.IS_ENABLE_CREATE_AGREEMENT));
		vo.setIsShowAgreementSupport(commonDao.getParameterValueAsBoolean(Constants.IS_ENABLE_AGREEMENT_SUPPORT));
		vo.setIsShowAgreementNotifyAction(commonDao.getParameterValueAsBoolean(Constants.IS_ENABLE_AGREEMENT_NOTIFY_ACTION));
		vo.setIsEnableLock(commonService.getWebSocketConfigurationValue(Constants.ENABLE_LOCK));
		vo.setIsEnableSocket(commonService.getWebSocketConfigurationValue(Constants.ENABLE_SOCKET));
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String fetchEvaluationStop(CommonVO vo) {
//		return commonDao.convertObjectToJSON(evaluationDao.getReviewStopEvaluvationBasedOnProposalStatus(vo.getProposalStatusCode()));
		List<String> activityTypeCodes = vo.getActivityTypeCodes();
		Set<Integer> finalRoles = new HashSet<>();
		List<EvaluationStop> evaluationStopList = new ArrayList<>();
		Boolean isExit = false;
		if (activityTypeCodes != null && !activityTypeCodes.isEmpty()) {
			String activityTypeCode = activityTypeCodes.get(0);
			activityTypeCodes.remove(0);
			List<Integer> roleIds = evaluationDao.getEvaluationRolesBasedOnProposalStatusAndActivityTypeCode(vo.getProposalStatusCode(), activityTypeCode);
			if (!activityTypeCodes.isEmpty()) {
				if (!roleIds.isEmpty()) {
					for (String activityType : activityTypeCodes) {
						finalRoles.clear();
						List<Integer> subroleIds = evaluationDao.getEvaluationRolesBasedOnProposalStatusAndActivityTypeCode(vo.getProposalStatusCode(), activityType);
						if (subroleIds != null && !subroleIds.isEmpty()) {
							Integer size = subroleIds.size();
							Boolean isFound = false;
							for (Integer subroleId : subroleIds) {
								Integer count = 1;
								if (roleIds.contains(subroleId)) {
									finalRoles.add(subroleId);
									isFound = true;
								}
								if (size.equals(count) && Boolean.FALSE.equals(isFound)) {
									isExit = true;
								}
								count = count + 1;
							}
							roleIds.clear();
							roleIds.addAll(finalRoles);
						} else {
							finalRoles.clear();
							isExit = true;
						}
						if (Boolean.TRUE.equals(isExit)) {
							finalRoles.clear();
						}
					}
				}	
			} else {
				if (roleIds != null && !roleIds.isEmpty()) {
					for (Integer role : roleIds) {
						finalRoles.add(role);
					}
				}
			}	
		}
		if (finalRoles != null && !finalRoles.isEmpty() && Boolean.FALSE.equals(isExit)) {
			List<Role> roles = evaluationDao.getRolesBasedonRolesIds(finalRoles);
			if (roles != null && !roles.isEmpty()) {
				for (Role role : roles) {
					EvaluationStop evaluationStop = new EvaluationStop();
					evaluationStop.setRoleId(role.getRoleId());
					evaluationStop.setRole(role);
					evaluationStop.setDescription(role.getDescription());
					evaluationStopList.add(evaluationStop);
				}
				//vo.setRoles(roles);
				vo.setEvaluationStopList(evaluationStopList);
			}
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String getAgreementDashBoardData(AgreementDashboardVO vo) throws Exception {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		try {
			dashBoardProfile = dashboardDao.getDashBoardDataForAgreement(vo);
			//dashBoardProfile.setUnitAdministrators(loginDao.isUnitAdmin(vo.getPersonId()));
			//dashBoardProfile.setAgreementPeopleType(agreementDao.fetchAgreementPeopleTypes());
			dashBoardProfile.setPersons(personService.getPersonBasedOnRoleAndRight());
			dashBoardProfile.setAgreementAdminGroups(commonDao.fetchAdminGroupsBasedOnModuleCode(Constants.AGREEMENT_MODULE_CODE));
		} catch (Exception e) {
			logger.error("Error in method getAgreementDashBoardData", e);
		}
		return commonDao.convertObjectToJSON(dashBoardProfile);
	}

	@Override
	public String getNegotaiationDashBoardData(NegotiationDashboardVO vo) throws Exception {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		try {
			dashBoardProfile = dashboardDao.getDashBoardDataForNegotiations(vo);
			dashBoardProfile.setUnitAdministrators(loginDao.isUnitAdmin(vo.getPersonId()));
		} catch (Exception e) {
			logger.error("Error in method getNegotiationDashBoardData", e);
		}
		return commonDao.convertObjectToJSON(dashBoardProfile);
	}

	@Override
	public String getGrantCallDashBoardData(GrantCallDashboardVO vo) throws Exception {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		try {
			vo.setIsDownload(false);
			dashBoardProfile = dashboardDao.getDashBoardDataForGrantCall(vo);
			dashBoardProfile.setUnitAdministrators(loginDao.isUnitAdmin(vo.getPersonId()));
		} catch (Exception e) {
			logger.error("Error in method getGrantCallDashBoardData", e);
		}
		return commonDao.convertObjectToJSON(dashBoardProfile);
	}

	@Override
	public String getInstituteProposalDashBoardData(InstituteProposalDashboardVO vo) throws Exception {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		try {
			vo.setIsDownload(false);
			dashBoardProfile = dashboardDao.getDashBoardDataForInstProposal(vo);
			dashBoardProfile.setUnitAdministrators(loginDao.isUnitAdmin(vo.getPersonId()));
		} catch (Exception e) {
			logger.error("Error in method getInstituteProposalDashBoardData", e);
		}
		return commonDao.convertObjectToJSON(dashBoardProfile);
	}

	@Override
	public String getAwardDashBoardData(AwardDashboardVO vo) throws Exception {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		try {
			vo.setIsDownload(false);
			dashBoardProfile = dashboardDao.dashboardDatasForAward(vo);
			dashBoardProfile.setUnitAdministrators(loginDao.isUnitAdmin(vo.getPersonId()));
		} catch (Exception e) {
			logger.error("Error in method getAwardDashBoardData", e);
		}
		return commonDao.convertObjectToJSON(dashBoardProfile);
	}

	@Override
	public ResponseEntity<byte[]> getResponseEntityForAwardDownload(AwardDashboardVO vo, XSSFWorkbook workbook) throws Exception {
		logger.info("--------- getResponseEntityForExcelOrPDFDownload ---------");
		byte[] byteArray = null;
		String exportType = vo.getExportType();
		String documentHeading = vo.getDocumentHeading();
		if (exportType.equals("pdf")) {
			byteArray = generatePDFFileByteArray(documentHeading, workbook);
		} else {
			ByteArrayOutputStream bos = new ByteArrayOutputStream();
			workbook.write(bos);
			byteArray = bos.toByteArray();
		}
		return getResponseEntity(byteArray);
	}

	@Override
	public XSSFWorkbook getXSSFWorkbookForAwardDashboard(AwardDashboardVO vo) throws Exception {
		XSSFWorkbook workbook = new XSSFWorkbook();
		vo.setIsDownload(true);
		DashBoardProfile dashBoardProfile = dashboardDao.dashboardDatasForAward(vo);
		XSSFSheet sheet = workbook.createSheet(vo.getDocumentHeading());
    commonService.addDetailsInHeader(workbook, sheet);
		if (commonDao.getParameterValueAsBoolean(Constants.IS_APPLICATION_ID_REQUIRED)) {
			if (vo.getTabName().equals("MY_AWARDS") || vo.getTabName().equals("ALL_AWARDS")) {
				Object[] tableHeadingRow = { "Award #", "Account Number", "Title", "Principal Investigator", "Lead Unit", "Funding Agency", "Award Type", " Award Status", "Grant Call" };
				prepareExcelSheetForAward(dashBoardProfile.getAwardViews(), sheet, tableHeadingRow, workbook, vo);
			} else if (vo.getTabName().equals("DRAFT_AWARDS") || vo.getTabName().equals("PENDING_AWARDS")) {
				Object[] tableHeadingRow = { "Award #", "Account Number", "Version Type", "Title", "Principal Investigator", "Lead Unit", "Funding Agency", "Award Type", " Award Status", "Workflow Status", "Grant Call" };
				prepareExcelSheetForAward(dashBoardProfile.getAwardViews(), sheet, tableHeadingRow, workbook, vo);
			}
		} else {
			if (vo.getTabName().equals("MY_AWARDS")) {
				Object[] tableHeadingRow = { "Award #", "Sponsor Award Number","Title", "Principal Investigator", "Lead Unit", "Sponsor", "Award Type", "Award Status" };
				prepareExcelSheetForAward(dashBoardProfile.getAwardViews(), sheet, tableHeadingRow, workbook, vo);
			} else if (vo.getTabName().equals("ALL_AWARDS")) {
				Object[] tableHeadingRow = { "Award #", "Version Type", "Sponsor Award Number", "Title", "Principal Investigator", "Lead Unit", "Sponsor", "Award Type", "Award Status", "Version Status" };
				prepareExcelSheetForAward(dashBoardProfile.getAwardViews(), sheet, tableHeadingRow, workbook, vo);
			} else if (vo.getTabName().equals("DRAFT_AWARDS") || vo.getTabName().equals("PENDING_AWARDS") || vo.getTabName().equals("HOLD_AWARDS")) {
				Object[] tableHeadingRow = { "Award #", "Version Type", "Sponsor Award Number", "Title", "Principal Investigator", "Lead Unit", "Sponsor", "Award Type", "Award Status" };
				prepareExcelSheetForAward(dashBoardProfile.getAwardViews(), sheet, tableHeadingRow, workbook, vo);
			}
		}
		return workbook;
	}

	@Override
	public String getProposalDashBoardData(ProposalDashboardVO vo) throws Exception {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		try {
			vo.setIsDownload(false);
			dashBoardProfile = dashboardDao.dashboardDatasForProposal(vo);
			dashBoardProfile.setUnitAdministrators(loginDao.isUnitAdmin(vo.getPersonId()));
			dashBoardProfile.setIsGrantcallAbbrevationRequired(commonDao.getParameterValueAsBoolean(Constants.IS_GRANTCALL_ABBREVATION_REQUIRED));
		} catch (Exception e) {
			logger.error("Error in method getProposalDashBoardData", e);
		}
		return commonDao.convertObjectToJSON(dashBoardProfile);
	}

	@Override
	public ResponseEntity<byte[]> getResponseEntityForProposalDownload(ProposalDashboardVO vo, XSSFWorkbook workbook) throws Exception {
		byte[] byteArray = null;
		String exportType = vo.getExportType();
		String documentHeading = vo.getDocumentHeading();
		if (exportType.equals("pdf")) {
			byteArray = generatePDFFileByteArray(documentHeading, workbook);
		} else {
			ByteArrayOutputStream bos = new ByteArrayOutputStream();
			workbook.write(bos);
			byteArray = bos.toByteArray();
		}
		return getResponseEntity(byteArray);
	}

	@Override
	public XSSFWorkbook getXSSFWorkbookForProposalDashboard(ProposalDashboardVO vo) throws Exception {
		XSSFWorkbook workbook = new XSSFWorkbook();
		vo.setIsDownload(true);
		DashBoardProfile dashBoardProfile = dashboardDao.dashboardDatasForProposal(vo);
		XSSFSheet sheet = workbook.createSheet(vo.getDocumentHeading());
    commonService.addDetailsInHeader(workbook, sheet);
		if (commonDao.getParameterValueAsBoolean(Constants.IS_APPLICATION_ID_REQUIRED)) {
			Object[] tableHeadingRow = { "Id#", "Title", "Category", "Principal Investigator", "Lead Unit" , "Proposal Type", "Status", "Funding Agency", "Grant Call", "Grant Call Closing Date" , "Sponsor Deadline Date"};
			prepareExcelSheetForProposal(dashBoardProfile.getProposal(), sheet, tableHeadingRow, workbook, vo);
		} else if (commonDao.getParameterValueAsBoolean(Constants.IS_GRANTCALL_ABBREVATION_REQUIRED)) {
			Object[] tableHeadingRow = { "Id#", "Title", "Principal Investigator", "Lead Unit", "Grant Call Abbrevation","Proposal Type", "Status", "Sponsor","Internal Deadline Date", "Sponsor Deadline Date" };
			prepareExcelSheetForProposal(dashBoardProfile.getProposal(), sheet, tableHeadingRow, workbook, vo);
		} else {
			Object[] tableHeadingRow = { "Id#", "Title", "Principal Investigator", "Lead Unit","Proposal Type", "Status", "Sponsor","Internal Deadline Date", "Sponsor Deadline Date" };
			prepareExcelSheetForProposal(dashBoardProfile.getProposal(), sheet, tableHeadingRow, workbook, vo);
		}
		return workbook;
	}

	@Override
	public String loadServiceRequestDashBoard(ServiceRequestDashboardVO vo) throws Exception {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		try {
			vo.setIsDownload(false);
			dashBoardProfile = dashboardDao.loadServiceRequestDashBoard(vo);
			dashBoardProfile.setUnitAdministrators(loginDao.isUnitAdmin(vo.getPersonId()));
		} catch (Exception e) {
			logger.error("Error in method loadServiceRequestDashBoard", e);
		}
		return commonDao.convertObjectToJSON(dashBoardProfile);
	}

	@Override
	public ResponseEntity<byte[]> getResponseEntityServiceRequestDashBoard(ServiceRequestDashboardVO vo, XSSFWorkbook workbook) throws IOException {
		byte[] byteArray = null;
		String exportType = vo.getExportType();
		String documentHeading = vo.getDocumentHeading();
		if (exportType.equals("pdf")) {
			byteArray = generatePDFFileByteArray(documentHeading, workbook);
		} else {
			ByteArrayOutputStream bos = new ByteArrayOutputStream();
			workbook.write(bos);
			byteArray = bos.toByteArray();
		}
		return getResponseEntity(byteArray);
	}

	@Override
	public XSSFWorkbook getXSSFWorkbookServiceRequestDashBoard(ServiceRequestDashboardVO vo) throws Exception {
		XSSFWorkbook workbook = new XSSFWorkbook();
		vo.setIsDownload(true);
		DashBoardProfile dashBoardProfile = dashboardDao.loadServiceRequestDashBoard(vo);
		XSSFSheet sheet = workbook.createSheet(vo.getDocumentHeading());
		commonService.addDetailsInHeader(workbook, sheet);
		Object[] tableHeadingRow = { "ID", "Type", "Category", "Subject", "Department", "Status", "Priority"};
		prepareExcelSheetForServiceRequest(dashBoardProfile.getServiceRequestList(), sheet, tableHeadingRow, workbook, vo);
		return workbook;
	}

	private XSSFWorkbook prepareExcelSheetHeader(XSSFSheet sheet, Object[] tableHeadingRow, String documentHeading, XSSFWorkbook workbook, XSSFCellStyle tableBodyStyle) {
		int headingCellNumber = 0;
		Row headerRow = sheet.createRow(0);
		Cell headingCell = headerRow.createCell(0);
		headingCell.setCellValue((String) documentHeading);
		sheet.addMergedRegion(new CellRangeAddress(0, 0, 0, tableHeadingRow.length - 1));
		XSSFFont headerFont = workbook.createFont();
		headerFont.setBold(true);
		headerFont.setFontHeightInPoints((short) 15);
		XSSFCellStyle headerStyle = workbook.createCellStyle();
		headerStyle.setAlignment(HorizontalAlignment.CENTER);
		headerStyle.setFont(headerFont);
		headingCell.setCellStyle(headerStyle);
		// Table head style and font creation code.
		Row tableHeadRow = sheet.createRow(1);
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
			cell.setCellValue((String) heading);
			cell.setCellStyle(tableHeadStyle);
		}
		return workbook;
	}

	private XSSFWorkbook prepareExcelSheetForServiceRequest(List<ServiceRequest> dashboardData, XSSFSheet sheet, Object[] tableHeadingRow, XSSFWorkbook workbook, ServiceRequestDashboardVO vo) {
		XSSFCellStyle tableBodyStyle = workbook.createCellStyle();
		String documentHeading = vo.getDocumentHeading();
		logger.info("documentHeading : {}", documentHeading);
		prepareExcelSheetHeader(sheet, tableHeadingRow, documentHeading, workbook, tableBodyStyle);
			int rowNumber = 2;
			for (ServiceRequest serviceRequest : dashboardData) {
				Row row = sheet.createRow(rowNumber++);
				int cellNumber = 0;

				Cell cell1 = assignCell(cellNumber, tableBodyStyle, row);
				if (serviceRequest.getServiceRequestId() != null)
					cell1.setCellValue(serviceRequest.getServiceRequestId());
				else
					cell1.setCellValue(" ");
				cellNumber++;

				Cell cell2 = assignCell(cellNumber, tableBodyStyle, row);
				if (serviceRequest.getServiceRequestTypeData() != null)
					cell2.setCellValue(serviceRequest.getServiceRequestTypeData());
				else
					cell2.setCellValue(" ");
				cellNumber++;

				Cell cell3 = assignCell(cellNumber, tableBodyStyle, row);
				if (serviceRequest.getServiceRequestCategory() != null)
					cell3.setCellValue(serviceRequest.getServiceRequestCategory());
				else
					cell3.setCellValue(" ");
				cellNumber++;

				Cell cell4 = assignCell(cellNumber, tableBodyStyle, row);
				if (serviceRequest.getSubject() != null)
					cell4.setCellValue(serviceRequest.getSubject());
				else
					cell4.setCellValue(" ");
				cellNumber++;

				Cell cell5 = assignCell(cellNumber, tableBodyStyle, row);
				if (serviceRequest.getUnitName() != null)
					cell5.setCellValue(serviceRequest.getUnitName());
				else
					cell5.setCellValue(" ");
				cellNumber++;

				Cell cell6 = assignCell(cellNumber, tableBodyStyle, row);
				if (serviceRequest.getServiceRequestStatusData() != null)
					cell6.setCellValue(serviceRequest.getServiceRequestStatusData());
				else
					cell6.setCellValue(" ");
				cellNumber++;

				Cell cell7 = assignCell(cellNumber, tableBodyStyle, row);
				if (serviceRequest.getsRPriority()!= null)
					cell7.setCellValue(serviceRequest.getsRPriority());
				else
					cell7.setCellValue("");
				cellNumber++;

				/*Cell cell8 = assignCell(cellNumber, tableBodyStyle, row);
				if (serviceRequest.getUpdateTimestamp() != null) {
					String dateValue = commonService.convertDateFormatBasedOnTimeZone(
							serviceRequest.getUpdateTimestamp().getTime(), Constants.DEFAULT_DATE_FORMAT);
					cell8.setCellValue((String) dateValue);
				} else {
					cell8.setCellValue(" ");
				}
				cellNumber++;*/
			}
		return workbook;
	}

	@Override
	public XSSFWorkbook getXSSFWorkbookForInstituteProposalDashboard(InstituteProposalDashboardVO vo) {
		vo.setIsDownload(true);
		DashBoardProfile dashBoardProfile = dashboardDao.getDashBoardDataForInstProposal(vo);
		XSSFWorkbook workbook = new XSSFWorkbook();
		XSSFSheet sheet = workbook.createSheet(vo.getDocumentHeading());
    commonService.addDetailsInHeader(workbook, sheet);
		Object[] tableHeadingRow = { "Proposal#", "Title", "Principal Investigator", "Category", "Type", "Status", "Sponsor",
				"Submission Date" };
		CommonVO commonVo = new CommonVO();
		commonVo.setDocumentHeading(vo.getDocumentHeading());
		prepareExcelSheetForInstituteProposal(dashBoardProfile.getInstituteProposal(), sheet, tableHeadingRow, workbook, commonVo);
		return workbook;
	}

	private void prepareExcelSheetForInstituteProposal(List<InstituteProposal> instituteProposals, XSSFSheet sheet,
			Object[] tableHeadingRow, XSSFWorkbook workbook, CommonVO vo) {
		XSSFCellStyle tableBodyStyle = workbook.createCellStyle();
		String documentHeading = vo.getDocumentHeading();
		logger.info("documentHeading : {}", documentHeading);
		prepareExcelSheetHeader(sheet, tableHeadingRow, documentHeading, workbook, tableBodyStyle);
		int rowNumber = 2;
		for (InstituteProposal instituteProposal : instituteProposals) {
			Row row = sheet.createRow(rowNumber++);
			int cellNumber = 0;

			Cell cell1 = assignCell(cellNumber, tableBodyStyle, row);
			if (instituteProposal.getProposalNumber() != null)
				cell1.setCellValue(instituteProposal.getProposalNumber());
			else
				cell1.setCellValue(" ");
			cellNumber++;

			Cell cell2 = assignCell(cellNumber, tableBodyStyle, row);
			if (instituteProposal.getTitle() != null)
				cell2.setCellValue(instituteProposal.getTitle());
			else
				cell2.setCellValue(" ");
			cellNumber++;

			Cell cell3 = assignCell(cellNumber, tableBodyStyle, row);
			if (instituteProposal.getPrincipalInvestigator() != null)
				cell3.setCellValue(instituteProposal.getPrincipalInvestigator());
			else
				cell3.setCellValue(" ");
			cellNumber++;

			Cell cell4 = assignCell(cellNumber, tableBodyStyle, row);
			if (instituteProposal.getApplicationActivityType() != null)
				cell4.setCellValue(instituteProposal.getApplicationActivityType());
			else
				cell4.setCellValue(" ");
			cellNumber++;

			Cell cell5 = assignCell(cellNumber, tableBodyStyle, row);
			if (instituteProposal.getApplicationType() != null)
				cell5.setCellValue(instituteProposal.getApplicationType());
			else
				cell5.setCellValue(" ");
			cellNumber++;

			Cell cell6 = assignCell(cellNumber, tableBodyStyle, row);
			if (instituteProposal.getApplicationStatus() != null)
				cell6.setCellValue(instituteProposal.getApplicationStatus());
			else
				cell6.setCellValue(" ");
			cellNumber++;

			Cell cell7 = assignCell(cellNumber, tableBodyStyle, row);
			if (instituteProposal.getSponsorName() != null)
				cell7.setCellValue(instituteProposal.getSponsorName());
			else
				cell7.setCellValue("");
			cellNumber++;

			Cell cell8 = assignCell(cellNumber, tableBodyStyle, row);
			if (instituteProposal.getSubmissionDate() != null) {
				String dateValue = commonService.convertDateFormatBasedOnTimeZone(
						instituteProposal.getSubmissionDate().getTime(), Constants.DEFAULT_DATE_FORMAT);
				cell8.setCellValue((String) dateValue);
			} else {
				cell8.setCellValue(" ");
			}
			cellNumber++;
		}
	}

	@Override
	public String canDeleteGrantCall(GrantCallDashboardVO vo) {
		vo.setIsGrantCallLinked(Boolean.TRUE.equals(grantCallDao.checkGrantCallLinked(vo.getGrantCallId())) ? Boolean.TRUE : Boolean.FALSE);
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String getClaimDashBoardData(ClaimDashboardVO vo) {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		try {
			vo.setIsDownload(false);
			dashBoardProfile = dashboardDao.getClaimDashBoardData(vo);
			dashBoardProfile.setUnitAdministrators(loginDao.isUnitAdmin(vo.getPersonId()));
		} catch (Exception e) {
			logger.error("Error in method getClaimDashBoardData", e);
		}
		return commonDao.convertObjectToJSON(dashBoardProfile);
	}

	@Override
	public String fibiProgressReportDashBoard(ProgressReportDashboardVO vo) {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		try {
			vo.setIsDownload(false);
			dashBoardProfile = dashboardDao.fibiProgressReportDashBoard(vo);
			dashBoardProfile.setUnitAdministrators(loginDao.isUnitAdmin(vo.getPersonId()));
		} catch (Exception e) {
			logger.error("Error in method fibiProgressReportDashBoard", e);
		}
		return commonDao.convertObjectToJSON(dashBoardProfile);
	}

	@Override
	public XSSFWorkbook getXSSFWorkbookForGrantCallDashboard(GrantCallDashboardVO vo) {
		vo.setIsDownload(true);
		DashBoardProfile dashBoardProfile = dashboardDao.getDashBoardDataForGrantCall(vo);
		XSSFWorkbook workbook = new XSSFWorkbook();
		XSSFSheet sheet = workbook.createSheet(vo.getDocumentHeading());
    	commonService.addDetailsInHeader(workbook, sheet);
		Object[] tableHeadingRow = { "GrantCall#", "Title", "Sponsor", "GrantCall Type", "Opening Date","Closing Date", "Submission Date", "Status" };
		CommonVO commonVo = new CommonVO();
		commonVo.setDocumentHeading(vo.getDocumentHeading());
		prepareExcelSheetForGrantCall(dashBoardProfile.getGrantCalls(), sheet, tableHeadingRow, workbook, commonVo);
		return workbook;
	}

	private void prepareExcelSheetForGrantCall(List<GrantCall> grantCalls, XSSFSheet sheet, Object[] tableHeadingRow,
			XSSFWorkbook workbook, CommonVO vo) {
		XSSFCellStyle tableBodyStyle = workbook.createCellStyle();
		String documentHeading = vo.getDocumentHeading();
		logger.info("documentHeading : {}", documentHeading);
		prepareExcelSheetHeader(sheet, tableHeadingRow, documentHeading, workbook, tableBodyStyle);
		int rowNumber = 2;
		for (GrantCall grantCall : grantCalls) {
			Row row = sheet.createRow(rowNumber++);
			int cellNumber = 0;

			Cell cell1 = assignCell(cellNumber, tableBodyStyle, row);
			if (grantCall.getGrantCallId() != null)
				cell1.setCellValue(grantCall.getGrantCallId());
			else
				cell1.setCellValue(" ");
			cellNumber++;

			Cell cell2 = assignCell(cellNumber, tableBodyStyle, row);
			if (grantCall.getGrantCallName() != null)
				cell2.setCellValue(grantCall.getGrantCallName());
			else
				cell2.setCellValue(" ");
			cellNumber++;

			Cell cell4 = assignCell(cellNumber, tableBodyStyle, row);
			if (grantCall.getSponsorName() != null)
				cell4.setCellValue(grantCall.getSponsorName());
			else
				cell4.setCellValue(" ");
			cellNumber++;

			Cell cell3 = assignCell(cellNumber, tableBodyStyle, row);
			if (grantCall.getGrantCallTypeDesc() != null)
				cell3.setCellValue(grantCall.getGrantCallTypeDesc());
			else
				cell3.setCellValue(" ");
			cellNumber++;

			Cell cell7 = assignCell(cellNumber, tableBodyStyle, row);
			if (grantCall.getOpeningDate() != null)
				cell7.setCellValue(commonService.convertDateFormatBasedOnTimeZone(grantCall.getOpeningDate().getTime(), Constants.DEFAULT_DATE_FORMAT));
			else
				cell7.setCellValue("");
			cellNumber++;

			Cell cell8 = assignCell(cellNumber, tableBodyStyle, row);
			if (grantCall.getClosingDate() != null)
				cell8.setCellValue(commonService.convertDateFormatBasedOnTimeZone(grantCall.getClosingDate().getTime(), Constants.DEFAULT_DATE_FORMAT));
			else
				cell8.setCellValue("");
			cellNumber++;
			
			Cell cell9 = assignCell(cellNumber, tableBodyStyle, row);
			if (grantCall.getInternalSubmissionDeadLineDate() != null) {
				String dateValue = commonService.convertDateFormatBasedOnTimeZone(grantCall.getInternalSubmissionDeadLineDate().getTime(), Constants.DEFAULT_DATE_FORMAT);
				cell9.setCellValue((String) dateValue);
			} else {
				cell9.setCellValue(" ");
			}
			cellNumber++;

			Cell cell6 = assignCell(cellNumber, tableBodyStyle, row);
			if (grantCall.getGrantCallStatusDesc() != null)
				cell6.setCellValue(grantCall.getGrantCallStatusDesc());
			else
				cell6.setCellValue(" ");
			cellNumber++;
		}
	}

	@Override
	public String getAgreementBasedOnCategory(AgreementDashboardVO vo) {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		try {
			dashBoardProfile = dashboardDao.getDashBoardDataForAgreementBasedOnCategory(vo);
		} catch (Exception e) {
			logger.error("Error in method getAgreementBasedOnCategory", e);
		}
		return commonDao.convertObjectToJSON(dashBoardProfile);
	}

	@Override
	public XSSFWorkbook getXSSFWorkbookForAgreementDashboard(AgreementDashboardVO vo) {
		XSSFWorkbook workbook = new XSSFWorkbook();
		vo.setIsDownload(true);
		DashBoardProfile dashBoardProfile = dashboardDao.getDashBoardDataForAgreement(vo);
		XSSFSheet sheet = workbook.createSheet(vo.getDocumentHeading());
    commonService.addDetailsInHeader(workbook, sheet);
		Object[] tableHeadingRow = { "ID", "Type of Agreement", TITLE , "Unit", "Organization", "Administrator", "Requestor", "Principal Investigator", "Status" };
		prepareExcelSheetForAgreement(dashBoardProfile.getAgreementHeaderList(), sheet, tableHeadingRow, workbook, vo);
		return workbook;
	}

	@SuppressWarnings("deprecation")
	private void prepareExcelSheetForAgreement(List<AgreementHeader> agreementHeaderList, XSSFSheet sheet,
			Object[] tableHeadingRow, XSSFWorkbook workbook, AgreementDashboardVO vo) {
		int headingCellNumber = 0;
		String documentHeading = vo.getExportHeading();
		logger.info(DOCUMENT_HEADING, documentHeading);
		// Excel sheet heading style and font creation code.
		Row headerRow = sheet.createRow(0);
		Cell headingCell = headerRow.createCell(0);
		headingCell.setCellValue(documentHeading);
		sheet.addMergedRegion(new CellRangeAddress(0, 0, 0, tableHeadingRow.length - 1));
		XSSFFont headerFont = workbook.createFont();
		headerFont.setBold(true);
		headerFont.setFontHeightInPoints((short) 15);
		XSSFCellStyle headerStyle = workbook.createCellStyle();
		headerStyle.setAlignment(HorizontalAlignment.CENTER);
		headerStyle.setFont(headerFont);
		headingCell.setCellStyle(headerStyle);
		// Table head style and font creation code.
		Row tableHeadRow = sheet.createRow(1);
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
		// Set table head data to each column.
		for (Object heading : tableHeadingRow) {
			Cell cell = tableHeadRow.createCell(headingCellNumber++);
			cell.setCellValue((String) heading);
			cell.setCellStyle(tableHeadStyle);
		}
		// Set table body data to each column.
		int rowNumber = 2;
		for (AgreementHeader agreementHeader : agreementHeaderList) {
			Row row = sheet.createRow(rowNumber++);
			int cellNumber = 0;

			Cell cell1 = assignCell(cellNumber, tableBodyStyle, row);
			if (agreementHeader.getAgreementRequestId() != null)
				cell1.setCellValue(agreementHeader.getAgreementRequestId());
			else
				cell1.setCellValue(" ");
			cellNumber++;

			Cell cell3 = assignCell(cellNumber, tableBodyStyle, row);
			if (agreementHeader.getAgreementTypeCode() != null)
				cell3.setCellValue(agreementHeader.getAgreementTypeCode());
			else
				cell3.setCellValue(" ");
			cellNumber++;

			Cell cell4 = assignCell(cellNumber, tableBodyStyle, row);
			if (agreementHeader.getTitle() != null)
				cell4.setCellValue(agreementHeader.getTitle());
			else
				cell4.setCellValue(" ");
			cellNumber++;

			Cell cell5 = assignCell(cellNumber, tableBodyStyle, row);
			if (agreementHeader.getUnitNumber() != null)
				cell5.setCellValue(commonService.getUnitFormatByUnitDetail(agreementHeader.getUnitNumber(), agreementHeader.getUnitName()));
			else
				cell5.setCellValue(" ");
			cellNumber++;

			Cell cell6 = assignCell(cellNumber, tableBodyStyle, row);
			if (agreementHeader.getOrganization() != null) {
				cell6.setCellValue(agreementHeader.getOrganization());
			} else {
				cell6.setCellValue(" ");
			}
			cellNumber++;

			Cell cell7 = assignCell(cellNumber, tableBodyStyle, row);
			if (agreementHeader.getAdminName() != null) {
				cell7.setCellValue(agreementHeader.getAdminName());
			} else {
				cell7.setCellValue(" ");
			}
			cellNumber++;

			Cell cell8 = assignCell(cellNumber, tableBodyStyle, row);
			if (agreementHeader.getRequestorName() != null) {
				cell8.setCellValue(agreementHeader.getRequestorName());
			} else {
				cell8.setCellValue(" ");
			}
			cellNumber++;

			Cell cell9 = assignCell(cellNumber, tableBodyStyle, row);
			if (agreementHeader.getPiFullName() != null) {
				cell9.setCellValue(agreementHeader.getPiFullName());
			} else {
				cell9.setCellValue(" ");
			}
			cellNumber++;
		
			Cell cell10 = assignCell(cellNumber, tableBodyStyle, row);
			if (agreementHeader.getStatusDescription() != null) {
				cell10.setCellValue(agreementHeader.getStatusDescription());
			} else {
				cell10.setCellValue(" ");
			}
			cellNumber++;

		}
	}

	@Override
	public XSSFWorkbook getXSSFWorkbookForAgreementCategoryDashboard(AgreementDashboardVO vo) {
		XSSFWorkbook workbook = new XSSFWorkbook();
		vo.setIsDownload(true);
		DashBoardProfile dashBoardProfile = dashboardDao.getDashBoardDataForAgreementBasedOnCategory(vo);
		XSSFSheet sheet = workbook.createSheet(vo.getExportHeading());
		commonService.addDetailsInHeader(workbook, sheet);
		Object[] tableHeadingRow = { "ID", "Type of Agreement", TITLE , "Unit", "Organization", "Administrator", "Requestor", "Principal Investigator", "Status" };
		prepareExcelSheetForAgreement(dashBoardProfile.getAgreementHeaderList(), sheet, tableHeadingRow, workbook, vo);
		autoSizeColumns(workbook);
		return workbook;
	}

	@Override
	public String getCOIDashboard(CoiDashboardVO vo) {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		try {
			dashBoardProfile = dashboardDao.getCOIDashboard(vo);
		} catch (Exception e) {
			logger.error("Error in method getCOIDashboard", e);
		}
		return commonDao.convertObjectToJSON(dashBoardProfile);
	}

	@Override
	public String getCOIAdminDashboard(@Valid CoiDashboardVO vo) {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		try {
			dashBoardProfile = dashboardDao.getCOIAdminDashboard(vo);
		} catch (Exception e) {
			logger.error("Error in method getCOIAdminDashboard", e);
		}
		return commonDao.convertObjectToJSON(dashBoardProfile);
	}

	@Override
	public String getSFIDashboard(CoiDashboardVO vo) {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		try {
			dashBoardProfile = dashboardDao.getSFIDashboard(vo);
		} catch (Exception e) {
			logger.error("Error in method getSFIDashboard", e);
		}
		return commonDao.convertObjectToJSON(dashBoardProfile);
	}

}
