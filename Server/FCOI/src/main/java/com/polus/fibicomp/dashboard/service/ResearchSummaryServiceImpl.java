package com.polus.fibicomp.dashboard.service;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.text.DateFormat;
import java.text.DecimalFormat;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;

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
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.dashboard.dao.ResearchSummaryDao;
import com.polus.fibicomp.dashboard.pojo.UserSelectedWidget;
import com.polus.fibicomp.pojo.DashBoardProfile;
import com.polus.fibicomp.pojo.Unit;
import com.polus.fibicomp.print.service.PrintService;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.vo.CommonVO;

@Transactional
@Service(value = "researchSummaryService")
public class ResearchSummaryServiceImpl implements ResearchSummaryService {

	protected static Logger logger = LogManager.getLogger(ResearchSummaryServiceImpl.class.getName());

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private ResearchSummaryDao researchSummaryDao;

	@Autowired
	private DashboardService dashboardService;

	@Autowired
	CommonService commonService;

	@Autowired
	public PrintService printService;

	DecimalFormat decimalFormat = new DecimalFormat(Constants.NUMBER_FORMAT_WITH_DECIMAL);

	@Override
	public String getDetailedSummaryData(CommonVO vo) {
		String personId = vo.getPersonId();
		String researchSummaryIndex = vo.getResearchSummaryIndex();
		String unitNumber = vo.getUnitNumber();
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		try {
			if (researchSummaryIndex.equals("PROPOSALSINPROGRESS")) {
				dashBoardProfile = researchSummaryDao.getProposalsInProgress(personId, unitNumber);
			}
			if (researchSummaryIndex.equals("APPROVALINPROGRESS")) {
				dashBoardProfile = researchSummaryDao.getApprovalInProgressProposals(personId, unitNumber);
			}
			if (researchSummaryIndex.equals("PROPOSALSSUBMITTED")) {
				dashBoardProfile = researchSummaryDao.getSubmittedProposals(personId, unitNumber);
			}
		} catch (Exception e) {
			logger.error("Error in method getDetailedSummaryData", e);
		}
		return commonDao.convertObjectToJSON(dashBoardProfile);
	}

	@Override
	public String getDonutChartDataBySponsor(CommonVO vo) {
		String donutChartData = null;
		String personId = vo.getPersonId();
		String sponsorCode = vo.getSponsorCode();
		String donutChartIndex = vo.getDonutChartIndex();
		String unitNumber = vo.getUnitNumber();
		try {
			if (donutChartIndex.equals("INPROGRESS")) {
				donutChartData = researchSummaryDao.getInProgressProposalsBySponsorExpanded(personId, sponsorCode, unitNumber);
			}
			if (donutChartIndex.equals("AWARDED")) {
				donutChartData = researchSummaryDao.getAwardedProposalsBySponsorExpanded(personId, sponsorCode, unitNumber);
			}
		} catch (Exception e) {
			logger.error("Error in method getPieChartDataByType", e);
		}
		return donutChartData;
	}

	@Override
	public String getPieChartDataByType(CommonVO vo) {
		String pieChartDataByType = null;
		String personId = vo.getPersonId();
		String sponsorCode = vo.getSponsorCode();
		String pieChartIndex = vo.getPieChartIndex();
		String unitNumber = vo.getUnitNumber();
		try {
			if (pieChartIndex.equals("AWARD")) {
				pieChartDataByType = researchSummaryDao.getAwardBySponsorTypes(personId, sponsorCode, unitNumber);
			}
			if (pieChartIndex.equals("PROPOSAL")) {
				pieChartDataByType = researchSummaryDao.getProposalBySponsorTypes(personId, sponsorCode, unitNumber);
			}
		} catch (Exception e) {
			logger.error("Error in method getPieChartDataByType", e);
		}
		return pieChartDataByType;
	}

	@Override
	public ResponseEntity<byte[]> getXSSFWorkbookForResearchSummary(CommonVO vo) {
		ResponseEntity<byte[]> attachmentData = null;
		XSSFWorkbook workbook = new XSSFWorkbook();
		String dashboardIndex = vo.getResearchSummaryIndex();
		logger.info("document heading {}", vo.getDocumentHeading());
		vo.setTabName(dashboardIndex);
		List<Object[]> dashboardData = new ArrayList<>();
		try {
			XSSFSheet sheet = null;
			XSSFCellStyle tableBodyStyle = workbook.createCellStyle();
			String heading = vo.getDocumentHeading();
			dashboardData = researchSummaryDao.getDetailedViewOfWidget(vo);
			if (dashboardIndex.equals("INPROGRESS_PROPOSALS") || dashboardIndex.equals("INPROGRESS_PROPOSALS_COUNT")) {
				if (dashboardIndex.equals("INPROGRESS_PROPOSALS_COUNT")) {
					vo.setDocumentHeading(heading.concat(" for ").concat(setDate(vo.getStartDate())).concat(" - ").concat(setDate(vo.getEndDate())));
				}
				sheet = workbook.createSheet("In Progress Proposals");
				Object[] tableHeadingRow = { "Proposal#", "Title", "Sponsor", "Budget", "Principal Investigator", "Status" };
				prepareExcelSheet(dashboardData, sheet, tableHeadingRow, workbook, vo);
			} else if (dashboardIndex.equals("APPROVAL_INPROGRESS_PROPOSALS") || dashboardIndex.equals("APPROVAL_INPROGRESS_PROPOSALS_COUNT")) {
				sheet = workbook.createSheet("Approval In Progress Proposals");
				if (dashboardIndex.equals("APPROVAL_INPROGRESS_PROPOSALS_COUNT")) {
					vo.setDocumentHeading(heading.concat(" for ").concat(setDate(vo.getStartDate())).concat(" - ").concat(setDate(vo.getEndDate())));
				}
				Object[] tableHeadingRow = { "Proposal#", "Title", "Sponsor", "Current Reviewers", "Budget", "Principal Investigator"};
				prepareApprovalInProgressProposal(tableHeadingRow, sheet, tableBodyStyle, dashboardData, workbook, vo.getDocumentHeading());
			} else if (dashboardIndex.equals("AWARDED_PROPOSALS") || dashboardIndex.equals("AWARDED_PROPOSALS_COUNT")) {
				sheet = workbook.createSheet("Submitted Proposals");
				if (dashboardIndex.equals("AWARDED_PROPOSALS_COUNT")) {
					vo.setDocumentHeading(heading.concat(" for ").concat(setDate(vo.getStartDate())).concat(" - ").concat(setDate(vo.getEndDate())));
				}
				Object[] tableHeadingRow = { "Proposal#", "Title", "Sponsor", "Budget", "Principal Investigator"};
				prepareExcelSheet(dashboardData, sheet, tableHeadingRow, workbook, vo);
			} else if (dashboardIndex.equals("INPROGRESS_PROPOSALS_BY_SPONSOR")) {
				sheet = workbook.createSheet("In Progress Proposals By Sponsor");
				Object[] tableHeadingRow = { "Proposal#", "Title", "Type", "Budget", "Principal Investigator", "Sponsor Deadline" };
				prepareInprogressProposalBySponsor(tableHeadingRow, sheet, tableBodyStyle, dashboardData, workbook, vo.getDocumentHeading());
			} else if (dashboardIndex.equals("RESEARCH_SUMMARY_BY_DEADLINE_DATE")) {
				vo.setDocumentHeading(heading.concat(" (").concat(setDate(vo.getStartDate())).concat(" - ").concat(setDate(vo.getEndDate())).concat(")"));
				sheet = workbook.createSheet("All Proposals By Deadline");
				Object[] tableHeadingRow = { "PD #", "Proposal Type", "Status", "Title", "Lead Unit Name – (Abbreviation)", "Lead PI", "Deadline Date", "Sponsor", "Prime Sponsor", "Activity Type", "Anticipated Award Type", "Announcement #", "Proposal Contact"};
				prepareResearchSummaryByDeadLineDate(tableHeadingRow, sheet, tableBodyStyle, dashboardData, workbook, vo.getDocumentHeading());
			}
			prepareWorkbookForResearchSummary(tableBodyStyle, sheet, workbook, dashboardIndex, dashboardData, vo);
			commonService.addDetailsInHeader(workbook,sheet);
			attachmentData = dashboardService.getResponseEntityForDownload(vo, workbook);
		} catch (Exception e) {
			logger.error("Error in method getXSSFWorkbookForResearchSummary", e);
		} finally {
			try {
				workbook.close();
			} catch (Exception e) {
				logger.error("Error occuredgetXSSFWorkbookForResearchSummary");
			}
		}
		return attachmentData;
	}

	private XSSFWorkbook prepareWorkbookForResearchSummary(XSSFCellStyle tableBodyStyle, XSSFSheet sheet, XSSFWorkbook workbook, String dashboardIndex, List<Object[]> dashboardData, CommonVO vo) {
		try {
		 if (dashboardIndex.equals("AWARDED_PROPOSALS_BY_SPONSOR")) {
				sheet = workbook.createSheet("Awarded Proposals By Sponsor");
				Object[] tableHeadingRow = { "Poposal#", "Title", "Principal Investigator","Type", "Category" };
				prepareAwardedProposalBySponsor(tableHeadingRow, sheet, tableBodyStyle, dashboardData, workbook, vo.getDocumentHeading());
			} else if (dashboardIndex.equals("AWARD_BY_SPONSOR_TYPE") || dashboardIndex.equals("ACTIVE_AWARDS_BY_SPONSOR_TYPE")) {
				sheet = workbook.createSheet("Awards by sponsor types");
				if(dashboardIndex.equals("AWARD_BY_SPONSOR_TYPE")) {
					Object[] tableHeadingRow = { "Award#", "Account", "Title", "Sponsor"};
					prepareAwardBySponsorTypeData(tableHeadingRow, sheet, tableBodyStyle, dashboardData, workbook, vo.getDocumentHeading());
				} else {
					Object[] tableHeadingRow = { "Award#", "Title", "Sponsor", "Anticipated Amount", "Principle Investigator"};
					prepareActiveAwardBySponsorTypeData(tableHeadingRow, sheet, tableBodyStyle, dashboardData, workbook, vo.getDocumentHeading());
				}
			} else if (dashboardIndex.equals("PROPOSAL_BY_SPONSOR_TYPE")) {
				sheet = workbook.createSheet("Proposal by sponsor types");
				Object[] tableHeadingRow = { "Proposal#", "Title", "Sponsor", "Proposal Type", "Principal Investigator", "Sponsor Deadline" };
				dashboardService.prepareExcelSheet(dashboardData, sheet, tableHeadingRow, workbook, vo);
			} else if (dashboardIndex.equals("PENDING_PROPOSALS_OF_SPONSORS")) {
				sheet = workbook.createSheet(vo.getDocumentHeading());
				Object[] tableHeadingRow = { "Proposal#", "Title", "Sponsor", "Principal Investigator",	"Budget" };
				prepareExcelSheet(dashboardData, sheet, tableHeadingRow, workbook, vo);
			} else if (dashboardIndex.equals("PENDING_AWARDS_OF_SPONSORS")) {
				sheet = workbook.createSheet(vo.getDocumentHeading());
				Object[] tableHeadingRow = { "Award #", "Title", "Sponsor", "Principal Investigator",	"Cost" };
				preparePendingAwardsOfSponsor(tableHeadingRow, sheet, tableBodyStyle, dashboardData, workbook, vo.getDocumentHeading());
			}  else if (dashboardIndex.equals("PENDING_PROPOSAL_BY_SPONSOR_TYPE")) {
				sheet = workbook.createSheet(vo.getDocumentHeading());
				Object[] tableHeadingRow = { "Proposal#", "Title", "Sponsor", "Budget", "Principal Investigator"};
				preparePendingProposalBySponsorType(tableHeadingRow, sheet, tableBodyStyle, dashboardData, workbook, vo.getDocumentHeading());
			} else if (dashboardIndex.equals("AWARD_BY_SPONSOR")) {
				sheet = workbook.createSheet(vo.getDocumentHeading());
				Object[] tableHeadingRow = { "Award #", "Account Number", "Title", "Sponsor Name"};
				prepareAwardBySponsorData(tableHeadingRow, sheet, tableBodyStyle, dashboardData, workbook, vo.getDocumentHeading());
			}
		} catch(Exception e) {
			logger.error("Error in method prepareReaserchSummaryWidgetExport", e);
		} 
		return workbook;
	}

	private void prepareResearchSummaryByDeadLineDate(Object[] tableHeadingRow, XSSFSheet sheet,
			XSSFCellStyle tableBodyStyle, List<Object[]> dashboardData, XSSFWorkbook workbook, String documentHeading) {
		int rowNumber = 0;
		prepareHeading(tableHeadingRow, workbook, sheet, documentHeading);
		rowNumber++;
		prepareExcelSheetHeader(sheet, tableHeadingRow, workbook, tableBodyStyle, rowNumber++);
		for (Object[] objectArray : dashboardData) {
			Row row = sheet.createRow(rowNumber++);
			int cellNumber = 0;
			Cell proposalIdCell = assignCell(cellNumber, tableBodyStyle, row);
			if (objectArray[0] != null) {
				proposalIdCell.setCellValue((Integer) objectArray[0]);
			} else {
				proposalIdCell.setCellValue(" ");
			}
			cellNumber++;
			Cell typeCell = assignCell(cellNumber, tableBodyStyle, row);
			typeCell.setCellValue(objectArray[1] != null ? (String) objectArray[1] : "");
			cellNumber++;
			Cell statusCell = assignCell(cellNumber, tableBodyStyle, row);
			statusCell.setCellValue(objectArray[2] != null ? (String) objectArray[2] : "");
			cellNumber++;
			Cell titleCell = assignCell(cellNumber, tableBodyStyle, row);
			titleCell.setCellValue(objectArray[3] != null ? (String) objectArray[3] : "");
			cellNumber++;
			Cell leadUnitCell = assignCell(cellNumber, tableBodyStyle, row);
			String leadUnitAcronym = (String) objectArray[14] != null ? ("(" + (String) objectArray[14] + ")") : "";
			String leadUnit = (String) objectArray[5] != null ? (((String) objectArray[4] + " - " + (String) objectArray[5]) + leadUnitAcronym) : "";
			leadUnitCell.setCellValue(leadUnit);
			cellNumber++;
			Cell piCell = assignCell(cellNumber, tableBodyStyle, row);
			piCell.setCellValue(objectArray[6] != null ? (String) objectArray[6] : "");
			cellNumber++;
			Cell sponsorDeadlineCell = assignCell(cellNumber, tableBodyStyle, row);
			if (objectArray[7] != null) {
				sponsorDeadlineCell.setCellValue(setDate(objectArray[7].toString()));
            } else {
            	sponsorDeadlineCell.setCellValue(" ");
            }
			cellNumber++;
			Cell sponsorCell = assignCell(cellNumber, tableBodyStyle, row);
			sponsorCell.setCellValue(objectArray[8] != null ? (String) objectArray[8] : "");
			cellNumber++;
			Cell primeSponsorCell = assignCell(cellNumber, tableBodyStyle, row);
			primeSponsorCell.setCellValue(objectArray[9] != null ? (String) objectArray[9] : "");
			cellNumber++;
			Cell activityTypeCell = assignCell(cellNumber, tableBodyStyle, row);
			activityTypeCell.setCellValue(objectArray[10] != null ? (String) objectArray[10] : "");
			cellNumber++;
			Cell anticipatedAwardTypeCell = assignCell(cellNumber, tableBodyStyle, row);
			anticipatedAwardTypeCell.setCellValue(objectArray[11] != null ? (String) objectArray[11] : "");
			cellNumber++;
			Cell announcementTypeCell = assignCell(cellNumber, tableBodyStyle, row);
			announcementTypeCell.setCellValue(objectArray[12] != null ? (String) objectArray[12] : "");
			cellNumber++;
			Cell proposalContactCell = assignCell(cellNumber, tableBodyStyle, row);
			proposalContactCell.setCellValue(objectArray[13] != null ? (String) objectArray[13] : "");
			cellNumber++;
		}
		autoSizeColumns(workbook);
	}

	private void preparePendingProposalBySponsorType(Object[] tableHeadingRow, XSSFSheet sheet,
		XSSFCellStyle tableBodyStyle, List<Object[]> dashboardData, XSSFWorkbook workbook, String documentHeading) {
		int rowNumber = 0;
		prepareHeading(tableHeadingRow, workbook, sheet, documentHeading);
		rowNumber++;
		prepareExcelSheetHeader(sheet, tableHeadingRow, workbook, tableBodyStyle, rowNumber++);
		for (Object[] objectArray : dashboardData) {
			Row row = sheet.createRow(rowNumber++);
			int cellNumber = 0;
			Cell proposalIdCell = assignCell(cellNumber, tableBodyStyle, row);
			if (objectArray[0] != null) {
				proposalIdCell.setCellValue((Integer) objectArray[0]);
			} else {
				proposalIdCell.setCellValue(" ");
			}
			cellNumber++;

			Cell titleCell = assignCell(cellNumber, tableBodyStyle, row);
			titleCell.setCellValue(objectArray[1] != null ? (String) objectArray[1] : "");
			cellNumber++;

			Cell sponsorNameCell = assignCell(cellNumber, tableBodyStyle, row);
			sponsorNameCell.setCellValue(objectArray[2] != null ? (String) objectArray[2] : "");
			cellNumber++;

			Cell budgetCell = assignCell(cellNumber, tableBodyStyle, row);
			budgetCell.setCellValue(objectArray[3] != null ? Constants.DOLLAR_SYMBOL + decimalFormat.format(new BigDecimal((String) objectArray[3])) : "");
			cellNumber++;

			Cell piCell = assignCell(cellNumber, tableBodyStyle, row);
			piCell.setCellValue(objectArray[4] != null ? (String) objectArray[4] : "");
			cellNumber++;
		}
		autoSizeColumns(workbook);
    }

	private void preparePendingAwardsOfSponsor(Object[] tableHeadingRow, XSSFSheet sheet, XSSFCellStyle tableBodyStyle,
		List<Object[]> dashboardData, XSSFWorkbook workbook, String documentHeading) {
		int rowNumber = 0;
		prepareHeading(tableHeadingRow, workbook, sheet, documentHeading);
		rowNumber++;
		prepareExcelSheetHeader(sheet, tableHeadingRow, workbook, tableBodyStyle, rowNumber++);
		for (Object[] objectArray : dashboardData) {
			Row row = sheet.createRow(rowNumber++);
			int cellNumber = 0;
			Cell awardCell = assignCell(cellNumber, tableBodyStyle, row);
			if (objectArray[0] != null) {
				String stringValue = ((BigDecimal) objectArray[0]).toString();
				awardCell.setCellValue(stringValue);
			} else {
				awardCell.setCellValue(" ");
			}
			cellNumber++;

			Cell titleCell = assignCell(cellNumber, tableBodyStyle, row);
			titleCell.setCellValue(objectArray[1] != null ? (String) objectArray[1] : "");
			cellNumber++;

			Cell sponsorCell = assignCell(cellNumber, tableBodyStyle, row);
			sponsorCell.setCellValue(objectArray[2] != null ? (String) objectArray[2] : "");
			cellNumber++;

			Cell piCell = assignCell(cellNumber, tableBodyStyle, row);
			piCell.setCellValue(objectArray[3] != null ? (String) objectArray[3] : "");
			cellNumber++;

			Cell budgetCell = assignCell(cellNumber, tableBodyStyle, row);
			DecimalFormat decimalFormat = new DecimalFormat(Constants.NUMBER_FORMAT_WITH_DECIMAL);
			if (objectArray[4] != null) {
				String stringValue = ((BigDecimal) objectArray[4]).toString();
				XSSFCellStyle headStyle = workbook.createCellStyle();
				headStyle.setAlignment(HorizontalAlignment.RIGHT);
				budgetCell.setCellStyle(headStyle);
				budgetCell.setCellValue(Constants.DOLLAR_SYMBOL + decimalFormat.format(new BigDecimal(stringValue)));
			} else {
				budgetCell.setCellValue(" ");
			}
			cellNumber++;
		}
		autoSizeColumns(workbook);
	}

	public void prepareExcelSheet(List<Object[]> dashboardData, XSSFSheet sheet, Object[] tableHeadingRow,
			XSSFWorkbook workbook, CommonVO vo) {
		XSSFCellStyle tableBodyStyle = workbook.createCellStyle();
		String documentHeading = vo.getDocumentHeading();
		int rowNumber = 0;
		prepareHeading(tableHeadingRow, workbook, sheet, documentHeading);
		rowNumber++;
		prepareExcelSheetHeader(sheet, tableHeadingRow, workbook, tableBodyStyle, rowNumber++);
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
					cell.setCellValue(stringValue);
				} else if (objectData instanceof BigDecimal) {
					DecimalFormat decimalFormat = new DecimalFormat(Constants.NUMBER_FORMAT_WITH_DECIMAL);
					String stringValue = ((BigDecimal) objectData).toString();
					XSSFCellStyle headStyle = workbook.createCellStyle();
					headStyle.setAlignment(HorizontalAlignment.RIGHT);
					cell.setCellStyle(headStyle);
					cell.setCellValue(Constants.DOLLAR_SYMBOL + decimalFormat.format(new BigDecimal(stringValue)));
				} else if (objectData instanceof Date) {
					if (objectData != null) {
						Date date = (Date) objectData;
						String dateValue = commonService.convertDateFormatBasedOnTimeZone(date.getTime(),
								Constants.DEFAULT_DATE_FORMAT);
						cell.setCellValue(dateValue);
					}
				} else if (objectData == null) {
					cell.setCellValue(" ");
				}
			}
		}
		autoSizeColumns(workbook);
	}

	private String setDate(String dateValue) { 
		try {
			DateFormat fromDate = new SimpleDateFormat("yyyy-mm-dd");
			DateFormat toDate = new SimpleDateFormat("dd/mm/yyyy");
			return toDate.format(fromDate.parse(dateValue));
		} catch (Exception e) {
			logger.error("error in setDate {}", e.getMessage());
			return dateValue;
		}
	}

	private void prepareActiveAwardBySponsorTypeData(Object[] tableHeadingRow, XSSFSheet sheet,
			XSSFCellStyle tableBodyStyle, List<Object[]> dashboardData, XSSFWorkbook workbook, String documentHeading) {
		int rowNumber = 0;
		prepareHeading(tableHeadingRow, workbook, sheet, documentHeading);
		rowNumber++;
		prepareExcelSheetHeader(sheet, tableHeadingRow, workbook, tableBodyStyle, rowNumber++);
		for (Object[] objectArray : dashboardData) {
			Row row = sheet.createRow(rowNumber++);
			int cellNumber = 0;
			Cell awardCell = assignCell(cellNumber, tableBodyStyle, row);
			awardCell.setCellValue(objectArray[0] != null ? (String) objectArray[0] : "");
			cellNumber++;

			Cell titleCell = assignCell(cellNumber, tableBodyStyle, row);
			titleCell.setCellValue(objectArray[2] != null ? (String) objectArray[2] : "");
			cellNumber++;

			Cell sponsorCell = assignCell(cellNumber, tableBodyStyle, row);
			sponsorCell.setCellValue(objectArray[3] != null ? (String) objectArray[3] : "");
			cellNumber++;

			Cell budgetCell = assignCell(cellNumber, tableBodyStyle, row);
			DecimalFormat decimalFormat = new DecimalFormat(Constants.NUMBER_FORMAT_WITH_DECIMAL);
			if (objectArray[5] != null) {
				String stringValue = ((BigDecimal) objectArray[5]).toString();
				XSSFCellStyle headStyle = workbook.createCellStyle();
				headStyle.setAlignment(HorizontalAlignment.RIGHT);
				budgetCell.setCellStyle(headStyle);
				if (stringValue.equals("0.00")) {
					budgetCell.setCellValue("-");
				} else {
					budgetCell.setCellValue(Constants.DOLLAR_SYMBOL + decimalFormat.format(new BigDecimal(stringValue)));
				}
			} else {
				budgetCell.setCellValue(" ");
			}
			cellNumber++;

			Cell piCell = assignCell(cellNumber, tableBodyStyle, row);
			piCell.setCellValue(objectArray[6] != null ? (String) objectArray[6] : "");
			cellNumber++;
		}
		autoSizeColumns(workbook);
	}

	private void prepareAwardBySponsorData(Object[] tableHeadingRow, XSSFSheet sheet, XSSFCellStyle tableBodyStyle,
			List<Object[]> dashboardData, XSSFWorkbook workbook, String documentHeading) {
		int rowNumber = 0;
		prepareHeading(tableHeadingRow, workbook, sheet, documentHeading);
		rowNumber++;
		prepareExcelSheetHeader(sheet, tableHeadingRow, workbook, tableBodyStyle, rowNumber++);
		for (Object[] objectArray : dashboardData) {
			Row row = sheet.createRow(rowNumber++);
			int cellNumber = 0;
			Cell awardCell = assignCell(cellNumber, tableBodyStyle, row);
			awardCell.setCellValue(objectArray[0] != null ? (String) objectArray[0] : "");
			cellNumber++;

			Cell accountCell = assignCell(cellNumber, tableBodyStyle, row);
			accountCell.setCellValue(objectArray[1] != null ? (String) objectArray[1] : "");
			cellNumber++;

			Cell titleCell = assignCell(cellNumber, tableBodyStyle, row);
			titleCell.setCellValue(objectArray[2] != null ? (String) objectArray[2] : "");
			cellNumber++;


			Cell sponsorCell = assignCell(cellNumber, tableBodyStyle, row);
			sponsorCell.setCellValue(objectArray[3] != null ? (String) objectArray[3] : "");
			cellNumber++;
		}
		autoSizeColumns(workbook);
	}

	private void prepareInprogressProposalBySponsor(Object[] tableHeadingRow, XSSFSheet sheet,
			XSSFCellStyle tableBodyStyle, List<Object[]> dashboardData, XSSFWorkbook workbook, String documentHeading) {
		int rowNumber = 0;
		prepareHeading(tableHeadingRow, workbook, sheet, documentHeading);
		rowNumber++;
		prepareExcelSheetHeader(sheet, tableHeadingRow, workbook, tableBodyStyle, rowNumber++);
		for (Object[] objectArray : dashboardData) {
			Row row = sheet.createRow(rowNumber++);
			int cellNumber = 0;
			Cell proposalIdCell = assignCell(cellNumber, tableBodyStyle, row);
			if (objectArray[0] != null) {
				proposalIdCell.setCellValue((Integer) objectArray[0]);
			} else {
				proposalIdCell.setCellValue(" ");
			}
			cellNumber++;

			Cell titleCell = assignCell(cellNumber, tableBodyStyle, row);
			titleCell.setCellValue(objectArray[1] != null ? (String) objectArray[1] : "");
			cellNumber++;

			Cell typeCell = assignCell(cellNumber, tableBodyStyle, row);
			typeCell.setCellValue(objectArray[4] != null ? (String) objectArray[4] : "");
			cellNumber++;

			Cell budgetCell = assignCell(cellNumber, tableBodyStyle, row);
			budgetCell.setCellValue(objectArray[5] != null ? Constants.DOLLAR_SYMBOL + decimalFormat.format(new BigDecimal((String) objectArray[5])) : "");
			cellNumber++;

			Cell piCell = assignCell(cellNumber, tableBodyStyle, row);
			piCell.setCellValue(objectArray[2] != null ? (String) objectArray[2] : "");
			cellNumber++;

			Cell sponsorCell = assignCell(cellNumber, tableBodyStyle, row);
			if (objectArray[6] != null) {
                sponsorCell.setCellValue(setDate(objectArray[6].toString()));
            } else {
            	sponsorCell.setCellValue(" ");
            }
			cellNumber++;
		}
		autoSizeColumns(workbook);
	}

	private void prepareAwardBySponsorTypeData(Object[] tableHeadingRow, XSSFSheet sheet, XSSFCellStyle tableBodyStyle,
			List<Object[]> dashboardData, XSSFWorkbook workbook, String documentHeading) {
		int rowNumber = 0;
		prepareHeading(tableHeadingRow, workbook, sheet, documentHeading);
		rowNumber++;
		prepareExcelSheetHeader(sheet, tableHeadingRow, workbook, tableBodyStyle, rowNumber++);
		for (Object[] objectArray : dashboardData) {
			Row row = sheet.createRow(rowNumber++);
			int cellNumber = 0;
			Cell awardCell = assignCell(cellNumber, tableBodyStyle, row);
			awardCell.setCellValue(objectArray[0] != null ? (String) objectArray[0] : "");
			cellNumber++;

			Cell accountCell = assignCell(cellNumber, tableBodyStyle, row);
			accountCell.setCellValue(objectArray[1] != null ? (String) objectArray[1] : "");
			cellNumber++;

			Cell titleCell = assignCell(cellNumber, tableBodyStyle, row);
			titleCell.setCellValue(objectArray[2] != null ? (String) objectArray[2] : "");
			cellNumber++;

			Cell sponsorCell = assignCell(cellNumber, tableBodyStyle, row);
			sponsorCell.setCellValue(objectArray[3] != null ? (String) objectArray[3] : "");
			cellNumber++;
		}
		autoSizeColumns(workbook);
	}

	private void prepareAwardedProposalBySponsor(Object[] tableHeadingRow, XSSFSheet sheet, XSSFCellStyle tableBodyStyle,
		List<Object[]> dashboardData, XSSFWorkbook workbook, String documentHeading) {
		int rowNumber = 0;
		prepareHeading(tableHeadingRow, workbook, sheet, documentHeading);
		rowNumber++;
		prepareExcelSheetHeader(sheet, tableHeadingRow, workbook, tableBodyStyle, rowNumber++);
		for (Object[] objectArray : dashboardData) {
			Row row = sheet.createRow(rowNumber++);
			int cellNumber = 0;
			Cell proposalIdCell = assignCell(cellNumber, tableBodyStyle, row);
			proposalIdCell.setCellValue(objectArray[0] != null ? (String) objectArray[0] : "");
			cellNumber++;

			Cell titleCell = assignCell(cellNumber, tableBodyStyle, row);
			titleCell.setCellValue(objectArray[1] != null ? (String) objectArray[1] : "");
			cellNumber++;

			Cell piCell = assignCell(cellNumber, tableBodyStyle, row);
			piCell.setCellValue(objectArray[2] != null ? (String) objectArray[2] : "");
			cellNumber++;

			Cell typeCell = assignCell(cellNumber, tableBodyStyle, row);
			typeCell.setCellValue(objectArray[6] != null ? (String) objectArray[6] : "");
			cellNumber++;

			Cell categoryCell = assignCell(cellNumber, tableBodyStyle, row);
			categoryCell.setCellValue(objectArray[4] != null ? (String) objectArray[4] : "");
			cellNumber++;
		}
		autoSizeColumns(workbook);
	}

	private XSSFWorkbook prepareHeading(Object[] tableHeadingRow, XSSFWorkbook workbook, XSSFSheet sheet, String documentHeading) {
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
		return workbook;
	}

	 private void autoSizeColumns(XSSFWorkbook workbook) {
	        int numberOfSheets = workbook.getNumberOfSheets();
	        for (int i = 0; i < numberOfSheets; i++) {
	            XSSFSheet sheet = workbook.getSheetAt(i);
	            if (sheet.getPhysicalNumberOfRows() > 0) {
	                Row row = sheet.getRow(1);
	                if (row != null) {
	                	Iterator<Cell> cellIterator = row.cellIterator();
	 	                while (cellIterator.hasNext()) {
	 	                    Cell cell = cellIterator.next();
	 	                    int columnIndex = cell.getColumnIndex();
	 	                    sheet.autoSizeColumn(columnIndex);
	 	                }
	                }
	            }
	        }
	    }

	private void prepareApprovalInProgressProposal(Object[] tableHeadingRow, XSSFSheet sheet,
			XSSFCellStyle tableBodyStyle, List<Object[]> dashboardData, XSSFWorkbook workbook, String documentHeading) {
		int rowNumber = 0;
		prepareHeading(tableHeadingRow, workbook, sheet, documentHeading);
		rowNumber++;
		prepareExcelSheetHeader(sheet, tableHeadingRow, workbook, tableBodyStyle, rowNumber++);
		for (Object[] objectArray : dashboardData) {
			Row row = sheet.createRow(rowNumber++);
			int cellNumber = 0;
			Cell proposalIdCell = assignCell(cellNumber, tableBodyStyle, row);
			if (objectArray[0] != null) {
				proposalIdCell.setCellValue((Integer) objectArray[0]);
			} else {
				proposalIdCell.setCellValue(" ");
			}
			cellNumber++;

			Cell titleCell = assignCell(cellNumber, tableBodyStyle, row);
			titleCell.setCellValue(objectArray[1] != null ? (String) objectArray[1] : "");
			cellNumber++;

			Cell sponsorCell = assignCell(cellNumber, tableBodyStyle, row);
			sponsorCell.setCellValue(objectArray[2] != null ? (String) objectArray[2] : "");
			cellNumber++;

			Cell reviewerCell = assignCell(cellNumber, tableBodyStyle, row);
			reviewerCell.setCellValue(objectArray[5] != null ? (String) objectArray[5] : "");
			cellNumber++;

			Cell budgetCell = assignCell(cellNumber, tableBodyStyle, row);
			DecimalFormat decimalFormat = new DecimalFormat(Constants.NUMBER_FORMAT_WITH_DECIMAL);
			if (objectArray[3] != null) {
				String stringValue = ((BigDecimal) objectArray[3]).toString();
				XSSFCellStyle headStyle = workbook.createCellStyle();
				headStyle.setAlignment(HorizontalAlignment.RIGHT);
				budgetCell.setCellStyle(headStyle);
				budgetCell.setCellValue(Constants.DOLLAR_SYMBOL + decimalFormat.format(new BigDecimal(stringValue)));
			} else {
				budgetCell.setCellValue(" ");
			}
			cellNumber++;

			Cell piCell = assignCell(cellNumber, tableBodyStyle, row);
			piCell.setCellValue(objectArray[4] != null ? (String) objectArray[4] : "");
			cellNumber++;
		}
		autoSizeColumns(workbook);
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
			cell.setCellValue((String) heading);
			cell.setCellStyle(tableHeadStyle);
		}
		return workbook;
	}

	@Override
	public String getResearchSummaryTable(CommonVO vo) {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		String personId = AuthenticatedUser.getLoginPersonId();
		dashBoardProfile.setSummaryViews(researchSummaryDao.getSummaryTable(personId, vo.getUnitNumber()));
		return commonDao.convertObjectToJSON(dashBoardProfile);
	}

	@Override
	public String getExpenditureVolumeChart(CommonVO vo) {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		dashBoardProfile.setExpenditureVolumes(researchSummaryDao.getExpenditureVolumeChart(vo.getPersonId(), vo.getUnitNumber()));
		return commonDao.convertObjectToJSON(dashBoardProfile);
	}

	@Override
	public String getSummaryAwardPieChart(CommonVO vo) {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		dashBoardProfile.setSummaryAwardPieChart(researchSummaryDao.getSummaryAwardPieChart(vo.getPersonId(), vo.getUnitNumber()));
		return commonDao.convertObjectToJSON(dashBoardProfile);
	}

	@Override
	public String getSummaryProposalPieChart(CommonVO vo) {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		dashBoardProfile.setSummaryProposalPieChart(researchSummaryDao.getSummaryProposalPieChart(vo.getPersonId(), vo.getUnitNumber()));
		return commonDao.convertObjectToJSON(dashBoardProfile);
	}

	@Override
	public String getSummaryInProgressProposalDonutChart(CommonVO vo) {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		dashBoardProfile.setSummaryProposalDonutChart(researchSummaryDao.getSummaryInProgressProposalDonutChart(vo.getPersonId(), vo.getUnitNumber()));
		return commonDao.convertObjectToJSON(dashBoardProfile);
	}

	@Override
	public String getSummaryAwardedProposalDonutChart(CommonVO vo) {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		dashBoardProfile.setSummaryAwardDonutChart(researchSummaryDao.getSummaryAwardedProposalDonutChart(vo.getPersonId(), vo.getUnitNumber()));
		return commonDao.convertObjectToJSON(dashBoardProfile);
	}

	@Override
	public String getAllQuickLinksOrEvents() {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		dashBoardProfile.setQuickLinks(researchSummaryDao.getAllQuickLinksOrEvents());
		return commonDao.convertObjectToJSON(dashBoardProfile);
	}

	@Override
	public String getWidgetLookups() {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		dashBoardProfile.setWidgetLookups(researchSummaryDao.getAllWidgetLookups());
		dashBoardProfile.setUserSelectedWidgets(researchSummaryDao.getUserSelectedWidgets(AuthenticatedUser.getLoginPersonId()));
		return commonDao.convertObjectToJSON(dashBoardProfile);
	}

	@Override
	public String saveUserSelectedWidget(DashBoardProfile dashBoardProfile) {
		try {
			UserSelectedWidget userSelectedWidget = dashBoardProfile.getUserSelectedWidget();
			userSelectedWidget = researchSummaryDao.checkIfWidgetAlreadyExist(userSelectedWidget.getWidgetId(), userSelectedWidget.getPersonId());
			if (userSelectedWidget == null){
				userSelectedWidget = dashBoardProfile.getUserSelectedWidget();
				researchSummaryDao.saveUserSelectedWidget(userSelectedWidget);
			} else {
				dashBoardProfile.setUserSelectedWidget(userSelectedWidget);
			}
		} catch (Exception e) {
			logger.error("error occured in saveUserSelectedWidget : {}", e.getMessage());
		}
		return commonDao.convertObjectToJSON(dashBoardProfile);
	}

	@Override
	public String deleteUserSelectedWidget(Integer selectedWidgetId) {
		try {
			researchSummaryDao.deleteUserSelectedWidget(selectedWidgetId);
			return commonDao.convertObjectToJSON("Successfully deleted");
		} catch (Exception e) {
			logger.error("Error occuerd in deleteUserSelectedWidget : {}", e.getMessage());
			return commonDao.convertObjectToJSON("failed to delete");
		}
	}

	@Override
	public String updateWidgetSortOrder(DashBoardProfile dashBoardProfile) {
		dashBoardProfile.getUserSelectedWidgets().stream().forEach(selectedWidget -> {
			researchSummaryDao.saveUserSelectedWidget(selectedWidget);
		});
		return commonDao.convertObjectToJSON(dashBoardProfile);
	}

	@Override
	public String getResearchSummaryDatasByWidget(CommonVO commonVO) {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		dashBoardProfile.setWidgetDatas(researchSummaryDao.getResearchSummaryDatasByWidget(commonVO));
		return commonDao.convertObjectToJSON(dashBoardProfile);
	}

	@Override
	public String getDetailedViewOfWidget(CommonVO commonVO) {
		DashBoardProfile dashBoardProfile = new DashBoardProfile();
		dashBoardProfile.setWidgetDatas(researchSummaryDao.getDetailedViewOfWidget(commonVO));
		return commonDao.convertObjectToJSON(dashBoardProfile);
	}

	@Override
	public String getAgreementSummary(CommonVO vo) {
		return researchSummaryDao.getDashBoardResearchSummary(vo.getPersonId(), vo.getIsAdmin());
	}
	
	@Override
	public String getUnitWithRights(String personId) {
		List<Unit> units = researchSummaryDao.getUnitWithRights(personId);
		return commonDao.convertObjectToJSON(units);
	}

}
