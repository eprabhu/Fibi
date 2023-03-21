package com.polus.fibicomp.export.service;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.math.BigDecimal;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.poi.ss.usermodel.BorderStyle;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Row;
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

import com.polus.fibicomp.award.dao.AwardDao;
import com.polus.fibicomp.award.datesandamounts.dao.DatesAndAmountDao;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardAmountInfo;
import com.polus.fibicomp.award.pojo.AwardCostShare;
import com.polus.fibicomp.award.pojo.AwardFundingProposal;
import com.polus.fibicomp.award.pojo.AwardPerson;
import com.polus.fibicomp.budget.dao.AwardBudgetDao;
import com.polus.fibicomp.budget.pojo.AwardBudgetHeader;
import com.polus.fibicomp.budget.service.AwardBudgetService;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.dashboard.vo.AwardDashboardVO;
import com.polus.fibicomp.export.dao.ExportDataDao;
import com.polus.fibicomp.grantcall.dao.GrantCallDao;
import com.polus.fibicomp.grantcall.pojo.GrantCall;
import com.polus.fibicomp.ip.pojo.InstituteProposalAdminDetail;
import com.polus.fibicomp.proposal.dao.ProposalDao;
import com.polus.fibicomp.proposal.pojo.Proposal;

@Transactional
@Service(value = "exportDataService")
public class ExportDataServiceImpl implements ExportDataService{

	protected static Logger logger = LogManager.getLogger(ExportDataServiceImpl.class.getName());

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private ExportDataDao exportDataDao;

	@Autowired
	private ProposalDao proposalDao;

	@Autowired
	private GrantCallDao grantCallDao;

	@Autowired
	private CommonService commonService;

	@Autowired
	private AwardBudgetDao awardBudgetDao;

	@Autowired
	private DatesAndAmountDao datesAndAmountDao;

	@Autowired
	private AwardDao awardDao;

	@Autowired
	private AwardBudgetService awardBudgetService;

	@Override
	public XSSFWorkbook getXSSFWorkbookForActive(AwardDashboardVO vo) throws Exception {
		XSSFWorkbook workbook = new XSSFWorkbook();
		vo.setIsDownload(true);
		List<Award> awards = exportDataDao.getAllActiAwards();
		XSSFSheet sheet = workbook.createSheet("ACTIVE AWARDS");
		commonService.addDetailsInHeader(workbook,sheet);
		if (commonDao.getParameterValueAsBoolean(Constants.IS_APPLICATION_ID_REQUIRED)) {
			Object[] tableHeadingRow = { "grantsID", "GrantsType", "FundInitiative", "ProjectTitle", "Funding Agency",
					"NamePI_Programme", "NamePI_Project_1", "NamePI_Project_2", "NamePI_Project_3",
					"NamePI_Project_4", "NamePI_Project_5", "NamePI_Project_6", "NamePI_Project_7", "NamePI_Project_8",
					"NamePI_Project_9", "NamePI_Project_10", "NamePI_Project_11", "NamePI_Project_12",
					"NamePI_Project_13", "NamePI_Project_14", "NamePI_Project_15", "NameCoPI_Project_1",
					"NameCoPI_Project_2", "NameCoPI_Project_3", "NameCoPI_Project_4", "NameCoPI_Project_5",
					"NameCoPI_Project_6", "NameCoPI_Project_7", "NameCoPI_Project_8", "NameCoPI_Project_9",
					"NameCoPI_Project_10", "NameCoPI_Project_11", "NameCoPI_Project_12", "NameCoPI_Project_13",
					"NameCoPI_Project_14", "NameCoPI_Project_15", "NameColl_1", "NameColl_2", "NameColl_3",
					"NameColl_4", "NameColl_5", "NameColl_6", "NameColl_7", "NameColl_8", "NameColl_9", "NameColl_10",
					"NameColl_11", "NameColl_12", "NameColl_13", "NameColl_14", "NameColl_15", "NameMember_1",
					"NameMember_2", "NameMember_3", "NameMember_4", "NameMember_5", "NameMember_6", "NameMember_7",
					"NameMember_8", "NameMember_9", "NameMember_10", "TotalBudgetRequested", "ApplOutcome",
					"SubmissionDate", "StartDate", "LatestEndDate", "AwardDate", "TotAwardedGrantAmount" };
			prepareExcelSheetForActiceAward(awards, sheet, tableHeadingRow, workbook, vo);
		} else {
			Object[] tableHeadingRow = { "grantsID", "GrantsType", "FundInitiative", "ProjectTitle", "Funding Agency",
					"NamePI_Programme", "NamePI_Project_1", "NamePI_Project_2", "NamePI_Project_3",
					"NamePI_Project_4", "NamePI_Project_5", "NamePI_Project_6", "NamePI_Project_7", "NamePI_Project_8",
					"NamePI_Project_9", "NamePI_Project_10", "NamePI_Project_11", "NamePI_Project_12",
					"NamePI_Project_13", "NamePI_Project_14", "NamePI_Project_15", "NameCoPI_Project_1",
					"NameCoPI_Project_2", "NameCoPI_Project_3", "NameCoPI_Project_4", "NameCoPI_Project_5",
					"NameCoPI_Project_6", "NameCoPI_Project_7", "NameCoPI_Project_8", "NameCoPI_Project_9",
					"NameCoPI_Project_10", "NameCoPI_Project_11", "NameCoPI_Project_12", "NameCoPI_Project_13",
					"NameCoPI_Project_14", "NameCoPI_Project_15", "NameColl_1", "NameColl_2", "NameColl_3",
					"NameColl_4", "NameColl_5", "NameColl_6", "NameColl_7", "NameColl_8", "NameColl_9", "NameColl_10",
					"NameColl_11", "NameColl_12", "NameColl_13", "NameColl_14", "NameColl_15", "NameMember_1",
					"NameMember_2", "NameMember_3", "NameMember_4", "NameMember_5", "NameMember_6", "NameMember_7",
					"NameMember_8", "NameMember_9", "NameMember_10", "TotalBudgetRequested", "ApplOutcome",
					"SubmissionDate", "StartDate", "LatestEndDate", "AwardDate", "TotAwardedGrantAmount" };
			prepareExcelSheetForActiceAward(awards, sheet, tableHeadingRow, workbook, vo);
		}
		return workbook;
	}

	private void prepareExcelSheetForActiceAward(List<Award> awards, XSSFSheet sheet, Object[] tableHeadingRow,
			XSSFWorkbook workbook, AwardDashboardVO vo) {	 
		int headingCellNumber = 0;
//		String documentHeading = "ACTIVE AWARDS";
//		logger.info("documentHeading : " + documentHeading);
//		// Excel sheet heading style and font creation code.
//		Row headerRow = sheet.createRow(0);
//		Cell headingCell = headerRow.createCell(0);
//		headingCell.setCellValue((String) documentHeading);
//		sheet.addMergedRegion(new CellRangeAddress(0, 0, 0, tableHeadingRow.length - 1));
//		XSSFFont headerFont = workbook.createFont();
//		headerFont.setBold(true);
//		headerFont.setFontHeightInPoints((short) 15);
//		XSSFCellStyle headerStyle = workbook.createCellStyle();
//		headerStyle.setAlignment(CellStyle.ALIGN_CENTER);
//		headerStyle.setFont(headerFont);
//		headingCell.setCellStyle(headerStyle);
		// Table head style and font creation code.
		Row tableHeadRow = sheet.createRow(0);
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
		int rowNumber = 1;
		for (Award awardData : awards) {
			Proposal proposal =  getProposalDetailsOfAward(awardData.getAwardId());
			GrantCall grantCall = new GrantCall();
			String applicationId = getProposalDetailsOfAward(awardData.getAwardId()).getApplicationId();
			if (proposal != null && proposal.getGrantCallId() != null) {
				grantCall = grantCallDao.fetchGrantCallById(proposal.getGrantCallId());
			}
			AwardBudgetHeader awardBudgetHeader = awardBudgetDao.getAwardBudgetHeaderByAwardId(awardData.getAwardId());
			Row row = sheet.createRow(rowNumber++);
			int cellNumber = 0;
			Cell cell1 = assignCell(cellNumber, tableBodyStyle, row);
			if (applicationId != null)
				cell1.setCellValue(applicationId);
			else
				cell1.setCellValue(awardData.getAwardNumber());
			cellNumber++;

			Cell cell2 = assignCell(cellNumber, tableBodyStyle, row);
			if (proposal != null && proposal.getGrantTypeCode() != null)
				cell2.setCellValue(proposal.getGrantCallType().getDescription());
			else
				cell2.setCellValue(" ");
			cellNumber++;

			Cell cell3 = assignCell(cellNumber, tableBodyStyle, row);
			if (grantCall != null && grantCall.getFundingSchemeId() != null)
				cell3.setCellValue(grantCall.getSponsorFundingScheme().getDescription());
			else
				cell3.setCellValue(" ");
			cellNumber++;

			Cell cell4 = assignCell(cellNumber, tableBodyStyle, row);
			if (awardData.getTitle() != null)
				cell4.setCellValue(awardData.getTitle());
			else
				cell4.setCellValue(" ");
			cellNumber++;

			Cell cell5 = assignCell(cellNumber, tableBodyStyle, row);
			if (awardData.getSponsor() != null)
				cell5.setCellValue(commonService.getSponsorFormatBySponsorDetail(awardData.getSponsor().getSponsorCode(), awardData.getSponsor().getSponsorName(), awardData.getSponsor().getAcronym()));
			else if (proposal != null && proposal.getSponsor() != null)
				cell5.setCellValue(commonService.getSponsorFormatBySponsorDetail(proposal.getSponsor().getSponsorCode(), proposal.getSponsor().getSponsorName(), proposal.getSponsor().getAcronym()));
			else
				cell5.setCellValue(" ");
			cellNumber++;

			
			Cell cell6 = assignCell(cellNumber, tableBodyStyle, row);
			if (awardData.getAwardPersons() != null && !awardData.getAwardPersons().isEmpty()) {
				for (AwardPerson awardPerson : awardData.getAwardPersons()) {
					if (awardPerson.getPersonRoleId().equals(3) && awardPerson.getPersonId() != null) {
						cell6.setCellValue(awardPerson.getFullName());
					}
				}
			}
			else {
				cell6.setCellValue(" ");
			}
			cellNumber++;

			Cell cell7 = assignCell(cellNumber, tableBodyStyle, row);
			int tempCellNumber = cellNumber;
			if (awardData.getAwardPersons() != null && !awardData.getAwardPersons().isEmpty()) {
				for (AwardPerson awardPerson : awardData.getAwardPersons()) {
					if (awardPerson.getPersonRoleId().equals(9) && awardPerson.getPersonId() != null) {
						cell7.setCellValue(awardPerson.getFullName());
						cellNumber++;
					}
					cell7 = assignCell(cellNumber, tableBodyStyle, row);
				}
			}
			else {
				cell7.setCellValue(" ");
			}
			cellNumber = tempCellNumber + 15;

			Cell cell8 = assignCell(cellNumber, tableBodyStyle, row);
			tempCellNumber = cellNumber;
			if (awardData.getAwardPersons() != null && !awardData.getAwardPersons().isEmpty()) {
				for (AwardPerson awardPerson : awardData.getAwardPersons()) {
					if (awardPerson.getPersonRoleId().equals(1) && awardPerson.getPersonId() != null) {
						cell8.setCellValue(awardPerson.getFullName());
						cellNumber++;
					}
					cell8 = assignCell(cellNumber, tableBodyStyle, row);
				}
			}
			else {
				cell8.setCellValue(" ");
			}
			cellNumber = tempCellNumber + 15;

			Cell cell9 = assignCell(cellNumber, tableBodyStyle, row);
			tempCellNumber = cellNumber;
			if (awardData.getAwardPersons() != null && !awardData.getAwardPersons().isEmpty()) {
				for (AwardPerson awardPerson : awardData.getAwardPersons()) {
					if (awardPerson.getPersonRoleId().equals(6) && awardPerson.getPersonId() != null) {
						cell9.setCellValue(awardPerson.getFullName());
						cellNumber++;
					}
					cell9 = assignCell(cellNumber, tableBodyStyle, row);
				}
			}
			else {
				cell9.setCellValue(" ");
			}
			cellNumber = tempCellNumber + 15;

			Cell cell10 = assignCell(cellNumber, tableBodyStyle, row);
			tempCellNumber = cellNumber;
			if (awardData.getAwardPersons() != null && !awardData.getAwardPersons().isEmpty()) {
				for (AwardPerson awardPerson : awardData.getAwardPersons()) {
					if (awardPerson.getPersonRoleId().equals(7) && awardPerson.getPersonId() != null) {
						cell10.setCellValue(awardPerson.getFullName());
						cellNumber++;
					}
					cell10 = assignCell(cellNumber, tableBodyStyle, row);
				}
			}
			else {
				cell10.setCellValue(" ");
			}
			cellNumber = tempCellNumber + 10;

			Cell cell11 = assignCell(cellNumber, tableBodyStyle, row);
			if (awardBudgetHeader != null)
				cell11.setCellValue(awardBudgetHeader.getTotalCost().toString());
			else
				cell11.setCellValue(" ");
			cellNumber++;

			Cell cell12 = assignCell(cellNumber, tableBodyStyle, row);
			if (awardData.getStatusCode().equals(Constants.AWARD_STATUS_CODE_AWARDED) || awardData.getStatusCode().equals(Constants.AWARD_STATUS_CODE_PENDING) || 
					awardData.getStatusCode().equals("4") || awardData.getStatusCode().equals(Constants.AWARD_STATUS_CODE_CLOSED)
					|| awardData.getStatusCode().equals("10"))
				cell12.setCellValue("Awarded");
			else if (awardData.getStatusCode().equals(Constants.AWARD_STATUS_CODE_WITHDRAW)) 
				cell12.setCellValue(awardData.getAwardStatus().getDescription());
			else if (awardData.getStatusCode().equals(Constants.AWARD_STATUS_CODE_RESCINDED))
				cell12.setCellValue(awardData.getAwardStatus().getDescription());
			else
				cell12.setCellValue(" ");
			cellNumber++;

			Cell cell13 = assignCell(cellNumber, tableBodyStyle, row);
			if (proposal != null && proposal.getGrantTypeCode() != null && proposal.getGrantTypeCode().equals(Constants.GRANT_CALL_TYPE_INTERNAL) && proposal.getSubmissionDate() != null)
				cell13.setCellValue(commonService.convertDateFormatBasedOnTimeZone(proposal.getSubmissionDate().getTime(),Constants.DEFAULT_DATE_FORMAT));
			else if (proposal != null && proposal.getGrantTypeCode() != null && proposal.getGrantTypeCode().equals(Constants.GRANT_CALL_TYPE_EXTERNAL) && grantCall != null && grantCall.getClosingDate() != null) {
				cell13.setCellValue(commonService.convertDateFormatBasedOnTimeZone(grantCall.getClosingDate().getTime(),Constants.DEFAULT_DATE_FORMAT));
			}
			else
				cell13.setCellValue(" ");
			cellNumber++;

			Cell cell14 = assignCell(cellNumber, tableBodyStyle, row);
			if (awardData.getStatusCode().equals(Constants.AWARD_STATUS_CODE_AWARDED) && awardData.getBeginDate() != null)
				cell14.setCellValue(commonService.convertDateFormatBasedOnTimeZone(awardData.getBeginDate().getTime(),Constants.DEFAULT_DATE_FORMAT));
			else
				cell14.setCellValue(" ");
			cellNumber++;

			Cell cell15 = assignCell(cellNumber, tableBodyStyle, row);
			if (awardData.getStatusCode().equals(Constants.AWARD_STATUS_CODE_AWARDED) && awardData.getFinalExpirationDate() != null)
				cell15.setCellValue(commonService.convertDateFormatBasedOnTimeZone(awardData.getFinalExpirationDate().getTime(),Constants.DEFAULT_DATE_FORMAT));
			else
				cell15.setCellValue(" ");
			cellNumber++;

			Cell cell16 = assignCell(cellNumber, tableBodyStyle, row);
			if (awardData.getAwardEffectiveDate() != null)
				cell16.setCellValue(commonService.convertDateFormatBasedOnTimeZone(awardData.getAwardEffectiveDate().getTime(),Constants.DEFAULT_DATE_FORMAT));
			else
				cell16.setCellValue(" ");
			cellNumber++;

			Cell cell17 = assignCell(cellNumber, tableBodyStyle, row);
			if (awardBudgetHeader != null)
				cell17.setCellValue(totalAwardGrantedAmount(awardData.getAwardId()));
			else
				cell17.setCellValue(" ");
			cellNumber++;
		}
	}

	private String totalAwardGrantedAmount(Integer awardId) {
		List<AwardCostShare> awardCostShares = datesAndAmountDao.getCostShareTypesByAwardId(awardId);
		BigDecimal totalCostShare = BigDecimal.ZERO;
		BigDecimal totalAwardGrantedAmount = BigDecimal.ZERO;
		if (awardCostShares != null && !awardCostShares.isEmpty()) {
			for (AwardCostShare awardCostShare : awardCostShares) {
				if (awardCostShare.getCommitmentAmount() != null) {
					totalCostShare = totalCostShare.add(awardCostShare.getCommitmentAmount());
				}
			}
		}
		AwardAmountInfo awardAmountInfo = awardBudgetService.fetchLatestAwardAmountInfo(awardId, awardDao.getAwardNumberBasedOnAwardId(awardId));
		if (awardAmountInfo != null && awardAmountInfo.getAnticipatedTotalAmount() != null) {
			totalAwardGrantedAmount = totalCostShare.add(awardAmountInfo.getAnticipatedTotalAmount());
		} else {
			totalAwardGrantedAmount = totalCostShare;
		}
		return totalAwardGrantedAmount.toString();
	}

	private Proposal getProposalDetailsOfAward(Integer awardId) {
		Proposal proposal = new Proposal();
		List<AwardFundingProposal> awardFundingProposals = exportDataDao.getAwardFundingProposal(awardId);
		if (awardFundingProposals != null && !awardFundingProposals.isEmpty()) {
			InstituteProposalAdminDetail adminDetail = exportDataDao.getInstituteProposalAdminDetail(awardFundingProposals.get(0).getProposalId());
			if (adminDetail != null) {
				proposal =proposalDao.fetchProposalById(adminDetail.getDevProposalId());
			}
		}
		return proposal;
	}

	@Override
	public ResponseEntity<byte[]> getResponseEntityForActiveAward(AwardDashboardVO vo, XSSFWorkbook workbook)
			throws Exception {
		byte[] byteArray = null;
		Integer count = 1;
		SimpleDateFormat simpleDateFormat = new SimpleDateFormat("yyyyMMdd");
		String currentDate = simpleDateFormat.format(new Date());
		String workingDirectory = "E:/Fibi_data/";
		String fileName = "Faculty_CV_Grants_" + currentDate + "_"+ count +".xls";
		boolean isFound = true;
		while (isFound) {
			File workingDirFile = new File(workingDirectory);
			File testfile = new File(workingDirFile, fileName);
			if (testfile.exists()) {
			   count = count + 1;
			   fileName = "Faculty_CV_Grants_" + currentDate + "_"+ count +".xls";
			 } else {
				 isFound = false;
			 }
		}
		ByteArrayOutputStream bos = new ByteArrayOutputStream();
		workbook.write(bos);
		File file = new File(workingDirectory + fileName);
		FileOutputStream fileOut =  new FileOutputStream(file);  
		workbook.write(fileOut);
		byteArray = bos.toByteArray();
		return getResponseEntityActive(byteArray);
	}

	private ResponseEntity<byte[]> getResponseEntityActive(byte[] bytes) {
		ResponseEntity<byte[]> attachmentData = null;
		try {
			HttpHeaders headers = new HttpHeaders();
			headers.setContentType(MediaType.parseMediaType("application/octet-stream"));
			headers.setContentLength(bytes.length);
			headers.setCacheControl("must-revalidate, post-check=0, pre-check=0");
			headers.setPragma("public");
			attachmentData = new ResponseEntity<byte[]>(bytes, headers, HttpStatus.OK);
		} catch (Exception e) {
			logger.error("Error in method getResponseEntity", e);
		}
		return attachmentData;
	}

	private Cell assignCell(int cellNumber, XSSFCellStyle tableBodyStyle, Row row) {
		Cell cell = row.createCell(cellNumber);
		cell.setCellStyle(tableBodyStyle);
		return cell;
	}

}
