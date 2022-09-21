package com.polus.fibicomp.grantcall.service;

import java.io.ByteArrayOutputStream;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import com.polus.fibicomp.common.service.CommonService;
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
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.polus.fibicomp.committee.dao.CommitteeDao;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.grantcall.dao.GrantCallDao;
import com.polus.fibicomp.grantcall.dao.GrantCallIOIDao;
import com.polus.fibicomp.grantcall.pojo.GrantCall;
import com.polus.fibicomp.grantcall.pojo.GrantCallIOIHeader;
import com.polus.fibicomp.grantcall.pojo.GrantCallIOIMembers;
import com.polus.fibicomp.grantcall.vo.GrantCallIOIVO;
import com.polus.fibicomp.notification.email.service.EmailService;
import com.polus.fibicomp.notification.email.vo.EmailServiceVO;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.proposal.lookup.dao.ProposalLookUpDao;
import com.polus.fibicomp.questionnaire.dto.QuestionnaireDataBus;
import com.polus.fibicomp.questionnaire.service.QuestionnaireService;

@Transactional
@Service(value = "grantCallIOIService")
public class GrantCallIOIServiceImpl implements GrantCallIOIService {
	
	protected static Logger logger = LogManager.getLogger(GrantCallIOIServiceImpl.class.getName());

	@Autowired
	private GrantCallDao grantCallDao;

	@Autowired
	private GrantCallIOIDao grantCallIOIDao;

	@Autowired
	private CommitteeDao committeeDao;

	@Autowired
	public CommonDao commonDao;

	@Autowired
	@Qualifier(value = "personDao")
	private PersonDao personDao;

	@Autowired
	private ProposalLookUpDao proposalLookUpDao;

	@Autowired
	private EmailService emailService;

	@Autowired
	private QuestionnaireService questionnaireService;

	@Autowired
	CommonService commonService;

	@Override
	public String saveOrUpdateGrantIOIDetails(GrantCallIOIVO vo) {
		GrantCallIOIHeader grantCallIOI = vo.getGrantCallIOI();
		GrantCall grantCall = grantCallDao.fetchGrantCallById(grantCallIOI.getGrantCallId());
		Integer grantIOIId = grantCallIOI.getGrantCallIOIId();
		if (!grantCall.getGrantStatusCode().equals((Constants.GRANT_CALL_STATUS_CODE_CLOSED))) {
			grantCallIOI.setUpdateUser(grantCallIOI.getUpdateUser());
			grantCallIOI.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
			grantCallIOI = grantCallIOIDao.saveOrUpdateGrantCallIOI(grantCallIOI);
			grantCallIOI.setGrantCallId(grantCall.getGrantCallId());
			vo.setGrantCallIOI(grantCallIOI);
			vo.setGrantCallIOIId(grantCallIOI.getGrantCallIOIId());
			if (grantIOIId == null) {
				vo.setMessage("IOI created successfully");
			} else {
				vo.setMessage("IOI updated successfully");
			}
		}
		if (grantCallIOI.getGrantIOIStatusCode().equals(Constants.GRANT_CALL_IOI_STATUS_CODE_SUBMITTED)) {
			sendGrantCallIOINotification(vo, Constants.GRANTCALL_IOI_NOTIFICATION_CODE, new HashSet<>());
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String deleteIOIMember(GrantCallIOIVO vo) {
		GrantCallIOIMembers grantCallIOIMember = grantCallIOIDao.fetchIOIMember(vo.getGrantIOIMemberId());
		grantCallIOIDao.deleteIOIMember(grantCallIOIMember);
		vo.setMessage("Member deleted successfully");
		return committeeDao.convertObjectToJSON(vo);
	}

	@Override
	public String deleteIOIListItem(GrantCallIOIVO vo) {
		grantCallIOIDao.deleteGrantCallIOI(grantCallIOIDao.fetchGrantIOIByIOIId(vo.getGrantCallIOIId()));
		vo.setMessage("IOI deleted successfully");
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String loadGrantCallIOIByGrantId(GrantCallIOIVO vo) {
		try {
			vo.setGrantCallIOIHeaders(grantCallIOIDao.getDashboardDataForIOI(vo.getGrantCallId(),vo.getPersonId(), vo.getTabName()));
		} catch (Exception e) {
			e.printStackTrace();
		}
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String saveOrUpdateIOIMembers(GrantCallIOIVO vo) {
		GrantCallIOIMembers grantCallIOIMember = vo.getGrantCallIOIMembers();
		grantCallIOIMember.setUpdateUser(grantCallIOIMember.getUpdateUser());
		grantCallIOIMember.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
		grantCallIOIMember = grantCallIOIDao.saveOrUpdateIOIMembers(grantCallIOIMember);
		vo.setGrantCallIOIMembers(grantCallIOIMember);
		return commonDao.convertObjectToJSON(vo);
	}

	@Override
	public String createOrEditGrantCallIOI(GrantCallIOIVO vo) {
		if (vo.getGrantCallIOIId() != null) {
			GrantCallIOIHeader grantIOIdetails = grantCallIOIDao.fetchGrantIOIByIOIId(vo.getGrantCallIOIId());
			vo.setGrantCallIOI(grantIOIdetails);
			if (grantIOIdetails.getQuestionnaireAnswerId() != null) {
				QuestionnaireDataBus questionnaireDataBus = new QuestionnaireDataBus();
				questionnaireDataBus.setQuestionnaireAnswerHeaderId(grantIOIdetails.getQuestionnaireAnswerId());
				boolean status = false;
				try {
					status = questionnaireService.isQuestionnaireComplete(questionnaireDataBus);
				} catch (Exception e) {
					e.printStackTrace();
				}
				vo.setCompleted(status);
				vo.setIsAnswered(true);
			}
			vo.setGrantCallIOIMemberList(grantCallIOIDao.fetchIOIMembersBasedOnIOIId(vo.getGrantCallIOIId()));
		}
		vo.setProposalPersonRoles(proposalLookUpDao.fetchAllProposalPersonRoles());
		vo.setGrantCallIOIQuestionnaire(grantCallDao.fetchGrantCallIOIQuestionnaireByGrantId(vo.getGrantCallId()));
		return commonDao.convertObjectToJSON(vo);
	}

	public GrantCallIOIVO sendGrantCallIOINotification(GrantCallIOIVO vo, Integer notificationTypeId, Set<NotificationRecipient> dynamicEmailRecipients) {
		EmailServiceVO emailServiceVO = new EmailServiceVO();
		emailServiceVO.setNotificationTypeId(notificationTypeId);
		emailServiceVO.setModuleCode(Constants.GRANTCALL_MODULE_CODE);
		emailServiceVO.setSubModuleCode(Constants.GRANTCALL_IOI_SUBMODULE_CODE.toString());
		emailServiceVO.setModuleItemKey(vo.getGrantCallIOI().getGrantCallId().toString());
		emailServiceVO.setSubModuleItemKey(vo.getGrantCallIOI().getGrantCallIOIId().toString());
		emailServiceVO.setRecipients(dynamicEmailRecipients);
		emailServiceVO.setPlaceHolder(getDynamicPlaceholders(vo));
		emailServiceVO = emailService.sendEmail(emailServiceVO);
		return vo;
	}

	private Map<String, String> getDynamicPlaceholders(GrantCallIOIVO vo) {
		Map<String, String> placeHolder = new HashMap<String, String>();
		Person person = personDao.getPersonDetailById(vo.getGrantCallIOI().getPrincipalInvestigatorId());
		placeHolder.put("{PRINCIPAL_INVESTIGATOR}", person.getFullName());
		return placeHolder;
	}
	
	@Override
	public ResponseEntity<byte[]> getResponseEntityForIOIDownload(GrantCallIOIVO vo, XSSFWorkbook workbook) throws Exception {
		byte[] byteArray = null;
		ByteArrayOutputStream bos = new ByteArrayOutputStream();
		workbook.write(bos);
		byteArray = bos.toByteArray();
		return getResponseEntity(byteArray);
	}
	
	private ResponseEntity<byte[]> getResponseEntity(byte[] bytes) {
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
			e.printStackTrace();
		}
		return attachmentData;
	}

	@Override
	public XSSFWorkbook getXSSFWorkbookForIOI(GrantCallIOIVO vo) throws Exception {
		XSSFWorkbook workbook = new XSSFWorkbook();
		GrantCall grantCall = grantCallDao.fetchGrantCallById(vo.getGrantCallId());
		List<GrantCallIOIHeader> grantCallIOIHeaders = grantCallIOIDao.getDashboardDataForIOI(vo.getGrantCallId(), vo.getPersonId(), vo.getTabName());
		if (grantCallIOIHeaders != null && !grantCallIOIHeaders.isEmpty()) {
			for (GrantCallIOIHeader grantCallIOIHeader : grantCallIOIHeaders) {
				grantCallIOIHeader.setGrantCallId(grantCall.getGrantCallId());
			}
		}
		XSSFSheet sheet = workbook.createSheet(vo.getDocumentHeading());
		commonService.addDetailsInHeader(workbook,sheet);
		Object[] tableHeadingRow = { "Title of Grant Call","Grant Call Abbreviation", "Project Title", "Home Unit", "PI", "Submitted Department", "Team Member-Count", "Cost Requested", "Created User" };
		prepareExcelSheetForGrantCallIOI(grantCallIOIHeaders, sheet, tableHeadingRow, workbook, vo);
		return workbook;
	}

	private void prepareExcelSheetForGrantCallIOI(List<GrantCallIOIHeader> grantCallIOIHeaders, XSSFSheet sheet, Object[] tableHeadingRow, XSSFWorkbook workbook, GrantCallIOIVO vo) {
		int headingCellNumber = 0;
		String documentHeading = vo.getDocumentHeading();
		logger.info("documentHeading : " + documentHeading);
		// Excel sheet heading style and font creation code.
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
		for (GrantCallIOIHeader grantCallIOIHeaderData : grantCallIOIHeaders) {
			Row row = sheet.createRow(rowNumber++);
			int cellNumber = 0;
			GrantCall grantCall = grantCallDao.fetchGrantCallById(grantCallIOIHeaderData.getGrantCallId());
			Cell cell1 = assignCell(cellNumber, tableBodyStyle, row);
			if (grantCall.getGrantCallName() != null)
				cell1.setCellValue(grantCall.getGrantCallName());
			else
				cell1.setCellValue(" ");
			cellNumber++;

			Cell cell2 = assignCell(cellNumber, tableBodyStyle, row);
			if (grantCall.getAbbrevation() != null)
				cell2.setCellValue(grantCall.getAbbrevation());
			else
				cell2.setCellValue(" ");
			cellNumber++;

			Cell cell3 = assignCell(cellNumber, tableBodyStyle, row);
			if (grantCallIOIHeaderData.getProjectTitle() != null)
				cell3.setCellValue(grantCallIOIHeaderData.getProjectTitle());
			else
				cell3.setCellValue(" ");
			cellNumber++;

			Cell cell4 = assignCell(cellNumber, tableBodyStyle, row);
			String unitName = commonDao.getUnitName(grantCallIOIHeaderData.getUnitNumber());
			if (unitName != null)
				cell4.setCellValue(unitName);
			else
				cell4.setCellValue(" ");
			cellNumber++;

			Cell cell5 = assignCell(cellNumber, tableBodyStyle, row);
			if (grantCallIOIHeaderData.getPiFullName() != null)
				cell5.setCellValue(grantCallIOIHeaderData.getPiFullName());
			else
				cell5.setCellValue(" ");
			cellNumber++;

			Cell cell6 = assignCell(cellNumber, tableBodyStyle, row);
			if (grantCallIOIHeaderData.getSubmittingUnitName() != null)
				cell6.setCellValue(grantCallIOIHeaderData.getSubmittingUnitName());
			else
				cell6.setCellValue(" ");
			cellNumber++;

			Cell cell7 = assignCell(cellNumber, tableBodyStyle, row);
			if (grantCallIOIHeaderData.getMemberCount() != null)
				cell7.setCellValue(grantCallIOIHeaderData.getMemberCount());
			else
				cell7.setCellValue(" ");
			cellNumber++;

			Cell cell8 = assignCell(cellNumber, tableBodyStyle, row);
			if (grantCallIOIHeaderData.getRequestedDirectCost() != null)
				cell8.setCellValue(grantCallIOIHeaderData.getRequestedDirectCost().toString());
			else
				cell8.setCellValue(" ");
			cellNumber++;

			Cell cell9 = assignCell(cellNumber, tableBodyStyle, row);
			if (grantCallIOIHeaderData.getCreateUser() != null)
				cell9.setCellValue(grantCallIOIHeaderData.getCreateUser());
			else
				cell9.setCellValue(" ");
			cellNumber++;
		}
	}

	private Cell assignCell(int cellNumber, XSSFCellStyle tableBodyStyle, Row row) {
		Cell cell = row.createCell(cellNumber);
		cell.setCellStyle(tableBodyStyle);
		return cell;
	}
}
