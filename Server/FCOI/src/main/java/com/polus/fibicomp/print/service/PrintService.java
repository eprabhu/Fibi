package com.polus.fibicomp.print.service;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.text.ParseException;
import java.util.List;

import javax.servlet.http.HttpServletResponse;
import javax.validation.Valid;

import com.polus.fibicomp.common.dto.ResponseData;
import com.polus.fibicomp.pojo.LetterTemplateType;
import org.apache.poi.xssf.usermodel.XSSFCellStyle;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.http.ResponseEntity;

import com.itextpdf.text.Document;
import com.itextpdf.text.DocumentException;
import com.itextpdf.text.Font;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.print.dto.QuestionnairePrintParameter;
import com.polus.fibicomp.print.vo.PrintVO;
import com.polus.fibicomp.vo.CommonVO;

/**
 * @author ragendhu.s
 *
 */
public interface PrintService {

	/**
	 * This method is used to generate proposal report.
	 * @param response - Object of HttpServletResponse class.
	 * @param printVO
	 */
	public void generateProposalReport(HttpServletResponse response, PrintVO printVO);

	/**
	 * This method is used to generate Budget report.
	 * @param response - Object of HttpServletResponse class.
	 * @param PrintVO - vo Object
	 */
	public ResponseEntity<byte[]> generateBudgetReport(HttpServletResponse response, PrintVO vo);

	/**
	 * This method is used to generate award pdf report.
	 * @param response - Object of HttpServletResponse class.
	 * @param awardId - award id.
	 * @param personId - person id.
	 */
	public ResponseEntity<byte[]> generateAwardReport(HttpServletResponse response, Integer awardId, String personId);

	/**
	 * This method is used to generate notify award pdf report.
	 * @param response - Object of HttpServletResponse class.
	 * @param awardId - award id.
	 * @param personId - person id.
	 */
	public ResponseEntity<byte[]> generateNotifyAwardReport(HttpServletResponse response, Integer awardId);

	/**
	 * This method is used to generate questionnaire report.
	 * @param proposalId - proposalId.
	 * @param personId - personId.
	 * @param userName - userName.
	 * @param questionnaireId - questionnaireId.
	 * @param isSingleQuestionnairePrint - isSingleQuestionnairePrint
	 * @return questionnaire report data.
	 */
	public ResponseEntity<byte[]> generateQuestionnaireReport(CommonVO vo);

	/**
	 * This method is used to get byte array of proposal print.
	 * @param data - byte array
	 * @param proposalId - proposalId.
	 * @param personId - person id.
	 * @param userName - userName
	 * @param letterTemplateType
	 */
	public byte[] setMergePlaceHoldersOfProposal(byte[] data, Integer proposalId, String personId, String userName,
												 Integer subModuleCode, String subModuleItemCode, LetterTemplateType letterTemplateType);

	public byte[] getTemplateData(String templateTypeCode);

	public byte[] mergePlaceHoldersOfNotifyAward(byte[] data, Integer awardId);
	
	public ResponseEntity<byte[]> generateAwardBudgetReport(HttpServletResponse response, PrintVO vo);

	public ResponseEntity<byte[]> generateNotifyAwardReports(HttpServletResponse response, Integer awardId, String awardNumber, Integer sequenceNumber, String updateUser);

	public byte[] mergePlaceHoldersOfNotifyAwards(byte[] data, Integer awardId);

	/**
	 * This method is used to generate budget summary excel report.
	 * @param response - HttpServletResponse object.
	 * @param proposalId - proposalId.
	 * @param budgetId - budgetId.
	 * @return budget summary report data.
	 */
	public ResponseEntity<byte[]> generateBudgetSummaryExcelReport(HttpServletResponse response, Integer proposalId, Integer budgetId);

	/**
	 * This method is used to generate the questionnaire report in pdf and excel
	 * @param response
	 * @param vo
	 * @return questionnaire report
	 */
	public ResponseEntity<byte[]> generateQuestionnaireReport(HttpServletResponse response, CommonVO vo);

	/**
	 * This method is used to set the attachment headers for download
	 * @param fileName
	 * @param data
	 * @return response entity of byte array.
	 */
	public ResponseEntity<byte[]> setAttachmentContent(String fileName, byte[] data);

	/**
	 * This method is used to generate proposal detailed budget excel report.
	 * @param response - HttpServletResponse object.
	 * @param budgetId - budgetId.
	 * @return proposal detailed budget report data.
	 */
	public ResponseEntity<byte[]> generateProposalDetailedBudgetExcelReport(HttpServletResponse response, Integer budgetId);

	/**
	 * This method is used to generate proposal simple budget excel report.
	 * @param response - HttpServletResponse object.
	 * @param budgetId - budgetId.
	 * @return proposal simple budget report data.
	 */
	public ResponseEntity<byte[]> generateProposalSimpleBudgetExcelReport(HttpServletResponse response, Integer budgetId);

	/**
	 * This method is used to generate award detailed budget excel report.
	 * @param response - HttpServletResponse object.
	 * @param budgetId - budgetId.
	 * @return proposal simple budget report data.
	 */
	public ResponseEntity<byte[]> generateAwardDetailedBudgetExcelReport(HttpServletResponse response, Integer budgetId);

	/**
	 * This method is used to generate award budget summary excel report.
	 * @param response - HttpServletResponse object.
	 * @param awardId - awardId.
	 * @param budgetId - budgetId.
	 * @return budget summary report data.
	 */
	public ResponseEntity<byte[]> generateAwardBudgetSummaryExcelReport(HttpServletResponse response, Integer awardId, Integer budgetId);

	public ByteArrayInputStream generateBudgetPDF(PrintVO vo) throws DocumentException, ParseException;

	/**
	 * This method is used to generate  excel sheet heading.
	 * @param sheet
	 * @param heading
	 * @param workbook
	 * @param columnNumber
	 * @param rowNumber
	 * @return XSSFWorkbook.
	 */
	public XSSFWorkbook prepareExcelSheetHeading(XSSFSheet sheet, String heading, XSSFWorkbook workbook, int columnNumber, int rowNumber);

	/**
	 * This method is used to generate  excel sheet heading.
	 * @param sheet
	 * @param rowNumber
	 * @param awardId
	 * @param workbook
	 * @return rowNumber.
	 */
	public Integer prepareAwardGeneralInformationForExcel(XSSFSheet sheet, int rowNumber, String awardId, XSSFWorkbook workbook, String timesheetPersonId, Boolean isAwardTimesheetPrint);

	/**
	 * This method is used to generate  excel sheet heading.
	 * @param sheet
	 * @param tableHeadingRow
	 * @param workbook
	 * @param tableBodyStyle
	 * @param rowNumber
	 * @return workbook.
	 */
	public XSSFWorkbook prepareExcelSheetHeader(XSSFSheet sheet, Object[] tableHeadingRow, XSSFWorkbook workbook, XSSFCellStyle tableBodyStyle, int rowNumber);

	/**
	 * This method is used to generate  excel sheet heading row.
	 * @param sheet
	 * @param tableBodyStyle
	 * @param workbook
	 * @param tableHeadStyle
	 * @param rowNumber
	 * @param headingName
	 */
	public void prepareHeadingRowForExcel(XSSFSheet sheet, XSSFWorkbook workbook, XSSFCellStyle tableHeadStyle, XSSFCellStyle tableBodyStyle, String headingName, int rowNumber);

	/**
	 * This method is used to generate auto size excel columns.
	 * @param workbook
	 * @param rowNumber
	 */
	public void autoSizeColumns(XSSFWorkbook workbook, int rowNumber);

	/**
	 * This method is used to generate auto size excel columns.
	 * @param workbook
	 * @param rowNumber
	 * @return XSSFCellStyle.
	 */
	public XSSFCellStyle prepareTableRowFont(XSSFWorkbook workbook);

	/**
	 * This method is used to exportCurrentAndPending.
	 * @param response - HttpServletResponse object.
	 * @param moduleCode - moduleCode.
	 * @param personId - personId.
	 * @param moduleItemKey - moduleItemKey.
	 * @return budget summary report data.
	 */
	public ResponseEntity<byte[]> exportCurrentAndPending(HttpServletResponse response, Integer moduleCode, String personId, String moduleItemKey);

	/**
	 * This method is used to generate excel report for award expense, purchase and revenue.
	 * @param vo
	 * @return
	 * @throws Exception
	 */
	public ResponseEntity<byte[]> generateAwardExpenseExcelReport(PrintVO vo) throws Exception;

	/**
	 * This method is used to generate pdf report for award expense, purchase and revenue.
	 * @param response
	 * @param vo
	 * @return
	 * @throws Exception
	 */
	public ResponseEntity<byte[]> generateAwardExpensePDFReport(HttpServletResponse response, PrintVO vo) throws Exception;

	public List<QuestionnairePrintParameter> setQuestionnaireDataBasedOnQuestionnaireId(CommonVO vo);

	/**
	 * This method is used to generate excel and pdf report for award key person timesheet
	 * @param vo
	 * @param response
	 * @return
	 * @throws Exception
	 */
	public ResponseEntity<byte[]> generateAwardKeyPersonTimesheetReport(HttpServletResponse response, CommonVO vo);

	/**
	 * This method is used to convert long string to paragraph.
	 * @param title
	 */
	public String convertStringToParagraph(String description);

	public void prepareAwardGeneralInfo(Document document, Font subHeadFont, Font bodyFont, Font bodyTextFont, Award award, Boolean true1, String awardPersonName, String awardId) throws DocumentException;

	public LetterTemplateType getFileNameByTemplateCode(String templateTypeCode);

    /**
	 * This method is used to get byte array of service Request print.
	 * @param data - byte array
	 * @param proposalId - serviceRequestId.
	 * @param personId - person id.
	 * @param userName - userName
	 * @param userName - subModuleCode
	 * @param userName - subModuleItemCode
	 */
	public byte[] setMergePlaceHoldersOfServiceRequest(byte[] data, Integer serviceRequestId, String personId, String userName, Integer subModuleCode, String subModuleItemCode) throws IOException ;

	/**
	 * This methode used to get Letter Template by module code
	 *
	 * @param moduleCode
	 * @return list of LetterTemplateType
	 */
	ResponseEntity<ResponseData> getAllLetterTemplateTypes(Integer moduleCode);

	/**
	 * This method used to generate template for Ip
	 * @param response
	 * @param printVO
	 */
	public void generateIpReport(HttpServletResponse response, @Valid PrintVO printVO);

	/**
	 * This method used to set MergePlaceHolders for Ip
	 * @param letterTemplate,instituteProposalId and data
	 */
	public byte[] setMergePlaceHoldersOfIp(byte[] data, Integer instituteProposalId, LetterTemplateType letterTemplate);
}
