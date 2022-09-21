package com.polus.fibicomp.print.service;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.nio.charset.StandardCharsets;
import java.sql.SQLException;
import java.text.DecimalFormat;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Comparator;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;
import java.util.zip.ZipOutputStream;

import javax.servlet.ServletOutputStream;
import javax.servlet.http.HttpServletResponse;
import javax.transaction.Transactional;

import com.polus.fibicomp.common.dto.ResponseData;
import com.polus.fibicomp.customdataelement.pojo.CustomDataElementUsage;
import com.polus.fibicomp.pojo.*;
import com.polus.fibicomp.print.dto.*;
import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.StringUtils;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.poi.openxml4j.opc.OPCPackage;
import org.apache.poi.openxml4j.opc.PackagePart;
import org.apache.poi.openxml4j.opc.PackagePartName;
import org.apache.poi.openxml4j.opc.PackagingURIHelper;
import org.apache.poi.ss.usermodel.BorderStyle;
import org.apache.poi.ss.usermodel.Cell;
import org.apache.poi.ss.usermodel.Header;
import org.apache.poi.ss.usermodel.HorizontalAlignment;
import org.apache.poi.ss.usermodel.Row;
import org.apache.poi.ss.usermodel.Workbook;
import org.apache.poi.ss.util.CellRangeAddress;
import org.apache.poi.ss.util.RegionUtil;
import org.apache.poi.xssf.usermodel.XSSFCellStyle;
import org.apache.poi.xssf.usermodel.XSSFFont;
import org.apache.poi.xssf.usermodel.XSSFPictureData;
import org.apache.poi.xssf.usermodel.XSSFRelation;
import org.apache.poi.xssf.usermodel.XSSFSheet;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.core.io.ClassPathResource;
import org.springframework.core.io.Resource;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.FileCopyUtils;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.itextpdf.text.BaseColor;
import com.itextpdf.text.Document;
import com.itextpdf.text.DocumentException;
import com.itextpdf.text.Font;
import com.itextpdf.text.FontFactory;
import com.itextpdf.text.PageSize;
import com.itextpdf.text.Paragraph;
import com.itextpdf.text.Phrase;
import com.itextpdf.text.pdf.PdfPCell;
import com.itextpdf.text.pdf.PdfPTable;
import com.itextpdf.text.pdf.PdfWriter;
import com.polus.fibicomp.applicationexception.dto.ApplicationException;
import com.polus.fibicomp.award.comparator.AwardBudgetSummaryComparatorByBudgetCategoryName;
import com.polus.fibicomp.award.dao.AwardDao;
import com.polus.fibicomp.award.datesandamounts.dao.DatesAndAmountDao;
import com.polus.fibicomp.award.datesandamounts.service.DatesAndAmountService;
import com.polus.fibicomp.award.expense.pojo.AwardExpenseTransaction;
import com.polus.fibicomp.award.expense.service.AwardExpenseService;
import com.polus.fibicomp.award.pojo.AccountType;
import com.polus.fibicomp.award.pojo.Award;
import com.polus.fibicomp.award.pojo.AwardAmountInfo;
import com.polus.fibicomp.award.pojo.AwardAttachment;
import com.polus.fibicomp.award.pojo.AwardContact;
import com.polus.fibicomp.award.pojo.AwardContactType;
import com.polus.fibicomp.award.pojo.AwardCostShare;
import com.polus.fibicomp.award.pojo.AwardFundingProposal;
import com.polus.fibicomp.award.pojo.AwardKPI;
import com.polus.fibicomp.award.pojo.AwardKPICriteria;
import com.polus.fibicomp.award.pojo.AwardKeyPersonTimesheet;
import com.polus.fibicomp.award.pojo.AwardKeyword;
import com.polus.fibicomp.award.pojo.AwardMileStone;
import com.polus.fibicomp.award.pojo.AwardPerson;
import com.polus.fibicomp.award.pojo.AwardPersonUnit;
import com.polus.fibicomp.award.pojo.AwardProjectTeam;
import com.polus.fibicomp.award.pojo.AwardResearchArea;
import com.polus.fibicomp.award.pojo.AwardSpecialReview;
import com.polus.fibicomp.award.pojo.AwardSubContract;
import com.polus.fibicomp.award.pojo.AwardType;
import com.polus.fibicomp.award.pojo.CostShareType;
import com.polus.fibicomp.award.revenue.dao.AwardRevenueDao;
import com.polus.fibicomp.award.revenue.pojo.AwardRevenueTransactions;
import com.polus.fibicomp.award.service.AwardService;
import com.polus.fibicomp.budget.dao.AwardBudgetDao;
import com.polus.fibicomp.budget.dao.BudgetDao;
import com.polus.fibicomp.budget.pojo.AwardBudgetDetail;
import com.polus.fibicomp.budget.pojo.AwardBudgetFundType;
import com.polus.fibicomp.budget.pojo.AwardBudgetHeader;
import com.polus.fibicomp.budget.pojo.AwardBudgetNonPersonDetail;
import com.polus.fibicomp.budget.pojo.AwardBudgetPeriod;
import com.polus.fibicomp.budget.pojo.AwardBudgetPerson;
import com.polus.fibicomp.budget.pojo.AwardBudgetPersonalDetail;
import com.polus.fibicomp.budget.pojo.BudgetCategory;
import com.polus.fibicomp.budget.pojo.BudgetDetail;
import com.polus.fibicomp.budget.pojo.BudgetHeader;
import com.polus.fibicomp.budget.pojo.BudgetPeriod;
import com.polus.fibicomp.budget.pojo.BudgetPerson;
import com.polus.fibicomp.budget.pojo.BudgetPersonalDetails;
import com.polus.fibicomp.budget.service.AwardBudgetService;
import com.polus.fibicomp.budget.service.BudgetService;
import com.polus.fibicomp.budget.vo.AwardBudgetPeriodSummary;
import com.polus.fibicomp.budget.vo.AwardBudgetSummaryVO;
import com.polus.fibicomp.budget.vo.BudgetPeriodSummary;
import com.polus.fibicomp.budget.vo.BudgetSummaryVO;
import com.polus.fibicomp.budget.vo.PeriodCost;
import com.polus.fibicomp.budget.vo.SimpleBudgetLineItemVO;
import com.polus.fibicomp.budget.vo.SimpleBudgetVO;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.compilance.pojo.ProposalSpecialReview;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.currentandpending.dto.CPReportProjectDetailDTO;
import com.polus.fibicomp.currentandpending.dto.CurrentAndPendingModuleDTO;
import com.polus.fibicomp.currentandpending.dto.CurrentAndPendingModulePrintDTO;
import com.polus.fibicomp.currentandpending.dto.CurrentAndPendingPersonDTO;
import com.polus.fibicomp.currentandpending.service.CurrentAndPendingService;
import com.polus.fibicomp.currentandpending.vo.CurrentAndPendingVO;
import com.polus.fibicomp.customdataelement.dao.CustomDataElementDao;
import com.polus.fibicomp.customdataelement.pojo.CustomData;
import com.polus.fibicomp.customdataelement.pojo.CustomDataElements;
import com.polus.fibicomp.dashboard.service.DashboardService;
import com.polus.fibicomp.dbengine.DBEngine;
import com.polus.fibicomp.dbengine.Parameter;
import com.polus.fibicomp.general.dao.GeneralInformationDao;
import com.polus.fibicomp.grantcall.dao.GrantCallDao;
import com.polus.fibicomp.grantcall.pojo.GrantCall;
import com.polus.fibicomp.ip.dao.InstitutionalProposalDao;
import com.polus.fibicomp.ip.pojo.InstituteProposal;
import com.polus.fibicomp.ip.pojo.InstituteProposalBudgetHeader;
import com.polus.fibicomp.ip.pojo.InstituteProposalBudgetPeriod;
import com.polus.fibicomp.ip.pojo.InstituteProposalComment;
import com.polus.fibicomp.ip.pojo.InstituteProposalPerson;
import com.polus.fibicomp.ip.pojo.InstituteProposalPersonUnit;
import com.polus.fibicomp.ip.pojo.InstituteProposalResearchArea;
import com.polus.fibicomp.ip.pojo.InstituteProposalSpecialReview;
import com.polus.fibicomp.ip.pojo.InstituteProposalStatus;
import com.polus.fibicomp.ip.service.InstitutionalProposalService;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.person.pojo.Person;
import com.polus.fibicomp.person.timesheetdetail.service.TimesheetService;
import com.polus.fibicomp.person.timesheetdetail.vo.TimesheetVO;
import com.polus.fibicomp.person.vo.PersonVO;
import com.polus.fibicomp.persontraining.dao.PersonTrainingDao;
import com.polus.fibicomp.persontraining.pojo.PersonTraining;
import com.polus.fibicomp.print.agreement.service.AgreementPrintService;
import com.polus.fibicomp.print.vo.PrintVO;
import com.polus.fibicomp.progressreport.dao.ProgressReportDao;
import com.polus.fibicomp.progressreport.pojo.AwardProgressReport;
import com.polus.fibicomp.proposal.comparator.BudgetSummaryComparatorByBudgetCategoryName;
import com.polus.fibicomp.proposal.comparator.SimpleBudgetDetailComparatorByBudgetCategoryCode;
import com.polus.fibicomp.proposal.comparator.SimpleBudgetDetailComparatorBySystemGenerated;
import com.polus.fibicomp.proposal.dao.ProposalDao;
import com.polus.fibicomp.proposal.module.dao.ProposalModuleDao;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.proposal.pojo.ProposalComment;
import com.polus.fibicomp.proposal.pojo.ProposalKPI;
import com.polus.fibicomp.proposal.pojo.ProposalKPICriteria;
import com.polus.fibicomp.proposal.pojo.ProposalKeyword;
import com.polus.fibicomp.proposal.pojo.ProposalMileStone;
import com.polus.fibicomp.proposal.pojo.ProposalPerson;
import com.polus.fibicomp.proposal.pojo.ProposalPersonUnit;
import com.polus.fibicomp.proposal.pojo.ProposalProjectTeam;
import com.polus.fibicomp.proposal.pojo.ProposalResearchArea;
import com.polus.fibicomp.proposal.pojo.ProposalSponsor;
import com.polus.fibicomp.proposal.pojo.ProposalOrganization;
import com.polus.fibicomp.proposal.service.ProposalService;
import com.polus.fibicomp.questionnaire.dto.QuestionnaireDataBus;
import com.polus.fibicomp.questionnaire.service.QuestionnaireService;
import com.polus.fibicomp.report.vo.VmlDrawing;
import com.polus.fibicomp.roles.pojo.PersonRoles;
import com.polus.fibicomp.rolodex.dao.RolodexDao;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.servicerequest.dao.ServiceRequestDao;
import com.polus.fibicomp.servicerequest.pojo.ServiceRequest;
import com.polus.fibicomp.utils.QueryBuilder;
import com.polus.fibicomp.vo.CommonVO;
import com.polus.fibicomp.vo.LookUp;

import fr.opensagres.xdocreport.converter.ConverterTypeTo;
import fr.opensagres.xdocreport.converter.ConverterTypeVia;
import fr.opensagres.xdocreport.converter.Options;
import fr.opensagres.xdocreport.core.document.SyntaxKind;
import fr.opensagres.xdocreport.core.io.internal.ByteArrayOutputStream;
import fr.opensagres.xdocreport.document.IXDocReport;
import fr.opensagres.xdocreport.document.registry.XDocReportRegistry;
import fr.opensagres.xdocreport.template.IContext;
import fr.opensagres.xdocreport.template.TemplateEngineKind;
import fr.opensagres.xdocreport.template.formatter.FieldsMetadata;

@Transactional
@Service(value = "printService")
public class PrintServiceImpl implements PrintService {

	protected static Logger logger = LogManager.getLogger(PrintServiceImpl.class.getName());

	@Autowired
	@Qualifier(value = "proposalDao")
	private ProposalDao proposalDao;

	@Autowired
	private GrantCallDao grantCallDao;

	@Autowired
	private QuestionnaireService questionnaireService;

	@Autowired
	private DBEngine dbEngine;

	@Autowired
	private ProposalModuleDao proposalModuleDao;

	@Autowired
	private BudgetDao budgetDao;

	@Autowired
	private AwardBudgetDao awardBudgetDao;

	@Autowired
	private CommonService commonService;

	@Autowired
	private AwardDao awardDao;

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private DashboardService dashboardService;

	@Autowired
	private AwardService awardService;

	@Autowired
	private PersonDao personDao;

	@Autowired
	private DatesAndAmountDao datesAndAmountDao;

	@Autowired
	private AwardBudgetService awardBudgetService;

	@Autowired
	private BudgetDao proposalBudgetDao;

	@Autowired
	private RolodexDao rolodexDao;

	@Autowired
	private CustomDataElementDao customDataElementDao;

	@Autowired
	private BudgetService budgetService;

	@Autowired
	private CurrentAndPendingService currentAndPendingService;

	@Autowired
	private AwardRevenueDao awardRevenueDao;

	@Autowired
	private AwardExpenseService awardExpenseService;

	@Autowired
	private DatesAndAmountService datesAndAmountService;

	@Autowired
	private ProgressReportDao progressReportDao;

    @Autowired
    private TimesheetService timesheetService;

    @Autowired
    private AgreementPrintService agreementPrintService;

    @Autowired
    private ServiceRequestDao serviceRequestDao;

    @Autowired
	private ProposalService proposalService;

    @Autowired
	private InstitutionalProposalService institutionalProposalService;

    @Autowired
	private PersonTrainingDao personTrainingDao;

	@Autowired
	private InstitutionalProposalDao institutionalProposalDao;

	@Autowired
	private GeneralInformationDao generalInformationDao;

	private static final String PUBLIC = "public";
	private static final String RESULT = "Result";
	private static final String CONTENT_TYPE = "application/octet-stream";
	private static final String CACHE_CONTROL = "must-revalidate, post-check=0, pre-check=0";
	private static final String CONTENT_DISPOSITION = "Content-Disposition";
	private static final String ATTACHMENT_FILENAME = "attachment; filename=\"";
	private static final String POJECT_TEAM_DETAILS = "projectTeamDetails";
	private static final String EMPLOYEE = "Employee";
	private static final String NON_EMPLOYEE = "Non Employee";
	private static final String ANSWER_TYPE = "ANSWER_TYPE";
	private static final String QUESTION = "QUESTION";
	private static final String TITLE = "TITLE";
	private static final String ACTIVITY_TYPE = "ACTIVITY_TYPE";
	private static final String AWARD_ID = "AWARD_ID";
	private static final String AWARD_NUMBER = "AWARD_NUMBER";
	DecimalFormat decimalFormat = new DecimalFormat(Constants.NUMBER_FORMAT_WITH_DECIMAL);
	private static final Boolean IS_NOT_SYSTEM_DENERATED_COST_ELEMENT = false;
	DecimalFormat numberFormat = new DecimalFormat(Constants.NUMBER_FORMAT_WITHOUT_DECIMAL);
	//private static final String EXPENSE_FROM_SAP = "Committed Expense from SAP";
	//private static final String EXPENSE_FROM_NON_SAP = "Manually entered committed items";
	private static final String BUDGET_STATUS = "Budget Status";
	private static final String DIRECT_COST = "Direct Cost";
	private static final String OVER_HEAD_RATE_TYPE = "Over Head Rate Type";
	private static final String INDIRECT_COST = "Indirect Cost";
	private static final String MODIFIED_DIRECT_COST = "Modified Direct Cost";
	private static final String TOTAL_IN_KIND = "Total In-Kind";
	private static final String TOTAL_COST = "Total Cost";
	private static final String DESCRIPTION = "Description";
	private static final String TOTAL_REQUESTED_COST = "Total Requested Cost";
	private static final String BUDGET_SUMMARY = "Budget Summary";
	private static final String GENERAL_PROPOSAL_INFORMATION = "General Proposal Information";
	private static final String CATEGORY = "Category";
	private static final String GRAND_TOTAL = "Grand Total";
	private static final String ANS_PERSON_FULL_NAME = "ANS_PERSON_FULL_NAME";
	private static final String DEAN_SALUTATION = "Dean, ";
	private static final String ORTT_SALUTATION = "Director, ";
	private static final String ORTT_SCHOOL = "ORTT";
	private static final String SR_ID ="SR_ID";
	private static final String DOCX = "docx";
	private static final String START_DATE = "START_DATE";
	private static final String ABSTRACT = "ABSTRACT";
	private static final String END_DATE = "END_DATE";
	private static final String SUBMISSION_DATE = "SUBMISSION_DATE";
	private static final String INTERNAL_DEAD_LINE_DATE = "INTERNAL_DEAD_LINE_DATE";
	private static final String SPONSOR_DEAD_LINE_DATE = "SPONSOR_DEAD_LINE_DATE";
	private static final String SPONSOR_PROPOSAL_NUMBER = "SPONSOR_PROPOSAL_NUMBER";
	private static final String LEAD_UNIT = "LEAD_UNIT";
	private static final String GRANT_CALL_TYPE = "GRANT_CALL_TYPE";
	private static final String SPONSOR_NAME = "SPONSOR_NAME";
	private static final String PRIME_SPONSOR = "PRIME_SPONSOR";
	private static final String FUNDING_OPPORTUNITY_NUMBER = "FUNDING_OPPORTUNITY_NUMBER";
	private static final String DURATION = "DURATION";
	private static final String AWARD_TYPE = "AWARD_TYPE";
	private static final String RESEARCH_DESCRIPTION = "RESEARCH_DESCRIPTION";
	private static final String MULTI_DISCIPLINARY_DESCRIPTION = "MULTI_DISCIPLINARY_DESCRIPTION";
	private static final String MASTER_PROPOSAL ="MASTER_PROPOSAL";
	private static final String COMPLETE ="Complete";
	private static final String INCOMPLETE ="Incomplete";
	
	@Override
	public void generateProposalReport(HttpServletResponse response, PrintVO printVO) {
		try {
			if (printVO.getLetterTemplateTypeCodes().size() == 1) {
				generateProposalReport(response, printVO.getLetterTemplateTypeCodes().get(0), printVO.getProposalId(), printVO.getPersonId(), printVO.getUserName());
			} else {
				generateProposalReport(response, printVO.getLetterTemplateTypeCodes(), printVO.getProposalId(), printVO.getPersonId(), printVO.getUserName());
			}
		} catch (Exception e) {
			logger.error("Exception in generateProposalReport : {}", e.getMessage());
		}
	}

	private void generateProposalReport(HttpServletResponse response, String templateTypeCode, Integer proposalId,
										String personId, String userName) throws SQLException, IOException {
		LetterTemplateType letterTemplate = commonDao.getLetterTemplate(templateTypeCode);
		byte[] bFile = letterTemplate.getCorrespondenceTemplate().getBytes(1l, (int)letterTemplate.getCorrespondenceTemplate().length());
		byte[] mergedOutput = setMergePlaceHoldersOfProposal(bFile, proposalId, personId, userName, null, null, letterTemplate);
		String generatedFileName = RESULT + System.nanoTime() + "." +letterTemplate.getPrintFileType();
		HttpHeaders headers = new HttpHeaders();
		headers.setContentType(MediaType.parseMediaType(CONTENT_TYPE));
		headers.setContentDispositionFormData(generatedFileName, generatedFileName);
		headers.setContentLength(mergedOutput.length);
		headers.setCacheControl(CACHE_CONTROL);
		headers.setPragma(PUBLIC);
		response.setCharacterEncoding(StandardCharsets.UTF_8.name());
		response.setContentType(CONTENT_TYPE);
		response.setContentLength(mergedOutput.length);
		response.setHeader(CONTENT_DISPOSITION, ATTACHMENT_FILENAME + generatedFileName + "\"");
		FileCopyUtils.copy(mergedOutput, response.getOutputStream());
	}

	private void generateProposalReport(HttpServletResponse response, List<String> templateTypeCodes, Integer proposalId,
										String personId, String userName) throws SQLException, IOException {
		String fileName = "PROPOSAL_#" + proposalId + System.nanoTime();
		response.setContentType("application/zip");
		response.setHeader("Content-Disposition", "attachment;filename=\"" + fileName + ".zip" + "\"");
		java.io.ByteArrayOutputStream baos = new java.io.ByteArrayOutputStream();
		ZipOutputStream zos = new ZipOutputStream(baos);
		for (String templateTypeCode : templateTypeCodes) {
			LetterTemplateType letterTemplate = commonDao.getLetterTemplate(templateTypeCode);
			byte[] bFile = letterTemplate.getCorrespondenceTemplate().getBytes(1l, (int)letterTemplate.getCorrespondenceTemplate().length());
			byte[] mergedOutput = setMergePlaceHoldersOfProposal(bFile, proposalId, personId, userName, null, null, letterTemplate);
			zos.putNextEntry(new ZipEntry("Proposal_"+ templateTypeCode + "_" + System.nanoTime() + "." +letterTemplate.getPrintFileType()));
			zos.write(mergedOutput);
		}
		zos.closeEntry();
		zos.flush();
		baos.flush();
		zos.close();
		baos.close();
		ServletOutputStream op = response.getOutputStream();
		op.write(baos.toByteArray());
		op.flush();
		op.close();
	}

	public ResponseEntity<byte[]> generateBudgetReport(HttpServletResponse response, PrintVO vo) {
		ResponseEntity<byte[]> attachmentData = null;
		try {
			ByteArrayInputStream bis = generateBudgetPDF(vo);
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
			logger.error("Exception in generateBudgetReport : {}", e.getMessage());
		}
		return attachmentData;
	}

	@Override
	public byte[] getTemplateData(String templateTypeCode) {
		byte[] data = null;
		try {
			data = getLetterTemplate(templateTypeCode);
		} catch (Exception e) {
			logger.error("Exception in getTemplateData : {}", e.getMessage());
		}
		return data;
	}

	private byte[] getLetterTemplate(String templateTypeCode) {
		byte[] data = null;
		try {
			String query = QueryBuilder.selectLetterTemplate(templateTypeCode);
			ArrayList<HashMap<String, Object>> dataList = dbEngine.executeQuerySQL(new ArrayList<Parameter>(), query);
			if (!dataList.isEmpty()) {
				java.io.ByteArrayOutputStream baos = (java.io.ByteArrayOutputStream) dataList.get(0)
						.get("CORRESPONDENCE_TEMPLATE");
				data = baos.toByteArray();
			}
		} catch (Exception e) {
			logger.error("Exception in getLetterTemplate : {}", e.getMessage());
		}
		return data;
	}

	@Override
	public byte[] setMergePlaceHoldersOfProposal(byte[] data, Integer proposalId, String personId, String userName, Integer subModuleCode, String subModuleItemCode, LetterTemplateType letterTemplate) {
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		try {
			InputStream myInputStream = new ByteArrayInputStream(data);
			IXDocReport report = XDocReportRegistry.getRegistry().loadReport(myInputStream,
					TemplateEngineKind.Velocity);
			FieldsMetadata fieldsMetadata = report.createFieldsMetadata();
			Proposal proposal = proposalDao.fetchProposalById(proposalId);
			proposal.setProposalPersons(proposalModuleDao.fetchProposalPersonBasedOnProposalId(proposalId));

			List<ProposalPerson> proposalPersons = proposalModuleDao.fetchProposalPersonBasedOnProposalId(proposalId);
			fieldsMetadata.load("proposalPersonList", ProposalPrintParameter.class, true);
			List<ProposalPrintParameter> proposalPrintPersons = setProposalPersonDetails(proposalPersons);

			List<ProposalProjectTeam> proposalProjectTeams = proposalModuleDao
					.fetchProposalProjectTeamBasedOnProposalId(proposal.getProposalId());
			fieldsMetadata.load(POJECT_TEAM_DETAILS, ProposalPrintParameter.class, true);
			List<ProposalPrintParameter> projectTeamDetails = setProposalProjectDetails(proposalProjectTeams);

			fieldsMetadata.load("proposalResearchAreas", ProposalPrintParameter.class, true);
			List<ProposalPrintParameter> proposalResearchAreas = setProposalResearchAreaDetails(proposal);

			List<ProposalSponsor> proposalFundDetails = proposalModuleDao
					.fetchProposalSponsorBasedOnProposalId(proposal.getProposalId());
			fieldsMetadata.load("proposalFundDetails", ProposalPrintParameter.class, true);
			List<ProposalPrintParameter> proposalPrintFundDetails = setProposalFundDetails(proposalFundDetails);

			List<ProposalSpecialReview> proposalSpecialReviews = proposalModuleDao
					.fetchProposalSpecialReviewBasedOnProposalId(proposal.getProposalId());
			fieldsMetadata.load("ProposalSpecialReviews", ProposalPrintParameter.class, true);
			List<ProposalPrintParameter> proposalPrintSpecialReviews = setProposalSpecialReviewDetails(
					proposalSpecialReviews);

			fieldsMetadata.load("proposalCertification", ProposalCertificationPrintParameter.class, true);
			List<ProposalCertificationPrintParameter> proposalCertificationQuestionnaires = prepareProposalCertification(userName, personId, proposalId, subModuleCode, subModuleItemCode);

			fieldsMetadata.load("questionnaires", QuestionnairePrintParameter.class, true);
			List<QuestionnairePrintParameter> questionnaires = setQuestionnaireDetails(userName, personId,
					Constants.DEV_PROPOSAL_MODULE_CODE, proposalId, subModuleCode == null ? Constants.DEV_PROPOSAL_SUBMODULE_CODE : subModuleCode,
							subModuleItemCode == null || subModuleItemCode.isEmpty() ? "" : subModuleItemCode);

			List<ProposalMileStone> proposalMilestones = proposalModuleDao
					.fetchProposalMileStonesBasedOnProposalId(proposal.getProposalId());
			fieldsMetadata.load("proposalMilestones", ProposalPrintParameter.class, true);
			List<ProposalPrintParameter> proposalPrintMilestones = setProposalMilestone(proposalMilestones);

			List<ProposalKPI> proposalKPIs = proposalModuleDao
					.fetchProposalKPIBasedOnProposalId(proposal.getProposalId());
			fieldsMetadata.load("proposalKPIs", ProposalKPIPrintParameter.class, true);
			List<ProposalKPIPrintParameter> proposalPrintKPIs = setproposalKPI(proposalKPIs);

			CurrentAndPendingVO currentAndPendingVO = new CurrentAndPendingVO();
			currentAndPendingVO.setModuleCode(Constants.DEV_PROPOSAL_MODULE_CODE);
			currentAndPendingVO.setModuleItemKey(proposalId.toString());
			List<CurrentAndPendingPersonDTO> currentAndPendingDtos = currentAndPendingService
					.preparePersonCurrentAndPendingDetails(currentAndPendingVO);
			List<CurrentAndPendingPersonDTO> currentAndPendingPersonDTOs = setCurrentAndPendingPrint(
					currentAndPendingDtos);
			fieldsMetadata.load("currentAndPendingPersonDTO", CurrentAndPendingPersonDTO.class, true);

			List<ProposalProjectTeam> proposalProjectTeamDetail = proposalModuleDao
					.fetchProposalProjectTeamBasedOnProposalId(proposal.getProposalId());
			fieldsMetadata.load("proposalProjectTeam", ProposalPrintParameter.class, true);
			List<ProposalPrintParameter> proposalProjectTeam = setProposalProjectTeamDetails(
					proposalProjectTeamDetail);

			List<ProposalComment> proposalCommentDetail = proposalService.prepareProposalComment(proposal.getProposalId());
			List<ProposalPrintParameter>  proposalComments = setProposalComments(proposalCommentDetail);
			fieldsMetadata.load("proposalComment", ProposalPrintParameter.class, true);

			fieldsMetadata.load("customData", CustomDataPrintParameter.class, true);
			List<CustomDataPrintParameter> customData = setCustomDataPrintDetails(proposal.getProposalId().toString(),
					Constants.DEV_PROPOSAL_MODULE_CODE);

			fieldsMetadata.load("proposalOrganization", ProposalOrganization.class, true);
			List<ProposalOrganization> proposalOrganization = loadProposalOrganization(proposal.getProposalId());

			List<BudgetHeader> budgetHeaders = budgetDao.fetchBudgetsByProposalId(proposalId);
			BudgetHeader budgetHeader = findBudgetHeader(budgetHeaders);

			IContext context = report.createContext();
			context.put("proposalPersonList", proposalPrintPersons);
			context.put(POJECT_TEAM_DETAILS, projectTeamDetails);
			context.put("proposalResearchAreas", proposalResearchAreas);
			context.put("proposalFundDetails", proposalPrintFundDetails);
			context.put("ProposalSpecialReviews", proposalPrintSpecialReviews);
			context.put("questionnaires", questionnaires);
			context.put("proposalMilestones", proposalPrintMilestones);
			context.put("proposalKPIs", proposalPrintKPIs);
			context.put("currentAndPendingPersonDTO", currentAndPendingPersonDTOs);
			context.put("proposalProjectTeam", proposalProjectTeam);
			context.put("proposalCertification", proposalCertificationQuestionnaires);
			context.put("proposalComment", proposalComments);
			context.put("customData", customData);
			context.put("proposalOrganization", proposalOrganization);
			context = setProposalPlaceHolderData(context, proposal, fieldsMetadata, budgetHeader);

			if (letterTemplate != null && letterTemplate.getPrintFileType().equals(DOCX)) {
				report.process(context, baos);
			} else {
				Options options = Options.getTo(ConverterTypeTo.PDF).via(ConverterTypeVia.XWPF);
				report.convert(context, options, baos);
			}
		} catch (Exception e) {
			logger.error("Exception in mergePlaceHolders : {}", e.getMessage());
		}
		return baos.toByteArray();
	}

	private List<ProposalPrintParameter> setProposalComments(List<ProposalComment> proposalCommentDetail) {
		List<ProposalPrintParameter> proposalCommentDetails = new ArrayList<>();
		proposalCommentDetail.stream().collect(Collectors.groupingBy(ProposalComment::getCommentTypeCode)).entrySet().forEach(proposalComment -> {
            ProposalPrintParameter proposalCommentPrint = new ProposalPrintParameter();
            List<String> comments = new ArrayList<>();
            proposalComment.getValue().stream().forEach(comment -> {
				 String commentData = Boolean.TRUE.equals(comment.getIsPrivate()) ? 
							comment.getComment().concat(" (Private)").concat("  Commented by ").concat(comment.getFullName()) : 
							comment.getComment().concat("  Commented by ").concat(comment.getFullName());
				proposalCommentPrint.setCommentType(comment.getCommentType().getDescription());
				comments.add(commentData);
				});
			proposalCommentPrint.setProposalComments(comments);
			proposalCommentDetails.add(proposalCommentPrint);
		});
		return proposalCommentDetails;
	}

	private List<ProposalCertificationPrintParameter> prepareProposalCertification(String userName, String personId, Integer proposalId, Integer subModuleCode, String subModuleItemCode) {
		List<ProposalCertificationPrintParameter> responseData = new ArrayList<>();
		List<ProposalPerson> proposalPersons = proposalDao.proposalPersonsForCertification(proposalId);
		proposalPersons.forEach(person-> {
			List<QuestionnairePrintParameter> certificationQuestionaire = setQuestionnaireDetails(userName, personId,
					Constants.DEV_PROPOSAL_MODULE_CODE, proposalId, subModuleCode == null ? Constants.DEV_PROPOSAL_PERSON_CERTIFICATION_SUBMODULE_CODE : subModuleCode,
							subModuleItemCode == null || subModuleItemCode.isEmpty() ? person.getPersonId() : "");
			ProposalCertificationPrintParameter certificationPrintParameter = new ProposalCertificationPrintParameter();
			certificationPrintParameter.setCertificationQuestionaire(certificationQuestionaire);
			String projectRole = Boolean.TRUE.equals(person.getProposalPersonRole().getShowProjectRole()) && person.getProjectRole() != null ? " (" + person.getProjectRole() + ")" : "";
			certificationPrintParameter.setPersonName(person.getFullName() != null ? person.getFullName().concat("(")
					.concat(person.getProposalPersonRole().getDescription().concat(projectRole)).concat(")") : "");
			responseData.add(certificationPrintParameter);
		});
		return responseData;
	}

	private List<ProposalPrintParameter> setProposalProjectTeamDetails(List<ProposalProjectTeam> proposalProjectTeams) {
		List<ProposalPrintParameter> proposalProjectTeamDetail = new ArrayList<>();
		if (!proposalProjectTeams.isEmpty()) {
			proposalProjectTeams.stream().forEach(proposalProjectTeam -> {
				String fullName = (proposalProjectTeam.getFullName() != null ? proposalProjectTeam.getFullName() + "" : "");
                String projectRole = (proposalProjectTeam.getProjectRole() != null ? proposalProjectTeam.getProjectRole() + "" : "");
                String designation = (proposalProjectTeam.getDesignation() != null ? proposalProjectTeam.getDesignation() + "" : "");
				String percentageCharged = (proposalProjectTeam.getPercentageCharged() != null ? proposalProjectTeam.getPercentageCharged() + "" : "");
				String startDate = (proposalProjectTeam.getStartDate() != null ? commonService.convertDateFormatBasedOnTimeZone(proposalProjectTeam.getStartDate().getTime(), Constants.DEFAULT_DATE_FORMAT) : "");
				String endDate = (proposalProjectTeam.getEndDate() != null ? commonService.convertDateFormatBasedOnTimeZone(proposalProjectTeam.getEndDate().getTime(), Constants.DEFAULT_DATE_FORMAT) : "");
				ProposalPrintParameter proposalPrintParameters = setproposalProjectTeam(fullName, projectRole, designation, percentageCharged, startDate, endDate);
				proposalProjectTeamDetail.add(proposalPrintParameters);
			});
		} else {
			proposalProjectTeamDetail.add(setproposalProjectTeam("", "", "", "", "", ""));
		}
		return proposalProjectTeamDetail;
	}

	private ProposalPrintParameter setproposalProjectTeam(String fullName, String projectRole, String designation, String percentageCharged, String startDate, String endDate) {
		ProposalPrintParameter proposalPrintParameters = new ProposalPrintParameter();
		proposalPrintParameters.setFullName(fullName);
		proposalPrintParameters.setProjectRole(projectRole);
		proposalPrintParameters.setDesignation(designation);
		proposalPrintParameters.setPercentageCharged(percentageCharged);
		if (!startDate.isEmpty() && !endDate.isEmpty()) {
            proposalPrintParameters.setStartDate(startDate + " to " + endDate);
		} else {
			proposalPrintParameters.setStartDate(startDate);
		}
		proposalPrintParameters.setEndDate(endDate);
		return proposalPrintParameters;
	}

	private List<ProposalPrintParameter> setProposalPersonDetails(List<ProposalPerson> proposalPersons) {
		List<ProposalPrintParameter> persons = new ArrayList<>();
		if (!proposalPersons.isEmpty()) {
			for (ProposalPerson proposalPerson : proposalPersons) {
				StringBuilder units = new StringBuilder(0);
				if (proposalPerson.getDepartment() != null && !proposalPerson.getDepartment().equals("")) {
					units.append(proposalPerson.getDepartment());
				}
				if (!proposalPerson.getUnits().isEmpty()) {
					for (ProposalPersonUnit unit : proposalPerson.getUnits()) {
						if (proposalPerson.getUnits().indexOf(unit) != 0 || proposalPerson.getDepartment() != null && !proposalPerson.getDepartment().equals("")) {
							units.append(", " + unit.getUnit().getUnitDetail());
						} else {
							units.append(unit.getUnit().getUnitDetail());
						}
					}
				}
				String percentageOfEffort = "";
				if (proposalPerson.getPercentageOfEffort() != null) {
					percentageOfEffort = percentageOfEffort + proposalPerson.getPercentageOfEffort();
				}
				String personType = "Employee";
				String organisation = "";
				if (proposalPerson.getRolodexId() != null) {
					personType = "Non Employee";
					Rolodex rolodex = rolodexDao.getRolodexDetailById(proposalPerson.getRolodexId());
					organisation = rolodex.getOrganization() == null ? "" : rolodex.getOrganizations().getOrganizationName();
				}
				ProposalPrintParameter proposalPrintParameters = setProposalPersonDetails(proposalPerson, percentageOfEffort, units.toString(), personType, organisation);
				persons.add(proposalPrintParameters);
			}
		} else {
			ProposalPrintParameter proposalPrintParameters = setProposalPersonDetails(null, "", "", "", "");
			persons.add(proposalPrintParameters);
		}
		return persons;
	}

	private List<BudgetPrintParameter> setBudgetPeriodsAndTotalList(List<BudgetPeriod> budgetPeriods) {
		List<BudgetPrintParameter> budgetPeriodsData = new ArrayList<>();
		if (budgetPeriods != null && !budgetPeriods.isEmpty()) {
			for (BudgetPeriod budgetPeriod : budgetPeriods) {
				BudgetPrintParameter budgetPeriodData = new BudgetPrintParameter();
				budgetPeriodData.setPeriod(budgetPeriod.getBudgetPeriod().toString());
				budgetPeriodData.setPeriodStartDate(budgetPeriod.getStartDate() != null ? commonService.convertDateFormatBasedOnTimeZone(budgetPeriod.getStartDate().getTime(), Constants.DEFAULT_DATE_FORMAT) : "");
				budgetPeriodData.setPeriodEndDate(budgetPeriod.getEndDate() != null ? commonService.convertDateFormatBasedOnTimeZone(budgetPeriod.getEndDate().getTime(), Constants.DEFAULT_DATE_FORMAT) : "");
				budgetPeriodData.setDirectCost(Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetPeriod.getTotalDirectCost()));
				budgetPeriodData.setIndirectCost(Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetPeriod.getTotalIndirectCost()));
				budgetPeriodData.setTotalModifiedDirectCost(Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetPeriod.getTotalModifiedDirectCost()));
				budgetPeriodData.setCostSharing(Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetPeriod.getCostSharingAmount()));
				budgetPeriodData.setUnderRecoveryAmount(Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetPeriod.getUnderrecoveryAmount()));
				budgetPeriodData.setTotalInKind(Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetPeriod.getTotalInKind()));
				budgetPeriodData.setTotalCost(Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetPeriod.getTotalCost()));
				budgetPeriodData.setTotalOfTotal((Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetPeriod.getTotalCost().add(budgetPeriod.getTotalInKind()))));
				budgetPeriodsData.add(budgetPeriodData);
			}
		} else {
			BudgetPrintParameter budgetPeriodData = new BudgetPrintParameter();
			budgetPeriodData.setPeriod("");
			budgetPeriodData.setPeriodStartDate("");
			budgetPeriodData.setPeriodEndDate("");
			budgetPeriodData.setDirectCost("");
			budgetPeriodData.setIndirectCost("");
			budgetPeriodData.setTotalModifiedDirectCost("");
			budgetPeriodData.setCostSharing("");
			budgetPeriodData.setUnderRecoveryAmount("");
			budgetPeriodData.setTotalInKind("");
			budgetPeriodData.setTotalCost("");
			budgetPeriodData.setTotalOfTotal("");
			budgetPeriodsData.add(budgetPeriodData);
		}
		return budgetPeriodsData;
	}

	private List<BudgetPrintParameter> prepareAwardBudgetPeriodsAndTotalData(List<AwardBudgetPeriod> budgetPeriods) {
		List<BudgetPrintParameter> budgetPeriodsData = new ArrayList<>();
		if (budgetPeriods != null) {
			for (AwardBudgetPeriod budgetPeriod : budgetPeriods) {
				BudgetPrintParameter budgetPeriodData = new BudgetPrintParameter();
				budgetPeriodData.setPeriod(budgetPeriod.getBudgetPeriod().toString());
				budgetPeriodData.setPeriodStartDate(budgetPeriod.getStartDate() != null ? commonService.convertDateFormatBasedOnTimeZone(budgetPeriod.getStartDate().getTime(), Constants.DEFAULT_DATE_FORMAT) : "");
				budgetPeriodData.setPeriodEndDate(budgetPeriod.getEndDate() != null ? commonService.convertDateFormatBasedOnTimeZone(budgetPeriod.getEndDate().getTime(), Constants.DEFAULT_DATE_FORMAT) : "");
				budgetPeriodData.setDirectCost(Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetPeriod.getTotalDirectCost()));
				budgetPeriodData.setIndirectCost(Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetPeriod.getTotalIndirectCost()));
				budgetPeriodData.setTotalModifiedDirectCost("");
				budgetPeriodData.setCostSharing("");
				budgetPeriodData.setUnderRecoveryAmount("");
				budgetPeriodData.setTotalCost(Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetPeriod.getTotalCost()));
				budgetPeriodsData.add(budgetPeriodData);
			}
		}
		return budgetPeriodsData;
	}

	private ProposalPrintParameter setProposalPersonDetails(ProposalPerson proposalPerson, String percentageOfEffort, String units, String personType, String organization) {
		ProposalPrintParameter proposalPrintParameters = new ProposalPrintParameter();
		if (proposalPerson != null) {
			if (proposalPerson.getDesignation() != null) {
				proposalPrintParameters.setDesignation(proposalPerson.getDesignation());
			} else {
				proposalPrintParameters.setDesignation("");
			}
			String conacatVal = "  (";
			String projectRole = Boolean.TRUE.equals(proposalPerson.getProposalPersonRole().getShowProjectRole()) && proposalPerson.getProjectRole() != null ? conacatVal.concat(proposalPerson.getProjectRole()).concat(")") : "";
			proposalPrintParameters.setRole(proposalPerson.getProposalPersonRole().getDescription().concat(projectRole));
			proposalPrintParameters.setFullName(proposalPerson.getFullName());
			proposalPrintParameters.setProjectRole(Boolean.TRUE.equals(proposalPerson.getProposalPersonRole().getShowProjectRole()) && proposalPerson.getProjectRole() != null ? proposalPerson.getProjectRole() : "");
			proposalPrintParameters.setCertificationStatus(getCertificationStatus(proposalPerson));	
			proposalPrintParameters.setTrainings(prepareProposalTrainingDetail(proposalPerson.getPersonId(), proposalPerson.getRolodexId()));
			proposalPrintParameters.setPersonId(proposalPerson.getPersonId());
			
		} else {
			proposalPrintParameters.setDesignation("");
			proposalPrintParameters.setRole("");
			proposalPrintParameters.setFullName("");
			proposalPrintParameters.setProjectRole("");
			proposalPrintParameters.setCertificationStatus("");
			proposalPrintParameters.setPersonId("");
		}
		proposalPrintParameters.setPercentageOfEffort(percentageOfEffort);
		proposalPrintParameters.setUnits(units);
		proposalPrintParameters.setPersonType(personType);
		proposalPrintParameters.setOrganization(organization);
		return proposalPrintParameters;
	}

	private List<PersonTraining> prepareProposalTrainingDetail(String personId, Integer rolodexId) {
		PersonVO vo = new PersonVO();
		vo.setCurrentPage(1);
		vo.setPageNumber(20);
		if (personId != null) {
			vo.setPersonId(personId);
		} else {
			vo.setPersonId(rolodexId.toString());
		}
		vo.setSortBy("updateTimeStamp");
		vo = personTrainingDao.getTrainingDashboard(vo);
		vo.getTrainings().forEach(training -> {
			training.setExpirationDate(training.getFollowupDate() != null ? commonService.convertDateFormatBasedOnTimeZone(training.getFollowupDate().getTime(), Constants.DEFAULT_DATE_FORMAT) : "");
			training.setCompletionDate(training.getDateAcknowledged() != null ? commonService.convertDateFormatBasedOnTimeZone(training.getDateAcknowledged().getTime(), Constants.DEFAULT_DATE_FORMAT) : "");
		});
		return vo.getTrainings();
	}

	public String getCertificationStatus(ProposalPerson proposalPerson) {
		  if (Constants.NO.equals(proposalPerson.getProposalPersonRole().getCertificationRequired()) || proposalPerson.getRolodexId() != null) {
		    return "Not Applicable";
		  } 
		  return Boolean.TRUE.equals(proposalPerson.getPersonCertified()) ? COMPLETE : INCOMPLETE;
		}

	private List<ProposalPrintParameter> setProposalProjectDetails(List<ProposalProjectTeam> proposalProjectTeams) {
		List<ProposalPrintParameter> projectTeams = new ArrayList<>();
		if (!proposalProjectTeams.isEmpty()) {
			for (ProposalProjectTeam proposalProjectTeam : proposalProjectTeams) {
				String isActive = "No";
				if (proposalProjectTeam.getIsActive().equals("Y")) {
					isActive = "Yes";
				}
				String activePeriod = "";
				String percentageCharged = "";
				if (proposalProjectTeam.getPercentageCharged() != null) {
					percentageCharged = proposalProjectTeam.getPercentageCharged() + "";
				}
				if (proposalProjectTeam.getStartDate() != null && proposalProjectTeam.getEndDate() != null) {
					activePeriod = activePeriod
							+ commonService.convertDateFormatBasedOnTimeZone(
									proposalProjectTeam.getStartDate().getTime(), Constants.DEFAULT_DATE_FORMAT)
							+ " to " + commonService.convertDateFormatBasedOnTimeZone(
									proposalProjectTeam.getEndDate().getTime(), Constants.DEFAULT_DATE_FORMAT);
				}
				ProposalPrintParameter proposalPrintParameters = setProposalProjectParameters(proposalProjectTeam,
						activePeriod, isActive, percentageCharged, false);
				projectTeams.add(proposalPrintParameters);
			}
		} else {
			projectTeams.add(setProposalProjectParameters(null, "", "", "", true));
		}
		return projectTeams;
	}

	private ProposalPrintParameter setProposalProjectParameters(ProposalProjectTeam proposalProjectTeam, String activePeriod, String isActive, String percentageCharged, Boolean isProjectsEmpty) {
		ProposalPrintParameter proposalPrintParameters = new ProposalPrintParameter();
		if (!isProjectsEmpty) {
			proposalPrintParameters.setProjectTeamMemberName(proposalProjectTeam.getFullName());
			proposalPrintParameters.setProjectRole(proposalProjectTeam.getProjectRole());
		} else {
			proposalPrintParameters.setProjectTeamMemberName("");
			proposalPrintParameters.setProjectRole("");
		}
		proposalPrintParameters.setActivePeriod(activePeriod);
		proposalPrintParameters.setIsActive(isActive);
		proposalPrintParameters.setPercentageCharged(percentageCharged);
		return proposalPrintParameters;
	}

	private List<ProposalPrintParameter> setProposalResearchAreaDetails(Proposal proposal) {
		List<ProposalResearchArea> researchAreas = proposalModuleDao.fetchProposalResearchAreaBasedOnProposalId(proposal.getProposalId());
		List<ProposalPrintParameter> projectTeams = new ArrayList<>();
		if (researchAreas.isEmpty()) {
			projectTeams.add(new ProposalPrintParameter("", "", "", ""));
		} else {
			for (ProposalResearchArea researchArea : researchAreas) {
				if (researchArea.getResearchTypeArea() != null) {
					if (researchArea.getResearchTypeSubArea() != null) {
						projectTeams.add(new ProposalPrintParameter(researchArea.getResearchTypeAreaCode(),
								researchArea.getResearchType().getDescription(),
								researchArea.getResearchTypeArea().getDescription(),
								researchArea.getResearchTypeSubArea().getDescription()));
					} else {
						projectTeams.add(new ProposalPrintParameter(researchArea.getResearchTypeAreaCode(),
								researchArea.getResearchType().getDescription(),
								researchArea.getResearchTypeArea().getDescription(), ""));
					}
				}
			}
		}
		return projectTeams;
	}

	private List<ProposalPrintParameter> setProposalFundDetails(List<ProposalSponsor> proposalFundDetails) {
		List<ProposalPrintParameter> proposalFunds = new ArrayList<>();
		if (proposalFundDetails.isEmpty()) {
			proposalFunds.add(setProposalFundParameters(null, "", "", true));
		} else {
			for (ProposalSponsor proposalSponsor : proposalFundDetails) {
				String sponsorName = "";
				String amount = "";
				if (proposalSponsor.getAmount() != null) {
					if (proposalSponsor.getCurrency() != null) {
						amount = Constants.DOLLAR_SYMBOL + decimalFormat.format(proposalSponsor.getAmount());
					}
				}
				if (proposalSponsor.getSponsor() != null) {
					sponsorName = sponsorName + proposalSponsor.getSponsor().getSponsorName();
				}
				ProposalPrintParameter proposalPrintParameters = setProposalFundParameters(proposalSponsor, sponsorName, amount, false);
				proposalFunds.add(proposalPrintParameters);
			}
		}
		return proposalFunds;
	}

	private ProposalPrintParameter setProposalFundParameters(ProposalSponsor proposalSponsor, String sponsorName, String amount, Boolean isFundDetailsEmpty) {
		ProposalPrintParameter proposalPrintParameters = new ProposalPrintParameter();
		if (Boolean.TRUE.equals(isFundDetailsEmpty)) {
			proposalPrintParameters.setSponsorName("");
			proposalPrintParameters.setSponsorType("");
			proposalPrintParameters.setFundStartDate("");
			proposalPrintParameters.setFundEndDate("");
			proposalPrintParameters.setFundingStatus("");
			proposalPrintParameters.setSponsorType("");
			proposalPrintParameters.setRolePlayed("");
			proposalPrintParameters.setProjectTitle("");
			proposalPrintParameters.setPercentageEffort("");
			proposalPrintParameters.setGrantCallName("");
			proposalPrintParameters.setCurrencyCode("");
		} else {
			proposalPrintParameters.setSponsorName(proposalSponsor.getFullName());
			if (proposalSponsor.getSponsorType() != null) {
				proposalPrintParameters.setSponsorType(proposalSponsor.getSponsorType().getDescription());
			} else {
				proposalPrintParameters.setSponsorType("");
			}
			if (proposalSponsor.getStartDate() != null) {
				proposalPrintParameters.setFundStartDate(commonService.convertDateFormatBasedOnTimeZone(
						proposalSponsor.getStartDate().getTime(), Constants.DEFAULT_DATE_FORMAT));
			} else {
				proposalPrintParameters.setFundStartDate("");
			}
			if (proposalSponsor.getEndDate() != null) {
				proposalPrintParameters.setFundEndDate(commonService.convertDateFormatBasedOnTimeZone(
						proposalSponsor.getEndDate().getTime(), Constants.DEFAULT_DATE_FORMAT));
			} else {
				proposalPrintParameters.setFundEndDate("");
			}
			if (proposalSponsor.getProposalFundingStatus() != null) {
				proposalPrintParameters.setFundingStatus(proposalSponsor.getProposalFundingStatus().getDescription());
			} else {
				proposalPrintParameters.setFundingStatus("");
			}
			if (proposalSponsor.getProposalPersonRole() != null) {
				proposalPrintParameters.setRolePlayed(proposalSponsor.getProposalPersonRole().getDescription());
			} else {
				proposalPrintParameters.setRolePlayed("");
			}
			if (proposalSponsor.getProjectTitle() != null) {
				proposalPrintParameters.setProjectTitle(proposalSponsor.getProjectTitle());
			} else {
				proposalPrintParameters.setProjectTitle("");
			}
			if (proposalSponsor.getPercentageOfEffort() != null) {
				proposalPrintParameters.setPercentageEffort(proposalSponsor.getPercentageOfEffort() + "");
			} else {
				proposalPrintParameters.setPercentageEffort("");
			}
			if (proposalSponsor.getGrantCallName() != null) {
				proposalPrintParameters.setGrantCallName(proposalSponsor.getGrantCallName());
			} else {
				proposalPrintParameters.setGrantCallName("");
			}
			if (proposalSponsor.getCurrencyCode() != null) {
				proposalPrintParameters.setCurrencyCode(proposalSponsor.getCurrencyCode());
			} else {
				proposalPrintParameters.setCurrencyCode("");
			}
		}
		proposalPrintParameters.setFundingSource(sponsorName);
		proposalPrintParameters.setFundAmount(amount);
		return proposalPrintParameters;
	}

	private List<ProposalPrintParameter> setProposalSpecialReviewDetails(List<ProposalSpecialReview> proposalSpecialReviews) {
		List<ProposalPrintParameter> proposalSpecialReviewList = new ArrayList<>();
		if (proposalSpecialReviews.isEmpty()) {
			proposalSpecialReviewList.add(new ProposalPrintParameter("", "", "", "", "", "", ""));
		} else {
			Map<String, String> nonEmployees = new HashMap<>();
			Map<String, String> persons = new HashMap<>();
			for (ProposalSpecialReview proposalSpecialReview : proposalSpecialReviews) {
				String specialReviewType = "";
				String approvalType = "";
				String protocolNumber = "";
				String applicationDate = "";
				String approvalDate = "";
				String expirationDate = "";
				String comment = "";
				proposalSpecialReview = proposalService.setIntegratedProposalSpecilReviewDetail(persons, nonEmployees, proposalSpecialReview);
				if (proposalSpecialReview.getSpecialReviewType() != null) {
					specialReviewType = specialReviewType + proposalSpecialReview.getSpecialReviewType().getDescription();
				}
				if (proposalSpecialReview.getApprovalType() != null) {
					approvalType = approvalType + proposalSpecialReview.getApprovalType().getDescription();
				}
				if (proposalSpecialReview.getProtocolNumber() != null) {
					protocolNumber = protocolNumber + proposalSpecialReview.getProtocolNumber();
				}
				if (proposalSpecialReview.getApplicationDate() != null) {
					applicationDate = applicationDate + commonService.convertDateFormatBasedOnTimeZone(proposalSpecialReview.getApplicationDate().getTime(), Constants.DEFAULT_DATE_FORMAT);
				}
				if (proposalSpecialReview.getApprovalDate() != null) {
					approvalDate = approvalDate + commonService.convertDateFormatBasedOnTimeZone(proposalSpecialReview.getApprovalDate().getTime(), Constants.DEFAULT_DATE_FORMAT);
				}
				if (proposalSpecialReview.getExpirationDate() != null) {
					expirationDate = expirationDate + commonService.convertDateFormatBasedOnTimeZone(proposalSpecialReview.getExpirationDate().getTime(), Constants.DEFAULT_DATE_FORMAT);
				}
				if (proposalSpecialReview.getComments() != null) {
					comment = comment + proposalSpecialReview.getComments();
				}
				proposalSpecialReviewList.add(new ProposalPrintParameter(specialReviewType, approvalType, protocolNumber, applicationDate, approvalDate, expirationDate, comment));
			}
		}
		return proposalSpecialReviewList;
	}

	private List<QuestionnairePrintParameter> setQuestionnaireDetails(String userName, String personId, Integer moduleItemCode, Integer moduleItemKey, Integer subModuleCode, String subModuleItemKey) {
		List<QuestionnairePrintParameter> proposalQuestionnaires = new ArrayList<>();
		QuestionnaireDataBus questionnaireDataBusData = new QuestionnaireDataBus();
		questionnaireDataBusData.setActionPersonId(personId);
		questionnaireDataBusData.setActionUserId(userName);
		questionnaireDataBusData.setModuleItemCode(moduleItemCode);
		questionnaireDataBusData.setModuleItemKey(moduleItemKey.toString());
		questionnaireDataBusData.setModuleSubItemCode(subModuleCode);
		questionnaireDataBusData.setModuleSubItemKey(subModuleItemKey);
		try {
			questionnaireDataBusData = questionnaireService.getApplicableQuestionnaire(questionnaireDataBusData);
			List<QuestionnaireDataBus> questionnaireList = questionnaireService.getQuestionnaireList(questionnaireDataBusData);
			proposalQuestionnaires = setQuestionnaireList(questionnaireList, moduleItemCode, moduleItemKey, subModuleCode, subModuleItemKey);
		} catch (Exception e) {
			logger.error("Exception in setQuestionnaireDetails : {}", e.getMessage());
		}
		return proposalQuestionnaires;
	}

	@SuppressWarnings("unchecked")
	private List<QuestionnairePrintParameter> setQuestionnaireList(List<QuestionnaireDataBus> questionnaireList, Integer moduleItemCode, Integer moduleItemKey, Integer subModuleCode, String subModuleItemKey) {
		List<QuestionnairePrintParameter> questionnaires = new ArrayList<>();
	    SimpleDateFormat dateFormatter = new SimpleDateFormat("yy-MM-dd HH:mm:ss");
		if (questionnaireList.isEmpty()) {
			questionnaires.add(new QuestionnairePrintParameter("", new ArrayList<>(), null));
		} else {
			for (QuestionnaireDataBus questionnaireDataBus : questionnaireList) {
				if (questionnaireDataBus.getQuestionnaire() != null) {
					List<QuestionAndAnswer> questionAndAnswers = new ArrayList<>();
					StringBuilder questionnaireHeader = new StringBuilder();
					String dateValue = null;
					try {
						dateValue = commonService.convertDateFormatBasedOnTimeZone(dateFormatter.parse(questionnaireDataBus.getHeader().get("ANS_UPDATE_TIMESTAMP").toString()).getTime(), Constants.DEFAULT_DATE_FORMAT);
					} catch (Exception e) {
						dateValue = "";
					}
					if (questionnaireDataBus.getQuestionnaireCompleteFlag().equals("Y")) {
						questionnaireHeader.append(questionnaireDataBus.getQuestionnaireName()).append(" ( ").append(COMPLETE).append(" ) ")
								.append("  Last updated by ").append(questionnaireDataBus.getHeader().get(ANS_PERSON_FULL_NAME)).append(" on ").append(dateValue);
					} else {
						questionnaireHeader.append(questionnaireDataBus.getQuestionnaireName()).append(" ( ").append(INCOMPLETE).append(" ) ");
						if (questionnaireDataBus.getHeader().get(ANS_PERSON_FULL_NAME) != null ) {
							questionnaireHeader.append("  Last updated by").append(questionnaireDataBus.getHeader().get(ANS_PERSON_FULL_NAME)).append(" on ").append(dateValue);
						}
					}
					if (!questionnaireDataBus.getQuestionnaire().getQuestions().isEmpty()) {
						for (HashMap<String, Object> question : questionnaireDataBus.getQuestionnaire()
								.getQuestions()) {
							Boolean isrulePassed = Boolean.FALSE;
							if (question.get("RULE_ID") != null) {
								isrulePassed = generalInformationDao.evaluateRule(moduleItemCode, subModuleCode,
										moduleItemKey.toString(), Integer.parseInt(question.get("RULE_ID").toString()), AuthenticatedUser.getLoginPersonId(), AuthenticatedUser.getLoginUserName(), subModuleItemKey != null  && !subModuleItemKey.isEmpty() ? subModuleItemKey : Constants.SUBMODULE_ITEM_KEY);
							}
							if ((question.get("RULE_ID") != null && Boolean.TRUE.equals(isrulePassed)) || question.get("RULE_ID") == null) {
							HashMap<String, Object> answerMap = (HashMap<String, Object>) question.get("ANSWERS");
							if (question.get(ANSWER_TYPE) != null) {
								if (question.get(ANSWER_TYPE).equals("Checkbox")) {
									if (answerMap != null) {
										if (!answerMap.toString().isEmpty()) {
											QuestionAndAnswer questionAndAnswer = new QuestionAndAnswer();
											questionAndAnswer.setQuestion(question.get(QUESTION).toString());
											String answer = "";
											int index = 0;
											for (Map.Entry<String, Object> entry : answerMap.entrySet()) {
												if (index == answerMap.size() - 1) {
													answer = answer + entry.getKey();
												} else {
													answer = answer + entry.getKey() + ",";
												}
												index++;
											}
											questionAndAnswer.setAnswer(answer);
											if (questionAndAnswer.getAnswer() != null
													&& !questionAndAnswer.getAnswer().equals("1")) {
												questionAndAnswers.add(questionAndAnswer);
											}
										}
									}
                                }
                                //TODO need to take necessary steps to print Table
                                else if (Constants.TABLE.equals(question.get(ANSWER_TYPE))) {
                                    if (answerMap != null && !answerMap.toString().isEmpty()) {
                                        QuestionAndAnswer questionAndAnswer = new QuestionAndAnswer();
                                        questionAndAnswer.setQuestion(question.get(QUESTION).toString());
                                        questionAndAnswer.setAnswer("");
                                        questionAndAnswers.add(questionAndAnswer);
                                    }
                                } else if (Constants.SYSTEM_LOOKUP.equals(question.get(ANSWER_TYPE)) || Constants.USER_LOOKUP.equals(question.get(ANSWER_TYPE))) {
                                    if (answerMap != null && answerMap.get("0") != null  && !answerMap.get("0").toString().isEmpty()) {
										QuestionAndAnswer questionAndAnswer = new QuestionAndAnswer();
										questionAndAnswer.setQuestion(question.get(QUESTION).toString());
										ObjectMapper oMapper = new ObjectMapper();
										LookUp lookUp = oMapper.convertValue(answerMap.get("0"), LookUp.class);
                                        questionAndAnswer.setAnswer(lookUp.getDescription() !=null ? lookUp.getDescription(): "");
										questionAndAnswers.add(questionAndAnswer);
									}
								} else {
									if (answerMap != null && answerMap.get("1") != null) {
										if (!answerMap.get("1").toString().isEmpty()) {
											QuestionAndAnswer questionAndAnswer = new QuestionAndAnswer();
											questionAndAnswer.setQuestion(question.get(QUESTION).toString());
											questionAndAnswer.setAnswer(answerMap.get("1").toString());
											questionAndAnswers.add(questionAndAnswer);
										}
									}
								}
							}
						  }
						}
					}
//					} else {
//						questionnaireHeader = questionnaireHeader + questionnaireDataBus.getQuestionnaireName() + " ("
//								+ "In Complete" + ")";
//					}
					questionnaires.add(new QuestionnairePrintParameter(questionnaireHeader.toString(), questionAndAnswers, null));
				}
			}
		}
		return questionnaires;
	}

	public IContext setProposalPlaceHolderData(IContext context, Proposal proposal, FieldsMetadata fieldsMetadata, BudgetHeader budgetHeader) {
		fieldsMetadata.addFieldAsTextStyling("PROJECT_ABSTRACT", SyntaxKind.Html);
		fieldsMetadata.addFieldAsTextStyling("GRANT_CALL_THEME", SyntaxKind.Html);
		if (proposal.getTitle() != null) {
			context.put(TITLE, proposal.getTitle());
		} else {
			context.put(TITLE, "");
		}
		if (proposal.getInvestigator() != null) {
			context.put("PI", proposal.getInvestigator().getFullName());
		} else {
			context.put("PI", "");
		}
		if (proposal.getProposalStatus() != null) {
			context.put("STATUS", proposal.getProposalStatus().getDescription());
		} else {
			context.put("STATUS", "");
		}
		if (proposal.getProposalType() != null) {
			context.put("PROPOSAL_TYPE", proposal.getProposalType().getDescription());
		} else {
			context.put("PROPOSAL_TYPE", "");
		}
		if (proposal.getActivityType() != null) {
			context.put(ACTIVITY_TYPE, proposal.getActivityType().getDescription());
		} else {
			context.put(ACTIVITY_TYPE, "");
		}
        if (proposal.getSponsor() != null) {
            context.put(SPONSOR_NAME, commonService.getSponsorFormatBySponsorDetail(proposal.getSponsor().getSponsorCode(), proposal.getSponsor().getSponsorName(), proposal.getSponsor().getAcronym()));
        	context.put("SPONSOR_TYPE", proposal.getSponsor().getSponsorType() != null ? proposal.getSponsor().getSponsorType().getDescription() : "");
		} else {
            context.put(SPONSOR_NAME, "");
			context.put("SPONSOR_TYPE","");
        }
        if (proposal.getSponsorProposalNumber() != null) {
            context.put("SPR_PROPOSAL_ID", proposal.getSponsorProposalNumber());
        } else {
            context.put("SPR_PROPOSAL_ID", "");
        }
        if (proposal.getStartDate() != null) {
            String startDate = commonService.convertDateFormatBasedOnTimeZone(proposal.getStartDate().getTime(),
                    Constants.DEFAULT_DATE_FORMAT);
            context.put("START_DATE", startDate);
        } else {
            context.put("START_DATE", "");
        }
        if (proposal.getEndDate() != null) {
            String endDate = commonService.convertDateFormatBasedOnTimeZone(proposal.getEndDate().getTime(),
                    Constants.DEFAULT_DATE_FORMAT);
            context.put("END_DATE", endDate);
        } else {
            context.put("END_DATE", "");
        }
        if (proposal.getAwardType() != null) {
            context.put(AWARD_TYPE, proposal.getAwardType().getDescription());
        } else {
            context.put(AWARD_TYPE, "");
        }
		context.put(AWARD_NUMBER, proposal.getAwardNumber() != null? proposal.getAwardNumber() : "");
        if (proposal.getBaseProposalNumber() != null) {
            context.put(MASTER_PROPOSAL, proposal.getBaseProposalNumber());
        } else {
            context.put(MASTER_PROPOSAL, "");
        }
        if (proposal.getProgramAnnouncementNumber() != null) {
            context.put("PROGRAM_ANNOUNCEMENT_NUMBER", proposal.getProgramAnnouncementNumber());
        } else {
            context.put("PROGRAM_ANNOUNCEMENT_NUMBER", "");
        }
        if (proposal.getCfdaNumber() != null) {
            context.put("CDFA_NUMBER", proposal.getCfdaNumber());
        } else {
            context.put("CDFA_NUMBER", "");
        }
        if (proposal.getPrimeSponsor() != null) {
            context.put(PRIME_SPONSOR, commonService.getSponsorFormatBySponsorDetail(proposal.getPrimeSponsor().getSponsorCode(),
					proposal.getPrimeSponsor().getSponsorName(), proposal.getPrimeSponsor().getAcronym()));
        } else {
            context.put(PRIME_SPONSOR, "");
        }
        if (proposal.getSponsorDeadlineDate() != null) {
            context.put("SPONSOR_DEADLINE_DATE", commonService.convertDateFormatBasedOnTimeZone(
                    proposal.getSponsorDeadlineDate().getTime(), Constants.DEFAULT_DATE_FORMAT));
        } else {
            context.put("SPONSOR_DEADLINE_DATE", "");
        }
        if (proposal.getHomeUnitNumber() != null) {
            context.put(LEAD_UNIT, commonService.getUnitFormatByUnitDetail(proposal.getHomeUnitNumber(), proposal.getHomeUnitName()));
        } else {
            context.put(LEAD_UNIT, "");
        }
        context.put(DURATION, proposal.getDuration() != null ? proposal.getDuration() : "");
        if (!proposal.getProposalKeywords().isEmpty()) {
            String keywords = "";
            for (ProposalKeyword proposalKeyword : proposal.getProposalKeywords()) {
                if (proposal.getProposalKeywords().size() != proposal.getProposalKeywords().indexOf(proposalKeyword)
                        + 1) {
                    if (proposalKeyword.getScienceKeyword() != null) {
                        keywords = keywords + proposalKeyword.getScienceKeyword().getDescription() + ", ";
                    } else {
                        keywords = keywords + proposalKeyword.getKeyword() + ", ";
                    }
                } else {
                    if (proposalKeyword.getScienceKeyword() != null) {
                        keywords = keywords + proposalKeyword.getScienceKeyword().getDescription();
                    } else {
                        keywords = keywords + proposalKeyword.getKeyword();
                    }
                }
            }
            context.put("KEYWORDS", keywords);
        } else {
            context.put("KEYWORDS", "");
        }
        if (proposal.getAbstractDescription() != null) {
        	String abstractDescription = proposal.getAbstractDescription().replace("&nbsp;","");
            context.put("PROJECT_ABSTRACT", abstractDescription.replace("<br>", "<br></br>"));
        } else {
            context.put("PROJECT_ABSTRACT", "");
        }
        if (proposal.getGrantCallType() != null) {
            context.put(GRANT_CALL_TYPE, proposal.getGrantCallType().getDescription());
        } else {
            context.put(GRANT_CALL_TYPE, "");
        }
        if (proposal.getGrantCallId() != null) {
            GrantCall grantCall = grantCallDao.fetchGrantCallById(proposal.getGrantCallId());
            if (proposal.getGrantCallId() != null) {
                context.put("GRANT_CALL_ID", proposal.getGrantCallId());
            } else {
                context.put("GRANT_CALL_ID", "");
            }
            if (grantCall.getSponsor() != null) {
                context.put("FUNDING_AGENCY_NAME", grantCall.getSponsor().getSponsorName());
            } else {
                context.put("FUNDING_AGENCY_NAME", "");
            }
            if (grantCall.getGrantCallName() != null) {
                context.put("GRANT_CALL_NAME", grantCall.getGrantCallName());
            } else {
                context.put("GRANT_CALL_NAME", "");
            }
            if (grantCall.getGrantTheme() != null) {
            	String grantCallScheme = grantCall.getGrantTheme().replace("&nbsp;","");
                context.put("GRANT_CALL_THEME", grantCallScheme.replace("<br>", "<br></br>"));
            } else {
                context.put("GRANT_CALL_THEME", "");
            }
            if (grantCall.getSponsorFundingScheme() != null) {
                context.put("FUNDING_SCHEME_NAME", grantCall.getSponsorFundingScheme().getFundingScheme().getDescription());
            } else {
                context.put("FUNDING_SCHEME_NAME", "");
            }
            if (grantCall.getClosingDate() != null) {
                context.put("CLOSING_DATE", commonService.convertDateFormatBasedOnTimeZone(
                        grantCall.getClosingDate().getTime(), Constants.DEFAULT_DATE_FORMAT));
            } else {
                context.put("CLOSING_DATE", "");
            }
        } else {
            context.put("GRANT_CALL_ID", "");
            context.put("FUNDING_AGENCY_NAME", "");
            context.put("GRANT_CALL_NAME", "");
            context.put("GRANT_CALL_THEME", "");
            context.put("FUNDING_SCHEME_NAME", "");
            context.put("CLOSING_DATE", "");
        }
        if (proposal.getResearchDescription() != null) {
            context.put(RESEARCH_DESCRIPTION, proposal.getResearchDescription());
        } else {
            context.put(RESEARCH_DESCRIPTION, "");
        }
        if (proposal.getMultiDisciplinaryDescription() != null) {
            context.put(MULTI_DISCIPLINARY_DESCRIPTION, proposal.getMultiDisciplinaryDescription());
        } else {
            context.put(MULTI_DISCIPLINARY_DESCRIPTION, "");
        }
        if (proposal.getDisciplineCluster() != null) {
            context.put("DISCIPLINE_CLUSTER", proposal.getDisciplineCluster().getDescription());
        } else {
            context.put("DISCIPLINE_CLUSTER", "");
        }
        if (proposal.getInternalDeadLineDate() != null) {
            context.put("INTERNAL_DEADLINE_DATE", commonService.convertDateFormatBasedOnTimeZone(
                    proposal.getInternalDeadLineDate().getTime(), Constants.DEFAULT_DATE_FORMAT));
        } else {
            context.put("INTERNAL_DEADLINE_DATE", "");
        }
        if (proposal.getExternalFundingAgencyId() != null) {
            context.put("EXTERNAL_FUNDING_AGENCY_PROPOSAL_ID", proposal.getExternalFundingAgencyId());
        } else {
            context.put("EXTERNAL_FUNDING_AGENCY_PROPOSAL_ID", "");
        }
        if (proposal.getProgramAnnouncementNumber() != null) {
            context.put(FUNDING_OPPORTUNITY_NUMBER, proposal.getProgramAnnouncementNumber());
        } else {
            context.put(FUNDING_OPPORTUNITY_NUMBER, "");
        }
        if (proposal.getActivityType() != null) {
            context.put("CATEGORY", proposal.getActivityType().getDescription());
        } else {
            context.put("CATEGORY", "");
        }
        context.put("PROPOSAL_ID", proposal.getProposalId());
        if (proposal.getApplicationId() != null) {
            context.put("APPLICATION_ID", proposal.getApplicationId());
        } else {
            context.put("APPLICATION_ID", "");
        }
        if (proposal.getBaseProposalNumber() != null) {
			context.put("ORIGINAL_IP_ID", proposal.getBaseProposalNumber());
			String ipTitle = institutionalProposalService.getIPTitleForMasterProposal(proposal.getBaseProposalNumber());
			context.put("ORIGINAL_IP_TITLE", ipTitle != null ? ipTitle: "");
			InstituteProposal ipForMasterProposal = institutionalProposalDao.getIPForMasterProposal(proposal.getBaseProposalNumber());
			if(ipForMasterProposal != null && ipForMasterProposal.getInstProposalType() != null) {
				context.put("ORIGINAL_IP_TYPE", ipForMasterProposal.getInstProposalType().getDescription() != null ?
						ipForMasterProposal.getInstProposalType().getDescription(): "");
			}
        } else {
        	 context.put("ORIGINAL_IP_ID", "");
			 context.put("ORIGINAL_IP_TITLE", "");
			 context.put("ORIGINAL_IP_TYPE", "");
        }
        if (proposal.getAwardNumber() != null) {
          StringBuilder awardDetail = new StringBuilder();
          Award award = awardDao.fetchActiveAwardByAwardNumber(proposal.getAwardNumber());
          awardDetail.append(proposal.getAwardNumber()).append(" : ").append(award.getTitle());
          context.put("awardTitle", awardDetail);
        } else {
			context.put("awardTitle", "");
		}
		if (budgetHeader != null && budgetHeader.getTotalCost() != null) {
			context.put("TOTAL_COST", Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetHeader.getTotalCost()));
		} else {
			context.put("TOTAL_COST", "");
		}
		if (budgetHeader != null && budgetHeader.getTotalDirectCost() != null) {
			context.put("TOTAL_DIRECT_COST", Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetHeader.getTotalDirectCost()));
		} else {
			context.put("TOTAL_DIRECT_COST", "");
		}
		if (budgetHeader != null && budgetHeader.getTotalIndirectCost() != null) {
			context.put("TOTAL_INDIRECT_COST", Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetHeader.getTotalIndirectCost()));
		} else {
			context.put("TOTAL_INDIRECT_COST", "");
		}
//		if (budgetHeader != null && budgetHeader.getCostSharingAmount() != null) {
//			context.put("COST_SHARING_AMOUNT", Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetHeader.getCostSharingAmount()));
//		} else {
//			context.put("COST_SHARING_AMOUNT", "");
//		}
//		if (budgetHeader != null && budgetHeader.getUnderrecoveryAmount() != null) {
//			context.put("UNDERRECOVERY_AMOUNT", Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetHeader.getUnderrecoveryAmount()));
//		} else {
//			context.put("UNDERRECOVERY_AMOUNT", "");
//		}
		if (budgetHeader != null && budgetHeader != null && budgetHeader.getBudgetStatus() != null) {
			context.put("BUDGET_STATUS", budgetHeader.getBudgetStatus().getDescription() + "");
		} else {
			context.put("BUDGET_STATUS", "");
		}
//		if (budgetHeader != null && budgetHeader.getRateTypeCode() != null) {
//			context.put("OVER_HEAD_RATE_TYPE", budgetHeader.getRateType().getDescription() + "");
//		} else {
//			context.put("OVER_HEAD_RATE_TYPE", "");
//		}
//		if (budgetHeader != null && budgetHeader.getUnderrecoveryRateClassCode() != null) {
//			context.put("UNDER_RECOVERY_RATE_TYPE", budgetHeader.getUnderrecoveryRateType().getDescription() + "");
//		} else {
//			context.put("UNDER_RECOVERY_RATE_TYPE", "");
//		}
//		if (budgetHeader != null && budgetHeader.getCampusFlag() != null) {
//			if (budgetHeader.getCampusFlag().equals("F")) {
//				context.put("ON_OFF_CAMPUS_FLAG", "OFF" + "");
//			} else if (budgetHeader.getCampusFlag().equals("N")) {
//				context.put("ON_OFF_CAMPUS_FLAG", "ON" + "");
//			} else if (budgetHeader.getCampusFlag().equals("D")) {
//				context.put("ON_OFF_CAMPUS_FLAG", "BOTH" + "");
//			}
//		} else {
//			context.put("ON_OFF_CAMPUS_FLAG", "");
//		}
		if (budgetHeader != null && budgetHeader.getComments() != null) {
			context.put("DESCRIPTION", budgetHeader.getComments() + "");
		} else {
			context.put("DESCRIPTION", "");
		}
		if (budgetHeader != null && budgetHeader.getIsFinalBudget() != null) {
			if (budgetHeader.getIsFinalBudget()) {
				context.put("IS_BUDGET_FINAL", "Yes");
			} else {
				context.put("IS_BUDGET_FINAL", "No");
			}
		} else {
			context.put("IS_BUDGET_FINAL", "");
		}
//		if (budgetHeader != null && budgetHeader.getRateType() != null) {
//			context.put("OVERHEAD_RATE_TYPE", budgetDetail.getRateType().getDescription());
//		} else {
//			context.put("OVERHEAD_RATE_TYPE", "");
//		}
		if (budgetHeader != null && budgetHeader.getTotalModifiedDirectCost() != null) {
			context.put("MODIFIED_DIRECT_COST", Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetHeader.getTotalModifiedDirectCost()));
		} else {
			context.put("MODIFIED_DIRECT_COST", "");
		}
		if (budgetHeader != null && budgetHeader.getVersionNumber() != null) {
			context.put("VERSION_NUMBER", budgetHeader.getVersionNumber().toString());
		} else {
			context.put("VERSION_NUMBER", "");
		}
		if (budgetHeader != null) {
			context.put("SHOW_BUDGET_OH_RATE_PERCENTAGE", commonDao.getParameterValueAsBoolean(Constants.SHOW_BUDGET_OH_RATE_PERCENTAGE));
			context.put("ON_CAMPUS_RATE", budgetHeader.getOnCampusRates() != null ? budgetHeader.getOnCampusRates() : "");
			context.put("OFF_CAMPUS_RATE", budgetHeader.getOffCampusRates() != null ? budgetHeader.getOffCampusRates() : "");
		} else {
			context.put("SHOW_BUDGET_OH_RATE_PERCENTAGE", false);
			context.put("ON_CAMPUS_RATE", "");
			context.put("OFF_CAMPUS_RATE", "");
		} 
        if (proposal.getIpNumber() != null) {
            context.put("IP_NUMBER", proposal.getIpNumber());
            String ipTitle = institutionalProposalDao.getIPTitle(proposal.getIpNumber());
             context.put("IP_TITLE", ipTitle);
        } else {
            context.put("IP_NUMBER", "");
            context.put("IP_TITLE", "");
        }
		return context;
	}

	public IContext setBudgetPlaceHolderData(IContext context, BudgetHeader budgetDetail, Proposal proposal) {
		if (commonDao.getParameterValueAsBoolean(Constants.IS_APPLICATION_ID_REQUIRED)) {
			String applicationId = "";
			if (proposal.getApplicationId() != null) {
				applicationId = proposal.getApplicationId();
			}
			context.put("PROPOSAL_ID", applicationId + "");
		} else {
			context.put("PROPOSAL_ID", proposal.getProposalId() + "");
		}
		context.put(TITLE, proposal.getTitle());
		if (proposal.getInvestigator() != null) {
			context.put("PI", proposal.getInvestigator().getFullName());
		} else {
			context.put("PI", "");
		}
        if (proposal.getSponsor() != null) {
            context.put(SPONSOR_NAME, commonService.getSponsorFormatBySponsorDetail(proposal.getSponsor().getSponsorCode(), proposal.getSponsor().getSponsorName(), proposal.getSponsor().getAcronym()));
		} else {
			context.put(SPONSOR_NAME, "");
		}
		if (proposal.getStartDate() != null) {
			context.put("START_DATE", commonService.convertDateFormatBasedOnTimeZone(proposal.getStartDate().getTime(), Constants.DEFAULT_DATE_FORMAT));
		} else {
			context.put("START_DATE", "");
		}
		if (proposal.getEndDate() != null) {
			context.put("END_DATE", commonService.convertDateFormatBasedOnTimeZone(proposal.getEndDate().getTime(), Constants.DEFAULT_DATE_FORMAT));
		} else {
			context.put("END_DATE", "");
		}
		if (budgetDetail.getTotalCost() != null) {
			context.put("TOTAL_COST", Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetDetail.getTotalCost()));
		} else {
			context.put("TOTAL_COST", "");
		}
		if (budgetDetail.getTotalDirectCost() != null) {
			context.put("TOTAL_DIRECT_COST", Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetDetail.getTotalDirectCost()));
		} else {
			context.put("TOTAL_DIRECT_COST", "");
		}
		if (budgetDetail.getTotalIndirectCost() != null) {
			context.put("TOTAL_INDIRECT_COST", Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetDetail.getTotalIndirectCost()));
		} else {
			context.put("TOTAL_INDIRECT_COST", "");
		}
//		if (budgetDetail.getCostSharingAmount() != null) {
//			context.put("COST_SHARING_AMOUNT", Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetDetail.getCostSharingAmount()));
//		} else {
//			context.put("COST_SHARING_AMOUNT", "");
//		}
//		if (budgetDetail.getUnderrecoveryAmount() != null) {
//			context.put("UNDERRECOVERY_AMOUNT", Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetDetail.getUnderrecoveryAmount()));
//		} else {
//			context.put("UNDERRECOVERY_AMOUNT", "");
//		}
		if (budgetDetail.getBudgetStatus() != null) {
			context.put("BUDGET_STATUS", budgetDetail.getBudgetStatus().getDescription() + "");
		} else {
			context.put("BUDGET_STATUS", "");
		}
//		if (budgetDetail.getRateTypeCode() != null) {
//			context.put("OVER_HEAD_RATE_TYPE", budgetDetail.getRateType().getDescription() + "");
//		} else {
//			context.put("OVER_HEAD_RATE_TYPE", "");
//		}
//		if (budgetDetail.getUnderrecoveryRateClassCode() != null) {
//			context.put("UNDER_RECOVERY_RATE_TYPE", budgetDetail.getUnderrecoveryRateType().getDescription() + "");
//		} else {
//			context.put("UNDER_RECOVERY_RATE_TYPE", "");
//		}
//		if (budgetDetail.getCampusFlag() != null) {
//			if (budgetDetail.getCampusFlag().equals("F")) {
//				context.put("ON_OFF_CAMPUS_FLAG", "OFF" + "");
//			} else if (budgetDetail.getCampusFlag().equals("N")) {
//				context.put("ON_OFF_CAMPUS_FLAG", "ON" + "");
//			} else if (budgetDetail.getCampusFlag().equals("D")) {
//				context.put("ON_OFF_CAMPUS_FLAG", "BOTH" + "");
//			}
//		} else {
//			context.put("ON_OFF_CAMPUS_FLAG", "");
//		}
		if (budgetDetail.getComments() != null) {
			context.put("DESCRIPTION", budgetDetail.getComments() + "");
		} else {
			context.put("DESCRIPTION", "");
		}
		if (budgetDetail.getIsFinalBudget() != null) {
			if (budgetDetail.getIsFinalBudget()) {
				context.put("IS_BUDGET_FINAL", "Yes");
			} else {
				context.put("IS_BUDGET_FINAL", "No");
			}
		} else {
			context.put("IS_BUDGET_FINAL", "");
		}
//		if (budgetDetail.getRateType() != null) {
//			context.put("OVERHEAD_RATE_TYPE", budgetDetail.getRateType().getDescription());
//		} else {
//			context.put("OVERHEAD_RATE_TYPE", "");
//		}
		if (budgetDetail.getTotalModifiedDirectCost() != null) {
			context.put("MODIFIED_DIRECT_COST", Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetDetail.getTotalModifiedDirectCost()));
		} else {
			context.put("MODIFIED_DIRECT_COST", "");
		}
		if (budgetDetail.getVersionNumber() != null) {
			context.put("VERSION_NUMBER", budgetDetail.getVersionNumber().toString());
		} else {
			context.put("VERSION_NUMBER", "");
		}
		return prepareBudgetHeaderDetails(context, budgetDetail);
	}

	public ResponseEntity<byte[]> setHttpHeaderAndHttpResponseData(HttpServletResponse response,
			String generatedFileName, byte[] mergedOutput, ResponseEntity<byte[]> attachmentData) {
		try {
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
			logger.error("Exception in setHttpHeaderAndHttpResponseData : {}", e.getMessage());
		}
		return attachmentData;
	}

	@Override
	public ResponseEntity<byte[]> generateAwardReport(HttpServletResponse response, Integer awardId, String personId) {
		ResponseEntity<byte[]> attachmentData = null;
		try {
			byte[] bFile = null;
			List<AwardBudgetHeader> awardBudgetHeaders = awardBudgetDao.getAwardBudgetVersionsByAwardId(awardId);
			AwardBudgetHeader awardBudgetHeader = null;
			if (!awardBudgetHeaders.isEmpty()) {
				awardBudgetHeader = awardBudgetHeaders.get(awardBudgetHeaders.size() - 1);
				if (awardBudgetHeader.getBudgetTypeCode().equals(Constants.BUDGET_TYPE_REBUDGET) && (awardBudgetHeader
						.getBudgetStatusCode().equals(Constants.AWARD_BUDGET_STATUS_CODE_INPROGRESS))) {
					bFile = getTemplateData(Constants.BUDGET_VARIATION_REQUEST_LETTER_TEMPLATE_TYPE_CODE);
				} else {
					bFile = getTemplateData(Constants.AWARD_LETTER_TEMPLATE_TYPE_CODE);
				}
			} else {
				bFile = getTemplateData(Constants.AWARD_LETTER_TEMPLATE_TYPE_CODE);
			}
			byte[] mergedOutput = mergePlaceHoldersOfAward(bFile, awardId, personId);
			String generatedFileName = RESULT + System.nanoTime() + ".pdf";
			setHttpHeaderAndHttpResponseData(response, generatedFileName, mergedOutput, attachmentData);
		} catch (Exception e) {
			logger.error("Exception in generateAwardReport : {}", e.getMessage());
		}
		return attachmentData;
	}

	private byte[] mergePlaceHoldersOfAward(byte[] data, Integer awardId, String personId) {
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		try {
			InputStream myInputStream = new ByteArrayInputStream(data);
			IXDocReport report = XDocReportRegistry.getRegistry().loadReport(myInputStream,
					TemplateEngineKind.Velocity);
			FieldsMetadata fieldsMetadata = report.createFieldsMetadata();
			Award award = awardDao.fetchAwardByAwardId(awardId.toString());

			List<AwardBudgetHeader> awardBudgetHeaders = awardBudgetDao.getAwardBudgetVersionsByAwardId(awardId);
			AwardBudgetHeader awardBudgetHeader = null;
			if (!awardBudgetHeaders.isEmpty()) {
				awardBudgetHeader = awardBudgetHeaders.get(awardBudgetHeaders.size() - 1);
			}

			fieldsMetadata.load("awardFundingProposals", AwardPrintParameter.class, true);
			List<AwardPrintParameter> awardFundingProposals = setAwardFundingProposalDetails(awardId);

			fieldsMetadata.load("awardPersons", AwardPrintParameter.class, true);
			List<AwardPrintParameter> awardPersons = setAwardPersonDetails(award);

			fieldsMetadata.load(POJECT_TEAM_DETAILS, AwardPrintParameter.class, true);
			List<AwardPrintParameter> projectTeamDetails = setAwardProjectDetails(awardId);

			fieldsMetadata.load("awardContacts", AwardPrintParameter.class, true);
			List<AwardPrintParameter> awardContacts = setAwardContactDetails(awardId);

			fieldsMetadata.load("awardSpecialReviews", AwardPrintParameter.class, true);
			List<AwardPrintParameter> awardSpecialReviews = setAwardSpecialReviewDetails(awardId);

			fieldsMetadata.load("awardSubcontracts", AwardPrintParameter.class, true);
			List<AwardPrintParameter> awardSubcontracts = setAwardSubcontractDetails(awardId);

			fieldsMetadata.load("awardCostShares", AwardPrintParameter.class, true);
			List<AwardPrintParameter> awardCostShares = setAwardCostShareDetails(awardId);

			fieldsMetadata.load("awardResearchAreas", AwardPrintParameter.class, true);
			List<AwardPrintParameter> awardResearchAreas = setResearchAreas(awardId);

			fieldsMetadata.load("awardMilestones", AwardPrintParameter.class, true);
			List<AwardPrintParameter> awardMilestones = setAwardMilestone(awardId);

			List<AwardKPI> awardKPIs = awardDao.fetchAllAwardKPI(awardId);
			fieldsMetadata.load("awardKPIs", AwardPrintParameter.class, true);
			List<AwardKPIPrintParameter> awardKPIPrintParameters = setAwardKPI(awardKPIs);

			fieldsMetadata.load("questionnaires", QuestionnairePrintParameter.class, true);
			List<QuestionnairePrintParameter> questionnaires = setQuestionnaireDetails(null, personId,
					Constants.AWARD_MODULE_CODE, awardId, Constants.AWARD_SUBMODULE_CODE, Constants.SUBMODULE_ITEM_KEY);

			fieldsMetadata.load("dmpQuestionnaires", QuestionnairePrintParameter.class, true);
			List<QuestionnairePrintParameter> dmpQuestionnaires = setQuestionnaireDetails(null, personId,
					Constants.AWARD_MODULE_CODE, awardId, 3, "0");

			List<CustomData> otherInformations = customDataElementDao
                    .fetchCustomDataByParams(Constants.AWARD_MODULE_CODE, awardId, Constants.AWARD_SUBMODULE_CODE , Constants.SUBMODULE_ITEM_KEY);
			fieldsMetadata.load("otherInformations", AwardPrintParameter.class, true);
			List<AwardOtherInformations> awardOtherInformations = setOtherInformation(otherInformations);

			List<BudgetPeriodPrintParameter> awardBudgetPeriods = null;
			List<AwardBudgetPeriod> awardBudgetPeriodsData = null;
			List<AwardBudgetPerson> budgetPersons = null;
			List<AwardBudgetPeriod> budgetPeriods = null;
			if (awardBudgetHeader != null) {
				awardBudgetHeader.setFundCode(award.getAccountNumber());
				awardBudgetHeader.setFundCenter(award.getFundCenter());
				awardBudgetPeriodsData = awardBudgetDao
						.getAwardBudgetPeriodsByBudgetId(awardBudgetHeader.getBudgetId());
				budgetPersons = awardBudgetDao.getBudgetPersons(awardBudgetHeader.getBudgetId());
				budgetPeriods = awardBudgetService.setPeriodData(awardBudgetHeader);
				if (awardBudgetHeader.getVersionNumber() > 1) {
					setAwardBudgetHeaderBasedOnVersionNumber(awardBudgetHeader);
				}
			}

			fieldsMetadata.load("awardBudgetPeriodsAndTotal", BudgetPrintParameter.class, true);
			List<BudgetPrintParameter> awardBudgetPeriodsAndTotal = prepareAwardBudgetPeriodsAndTotalData(
					awardBudgetPeriodsData);

			fieldsMetadata.load("awardBudgetPersons", BudgetPrintParameter.class, true);
			List<BudgetPrintParameter> awardBudgetPersons = prepareAwardBudgetPersonnelsData(budgetPersons);

			fieldsMetadata.load("awardBudgetPeriods", BudgetPeriodPrintParameter.class, true);
			awardBudgetPeriods = prepareAwardBudgetPeriodsData(budgetPeriods);

			IContext context = report.createContext();
			List<AwardBudgetPeriodSummary> awardBudgetSummaryDetails = setAwardBudgetSummaryDetails(budgetPeriods,
					context);

			fieldsMetadata.load("datesAndAmountOverview", AwardDateAndAmountPrintParameter.class, true);
			List<AwardAmountInfo> datesAndAmountInfo = datesAndAmountService.prepareAwardAmountInfoForAward(award.getAwardId(), award.getAwardNumber(), award.getSequenceNumber());
			List<AwardDateAndAmountPrintParameter> awardDatesAndAmountOverview = setDatesAndAmountOverview(datesAndAmountInfo);
 
			fieldsMetadata.load("awardPayment", AwardPrintParameter.class, true);
			AwardPrintParameter awardPayment = setawardPayment(award);

			context.put("awardFundingProposals", awardFundingProposals);
			context.put("awardPersons", awardPersons);
			context.put(POJECT_TEAM_DETAILS, projectTeamDetails);
			context.put("awardContacts", awardContacts);
			context.put("awardSpecialReviews", awardSpecialReviews);
			context.put("awardSubcontracts", awardSubcontracts);
			context.put("awardCostShares", awardCostShares);
			context.put("awardResearchAreas", awardResearchAreas);
			context.put("awardMilestones", awardMilestones);
			context.put("awardKPIs", awardKPIPrintParameters);
			context.put("questionnaires", questionnaires);
			context.put("dmpQuestionnaires", dmpQuestionnaires);
			context.put("awardBudgetPeriodsAndTotal", awardBudgetPeriodsAndTotal);
			context.put("awardBudgetPersons", awardBudgetPersons);
			context.put("awardBudgetPeriods", awardBudgetPeriods);
			context.put("otherInformations", awardOtherInformations);
			context.put("budgetSummaryDetails", awardBudgetSummaryDetails);
			context.put("datesAndAmountOverview", awardDatesAndAmountOverview);
			context.put("awardPayment", awardPayment);
			context = setAwardPlaceHolderData(context, award, awardBudgetHeader);

			Options options = Options.getTo(ConverterTypeTo.PDF).via(ConverterTypeVia.XWPF);
			report.convert(context, options, baos);
		} catch (Exception e) {
			logger.error("Exception in mergePlaceHoldersOfAward : {}", e.getMessage());
		}
		return baos.toByteArray();
	}

	private AwardPrintParameter setawardPayment(Award award) {
		AwardPrintParameter awardPrintParameter = new AwardPrintParameter();
        awardPrintParameter.setAwardBasisOfPaymentType((award.getAwardBasisOfPayment() != null ? award.getAwardBasisOfPayment().getDescription() + "" : ""));
        awardPrintParameter.setAwardMethodOfPaymentType((award.getAwardMethodOfPayment() != null ? award.getAwardMethodOfPayment().getDescription() + "" : ""));
		awardPrintParameter.setAwardFrequencyType((award.getFrequency() != null ? award.getFrequency().getDescription() : ""));
        awardPrintParameter.setFinalInvoiceDue((award.getFinalInvoiceDue() != null ? award.getFinalInvoiceDue() + "" : ""));
        awardPrintParameter.setInvoiceNoOfCopies((award.getInvoiceNoOfCopies() != null ? award.getInvoiceNoOfCopies() + "" : ""));
		awardPrintParameter.setInvoiceInstructions((award.getInvoiceInstructions() != null ? award.getInvoiceInstructions() + "" : ""));
		return awardPrintParameter;
	}

	private List<AwardDateAndAmountPrintParameter> setDatesAndAmountOverview(List<AwardAmountInfo> datesAndAmountOverviews) {
		List<AwardDateAndAmountPrintParameter> datesAndAmountOverviewDetail = new ArrayList<>();
		if (datesAndAmountOverviews != null && !datesAndAmountOverviews.isEmpty()) {
			datesAndAmountOverviews.stream().forEach(awardAmountInfo -> {
				if (awardAmountInfo != null) {
                    String transactionType = (awardAmountInfo.getAwardAmountTransaction() != null && awardAmountInfo.getAwardAmountTransaction().getAwardTransactionType() != null ? awardAmountInfo.getAwardAmountTransaction().getAwardTransactionType().getDescription() + "" : "");
                    String proposalNumber = (awardAmountInfo.getAwardAmountTransaction() != null && awardAmountInfo.getAwardAmountTransaction().getFundingProposalNumber() != null ? awardAmountInfo.getAwardAmountTransaction().getFundingProposalNumber() + "" : "");
                    String sourceAwardNumber = (awardAmountInfo.getAwardAmountTransaction() != null && awardAmountInfo.getAwardAmountTransaction().getSourceAwardNumber() != null ? awardAmountInfo.getAwardAmountTransaction().getSourceAwardNumber() + "" : "");
                    String destinationAwardNumber = (awardAmountInfo.getAwardAmountTransaction() != null && awardAmountInfo.getAwardAmountTransaction().getDestinationAwardNumber() != null ? awardAmountInfo.getAwardAmountTransaction().getDestinationAwardNumber() + "" : "");
                    String noticeDate = (awardAmountInfo.getAwardAmountTransaction() != null && awardAmountInfo.getAwardAmountTransaction().getNoticeDate() != null ? commonService.convertDateFormatBasedOnTimeZone(awardAmountInfo.getAwardAmountTransaction().getNoticeDate().getTime(), Constants.DEFAULT_DATE_FORMAT) + "" : "");
                    String comment = (awardAmountInfo.getAwardAmountTransaction() != null && awardAmountInfo.getAwardAmountTransaction().getComments() != null ? awardAmountInfo.getAwardAmountTransaction().getComments() + "" : "");
					String obligationStartDate = (awardAmountInfo.getCurrentFundEffectiveDate() != null ? commonService.convertDateFormatBasedOnTimeZone(awardAmountInfo.getCurrentFundEffectiveDate().getTime(), Constants.DEFAULT_DATE_FORMAT) : "");
					String obligationEndDate = (awardAmountInfo.getObligationExpirationDate() != null ? commonService.convertDateFormatBasedOnTimeZone(awardAmountInfo.getObligationExpirationDate().getTime(), Constants.DEFAULT_DATE_FORMAT) : "");
                    String obligationChange = (awardAmountInfo.getObligatedChange() != null ? Constants.DOLLAR_SYMBOL + awardAmountInfo.getObligatedChange() + "" : "");
                    String anticipatedChange = (awardAmountInfo.getAnticipatedChange() != null ? Constants.DOLLAR_SYMBOL + awardAmountInfo.getAnticipatedChange() + "" : "");
					datesAndAmountOverviewDetail.add(new AwardDateAndAmountPrintParameter(transactionType, proposalNumber, sourceAwardNumber, destinationAwardNumber, noticeDate, obligationStartDate, obligationEndDate, obligationChange, anticipatedChange, comment));
				}
			});
		} else {
            datesAndAmountOverviewDetail.add(new AwardDateAndAmountPrintParameter("", "", "", "", "", "", "", "", "", ""));
		}
		return datesAndAmountOverviewDetail;
	}

	private List<AwardBudgetPeriodSummary> setAwardBudgetSummaryDetails(List<AwardBudgetPeriod> awardBudgetPeriods, IContext context) {
		Set<String> budgetCategoryCodes = new HashSet<>();
		Set<String> costElements = new HashSet<>();
		List<AwardBudgetSummaryVO> awardBudgetSummaryVOs = new ArrayList<>();
		List<AwardBudgetPeriodSummary> awardBudgetPeriodSummaries = new ArrayList<>();
		List<AwardBudgetDetail> awardBudgetDetails = new ArrayList<>();
		List<Integer> awardBudgetPeriodIds = new ArrayList<>();
		BigDecimal periodTotalSum = BigDecimal.ZERO;
		if (awardBudgetPeriods != null && !awardBudgetPeriods.isEmpty()) {
			periodTotalSum = prepareAwardBudgetSummaryDetails(awardBudgetPeriodSummaries, awardBudgetSummaryVOs, awardBudgetPeriods, awardBudgetPeriodIds, awardBudgetDetails, costElements, budgetCategoryCodes, periodTotalSum);
			context.put("awardBudgetSummaryVOs", awardBudgetSummaryVOs);
			context.put("PERIOD_TOTAL_SUM", Constants.DOLLAR_SYMBOL + decimalFormat.format(periodTotalSum));
		} else {
			AwardBudgetPeriodSummary awardBudgetPeriodSummary = new AwardBudgetPeriodSummary();
			awardBudgetPeriodSummary.setBudgetCategory("");
			awardBudgetPeriodSummary.setTotalLineItemCostSums("");
			AwardBudgetSummaryVO awardBudgetSummaryVO = new AwardBudgetSummaryVO();
			awardBudgetSummaryVO.setLineItemCosts("");
			awardBudgetSummaryVOs.add(awardBudgetSummaryVO);
			awardBudgetPeriodSummary.setBudgetSummaryVOs(awardBudgetSummaryVOs);
			context.put("PERIOD_TOTAL_SUM", "");
			awardBudgetPeriodSummaries.add(awardBudgetPeriodSummary);
			context.put("awardBudgetSummaryVOs", awardBudgetSummaryVOs);
		}
		return awardBudgetPeriodSummaries;
	}

	private BigDecimal prepareAwardBudgetSummaryDetails(List<AwardBudgetPeriodSummary> awardBudgetPeriodSummaries, List<AwardBudgetSummaryVO> awardBudgetSummaryVOs, List<AwardBudgetPeriod> awardBudgetPeriods, List<Integer> awardBudgetPeriodIds, List<AwardBudgetDetail> awardBudgetDetails, Set<String> costElements, Set<String> budgetCategoryCodes, BigDecimal periodTotalSum) {
		for (AwardBudgetPeriod awardBudgetPeriod : awardBudgetPeriods) {
			awardBudgetPeriodIds.add(awardBudgetPeriod.getBudgetPeriodId());
			awardBudgetDetails.addAll(awardBudgetPeriod.getBudgetDetails());
			AwardBudgetSummaryVO budgetSummaryVO = new AwardBudgetSummaryVO();
			budgetSummaryVO.setPeriodNumber(awardBudgetPeriod.getBudgetPeriod());
			budgetSummaryVO.setLineItemCost(awardBudgetPeriod.getTotalCost());
			budgetSummaryVO.setLineItemCosts(decimalFormat.format(awardBudgetPeriod.getTotalCost()));
			budgetSummaryVO.setLineItemCostValue(decimalFormat.format(awardBudgetPeriod.getTotalCost()));
			awardBudgetSummaryVOs.add(budgetSummaryVO);
		}
		for (AwardBudgetDetail awardBudgetDetail : awardBudgetDetails) {
			if (awardBudgetDetail.getBudgetCategoryCode().equals(Constants.BUDGET_CATEGORY_TYPE_CODE_SYS_GENERATED_COST)) {
				costElements.add(awardBudgetDetail.getCostElementCode());
			} else {
				budgetCategoryCodes.add(awardBudgetDetail.getBudgetCategoryCode());
			}
		}
		for (String budgetCategoryCode : budgetCategoryCodes) {
			AwardBudgetPeriodSummary awardBudgetPeriodSummary = new AwardBudgetPeriodSummary();
			BigDecimal totalLineItemCost;
			BigDecimal totalLineItemCostSum = BigDecimal.ZERO;
			List<AwardBudgetSummaryVO> totalLineItemCosts = new ArrayList<>();
			BudgetCategory budgetCategory = proposalBudgetDao.fetchBudgetCategoryBasedOnCode(budgetCategoryCode);
			awardBudgetPeriodSummary.setBudgetCategory(budgetCategory.getDescription());
			awardBudgetPeriodSummary.setSortOrder(budgetCategory.getSortOrder());
			for (AwardBudgetPeriod awardBudgetPeriod : awardBudgetPeriods) {
				totalLineItemCost = BigDecimal.ZERO;
				for (AwardBudgetDetail awardBudgetDetail : awardBudgetDetails) {
					if (budgetCategoryCode.equals(awardBudgetDetail.getBudgetCategoryCode()) && (awardBudgetPeriod.getBudgetPeriodId().equals(awardBudgetDetail.getBudgetPeriodId()))
							&& !budgetCategoryCode.equals(Constants.BUDGET_CATEGORY_TYPE_CODE_SYS_GENERATED_COST)) {
						totalLineItemCost = totalLineItemCost.add(awardBudgetDetail.getLineItemCost());
					}
				}
				totalLineItemCostSum = totalLineItemCostSum.add(totalLineItemCost);
				AwardBudgetSummaryVO budgetSummaryVO = new AwardBudgetSummaryVO();
				budgetSummaryVO.setPeriodNumber(awardBudgetPeriod.getBudgetPeriod());
				budgetSummaryVO.setLineItemCost(awardBudgetPeriod.getTotalCost());
				budgetSummaryVO.setLineItemCosts(decimalFormat.format(totalLineItemCost));
				budgetSummaryVO.setLineItemCostValue(decimalFormat.format(totalLineItemCost));
				totalLineItemCosts.add(budgetSummaryVO);
			}
			periodTotalSum = periodTotalSum.add(totalLineItemCostSum);
			awardBudgetPeriodSummary.setBudgetSummaryVOs(totalLineItemCosts);
			awardBudgetPeriodSummary.setTotalLineItemCostSum(totalLineItemCostSum);
			awardBudgetPeriodSummary.setTotalLineItemCostSums(decimalFormat.format(totalLineItemCostSum));
			awardBudgetPeriodSummary.setTotalFundRequestedCostSum(totalLineItemCostSum);
			awardBudgetPeriodSummary.setTotalLineItemCost(decimalFormat.format(totalLineItemCostSum));
			awardBudgetPeriodSummary.setTotalFundRequestedCost(decimalFormat.format(totalLineItemCostSum));
			awardBudgetPeriodSummaries.add(awardBudgetPeriodSummary);
		}
		for (String costElement : costElements) {
			AwardBudgetPeriodSummary awardBudgetPeriodSummary = new AwardBudgetPeriodSummary();
			BigDecimal totalLineItemCost;
			BigDecimal totalLineItemCostSum = BigDecimal.ZERO;
			List<AwardBudgetSummaryVO> totalLineItemCosts = new ArrayList<>();
			awardBudgetPeriodSummary.setBudgetCategory(proposalBudgetDao.fetchCostElementName(costElement));
			for (AwardBudgetPeriod awardBudgetPeriod : awardBudgetPeriods) {
				totalLineItemCost = BigDecimal.ZERO;
				for (AwardBudgetDetail awardBudgetDetail : awardBudgetDetails) {
					if (costElement.equals(awardBudgetDetail.getCostElementCode())
							&& (awardBudgetPeriod.getBudgetPeriodId().equals(awardBudgetDetail.getBudgetPeriodId()))
							&& awardBudgetDetail.getBudgetCategoryCode().equals(Constants.BUDGET_CATEGORY_TYPE_CODE_SYS_GENERATED_COST)) {
						totalLineItemCost = totalLineItemCost.add(awardBudgetDetail.getLineItemCost());
					}
				}
				totalLineItemCostSum = totalLineItemCostSum.add(totalLineItemCost);
				AwardBudgetSummaryVO awardBudgetSummaryVO = new AwardBudgetSummaryVO();
				awardBudgetSummaryVO.setPeriodNumber(awardBudgetPeriod.getBudgetPeriod());
				awardBudgetSummaryVO.setLineItemCost(totalLineItemCost);
				awardBudgetSummaryVO.setLineItemCosts(decimalFormat.format(totalLineItemCost));
				awardBudgetSummaryVO.setLineItemCostValue(decimalFormat.format(totalLineItemCost));
				totalLineItemCosts.add(awardBudgetSummaryVO);
			}
			periodTotalSum = periodTotalSum.add(totalLineItemCostSum);
			awardBudgetPeriodSummary.setBudgetSummaryVOs(totalLineItemCosts);
			awardBudgetPeriodSummary.setTotalLineItemCostSum(totalLineItemCostSum);
			awardBudgetPeriodSummary.setTotalLineItemCostSums(decimalFormat.format(totalLineItemCostSum));
			awardBudgetPeriodSummary.setTotalFundRequestedCostSum(totalLineItemCostSum);
			awardBudgetPeriodSummary.setTotalLineItemCost(decimalFormat.format(totalLineItemCostSum));
			awardBudgetPeriodSummary.setTotalFundRequestedCost(decimalFormat.format(totalLineItemCostSum));
			awardBudgetPeriodSummaries.add(awardBudgetPeriodSummary);
		}
		if (awardBudgetPeriodSummaries != null && !awardBudgetPeriodSummaries.isEmpty()) {
			Collections.sort(awardBudgetPeriodSummaries, new AwardBudgetSummaryComparatorByBudgetCategoryName());
		}
		return periodTotalSum;
	}

	private List<AwardPrintParameter> setAwardFundingProposalDetails(Integer awardId) {
		List<AwardFundingProposal> awardFundingProposals = awardDao.getAwardFundingProposals(awardId);
		// Set initial capacity of array list based on size of award funding proposal
		// list
		List<AwardPrintParameter> awardFundingProposalList = new ArrayList<>(
				awardFundingProposals.size() == 0 ? 1 : awardFundingProposals.size());
		if (!awardFundingProposals.isEmpty()) {
			for (AwardFundingProposal awardFundingProposal : awardFundingProposals) {
				addAwardFundingProposal(awardFundingProposal, awardFundingProposalList);
			}
		} else {
			addAwardFundingProposal(null, awardFundingProposalList);
		}
		return awardFundingProposalList;
	}

	private List<AwardPrintParameter> setAwardPersonDetails(Award award) {
		List<AwardPerson> awardPersons = award.getAwardPersons();
		// Set initial capacity of array list based on size of award person list
		List<AwardPrintParameter> awardPersonList = new ArrayList<>(awardPersons.size() == 0 ? 1 : awardPersons.size());
		if (!awardPersons.isEmpty()) {
			for (AwardPerson awardPerson : awardPersons) {
				addAwardPerson(awardPerson, awardPersonList);
			}
		} else {
			addAwardPerson(null, awardPersonList);
		}
		return awardPersonList;
	}

	private List<AwardPrintParameter> setAwardProjectDetails(Integer awardId) {
		List<AwardProjectTeam> awardProjectTeams = awardDao.getAwardProjectTeamList(awardId);
		// Set initial capacity of array list based on size of award project team list
		List<AwardPrintParameter> awardProjectTeamList = new ArrayList<>(
				awardProjectTeams.size() == 0 ? 1 : awardProjectTeams.size());
		if (!awardProjectTeams.isEmpty()) {
			for (AwardProjectTeam awardProjectTeam : awardProjectTeams) {
				addAwardProjectTeam(awardProjectTeam, awardProjectTeamList);
			}
		} else {
			addAwardProjectTeam(null, awardProjectTeamList);
		}
		return awardProjectTeamList;
	}

	private List<AwardPrintParameter> setAwardContactDetails(Integer awardId) {
		List<AwardContact> awardContacts = awardService.prepareAwardContactList(awardId);
		// Set initial capacity of array list based on size of award contacts list
		List<AwardPrintParameter> awardContactList = new ArrayList<>(
				awardContacts.size() == 0 ? 1 : awardContacts.size());
		if (!awardContacts.isEmpty()) {
			for (AwardContact awardContact : awardContacts) {
				addAwardContact(awardContact, awardContactList);
			}
		} else {
			addAwardContact(null, awardContactList);
		}
		return awardContactList;
	}

	private List<AwardPrintParameter> setAwardSpecialReviewDetails(Integer awardId) {
		List<AwardSpecialReview> awardSpecialReviews = awardDao.getAwardSpecialReviewsByAwardId(awardId);
		// Set initial capacity of array list based on size of award special review list
		List<AwardPrintParameter> awardSpecialReviewList = new ArrayList<>(
				awardSpecialReviews.size() == 0 ? 1 : awardSpecialReviews.size());
		if (!awardSpecialReviews.isEmpty()) {
			Map<String, String> nonEmployees = new HashMap<>();
			Map<String, String> persons = new HashMap<>();
			for (AwardSpecialReview awardSpecialReview : awardSpecialReviews) {
				addAwardSpecialReview(persons,nonEmployees, awardSpecialReview, awardSpecialReviewList);
			}
		} else {
			addAwardSpecialReview(null, null, null, awardSpecialReviewList);
		}
		return awardSpecialReviewList;
	}

	private List<AwardPrintParameter> setAwardSubcontractDetails(Integer awardId) {
		List<AwardSubContract> awardSubContracts = awardDao.getSubContractsByAwardId(awardId);
		// Set initial capacity of array list based on size of award sub contract list
		List<AwardPrintParameter> awardSubContractList = new ArrayList<>(
				awardSubContracts.size() == 0 ? 1 : awardSubContracts.size());
		if (!awardSubContracts.isEmpty()) {
			for (AwardSubContract awardSubContract : awardSubContracts) {
				addAwardSubcontract(awardSubContract, awardSubContractList);
			}
		} else {
			addAwardSubcontract(null, awardSubContractList);
		}
		return awardSubContractList;
	}

	private List<AwardPrintParameter> setAwardCostShareDetails(Integer awardId) {
		List<AwardCostShare> awardCostShares = datesAndAmountDao.getCostShareTypesByAwardId(awardId);
		// Set initial capacity of array list based on size of award cost share list
		List<AwardPrintParameter> awardCostShareList = new ArrayList<>(
				awardCostShares.size() == 0 ? 1 : awardCostShares.size());
		if (!awardCostShares.isEmpty()) {
			for (AwardCostShare awardCostShare : awardCostShares) {
				addAwardCostShare(awardCostShare, awardCostShareList);
			}
		} else {
			addAwardCostShare(null, awardCostShareList);
		}
		return awardCostShareList;
	}

	private List<AwardPrintParameter> setAwardMilestone(Integer awardId) {
		List<AwardMileStone> awardMilestones = awardDao.fetchAwardMileStonesBasedOnAwardId(awardId);
		awardMilestones = awardMilestones.stream().sorted(Comparator.comparing(AwardMileStone::getStartDate).reversed().thenComparing(AwardMileStone::getMilestone)).collect(Collectors.toList());
		// Set initial capacity of array list based on size of award Milestone List
		List<AwardPrintParameter> awardPrintParameters = new ArrayList<>(
				awardMilestones.isEmpty() ? 1 : awardMilestones.size());
		if (!awardMilestones.isEmpty()) {
			for (AwardMileStone awardMilestone : awardMilestones) {
				String milestone = (awardMilestone.getMilestone() != null ? awardMilestone.getMilestone() : "");
				String startDate = (awardMilestone.getStartDate() != null
						? commonService.convertDateFormatBasedOnTimeZone(awardMilestone.getStartDate().getTime(),
								Constants.DEFAULT_DATE_FORMAT)
						: "");
				String endDate = (awardMilestone.getEndDate() != null ? commonService.convertDateFormatBasedOnTimeZone(
						awardMilestone.getEndDate().getTime(), Constants.DEFAULT_DATE_FORMAT) : "");
				AwardPrintParameter awardPrintParameter = setAwardMilestone(milestone, startDate, endDate,
						awardMilestone.getDuration());
				awardPrintParameters.add(awardPrintParameter);
			}
		} else {
			awardPrintParameters.add(setAwardMilestone("", "", "", ""));
		}
		return awardPrintParameters;
	}

	private AwardPrintParameter setAwardMilestone(String milestone, String startDate, String endDate, String duration) {
		AwardPrintParameter awardPrintParameter = new AwardPrintParameter();
		awardPrintParameter.setMilestone(milestone);
		awardPrintParameter.setStartDate(startDate);
		awardPrintParameter.setEndDate(endDate);
		awardPrintParameter.setDuration(duration);
		return awardPrintParameter;
	}

	private List<AwardKPIPrintParameter> setAwardKPI(List<AwardKPI> awardKPIs) {
		List<AwardKPI> sortedAwardKPIs = new ArrayList<>();
		awardKPIs.forEach(kpi -> {
			AwardKPI newAwardKpi = new AwardKPI();
			BeanUtils.copyProperties(kpi, newAwardKpi);
			Comparator<AwardKPICriteria> comparator = Comparator.comparing(criteria -> criteria.getKpiCriteriaType().getDescription());
			newAwardKpi.setAwardKPICriterias(kpi.getAwardKPICriterias().stream().sorted(comparator).collect(Collectors.toList()));	
			sortedAwardKPIs.add(newAwardKpi);
		});	
		List<AwardKPIPrintParameter> awardKPIPrintParameters = new ArrayList<>();
		if (sortedAwardKPIs != null && !sortedAwardKPIs.isEmpty()) {
			for (AwardKPI awardKPI : sortedAwardKPIs) {
				AwardKPIPrintParameter awardKPIPrintParameter = new AwardKPIPrintParameter();
				if (awardKPI.getKpiType().getDescription() != null) {
					awardKPIPrintParameter.setKPIType(awardKPI.getKpiType().getDescription());
				}
				List<AwardKPICriteriaType> awardKPICriteriaTypes = new ArrayList<>();
				for (AwardKPICriteria awardKPICriteria : awardKPI.getAwardKPICriterias()) {
					AwardKPICriteriaType awardKPICriteriaType = new AwardKPICriteriaType();
					if (awardKPICriteria.getKpiCriteriaType().getDescription() != null) {
						awardKPICriteriaType.setAwardKPICriteriaType(awardKPICriteria.getKpiCriteriaType().getDescription());
					}
					if (awardKPICriteria.getTarget() != null) {
						awardKPICriteriaType.setTarget(awardKPICriteria.getTarget().toString());
					} else {
						awardKPICriteriaType.setTarget("");
					}
					awardKPICriteriaTypes.add(awardKPICriteriaType);
				}
				awardKPIPrintParameter.setAwardKPICriteriaTypes(awardKPICriteriaTypes);

				awardKPIPrintParameters.add(awardKPIPrintParameter);
			}
		} else {
			AwardKPIPrintParameter awardKPIPrintParameter = new AwardKPIPrintParameter();
			awardKPIPrintParameter.setKPIType("");
			AwardKPICriteriaType awardKPICriteriaType = new AwardKPICriteriaType();
			awardKPICriteriaType.setAwardKPICriteriaType("");
			awardKPICriteriaType.setTarget("");
			List<AwardKPICriteriaType> awardKPICriteriaTypes = new ArrayList<>();
			awardKPICriteriaTypes.add(awardKPICriteriaType);
			awardKPIPrintParameter.setAwardKPICriteriaTypes(awardKPICriteriaTypes);
			awardKPIPrintParameters.add(awardKPIPrintParameter);
		}
		return awardKPIPrintParameters;
	}

	private List<AwardOtherInformations> setOtherInformation(List<CustomData> otherInformations) {
		List<AwardOtherInformations> awardOtherInformations = new ArrayList<>();
		if (otherInformations != null && !otherInformations.isEmpty()) {
			for (CustomData otherInformation : otherInformations) {
				AwardOtherInformations awardOtherInformation = new AwardOtherInformations();
				CustomDataElements questions = customDataElementDao.fetchCustomElementById(otherInformation.getCustomDataElementsId());
				if (otherInformation.getValue() != null) {
                    awardOtherInformation.setOtherInformationanswer(questions.getDataType().equals(Constants.CUSTOM_DATA_TYPE_CHECKBOX) && otherInformation.getValue().equals("false") ? "" : 
                    	(otherInformation.getDescription() == null ? otherInformation.getValue() : otherInformation.getDescription()));
				}
				if (questions.getColumnLabel() != null) {
					awardOtherInformation.setOtherInformationsQuestions(questions.getColumnLabel());
				}
				awardOtherInformations.add(awardOtherInformation);
			}
		}
		return awardOtherInformations;
	}

	private IContext setAwardPlaceHolderData(IContext context, Award award, AwardBudgetHeader awardBudgetHeader) {
		DecimalFormat decimalFormat = new DecimalFormat(Constants.SINGAPORE_NUMBER_FORMAT_WITH_DECIMAL);
		String grantCallTitle = null;
		if (award.getGrantHeaderId() != null) {
			grantCallTitle = grantCallDao.getGrantCallTitleByGrantId(award.getGrantHeaderId());
		}
		context.put("ACCOUNT_NUMBER", award.getAccountNumber() == null ? "" : award.getAccountNumber());
		AccountType accountType = null;
		if (award.getAccountTypeCode() != null) {
			accountType = commonDao.fetchAccountTypeByAccountTypeCode(Integer.parseInt(award.getAccountTypeCode()));
		}
		context.put("ACCOUNT_TYPE", accountType == null ? "" : accountType.getDescription());
		AwardType awardType = null;
		if (award.getAwardTypeCode() != null) {
			awardType = awardDao.fetchAwardTypeByAwardTypeCode(award.getAwardTypeCode());
		}
		context.put(AWARD_TYPE, awardType == null ? "" : awardType.getDescription());
		ActivityType activityType = null;
		if (award.getActivityTypeCode() != null) {
			activityType = commonDao.fetchActivityTypeByActivityTypeCode(award.getActivityTypeCode());
		}
		context.put(ACTIVITY_TYPE, activityType == null ? "" : activityType.getDescription());
		context.put("AWARD_STATUS",
				award.getAwardStatus().getDescription() == null ? "" : award.getAwardStatus().getDescription());
		context.put("AWARD_TITLE", award.getTitle() == null ? "" : award.getTitle());
		context.put(LEAD_UNIT, award.getLeadUnit().getUnitName() == null ? "" : commonService.getUnitFormatByUnitDetail(award.getLeadUnit().getUnitNumber(),
				award.getLeadUnit().getUnitName()));
		context.put(SPONSOR_NAME,
                award.getSponsor() == null ? "" : commonService.getSponsorFormatBySponsorDetail(award.getSponsor().getSponsorCode(), award.getSponsor().getSponsorName(), award.getSponsor().getAcronym()));
		context.put("PRIME_SPONSOR_NAME",
                award.getPrimeSponsor() == null ? "" : commonService.getSponsorFormatBySponsorDetail(award.getPrimeSponsor().getSponsorCode(), award.getPrimeSponsor().getSponsorName(), award.getPrimeSponsor().getAcronym()));
		context.put("PROJECT_START_DATE",
				award.getBeginDate() == null ? ""
						: commonService.convertDateFormatBasedOnTimeZone(award.getBeginDate().getTime(),
								Constants.DEFAULT_DATE_FORMAT));
		context.put("PROJECT_END_DATE",
				award.getFinalExpirationDate() == null ? ""
						: commonService.convertDateFormatBasedOnTimeZone(award.getFinalExpirationDate().getTime(),
								Constants.DEFAULT_DATE_FORMAT));
		context.put("AWARD_EFFECTIVE_DATE",
				award.getAwardEffectiveDate() == null ? ""
						: commonService.convertDateFormatBasedOnTimeZone(award.getAwardEffectiveDate().getTime(),
								Constants.DEFAULT_DATE_FORMAT));
		setAwardKeywords(award, context);
		Double totalSubContractAmount = calculateTotalSubContractAmount(award.getAwardId());
		setTotalCommitmentAndCostShareAmount(award.getAwardId(), context);
		context.put("TOTAL_SUBCONTRACT_AMOUNT",
				Constants.DOLLAR_SYMBOL + decimalFormat.format(totalSubContractAmount).toString());
		context.put("AWARD_DURATION", award.getDuration());
		context.put(AWARD_ID, award.getAwardId().toString());
		context.put(AWARD_NUMBER, award.getAwardNumber());
		context.put("LEAD_PI", award.getPrincipalInvestigator() != null ? award.getPrincipalInvestigator() : "");
		context.put(RESEARCH_DESCRIPTION,
				award.getResearchDescription() == null ? "" : award.getResearchDescription());
		context.put(MULTI_DISCIPLINARY_DESCRIPTION,
				award.getMultiDisciplinaryDescription() == null ? "" : award.getMultiDisciplinaryDescription());
		context.put("SPONSOR_AWARD_NUMBER", award.getSponsorAwardNumber() != null ? award.getSponsorAwardNumber() : "");
		context.put("GRANT_CALL_TITLE", grantCallTitle != null ? grantCallTitle : "");
		if (awardBudgetHeader != null) {
			context.put("TOTAL_COST",
					awardBudgetHeader.getTotalCost() != null
							? Constants.DOLLAR_SYMBOL + decimalFormat.format(awardBudgetHeader.getTotalCost())
							: "");
			context.put("TOTAL_DIRECT_COST",
					awardBudgetHeader.getTotalDirectCost() != null
							? Constants.DOLLAR_SYMBOL + decimalFormat.format(awardBudgetHeader.getTotalDirectCost())
							: "");
			context.put("TOTAL_INDIRECT_COST",
					awardBudgetHeader.getTotalIndirectCost() != null
							? Constants.DOLLAR_SYMBOL + decimalFormat.format(awardBudgetHeader.getTotalIndirectCost())
							: "");
			context.put("BUDGET_STATUS",
					awardBudgetHeader.getBudgetStatus() != null ? awardBudgetHeader.getBudgetStatus().getDescription()
							: "");
			context.put("OVER_HEAD_RATE_TYPE",
					awardBudgetHeader.getRateType() != null ? awardBudgetHeader.getRateType().getDescription() : "");
			if (awardBudgetHeader.getOnOffCampusFlag() != null) {
				if (awardBudgetHeader.getOnOffCampusFlag().equals("N")) {
					context.put("ON_OFF_CAMPUS_FLAG", "OFF" + "");
				} else if (awardBudgetHeader.getOnOffCampusFlag().equals("Y")) {
					context.put("ON_OFF_CAMPUS_FLAG", "ON" + "");
				} else if (awardBudgetHeader.getOnOffCampusFlag().equals("D")) {
					context.put("ON_OFF_CAMPUS_FLAG", "BOTH" + "");
				}
			} else {
				context.put("ON_OFF_CAMPUS_FLAG", "");
			}
			context.put("DESCRIPTION", awardBudgetHeader.getComments() != null ? awardBudgetHeader.getComments() : "");
			context.put("OVERHEAD_RATE_TYPE",
					awardBudgetHeader.getRateType() != null ? awardBudgetHeader.getRateType().getDescription() : "");
			context.put("VERSION_NUMBER",
					awardBudgetHeader.getVersionNumber() != null ? awardBudgetHeader.getVersionNumber().toString()
							: "");
			context.put("START_DATE",
					awardBudgetHeader.getStartDate() != null ? commonService.convertDateFormatBasedOnTimeZone(
							awardBudgetHeader.getStartDate().getTime(), Constants.DEFAULT_DATE_FORMAT) : "");
			context.put("END_DATE",
					awardBudgetHeader.getEndDate() != null ? commonService.convertDateFormatBasedOnTimeZone(
							awardBudgetHeader.getEndDate().getTime(), Constants.DEFAULT_DATE_FORMAT) : "");
			context.put("FUND_CODE", awardBudgetHeader.getFundCode() != null ? awardBudgetHeader.getFundCode() : "");
			context.put("FUND_CENTRE",
					awardBudgetHeader.getFundCenter() != null ? awardBudgetHeader.getFundCenter() : "");
			context.put("BUDGET_TYPE",
					awardBudgetHeader.getBudgetType() != null ? awardBudgetHeader.getBudgetType().getDescription()
							: "");
			if (awardBudgetHeader.getBudgetTypeCode().equals(Constants.BUDGET_TYPE_REBUDGET)) {
				context.put("IS_BUDGET_VARIATION", true);
			} else {
				context.put("IS_BUDGET_VARIATION", false);
			}
			AwardAmountInfo awardAmountInfo = datesAndAmountDao.getLatestActiveAwardAmountInfo(award.getAwardNumber());
			if (awardAmountInfo != null) {
				BigDecimal availableFund = BigDecimal.ZERO;
				if (awardBudgetHeader.getTotalCost() != null) {
					availableFund = awardAmountInfo.getObliDistributableAmount()
							.subtract(awardBudgetHeader.getTotalCost());
				} else {
					availableFund = awardAmountInfo.getObliDistributableAmount();
				}
				context.put("AVAILABLE_FUND", Constants.DOLLAR_SYMBOL + decimalFormat.format(availableFund));
			} else {
				context.put("AVAILABLE_FUND", "");
			}
			context.put("SHOW_BUDGET_OH_RATE_PERCENTAGE", commonDao.getParameterValueAsBoolean(Constants.SHOW_BUDGET_OH_RATE_PERCENTAGE));
			context.put("ON_CAMPUS_RATE", awardBudgetHeader.getOnCampusRates() != null ? awardBudgetHeader.getOnCampusRates() : "");
			context.put("OFF_CAMPUS_RATE", awardBudgetHeader.getOffCampusRates() != null ? awardBudgetHeader.getOffCampusRates() : "");
		} else {
			context.put("TOTAL_COST", "");
			context.put("TOTAL_DIRECT_COST", "");
			context.put("TOTAL_INDIRECT_COST", "");
			context.put("BUDGET_STATUS", "");
			context.put("OVER_HEAD_RATE_TYPE", "");
			context.put("ON_OFF_CAMPUS_FLAG", "");
			context.put("DESCRIPTION", "");
			context.put("VERSION_NUMBER", "");
			context.put("START_DATE", "");
			context.put("END_DATE", "");
			context.put("FUND_CODE", "");
			context.put("FUND_CENTRE", "");
			context.put("BUDGET_TYPE", "");
			context.put("IS_BUDGET_VARIATION", false);
			context.put("AVAILABLE_FUND", "");
			context.put("SHOW_BUDGET_OH_RATE_PERCENTAGE", false);
			context.put("ON_CAMPUS_RATE", "");
			context.put("OFF_CAMPUS_RATE", "");
		}
		return context;
	}

	private void setAwardKeywords(Award award, IContext context) {
		// Set constructor argument of StringBuffer as 0 so that its initial capacity is
		// set to 0.
		StringBuffer keywords = new StringBuffer(0);
		if (!award.getAwardKeywords().isEmpty()) {
			for (AwardKeyword awardKeyword : award.getAwardKeywords()) {
				if (award.getAwardKeywords().size() != award.getAwardKeywords().indexOf(awardKeyword) + 1) {
					if (awardKeyword.getScienceKeyword() != null) {
						keywords.append(awardKeyword.getScienceKeyword().getDescription() + ", ");
					} else if (awardKeyword.getKeyword() != null) {
						keywords.append(awardKeyword.getKeyword() + ", ");
					}
				} else {
					if (awardKeyword.getScienceKeyword() != null) {
						keywords.append(awardKeyword.getScienceKeyword().getDescription());
					} else if (awardKeyword.getKeyword() != null) {
						keywords.append(awardKeyword.getKeyword());
					}
				}
			}
			context.put("AWARD_KEYWORDS", keywords.toString());
		} else {
			context.put("AWARD_KEYWORDS", "");
		}
	}

	private void setAwardPersonUnits(AwardPerson awardPerson, AwardPrintParameter awardPersonData) {
		StringBuffer units = new StringBuffer(0);
		if (awardPerson != null) {
			if (!awardPerson.getAwardPersonUnits().isEmpty()) {
				for (AwardPersonUnit unit : awardPerson.getAwardPersonUnits()) {
					if (awardPerson.getAwardPersonUnits().size() - 1 < awardPerson.getAwardPersonUnits()
							.indexOf(unit)) {
						units.append(unit.getUnit().getUnitDetail() + " , ");
					} else {
						units.append(unit.getUnit().getUnitDetail());
					}
				}
			} else {
				units.append("");
			}
		} else {
			units.append("");
		}
		awardPersonData.setAwardPersonUnits(units.toString());
	}

	private void addAwardFundingProposal(AwardFundingProposal awardFundingProposal, List<AwardPrintParameter> awardFundingProposalList) {
		AwardPrintParameter awardFundingProposalData = new AwardPrintParameter();
		if (awardFundingProposal != null) {
			InstituteProposal proposal = awardFundingProposal.getProposal();
			awardFundingProposalData.setFundingProposalNumber(proposal.getProposalNumber() == null ? "" : proposal.getProposalNumber());
			awardFundingProposalData.setFundingProposalTitle(proposal.getTitle() == null ? "" : proposal.getTitle());
			awardFundingProposalData.setFundingProposalLeadUnit(proposal.getHomeUnitName() == null ? "" : proposal.getHomeUnitName());
            awardFundingProposalData.setFundingProposalSponsor(proposal.getSponsor() == null ? "" : commonService.getSponsorFormatBySponsorDetail(proposal.getSponsor().getSponsorCode(), proposal.getSponsor().getSponsorName(), proposal.getSponsor().getAcronym()));
			InstituteProposalStatus instituteProposalStatus = proposal.getInstProposalStatus();
			awardFundingProposalData.setFundingProposalStatus(instituteProposalStatus == null ? "" : instituteProposalStatus.getDescription());
		} else {
			awardFundingProposalData.setFundingProposalNumber("");
			awardFundingProposalData.setFundingProposalTitle("");
			awardFundingProposalData.setFundingProposalLeadUnit("");
			awardFundingProposalData.setFundingProposalSponsor("");
			awardFundingProposalData.setFundingProposalStatus("");
		}
		awardFundingProposalList.add(awardFundingProposalData);
	}

	private void addAwardPerson(AwardPerson awardPerson, List<AwardPrintParameter> awardPersonList) {
		AwardPrintParameter awardPersonData = new AwardPrintParameter();
		if (awardPerson != null) {
			awardPersonData.setAwardPersonName(awardPerson.getFullName() == null ? "" : awardPerson.getFullName());
			String projectRole = Boolean.TRUE.equals(awardPerson.getProposalPersonRole().getShowProjectRole()) && awardPerson.getProjectRole() != null ?  " (" + awardPerson.getProjectRole() + ")" : "";
			awardPersonData.setAwardPersonRole(awardPerson.getProposalPersonRole() == null ? "" : awardPerson.getProposalPersonRole().getDescription().concat(projectRole));
			awardPersonData.setProjectRole(Boolean.TRUE.equals(awardPerson.getProposalPersonRole().getShowProjectRole()) && awardPerson.getProjectRole() != null ? awardPerson.getProjectRole() : "");
			awardPersonData.setAwardPersonPercentageOfEffort(awardPerson.getPercentageEffort() == null ? "" : awardPerson.getPercentageEffort().toString());
			awardPersonData.setAwardPersonDesignation(awardPerson.getDesignation() == null ? "" : awardPerson.getDesignation());
			setAwardPersonUnits(awardPerson, awardPersonData);
			if (awardPerson.getPersonId() != null) {
				awardPersonData.setAwardPersonMemberType(EMPLOYEE);
			} else {
				awardPersonData.setAwardPersonMemberType(NON_EMPLOYEE);
			}
		} else {
			awardPersonData.setAwardPersonName("");
			awardPersonData.setAwardPersonRole("");
			awardPersonData.setProjectRole("");
			awardPersonData.setAwardPersonPercentageOfEffort("");
			awardPersonData.setAwardPersonDesignation("");
			setAwardPersonUnits(null, awardPersonData);
			awardPersonData.setAwardPersonMemberType("");
		}
		awardPersonList.add(awardPersonData);
	}

	private void addAwardProjectTeam(AwardProjectTeam awardProjectTeam, List<AwardPrintParameter> awardProjectTeamList) {
		AwardPrintParameter awardProjectTeamData = new AwardPrintParameter();
		if (awardProjectTeam != null) {
			if (!awardProjectTeam.getNonEmployeeFlag()) {
				awardProjectTeamData.setProjectTeamMemberType(EMPLOYEE);
			} else {
				awardProjectTeamData.setProjectTeamMemberType(NON_EMPLOYEE);
			}
			awardProjectTeamData.setProjectTeamMemberName(
					awardProjectTeam.getFullName() == null ? "" : awardProjectTeam.getFullName());
			awardProjectTeamData.setProjectTeamMemberRole(
					awardProjectTeam.getProjectRole() == null ? "" : awardProjectTeam.getProjectRole());
			awardProjectTeamData.setProjectStartDate(awardProjectTeam.getStartDate() == null ? ""
					: commonService.convertDateFormatBasedOnTimeZone(awardProjectTeam.getStartDate().getTime(),
							Constants.DEFAULT_DATE_FORMAT));
			awardProjectTeamData.setProjectEndDate(awardProjectTeam.getEndDate() == null ? ""
					: commonService.convertDateFormatBasedOnTimeZone(awardProjectTeam.getEndDate().getTime(),
							Constants.DEFAULT_DATE_FORMAT));
			awardProjectTeamData.setPercentageCharged(awardProjectTeam.getPercentageCharged() == null ? ""
					: awardProjectTeam.getPercentageCharged().toString());
			awardProjectTeamData.setIsProjectActive(awardProjectTeam.getIsActive() == true ? "Yes" : "No");
			awardProjectTeamData.setProjectTeamMemberDesignation(
					awardProjectTeam.getDesignation() == null ? "" : awardProjectTeam.getDesignation());
		} else {
			awardProjectTeamData.setProjectTeamMemberType("");
			awardProjectTeamData.setProjectTeamMemberName("");
			awardProjectTeamData.setProjectTeamMemberRole("");
			awardProjectTeamData.setProjectStartDate("");
			awardProjectTeamData.setProjectEndDate("");
			awardProjectTeamData.setPercentageCharged("");
			awardProjectTeamData.setIsProjectActive("");
			awardProjectTeamData.setProjectTeamMemberDesignation("");
		}
		awardProjectTeamList.add(awardProjectTeamData);
	}

	private void addAwardContact(AwardContact awardContact, List<AwardPrintParameter> awardContactList) {
		AwardPrintParameter awardContactData = new AwardPrintParameter();
		if (awardContact != null) {
			awardContactData.setContactPersonName(awardContact.getFullName() == null ? "" : awardContact.getFullName());
			AwardContactType awardContactType = awardContact.getAwardContactType();
			awardContactData.setContactType(awardContactType == null ? "" : awardContactType.getDescription());
			awardContactData.setContactPersonEmail(
					awardContact.getEmailAddress() == null ? "" : awardContact.getEmailAddress());
			awardContactData
					.setContactPersonPhone(awardContact.getPhoneNumber() == null ? "" : awardContact.getPhoneNumber());
			awardContactData.setContactPersonDesignation(
					awardContact.getDesignation() == null ? "" : awardContact.getDesignation());
		} else {
			awardContactData.setContactPersonName("");
			awardContactData.setContactType("");
			awardContactData.setContactPersonEmail("");
			awardContactData.setContactPersonPhone("");
			awardContactData.setContactPersonDesignation("");
		}
		awardContactList.add(awardContactData);
	}

	private void addAwardSpecialReview(Map<String, String> persons, Map<String, String> nonEmployees, AwardSpecialReview awardSpecialReview, List<AwardPrintParameter> awardSpecialReviewList) {
		AwardPrintParameter awardContactData = new AwardPrintParameter();
		if (awardSpecialReview != null) {
			SpecialReviewType specialReviewType = awardSpecialReview.getSpecialReview();
			awardContactData.setSpecialReviewType(specialReviewType == null ? "" : specialReviewType.getDescription());
			awardSpecialReview = awardService.setIntegratedAwardSpecialReviews(persons, nonEmployees, awardSpecialReview);
			SpecialReviewApprovalType specialReviewApprovalType = commonDao
					.fetchSpecialReviewApprovalTypeByTypeCode(awardSpecialReview.getApprovalTypeCode().toString());
			awardContactData.setSpecialReviewApprovalStatus(
					specialReviewApprovalType == null ? "" : specialReviewApprovalType.getDescription());
			awardContactData.setSpecialReviewProtocalNumber(
					awardSpecialReview.getProtocolNumber() == null ? "" : awardSpecialReview.getProtocolNumber());
			awardContactData.setSpecialReviewApplicationDate(awardSpecialReview.getApplicationDate() == null ? ""
					: commonService.convertDateFormatBasedOnTimeZone(awardSpecialReview.getApplicationDate().getTime(),
							Constants.DEFAULT_DATE_FORMAT));
			awardContactData.setSpecialReviewApprovalDate(awardSpecialReview.getApprovalDate() == null ? ""
					: commonService.convertDateFormatBasedOnTimeZone(awardSpecialReview.getApprovalDate().getTime(),
							Constants.DEFAULT_DATE_FORMAT));
			awardContactData.setSpecialReviewExpirationDate(awardSpecialReview.getExpirationDate() == null ? ""
					: commonService.convertDateFormatBasedOnTimeZone(awardSpecialReview.getExpirationDate().getTime(),
							Constants.DEFAULT_DATE_FORMAT));
			awardContactData.setSpecialReviewComment(
					awardSpecialReview.getComments() == null ? "" : awardSpecialReview.getComments());
		} else {
			awardContactData.setSpecialReviewType("");
			awardContactData.setSpecialReviewApprovalStatus("");
			awardContactData.setSpecialReviewProtocalNumber("");
			awardContactData.setSpecialReviewApplicationDate("");
			awardContactData.setSpecialReviewApprovalDate("");
			awardContactData.setSpecialReviewExpirationDate("");
			awardContactData.setSpecialReviewComment("");
		}
		awardSpecialReviewList.add(awardContactData);
	}

	private void addAwardSubcontract(AwardSubContract awardSubContract, List<AwardPrintParameter> awardSubContractList) {
		DecimalFormat decimalFormat = new DecimalFormat(Constants.SINGAPORE_NUMBER_FORMAT_WITH_DECIMAL);
		AwardPrintParameter awardSubContractData = new AwardPrintParameter();
		if (awardSubContract != null) {
			Organization organization = awardSubContract.getOrganization();
			awardSubContractData
					.setSubContractOrganisationName(organization == null ? "" : organization.getOrganizationName());
			awardSubContractData.setSubContractAmount(awardSubContract.getAmount() == null ? ""
					: Constants.DOLLAR_SYMBOL + decimalFormat.format(awardSubContract.getAmount()).toString());
		} else {
			awardSubContractData.setSubContractOrganisationName("");
			awardSubContractData.setSubContractAmount("");
		}
		awardSubContractList.add(awardSubContractData);
	}

	private void addAwardCostShare(AwardCostShare awardCostShare, List<AwardPrintParameter> awardCostShareList) {
		DecimalFormat decimalFormat = new DecimalFormat(Constants.SINGAPORE_NUMBER_FORMAT_WITH_DECIMAL);
		AwardPrintParameter awardCostShareData = new AwardPrintParameter();
		if (awardCostShare != null) {
			CostShareType costShareType = commonDao
					.fetchCostShareTypeByCostShareTypeCode(awardCostShare.getCostShareTypeCode());
			awardCostShareData.setCostShareType(costShareType == null ? "" : costShareType.getDescription());
			awardCostShareData.setCostShareSource(awardCostShare.getSource() == null ? "" : awardCostShare.getSource());
			awardCostShareData.setCostShareDestination(awardCostShare.getDestination() == null ? "" : awardCostShare.getDestination());
			awardCostShareData.setCostShareVerificationDate(awardCostShare.getVerificationDate() == null ? "" : commonService.convertDateFormatBasedOnTimeZone(awardCostShare.getVerificationDate().getTime(), Constants.DEFAULT_DATE_FORMAT));
			awardCostShareData.setCostSharePercentage(awardCostShare.getCostSharePercentage() == null ? "" : awardCostShare.getCostSharePercentage().toString());
			awardCostShareData.setCostShareCommitmentAmount(awardCostShare.getCommitmentAmount() == null ? "" : Constants.DOLLAR_SYMBOL + decimalFormat.format(awardCostShare.getCommitmentAmount()).toString());
			awardCostShareData.setCostShareMetAmount(awardCostShare.getCostShareMet() == null ? "" : Constants.DOLLAR_SYMBOL + decimalFormat.format(awardCostShare.getCostShareMet()).toString());
			awardCostShareData.setCostShareFiscalYear(awardCostShare.getProjectPeriod() == null ? "" : awardCostShare.getProjectPeriod());
		} else {
			awardCostShareData.setCostShareType("");
			awardCostShareData.setCostShareSource("");
			awardCostShareData.setCostShareDestination("");
			awardCostShareData.setCostShareVerificationDate("");
			awardCostShareData.setCostSharePercentage("");
			awardCostShareData.setCostShareCommitmentAmount("");
			awardCostShareData.setCostShareMetAmount("");
			awardCostShareData.setCostShareFiscalYear("");
		}
		awardCostShareList.add(awardCostShareData);
	}

	private Double calculateTotalSubContractAmount(Integer awardId) {
		Double totalSubContractAmount = 0.0;
		List<AwardSubContract> awardSubContracts = awardDao.getSubContractsByAwardId(awardId);
		if (!awardSubContracts.isEmpty()) {
			for (AwardSubContract awardSubContract : awardSubContracts) {
				if (awardSubContract.getAmount() != null) {
					totalSubContractAmount = totalSubContractAmount + awardSubContract.getAmount();
				}
			}
		}
		return totalSubContractAmount;
	}

	private void setTotalCommitmentAndCostShareAmount(Integer awardId, IContext context) {
		DecimalFormat decimalFormat = new DecimalFormat(Constants.SINGAPORE_NUMBER_FORMAT_WITH_DECIMAL);
		List<AwardCostShare> awardCostShares = datesAndAmountDao.getCostShareTypesByAwardId(awardId);
		BigDecimal totalCommitmentAmount = BigDecimal.ZERO;
		BigDecimal totalCostShareAmountMet = BigDecimal.ZERO;
		if (!awardCostShares.isEmpty()) {
			for (AwardCostShare awardCostShare : awardCostShares) {
				if (awardCostShare.getCommitmentAmount() != null) {
					totalCommitmentAmount = totalCommitmentAmount.add(awardCostShare.getCommitmentAmount());
				}
				if (awardCostShare.getCostShareMet() != null) {
					totalCostShareAmountMet = totalCostShareAmountMet.add(awardCostShare.getCostShareMet());
				}
			}
		}
		context.put("TOTAL_COMMITMENT_AMOUNT",
				Constants.DOLLAR_SYMBOL + decimalFormat.format(totalCommitmentAmount).toString());
		context.put("TOTAL_COSTSHARE_AMOUNT",
				Constants.DOLLAR_SYMBOL + decimalFormat.format(totalCostShareAmountMet).toString());
	}

	@Override
	public ResponseEntity<byte[]> generateNotifyAwardReport(HttpServletResponse response, Integer awardId) {
		ResponseEntity<byte[]> attachmentData = null;
		try {
			byte[] bFile = getTemplateData(Constants.AWARD_NOTICE_LETTER_TEMPLATE_TYPE_CODE);
			byte[] mergedOutput = mergePlaceHoldersOfNotifyAward(bFile, awardId);
			String generatedFileName = RESULT + System.nanoTime() + ".pdf";
			setHttpHeaderAndHttpResponseData(response, generatedFileName, mergedOutput, attachmentData);
		} catch (Exception e) {
			logger.error("Exception in generateNotifyAwardReport : {}", e.getMessage());
		}
		return attachmentData;
	}

	@Override
	public byte[] mergePlaceHoldersOfNotifyAward(byte[] data, Integer awardId) {
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		try {
			InputStream myInputStream = new ByteArrayInputStream(data);
			IXDocReport report = XDocReportRegistry.getRegistry().loadReport(myInputStream,
					TemplateEngineKind.Velocity);
			Award award = awardDao.fetchAwardByAwardId(awardId.toString());

			IContext context = report.createContext();
			context = setNotifyAwardPlaceHolderData(context, award);

			Options options = Options.getTo(ConverterTypeTo.PDF).via(ConverterTypeVia.XWPF);
			report.convert(context, options, baos);

		} catch (Exception e) {
			e.printStackTrace();
			logger.error("Exception in mergePlaceHoldersOfNotifyAward : {}", e.getMessage());
		}
		return baos.toByteArray();
	}

	private IContext setNotifyAwardPlaceHolderData(IContext context, Award award) {
		List<AwardFundingProposal> awardFundingProposals = awardDao.getAwardFundingProposals(award.getAwardId());
		if (!awardFundingProposals.isEmpty()) {
			AwardFundingProposal awardFundingProposal = awardFundingProposals.get(0);
			Proposal proposal = proposalDao.fetchProposalById(awardDao.fetchDevProposalByInstProposal(awardFundingProposal.getProposalId()));
			proposal.setProposalPersons(proposalModuleDao.fetchProposalPersonBasedOnProposalId(proposal.getProposalId()));
			context.put("CURRENT_DATE",commonService.convertDateFormatBasedOnTimeZone(commonDao.getCurrentTimestamp().getTime(), Constants.LOA_DATE_FORMAT));
			context.put("PI_NAME", proposal.getInvestigator().getFullName());
			context.put("PI_DESIGNATION", proposal.getInvestigator().getDesignation() == null ? "" : proposal.getInvestigator().getDesignation());
			//context.put("PI_DEPARTMENT", proposal.getHomeUnitName() == null ? "" : proposal.getHomeUnitName());
			if (proposal.getInvestigator().getPersonId() != null) {
				Person piPerson = personDao.getPersonDetailById(proposal.getInvestigator().getPersonId());
				context.put("PI_DEPARTMENT", piPerson.getUnit() == null ? "" : piPerson.getUnit().getUnitName());
			} else {
				Rolodex rolodexPerson = rolodexDao.getRolodexDetailById(proposal.getInvestigator().getRolodexId());
				context.put("PI_DEPARTMENT", rolodexPerson.getOrganizations() == null ? "" : rolodexPerson.getOrganizations().getOrganizationName());
			}
			context.put("AWARD_TITLE", award.getTitle() == null ? "" : award.getTitle());
			context.put("AWARD_NUMBER", award.getAwardNumber() == null ? "" : award.getAwardNumber());
			context.put("APPLICATION_ID", proposal.getApplicationId() == null ? "" : proposal.getApplicationId());
			context.put("PROPOSAL_START_DATE", proposal.getStartDate() == null ? "" : commonService.convertDateFormatBasedOnTimeZone(proposal.getStartDate().getTime(), Constants.LOA_DATE_FORMAT));
			context.put("PROPOSAL_DURATION", proposal.getDuration() == null ? "" : proposal.getDuration());
			BudgetHeader budgetHeader = null;
			List<BudgetHeader> budgetHeaders = budgetDao.fetchBudgetsByProposalId(proposal.getProposalId());
			if (budgetHeaders != null && !budgetHeaders.isEmpty()) {
				for (BudgetHeader budgetHeaderData : budgetHeaders) {
					if (budgetHeaderData.getIsApprovedBudget()) {
						budgetHeader = budgetHeaderData;
					}
				}
			}
			if (budgetHeader != null) {
				// DecimalFormat decimalFormat = new DecimalFormat(Constants.SINGAPORE_NUMBER_FORMAT_WITH_DECIMAL); 
				context.put("TOTAL_COST", budgetHeader.getTotalCost() == null ? "" : Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetHeader.getTotalCost()));
			} else {
				context.put("TOTAL_COST", Constants.DOLLAR_SYMBOL +"0.00");
			}
			context.put("CATEGORY", proposal.getActivityType().getDescription());
			List<PersonRoles> grantAdminPersonRoles = personDao.getPersonRoleByUnitNumberAndRoleId(proposal.getHomeUnitNumber(), Constants.GRANT_ADMINISTRATOR_ROLE_ID);
			if (!grantAdminPersonRoles.isEmpty()) {
				PersonRoles personRole = grantAdminPersonRoles.get(0);
				Person person = personDao.getPersonDetailById(personRole.getPersonId());
				context.put("GRANT_ADMIN_NAME", person.getFullName() == null ? "" : person.getFullName());
				context.put("GRANT_ADMIN_EMAIL", person.getEmailAddress() == null ? "" : person.getEmailAddress());
				context.put("GRANT_ADMIN_PHONE", person.getMobileNumber() == null ? "" : person.getMobileNumber());
			} else {
				context.put("GRANT_ADMIN_NAME", "");
				context.put("GRANT_ADMIN_EMAIL", "");
				context.put("GRANT_ADMIN_PHONE", "");
			}
			List<PersonRoles> deanPersonRoles = new ArrayList<>();
			if (proposal.getActivityTypeCode().equals(Constants.GRANT_CALL_CATEGORY_A_TYPE_CODE)) {
				deanPersonRoles = personDao.getPersonRoleByUnitNumberAndRoleId(proposal.getHomeUnitNumber(), Constants.PROPOSAL_HOD_ROLE_TYPE_CODE);
				if (deanPersonRoles != null && !deanPersonRoles.isEmpty()) {
					context.put("DEAN_SALUTATION", DEAN_SALUTATION);
				} else {
					context.put("DEAN_SALUTATION", "");
				}
			} else {
				deanPersonRoles = personDao.getPersonRolesByRoleId(Constants.PROPOSAL_ORTT_ROLE_TYPE_CODE);
				if (deanPersonRoles != null && !deanPersonRoles.isEmpty()) {
					context.put("DEAN_SALUTATION", ORTT_SALUTATION);
				} else {
					context.put("DEAN_SALUTATION", "");
				}
			}	
			if (deanPersonRoles != null && !deanPersonRoles.isEmpty()) {
				PersonRoles personRole = deanPersonRoles.get(0);
				Person person = personDao.getPersonDetailById(personRole.getPersonId());
				context.put("DEAN_NAME", person.getFullName() == null ? "" : person.getFullName());
				context.put("DEAN_EMAIL", person.getEmailAddress() == null ? "" : person.getEmailAddress());
				context.put("DEAN_PHONE", person.getMobileNumber() == null ? "" : person.getMobileNumber());
				context.put("DEAN_FAX_NUMBER", person.getFaxNumber() == null ? "" : person.getFaxNumber());
				if (proposal.getActivityTypeCode().equals(Constants.GRANT_CALL_CATEGORY_A_TYPE_CODE)) {
					context.put("DEAN_SCHOOL", person.getUnit() == null ? "" : person.getUnit().getUnitName());
				} else {
					context.put("DEAN_SCHOOL", ORTT_SCHOOL);
				}			
			} else {
				context.put("DEAN_NAME", "");
				context.put("DEAN_EMAIL", "");
				context.put("DEAN_PHONE", "");
				context.put("DEAN_SCHOOL", "");
				context.put("DEAN_FAX_NUMBER", "");
			}
		}
		return context;
	}

	public AwardPerson getPIAwardPerson(List<AwardPerson> awardPersons) {
		AwardPerson awardPerson = null;
		if (awardPersons != null && !awardPersons.isEmpty()) {
			for (AwardPerson person : awardPersons) {
				if (person != null) {
					if (StringUtils.equals(person.getProposalPersonRole().getCode(),
							Constants.PRINCIPAL_INVESTIGATOR)) {
						awardPerson = person;
						break;
					}
				}
			}
		}
		return awardPerson;
	}

	@Override
	public ResponseEntity<byte[]> generateQuestionnaireReport(CommonVO vo) {
		ResponseEntity<byte[]> attachmentData = null;
		try {
			List<QuestionnairePrintParameter> questionnairePrintParameters = null;
			XSSFWorkbook workbook = new XSSFWorkbook();
			byte[] byteArray = null;
			if (vo.getIsSingleQuestionnairePrint()) {
				questionnairePrintParameters = setQuestionnaireDataBasedOnQuestionnaireId(vo);
				QuestionnairePrintParameter questionnairePrintParameter = questionnairePrintParameters.get(0);
				XSSFSheet sheet = workbook.createSheet(questionnairePrintParameter.getQuestionnaireName());
                commonService.addDetailsInHeader(workbook, sheet);
				prepareExcelSheetForQuestionnaire(questionnairePrintParameter, sheet, workbook, vo.getModuleCode(),
						vo.getModuleItemKey());
			} else {
				questionnairePrintParameters = setQuestionnaireDataForQuestionnairePrint(vo);
				for (QuestionnairePrintParameter questionnairePrintParameter : questionnairePrintParameters) {
					XSSFSheet sheet = workbook.createSheet(questionnairePrintParameter.getQuestionnaireName());
                    commonService.addDetailsInHeader(workbook, sheet);
					prepareExcelSheetForQuestionnaire(questionnairePrintParameter, sheet, workbook, vo.getModuleCode(),
							vo.getModuleItemKey());
				}
			}
			autoSizeColumns(workbook);
			ByteArrayOutputStream bos = new ByteArrayOutputStream();
			workbook.write(bos);
			byteArray = bos.toByteArray();
			attachmentData = getResponseEntity(byteArray);
		} catch (Exception e) {
			e.printStackTrace();
			logger.error("Exception in generateQuestionnaireReport :{}", e.getMessage());
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

	@Override
	public List<QuestionnairePrintParameter> setQuestionnaireDataBasedOnQuestionnaireId(CommonVO vo) {
		List<QuestionnairePrintParameter> proposalQuestionnaireList = new ArrayList<>();
		QuestionnaireDataBus questionnaireDataBusData = new QuestionnaireDataBus();
		questionnaireDataBusData.setActionPersonId(vo.getPersonId());
		questionnaireDataBusData.setActionUserId(vo.getUserName());
		questionnaireDataBusData.setModuleItemCode(vo.getModuleCode());
		questionnaireDataBusData.setModuleItemKey(vo.getModuleItemKey());
		questionnaireDataBusData.setModuleSubItemCode(vo.getSubModuleCode());
		questionnaireDataBusData.setModuleSubItemKey(vo.getSubModuleItemKey());
		try {
			questionnaireDataBusData = questionnaireService.getApplicableQuestionnaire(questionnaireDataBusData);
			List<QuestionnaireDataBus> questionnaireList = questionnaireService.getQuestionnaireList(questionnaireDataBusData);
			List<QuestionnaireDataBus> questionnaireDataBusList = new ArrayList<>(1);
			for (QuestionnaireDataBus questionnaireDataBus : questionnaireList) {
				if (questionnaireDataBus.getQuestionnaireId().equals(vo.getQuestionnaireId())) {
					questionnaireDataBusList.add(questionnaireDataBus);
				}
			}
			proposalQuestionnaireList = setQuestionnaireForExcelPrint(questionnaireDataBusList, vo);
		} catch (Exception e) {
			logger.error("Exception in setQuestionnaireDataBasedOnQuestionnaireId : {}", e.getMessage());
		}
		return proposalQuestionnaireList;
	}

	private void prepareExcelSheetForQuestionnaire(QuestionnairePrintParameter questionnaire, XSSFSheet sheet, XSSFWorkbook workbook, Integer moduleCode, String moduleItemKey) {
		int headingCellNumber = 0;
		int rowNumber = 0;
		if (moduleCode.equals(Constants.AWARD_MODULE_CODE)) {
            rowNumber = prepareAwardGeneralInformationForExcel(sheet, rowNumber, moduleItemKey, workbook, null, Boolean.FALSE);
		}
		// Excel sheet heading style and font creation code.
		Row headerRow = sheet.createRow(rowNumber++);
		Cell headingCell = headerRow.createCell(0);
		headingCell.setCellValue(questionnaire.getQuestionnaireName());
		XSSFFont headerFont = workbook.createFont();
		headerFont.setBold(true);
		headerFont.setFontHeightInPoints((short) 15);
		XSSFCellStyle headerStyle = workbook.createCellStyle();
		headerStyle.setAlignment(HorizontalAlignment.CENTER);
		headerStyle.setFont(headerFont);
		headingCell.setCellStyle(headerStyle);
		// Table head style and font creation code.
		Row tableHeadRow = sheet.createRow(rowNumber++);
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

		// Set table head data to each column.
		for (Object heading : tableHeadingRowData) {
			Cell cell = tableHeadRow.createCell(headingCellNumber++);
			cell.setCellValue((String) heading);
			cell.setCellStyle(tableHeadStyle);
		}

		sheet.addMergedRegion(new CellRangeAddress(0, 0, 0, tableHeadingRowData.length - 1));

		for (QuestionAndAnswer questionAndAnswer : questionnaire.getQuestionAndAnswerList()) {
			Row row = sheet.createRow(rowNumber++);
			int cellNumber = 0;
			Cell questionNumberCell = row.createCell(cellNumber++);
			questionNumberCell.setCellStyle(tableBodyStyle);
			questionNumberCell.setCellValue(questionNumber);
			questionNumber++;

			Cell questionCell = row.createCell(cellNumber++);
			questionCell.setCellStyle(tableBodyStyle);
			questionCell.setCellValue(questionAndAnswer.getQuestion());

			for (String answer : questionAndAnswer.getAnswersList()) {
				Cell answerCell = row.createCell(cellNumber++);
				answerCell.setCellStyle(tableBodyStyle);
				answerCell.setCellValue(answer);
			}
		}
	}

	public ResponseEntity<byte[]> getResponseEntityForDownload(HttpServletResponse response, XSSFWorkbook workbook) {
		ResponseEntity<byte[]> responseData = null;
		byte[] byteArray = null;
		byteArray = dashboardService.generatePDFFileByteArray("", workbook);
		String generatedFileName = RESULT + System.nanoTime() + ".pdf";
		return setHttpHeaderAndHttpResponseData(response, generatedFileName, byteArray, responseData);
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

	@SuppressWarnings("unchecked")
	private List<QuestionnairePrintParameter> setQuestionnaireForExcelPrint(List<QuestionnaireDataBus> questionnaireList, CommonVO vo) {
		List<QuestionnairePrintParameter> questionnaires = new ArrayList<>();
		SimpleDateFormat dateFormatter = new SimpleDateFormat("yy-MM-dd HH:mm:ss");
		if (!questionnaireList.isEmpty()) {
			for (QuestionnaireDataBus questionnaireDataBus : questionnaireList) {
				if (questionnaireDataBus.getQuestionnaire() != null) {
					List<QuestionAndAnswer> questionAndAnswers = new ArrayList<>();
					StringBuilder questionnaireHeader = new StringBuilder();
					String dateValue = null;
					try {
						dateValue = commonService.convertDateFormatBasedOnTimeZone(dateFormatter.parse(questionnaireDataBus.getHeader().get("ANS_UPDATE_TIMESTAMP").toString()).getTime(), Constants.DEFAULT_DATE_FORMAT);
					} catch (Exception e) {
						dateValue = "";
					}
					Integer maximumNumberOfAnswers = 0;
					if (questionnaireDataBus.getQuestionnaireCompleteFlag().equals("Y")) {
						questionnaireHeader.append(questionnaireDataBus.getQuestionnaireName()).append(" ( ").append("Complete").append(" ) ")
								.append(" Last updated by ").append(questionnaireDataBus.getHeader().get(ANS_PERSON_FULL_NAME)).append(" on ").append(dateValue);
					} else {
						questionnaireHeader.append(questionnaireDataBus.getQuestionnaireName()).append(" ( ").append("Incomplete").append(" ) ");
						if (questionnaireDataBus.getHeader().get(ANS_PERSON_FULL_NAME) != null) {
							questionnaireHeader.append(" Last updated by ").append(questionnaireDataBus.getHeader().get(ANS_PERSON_FULL_NAME)).append(" on ").append(dateValue);
						}
					}
					if (!questionnaireDataBus.getQuestionnaire().getQuestions().isEmpty()) {
						for (HashMap<String, Object> question : questionnaireDataBus.getQuestionnaire()
								.getQuestions()) {
							Boolean isrulePassed = Boolean.FALSE;
							if (question.get("RULE_ID") != null) {
								isrulePassed = generalInformationDao.evaluateRule(vo.getModuleCode(), vo.getSubModuleCode(),
										vo.getModuleItemKey(), Integer.parseInt(question.get("RULE_ID").toString()), AuthenticatedUser.getLoginPersonId(), AuthenticatedUser.getLoginUserName(), vo.getSubModuleItemKey() != null  && !vo.getSubModuleItemKey().isEmpty() ? vo.getSubModuleItemKey() : Constants.SUBMODULE_ITEM_KEY);
								System.out.println("ruleid ===" + question.get("RULE_ID") +isrulePassed);
							}
							if ((question.get("RULE_ID") != null && Boolean.TRUE.equals(isrulePassed)) || question.get("RULE_ID") == null) {
								HashMap<String, Object> answerMap = (HashMap<String, Object>) question.get("ANSWERS");
								if (question.get(ANSWER_TYPE) != null) {
									QuestionAndAnswer questionAndAnswer = new QuestionAndAnswer();
									questionAndAnswer.setQuestion(question.get(QUESTION).toString());
									if (question.get(ANSWER_TYPE).equals("Checkbox")) {
										if (answerMap != null) {
											if (!answerMap.toString().isEmpty()) {
												String answer = "";
												if (!answerMap.toString().equals("{1=}")) {
													if (answerMap.size() > maximumNumberOfAnswers) {
														maximumNumberOfAnswers = answerMap.size();
													}
													for (Map.Entry<String, Object> entry : answerMap.entrySet()) {
														questionAndAnswer.getAnswersList().add(entry.getKey());
													}
												} else {
													questionAndAnswer.getAnswersList().add(answer);
												}
											}
										} else {
											questionAndAnswer.getAnswersList().add("");
										}
										questionAndAnswers.add(questionAndAnswer);
	                                }
	                                //TODO need to take necessary steps to print Table
	                                else if (question.get(ANSWER_TYPE).equals("Table")) {
	                                    if (answerMap != null && !answerMap.toString().isEmpty()) {
	                                        questionAndAnswer.getAnswersList().add("");
	                                        questionAndAnswers.add(questionAndAnswer);
	                                    }
									} else if (Constants.SYSTEM_LOOKUP.equals(question.get(ANSWER_TYPE)) || Constants.USER_LOOKUP.equals(question.get(ANSWER_TYPE))) {
	                                    if (answerMap != null && answerMap.get("0") != null  && !answerMap.get("0").toString().isEmpty()) {
											ObjectMapper oMapper = new ObjectMapper();
											LookUp lookUp = oMapper.convertValue(answerMap.get("0"), LookUp.class);
	                                        questionAndAnswer.getAnswersList().add(lookUp.getDescription() !=null ? lookUp.getDescription(): "");
											questionAndAnswers.add(questionAndAnswer);
										}
									} else {
										if (answerMap.get("1") != null) {
											if (!answerMap.get("1").toString().isEmpty()) {
												questionAndAnswer.getAnswersList().add(answerMap.get("1").toString());
											} else {
												questionAndAnswer.getAnswersList().add("");
											}
										} else {
											questionAndAnswer.getAnswersList().add("");
										}
										questionAndAnswers.add(questionAndAnswer);
									}
								}
							}
						}
					}
					questionnaires.add(new QuestionnairePrintParameter(questionnaireHeader.toString(), questionAndAnswers,
							maximumNumberOfAnswers));
				}
			}
		}
		return questionnaires;
	}

	private List<QuestionnairePrintParameter> setQuestionnaireDataForQuestionnairePrint(CommonVO vo) {
		List<QuestionnairePrintParameter> proposalQuestionnaireList = new ArrayList<>();
		QuestionnaireDataBus questionnaireDataBusData = new QuestionnaireDataBus();
		questionnaireDataBusData.setActionPersonId(vo.getPersonId());
		questionnaireDataBusData.setActionUserId(vo.getUserName());
		questionnaireDataBusData.setModuleItemCode(vo.getModuleCode());
		questionnaireDataBusData.setModuleItemKey(vo.getModuleItemKey());
		questionnaireDataBusData.setModuleSubItemCode(vo.getSubModuleCode());
		questionnaireDataBusData.setModuleSubItemKey(vo.getSubModuleItemKey());
		try {
			questionnaireDataBusData = questionnaireService.getApplicableQuestionnaire(questionnaireDataBusData);
            List<QuestionnaireDataBus> questionnaireList = questionnaireService.getQuestionnaireList(questionnaireDataBusData);
			proposalQuestionnaireList = setQuestionnaireForExcelPrint(questionnaireList, vo);
		} catch (Exception e) {
			logger.error("Exception in setQuestionnaireDataForQuestionnairePrint : {}", e.getMessage());
		}
		return proposalQuestionnaireList;
	}

	private BudgetPersonalDetail addBudgetPersonalDetail() {
		BudgetPersonalDetail budgetPersonalDetailData = new BudgetPersonalDetail();
		budgetPersonalDetailData.setCostSharePercentage("");
		budgetPersonalDetailData.setCostShareAmount("");
		budgetPersonalDetailData.setSalary("");
		budgetPersonalDetailData.setAppliedSalary("");
		budgetPersonalDetailData.setFundRequested("");
		budgetPersonalDetailData.setPercentageEffort("");
		budgetPersonalDetailData.setStartDate("");
		budgetPersonalDetailData.setEndDate("");
		budgetPersonalDetailData.setPersonName("");
		budgetPersonalDetailData.setIoCode("");
		return budgetPersonalDetailData;
	}

	private List<ProposalPrintParameter> setProposalMilestone(List<ProposalMileStone> proposalMilestones) {
		List<ProposalPrintParameter> proposalMilestoneList = new ArrayList<>();
		if (proposalMilestones != null && !proposalMilestones.isEmpty()) {
			for (ProposalMileStone proposalMilestone : proposalMilestones) {
				String researchMilestone = (proposalMilestone.getMileStone() != null ? proposalMilestone.getMileStone() + "" : "");
				String startMonth = (proposalMilestone.getStartMonth() != null ? proposalMilestone.getStartMonth().toString() + "" : "");
				String duration = (proposalMilestone.getDuration() != null ? proposalMilestone.getDuration().toString() + "" : "");
				ProposalPrintParameter proposalPrintParameters = setproposalMilestone(researchMilestone, startMonth, duration);
				proposalMilestoneList.add(proposalPrintParameters);
			}
		} else {
			proposalMilestoneList.add(setproposalMilestone("", "", ""));
		}
		return proposalMilestoneList;
	}

	private ProposalPrintParameter setproposalMilestone(String researchMilestone, String startMonth, String duration) {
		ProposalPrintParameter proposalPrintParameters = new ProposalPrintParameter();
		proposalPrintParameters.setResearchMilestone(researchMilestone);
		proposalPrintParameters.setStartMonth(startMonth);
		proposalPrintParameters.setDuration(duration);
		return proposalPrintParameters;
	}

	private List<ProposalKPIPrintParameter> setproposalKPI(List<ProposalKPI> proposalKPIs) {
		List<ProposalKPIPrintParameter> proposalKPIPrintParameters = new ArrayList<>();
		if (proposalKPIs != null && !proposalKPIs.isEmpty()) {
			for (ProposalKPI proposalKpi : proposalKPIs) {
				ProposalKPIPrintParameter proposalKPIPrintParameter = new ProposalKPIPrintParameter();
				if (proposalKpi.getKpiType().getDescription() != null) {
					proposalKPIPrintParameter.setKPIType(proposalKpi.getKpiType().getDescription());
				}
				List<ProposalKPICriteriaType> proposalKPICriteriaTypes = new ArrayList<>();
				for (ProposalKPICriteria proposalKPICriteria : proposalKpi.getProposalKPICriterias()) {
					ProposalKPICriteriaType proposalKPICriteriaType = new ProposalKPICriteriaType();
					if (proposalKPICriteria.getKpiCriteriaType().getDescription() != null) {
						proposalKPICriteriaType
								.setProposalKPICriteriaType(proposalKPICriteria.getKpiCriteriaType().getDescription());
					}
					if (proposalKPICriteria.getTarget() != null) {
						proposalKPICriteriaType.setTarget(proposalKPICriteria.getTarget().toString());
					} else {
						proposalKPICriteriaType.setTarget("");
					}
					proposalKPICriteriaTypes.add(proposalKPICriteriaType);
				}
				proposalKPIPrintParameter.setProposalKPICriteriaTypes(proposalKPICriteriaTypes);
				proposalKPIPrintParameters.add(proposalKPIPrintParameter);
			}
		} else {
			ProposalKPIPrintParameter proposalKPIPrintParameter = new ProposalKPIPrintParameter();
			proposalKPIPrintParameter.setKPIType("");
			ProposalKPICriteriaType proposalKPICriteriaType = new ProposalKPICriteriaType();
			proposalKPICriteriaType.setProposalKPICriteriaType("");
			proposalKPICriteriaType.setTarget("");
			List<ProposalKPICriteriaType> proposalKPICriteriaTypes = new ArrayList<>();
			proposalKPICriteriaTypes.add(proposalKPICriteriaType);
			proposalKPIPrintParameter.setProposalKPICriteriaTypes(proposalKPICriteriaTypes);
			proposalKPIPrintParameters.add(proposalKPIPrintParameter);
		}
		return proposalKPIPrintParameters;
	}

	@Override
	public ResponseEntity<byte[]> generateAwardBudgetReport(HttpServletResponse response, PrintVO vo) {
		ResponseEntity<byte[]> attachmentData = null;
		try {
			ByteArrayInputStream bis = generateAwardBudgetPDF(vo);
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

	private List<BudgetPrintParameter> prepareAwardBudgetPersonnelsData(List<AwardBudgetPerson> awardBudgetPersons) {
		DecimalFormat decimalFormat = new DecimalFormat(Constants.NUMBER_FORMAT_WITH_DECIMAL);
		List<BudgetPrintParameter> budgetPersonnels = new ArrayList<>();
		if (awardBudgetPersons != null) {
			if (!awardBudgetPersons.isEmpty()) {
				for (AwardBudgetPerson awardBudgetPerson : awardBudgetPersons) {
					BudgetPrintParameter budgetPrintParameter = new BudgetPrintParameter();
					String personType = "";
					if (awardBudgetPerson.getPersonType() != null) {
						if (awardBudgetPerson.getPersonType().equals("P")) {
							personType = "Award Persons";
						} else if (awardBudgetPerson.getPersonType().equals("E")) {
							personType = EMPLOYEE;
						}
					}
					budgetPrintParameter.setPersonType(personType);
					budgetPrintParameter.setPersonName(
							awardBudgetPerson.getPersonName() != null ? awardBudgetPerson.getPersonName() : "");
					budgetPrintParameter.setJobType(
							awardBudgetPerson.getJobCodes() != null ? awardBudgetPerson.getJobCodes().getJobTitle()
									: "");
					budgetPrintParameter.setAppointmentType(awardBudgetPerson.getAppointmentType() != null
							? awardBudgetPerson.getAppointmentType().getDescription()
							: "");
					budgetPrintParameter.setEffectiveDate(awardBudgetPerson.getEffectiveDate() != null
							? commonService.convertDateFormatBasedOnTimeZone(
									awardBudgetPerson.getEffectiveDate().getTime(), Constants.DEFAULT_DATE_FORMAT)
							: "");
					budgetPrintParameter
							.setBaseSalary(awardBudgetPerson.getCalculationBase() != null
									? Constants.DOLLAR_SYMBOL
											+ decimalFormat.format(awardBudgetPerson.getCalculationBase()).toString()
									: "");
					budgetPrintParameter
							.setAnniversaryDate(awardBudgetPerson.getSalaryAnniversaryDate() != null
									? commonService.convertDateFormatBasedOnTimeZone(
											awardBudgetPerson.getSalaryAnniversaryDate().getTime(),
											Constants.DEFAULT_DATE_FORMAT)
									: "");
					budgetPersonnels.add(budgetPrintParameter);
				}
			} else {
				BudgetPrintParameter budgetPrintParameter = new BudgetPrintParameter();
				budgetPrintParameter.setPersonName("");
				budgetPrintParameter.setJobType("");
				budgetPrintParameter.setAppointmentType("");
				budgetPrintParameter.setEffectiveDate("");
				budgetPrintParameter.setBaseSalary("");
				budgetPrintParameter.setAnniversaryDate("");
				budgetPrintParameter.setPersonType("");
				budgetPersonnels.add(budgetPrintParameter);
			}
		}
		return budgetPersonnels;
	}

	private List<BudgetPeriodPrintParameter> prepareAwardBudgetPeriodsData(List<AwardBudgetPeriod> awardBudgetPeriods) {
		List<BudgetPeriodPrintParameter> budgetPeriods = new ArrayList<>();
		if (awardBudgetPeriods != null) {
			for (AwardBudgetPeriod budgetPeriod : awardBudgetPeriods) {
				BudgetPeriodPrintParameter budgetPeriodPrintParameter = new BudgetPeriodPrintParameter();
				budgetPeriodPrintParameter.setStartDate(budgetPeriod.getStartDate() == null ? ""
						: commonService.convertDateFormatBasedOnTimeZone(budgetPeriod.getStartDate().getTime(),
								Constants.DEFAULT_DATE_FORMAT));
				budgetPeriodPrintParameter.setEndDate(budgetPeriod.getEndDate() == null ? ""
						: commonService.convertDateFormatBasedOnTimeZone(budgetPeriod.getEndDate().getTime(),
								Constants.DEFAULT_DATE_FORMAT));
				budgetPeriodPrintParameter.setDirectCost(budgetPeriod.getTotalDirectCost() == null ? ""
						: Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetPeriod.getTotalDirectCost()));
				budgetPeriodPrintParameter.setIndirectCost(budgetPeriod.getTotalIndirectCost() == null ? ""
						: Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetPeriod.getTotalIndirectCost()));
				budgetPeriodPrintParameter.setTotalCost(budgetPeriod.getTotalCost() == null ? ""
						: Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetPeriod.getTotalCost()));
				List<LineItemData> lineItemDatas = new ArrayList<>();
				if (!budgetPeriod.getBudgetDetails().isEmpty()) {
					for (AwardBudgetDetail budgetDetail : budgetPeriod.getBudgetDetails()) {
						LineItemData lineItemData = new LineItemData();
						lineItemData.setInternalOrderCode(
								budgetDetail.getInternalOrderCode() != null ? budgetDetail.getInternalOrderCode() : "");
						lineItemData.setCostElement(budgetDetail.getCostElement() != null
								? budgetDetail.getCostElement().getCostElementDetail()
								: "");
						lineItemData.setQuantity(
								budgetDetail.getQuantity() != null ? budgetDetail.getQuantity().toString() : "");
						lineItemData.setBalanceToDate(budgetDetail.getBalanceToDate() != null
								? Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetDetail.getBalanceToDate())
								: "");
						lineItemData.setBudgetCategory(budgetDetail.getBudgetCategory() != null
								? budgetDetail.getBudgetCategory().getDescription()
								: "");
						lineItemData.setLineItemDescription(
								budgetDetail.getLineItemDescription() != null ? budgetDetail.getLineItemDescription()
										: "");
						lineItemData.setLineItemCost(budgetDetail.getLineItemCost() != null
								? Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetDetail.getLineItemCost())
								: "");
						lineItemData.setPreviousLineItemCost(budgetDetail.getPrevLineItemCost() != null
								? Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetDetail.getPrevLineItemCost())
								: "");
						if (!budgetDetail.getPersonsDetails().isEmpty()) {
							lineItemData.setHasBudgetPerson(true);
							for (AwardBudgetPersonalDetail budgetPersonalDetail : budgetDetail.getPersonsDetails()) {
								BudgetPersonalDetail budgetPersonalDetailData = new BudgetPersonalDetail();
								budgetPersonalDetailData
										.setAppliedSalary(budgetPersonalDetail.getSalaryRequested() == null ? ""
												: Constants.DOLLAR_SYMBOL + decimalFormat
														.format(budgetPersonalDetail.getSalaryRequested()));
								budgetPersonalDetailData
										.setPercentageEffort(budgetPersonalDetail.getPercentageEffort() == null ? ""
												: budgetPersonalDetail.getPercentageEffort().toString());
								budgetPersonalDetailData.setStartDate(budgetPersonalDetail.getStartDate() == null ? ""
										: commonService.convertDateFormatBasedOnTimeZone(
												budgetPersonalDetail.getStartDate().getTime(),
												Constants.DEFAULT_DATE_FORMAT));
								budgetPersonalDetailData.setEndDate(budgetPersonalDetail.getEndDate() == null ? ""
										: commonService.convertDateFormatBasedOnTimeZone(
												budgetPersonalDetail.getEndDate().getTime(),
												Constants.DEFAULT_DATE_FORMAT));
								budgetPersonalDetailData.setSalary(budgetPersonalDetail.getTotalSalary() == null ? ""
										: Constants.DOLLAR_SYMBOL
												+ decimalFormat.format(budgetPersonalDetail.getTotalSalary()));
								budgetPersonalDetailData
										.setIoCode(budgetPersonalDetail.getInternalOrderCode() == null ? ""
												: budgetPersonalDetail.getInternalOrderCode());
								AwardBudgetPerson budgetPerson = budgetPersonalDetail.getBudgetPerson();
								String personName = "";
								if (budgetPerson != null) {
									if (budgetPerson.getPersonName() != null) {
										personName = personName + budgetPerson.getPersonName();
										if (budgetPerson.getJobCodes() != null) {
											personName = personName + budgetPerson.getJobCodes().getJobTitle();
										}
									} else if (budgetPerson.getTbnPerson() != null) {
										personName = personName + budgetPerson.getTbnPerson().getPersonName();
									}
								}
								budgetPersonalDetailData.setPersonName(personName);
								lineItemData.getBudgetPersonalDetails().add(budgetPersonalDetailData);
							}
						} else {
							lineItemData.setHasBudgetPerson(false);
							lineItemData.getBudgetPersonalDetails().add(addBudgetPersonalDetail());
						}
						if (!budgetDetail.getNonPersonsDetails().isEmpty()) {
							lineItemData.setHasNonBudgetPerson(true);
							for (AwardBudgetNonPersonDetail nonPersonDetail : budgetDetail.getNonPersonsDetails()) {
								BudgetNonPersonDetail budgetNonPersonDetail = new BudgetNonPersonDetail();
								budgetNonPersonDetail.setDescription(nonPersonDetail.getDescription());
								budgetNonPersonDetail.setInternalOrderCode(nonPersonDetail.getInternalOrderCode() != null ? nonPersonDetail.getInternalOrderCode() : "");
								budgetNonPersonDetail.setLineItemCost(nonPersonDetail.getLineItemCost() != null ?  Constants.DOLLAR_SYMBOL
										+ decimalFormat.format(nonPersonDetail.getLineItemCost()) : "");
								lineItemData.getNonPersonsDetails().add(budgetNonPersonDetail);
							}
						} else {
							lineItemData.setHasNonBudgetPerson(false);
							lineItemData.getNonPersonsDetails().add(new BudgetNonPersonDetail("", "", ""));
						}
						BudgetCategory budgetCategory = budgetDetail.getCostElement().getBudgetCategory();
						if (budgetCategory != null) {
							if (!budgetCategory.getCode()
									.equals(Constants.BUDGET_CATEGORY_TYPE_CODE_SYS_GENERATED_COST)) {
								lineItemData.setIsBudgetCategoryNotSystemGenerated(true);
							} else {
								lineItemData.setIsBudgetCategoryNotSystemGenerated(false);
							}
						}
						lineItemDatas.add(lineItemData);
					}
				} else {
					LineItemData lineItemData = new LineItemData();
					lineItemData.setCostElement("");
					lineItemData.setLineItemDescription("");
					lineItemData.setQuantity("");
					lineItemData.setLineItemCost("");
					lineItemData.setCostSharingPercentage("");
					lineItemData.setCostSharingAmount("");
					lineItemData.setSponsorRequestedAmount("");
					lineItemData.setBudgetCategory("");
					lineItemData.setInternalOrderCode("");
					lineItemData.setPreviousLineItemCost("");
					lineItemData.setBalanceToDate("");
					lineItemData.setHasBudgetPerson(false);
					lineItemData.setIsBudgetCategoryNotSystemGenerated(true);
					lineItemData.getBudgetPersonalDetails().add(addBudgetPersonalDetail());
					lineItemData.setHasNonBudgetPerson(false);
					lineItemDatas.add(lineItemData);
				}
				budgetPeriodPrintParameter.setPeriod("Period " + budgetPeriod.getBudgetPeriod());
				budgetPeriodPrintParameter.setLineItemList(lineItemDatas);
				budgetPeriods.add(budgetPeriodPrintParameter);
			}
		} else {
			prepareBudgetPeriodsInformation(budgetPeriods);
		}
		return budgetPeriods;
	}

	@Override
	public ResponseEntity<byte[]> generateNotifyAwardReports(HttpServletResponse response, Integer awardId, String awardNumber, Integer sequenceNumber, String updateUser) {
		ResponseEntity<byte[]> attachmentData = null;
		try {
			Integer count = 0;
			String awardDescription = "Award Notice Letter";
			String fileName = "AwardNoticeLetter.pdf";
			byte[] bFile = getTemplateData(Constants.AWARD_BUDGET_NOTICE_TEMPLATE_TYPE_CODE);
			AwardAttachment awardAttachment = awardDao.getAwardAttachmentDetailsBasedOnParams(awardNumber, fileName, Constants.AWARD_ATTACHMENT_LETTER_OF_ACCEPTANCE);
			Integer documentId = 1;
			Integer versionNumber = 1;
			if (awardAttachment != null) {
				documentId = awardAttachment.getDocumentId();
				versionNumber = awardAttachment.getVersionNumber() + 1;
			}
			else {
				documentId = awardDao.getMaxDocumentIdBasedOnAwardNumber(awardNumber) + 1;
			}
			count = addDataForAwardNotice(attachmentData, bFile, response, awardId, awardNumber, sequenceNumber, updateUser, count, awardDescription, fileName, documentId, versionNumber);
			if (count == 1) {
				awardDescription = "Award Tier Notice";
				fileName = "AwardTierNoticeLetter.pdf";
				bFile = getTemplateData(Constants.AWARD_BUDGET_TIER_TEMPLATE_TYPE_CODE);
				AwardAttachment tierAwardAttachment = awardDao.getAwardAttachmentDetailsBasedOnParams(awardNumber, fileName, Constants.AWARD_ATTACHMENT_LETTER_OF_ACCEPTANCE);
				if (tierAwardAttachment != null) {
					documentId = tierAwardAttachment.getDocumentId();
					versionNumber = tierAwardAttachment.getVersionNumber() + 1;
				}
				else {
					documentId = awardDao.getMaxDocumentIdBasedOnAwardNumber(awardNumber) + 1;
				}
				addDataForAwardNotice(attachmentData, bFile, response, awardId, awardNumber, sequenceNumber, updateUser, count, awardDescription, fileName, documentId, versionNumber);
			}
		} catch (Exception e) {
			logger.error("Exception in generateNotifyAwardReport : {}", e.getMessage());
		}
		return attachmentData;
	}

	public Integer addDataForAwardNotice(ResponseEntity<byte[]> attachmentData, byte[] bFile, HttpServletResponse response, Integer awardId, String awardNumber, Integer sequenceNumber, String updateUser, Integer count, String awardDescription, String fileName, Integer documentId, Integer versionNumber) {
		byte[] mergedOutput = mergePlaceHoldersOfNotifyAwards(bFile, awardId);
		String generatedFileName = RESULT + System.nanoTime() + ".pdf";
		setHttpHeaderAndHttpResponseData(response, generatedFileName, mergedOutput, attachmentData);
		addAwardNoticeLetter(awardId, awardNumber, sequenceNumber, updateUser, bFile, awardDescription, fileName, documentId, versionNumber);
		count++;
		return count;
	}

	@Override
	public byte[] mergePlaceHoldersOfNotifyAwards(byte[] data, Integer awardId) {
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		try {
			InputStream myInputStream = new ByteArrayInputStream(data);
			IXDocReport report = XDocReportRegistry.getRegistry().loadReport(myInputStream,
					TemplateEngineKind.Velocity);
			Award award = awardDao.fetchAwardByAwardId(awardId.toString());
			IContext context = report.createContext();
			context = setNotifyAwardPlaceHolderDatas(context, award);

			Options options = Options.getTo(ConverterTypeTo.PDF).via(ConverterTypeVia.XWPF);
			report.convert(context, options, baos);

		} catch (Exception e) {
			e.printStackTrace();
			logger.error("Exception in mergePlaceHoldersOfNotifyAward : {}", e.getMessage());
		}
		return baos.toByteArray();
	}

	private IContext setNotifyAwardPlaceHolderDatas(IContext context, Award award) {
		DecimalFormat decimalFormat = new DecimalFormat(Constants.NUMBER_FORMAT_WITH_DECIMAL);
		context.put("CURRENT_DATE", commonService.convertDateFormatBasedOnTimeZone(
				commonDao.getCurrentTimestamp().getTime(), Constants.DEFAULT_DATE_FORMAT));
		context.put("PI_NAME", award.getPrincipalInvestigator());
		context.put(DURATION, award.getDuration() == null ? "" : award.getDuration());
		context.put("AWARD_TITLE", award.getTitle() == null ? "" : award.getTitle());
		context.put("START_DATE",
				award.getBeginDate() == null ? ""
						: commonService.convertDateFormatBasedOnTimeZone(award.getBeginDate().getTime(),
								Constants.DEFAULT_DATE_FORMAT));
		context.put("END_DATE",
				award.getFinalExpirationDate() == null ? ""
						: commonService.convertDateFormatBasedOnTimeZone(award.getFinalExpirationDate().getTime(),
								Constants.DEFAULT_DATE_FORMAT));
		AwardAmountInfo obligatedAmount = datesAndAmountDao.getLatestActiveAwardAmountInfo(award.getAwardNumber());
		if (obligatedAmount != null) {
			context.put("AMOUNT", obligatedAmount.getObligatedChange() == null ? ""
					: Constants.DOLLAR_SYMBOL + decimalFormat.format(obligatedAmount.getObligatedChange()));
		} else {
			context.put("AMOUNT", Constants.DOLLAR_SYMBOL + "0");
		}
		return context;
	}

	private AwardAttachment addAwardNoticeLetter(Integer awardId, String awardNumber, Integer sequenceNumber, String updateUser, byte[] bFile, String awardDescription, String fileName, Integer documentId, Integer versionNumber) {
		byte[] mergedOutput = mergePlaceHoldersOfNotifyAwards(bFile, awardId);
		AwardAttachment awardAttachment = new AwardAttachment();
		if (mergedOutput != null) {
			awardAttachment.setAwardId(awardId);
			awardAttachment.setAwardNumber(awardNumber);
			awardAttachment.setTypeCode(Constants.AWARD_ATTACHMENT_LETTER_OF_ACCEPTANCE);
			awardAttachment.setDescription(awardDescription);
			awardAttachment.setUpdateTimestamp(commonDao.getCurrentTimestamp());
			awardAttachment.setUpdateUser(updateUser);
			awardAttachment.setFileName(fileName);
			awardAttachment.setNarrativeStatusCode(Constants.NARRATIVE_STATUS_CODE_COMPLETE);
			awardAttachment.setNarrativeStatus(commonDao.getNarrativeStatusByCode(Constants.NARRATIVE_STATUS_CODE_COMPLETE));
			awardAttachment.setMimeType("application/pdf");
			awardAttachment.setVersionNumber(versionNumber);
			awardAttachment.setSequenceNumber(sequenceNumber);
			FileData fileData = new FileData();
			fileData.setAttachment(mergedOutput);
			fileData = commonDao.saveFileData(fileData);
			awardAttachment.setFileDataId(fileData.getFileDataId());
			awardAttachment.setDocumentStatusCode(Constants.DOCUMENT_STATUS_CODE_DRAFT);
			awardAttachment.setDocumentId(documentId);
			if (awardAttachment.getUpdateUser() != null) {
				awardAttachment.setLastUpdateUserFullName(personDao.getUserFullNameByUserName(awardAttachment.getUpdateUser()));
			}
			awardAttachment = awardDao.saveAttachment(awardAttachment);
		}
		return awardAttachment;
	}

	@Override
	public ResponseEntity<byte[]> generateBudgetSummaryExcelReport(HttpServletResponse response, Integer proposalId, Integer budgetId) {
		ResponseEntity<byte[]> attachmentData = null;
		try {
			XSSFWorkbook workbook = new XSSFWorkbook();
			XSSFSheet sheet = workbook.createSheet(BUDGET_SUMMARY);
            commonService.addDetailsInHeader(workbook, sheet);
			BudgetHeader budgetDetail = null;
			Proposal proposal = proposalDao.fetchProposalById(proposalId);
			proposal.setProposalPersons(proposalModuleDao.fetchProposalPersonBasedOnProposalId(proposalId));
			List<BudgetHeader> budgetHeaders = budgetDao.fetchBudgetsByProposalId(proposalId);
			if (budgetHeaders != null && !budgetHeaders.isEmpty()) {
				for (BudgetHeader budgetHeader : budgetHeaders) {
					if (budgetHeader.getBudgetId().equals(budgetId)) {
						budgetDetail = budgetHeader;
					}
				}
			}
			if (budgetDetail != null) {
				List<BudgetPeriod> budgetPeriods = budgetDetail.getBudgetPeriods();
				BudgetPrintParameter budgetPrintParameter = setBudgetSummaryDetailsForExcelPrint(budgetPeriods);
				prepareExcelSheetForBudgetSummary(budgetPrintParameter, budgetPeriods, sheet, workbook, proposal, budgetDetail);
			}
			ByteArrayOutputStream bos = new ByteArrayOutputStream();
			workbook.write(bos);
			byte[] byteArray = bos.toByteArray();
			attachmentData = getResponseEntityForExcelDownload(byteArray);
		} catch (Exception e) {
			logger.error("Exception in generateBudgetSummaryReport : {} ", e.getMessage());
		}
		return attachmentData;
	}

	private BudgetPrintParameter setBudgetSummaryDetailsForExcelPrint(List<BudgetPeriod> budgetPeriods) {
		BudgetPrintParameter budgetPrintParameter = new BudgetPrintParameter();
		Set<String> budgetCategoryCodes = new HashSet<>();
		Set<String> costElements = new HashSet<>();
		List<BudgetSummaryVO> budgetSummaryVOs = new ArrayList<>();
		List<BudgetPeriodSummary> budgetPeriodSummaries = new ArrayList<>();
		List<BudgetDetail> budgetDetails = new ArrayList<>();
		for (BudgetPeriod budgetPeriod : budgetPeriods) {
			budgetDetails.addAll(budgetPeriod.getBudgetDetails());
			BudgetSummaryVO budgetSummaryVO = new BudgetSummaryVO();
			budgetSummaryVO.setPeriodNumber(budgetPeriod.getBudgetPeriod());
			budgetSummaryVO.setLineItemCost(budgetPeriod.getTotalOfTotalCost());
			budgetSummaryVO.setTotalFundRequested(budgetPeriod.getTotalCost());
			budgetSummaryVO.setTotalCostSharingAmount(budgetPeriod.getCostSharingAmount());
			budgetSummaryVO.setTotalFundRequestedAmount(decimalFormat.format(budgetPeriod.getTotalCost()));
			budgetSummaryVOs.add(budgetSummaryVO);
		}
		budgetPrintParameter.setBudgetSummaryVOs(budgetSummaryVOs);
		for (BudgetDetail budgetDetail : budgetDetails) {
			if (budgetDetail.getBudgetCategoryCode().equals(Constants.BUDGET_CATEGORY_TYPE_CODE_BUDGET_SUMMARY_TOTAL)) {
				costElements.add(budgetDetail.getCostElementCode());
			} else {
				budgetCategoryCodes.add(budgetDetail.getBudgetCategoryCode());
			}
		}
		BudgetSummaryVO budgetSummaryTotals = new BudgetSummaryVO();
		budgetSummaryTotals = setBudgetPeriodSummaryBasedOnBudgetCategory(budgetPeriodSummaries, budgetPeriods, budgetCategoryCodes, budgetDetails, budgetSummaryTotals);
		budgetSummaryTotals = setBudgetPeriodSummaryBasedOnCostElements(budgetPeriodSummaries, budgetPeriods, costElements, budgetDetails, budgetSummaryTotals);
		budgetPrintParameter.setBudgetPeriodSummaries(budgetPeriodSummaries);
		budgetPrintParameter.setPeriodTotalCostShareSum(decimalFormat.format(budgetSummaryTotals.getPeriodCostShareTotalSum()));
		budgetPrintParameter.setPeriodTotalFundRequestedSum(decimalFormat.format(budgetSummaryTotals.getPeriodTotalFundRequestedSum()));
		budgetPrintParameter.setPeriodCostsTotalSum(decimalFormat.format(budgetSummaryTotals.getPeriodTotalSum()));
		if (!budgetPeriodSummaries.isEmpty()) {
			Collections.sort(budgetPeriodSummaries, new BudgetSummaryComparatorByBudgetCategoryName());
		}
		budgetPrintParameter.setBudgetPeriodSummaries(budgetPeriodSummaries);
		return budgetPrintParameter;
	}

	private BudgetSummaryVO setBudgetPeriodSummaryBasedOnBudgetCategory(List<BudgetPeriodSummary> budgetPeriodSummaries, List<BudgetPeriod> budgetPeriods, Set<String> budgetCategoryCodes, List<BudgetDetail> budgetDetails, BudgetSummaryVO budgetSummaryTotals) {
		BigDecimal periodTotalSum = BigDecimal.ZERO;
		BigDecimal periodCostShareTotalSum = BigDecimal.ZERO;
		BigDecimal periodTotalFundRequestedSum = BigDecimal.ZERO;
		for (String budgetCategoryCode : budgetCategoryCodes) {
			BudgetPeriodSummary budgetPeriodSummary = new BudgetPeriodSummary();
			BigDecimal totalLineItemCost = BigDecimal.ZERO;
			BigDecimal totalFundRequested = BigDecimal.ZERO;
			BigDecimal totalCostShareAmount = BigDecimal.ZERO;
			BigDecimal totalLineItemCostSum = BigDecimal.ZERO;
			BigDecimal totalCostShareAmountSum = BigDecimal.ZERO;
			BigDecimal totalFundRequestedSum = BigDecimal.ZERO;
			BigDecimal totalUnderRecoveryAmount = BigDecimal.ZERO;
			List<BudgetSummaryVO> totalLineItemCosts = new ArrayList<>();
			BudgetCategory budgetCategory = budgetDao.fetchBudgetCategoryBasedOnCode(budgetCategoryCode);
			budgetPeriodSummary.setBudgetCategory(budgetCategory.getDescription());
			budgetPeriodSummary.setSortOrder(budgetCategory.getSortOrder() == null ? 0 : budgetCategory.getSortOrder());
			for (BudgetPeriod budgetPeriod : budgetPeriods) {
				totalLineItemCost = BigDecimal.ZERO;
				totalFundRequested = BigDecimal.ZERO;
				totalCostShareAmount = BigDecimal.ZERO;
				totalUnderRecoveryAmount = BigDecimal.ZERO;
				for (BudgetDetail budgetDetail : budgetDetails) {
					if (budgetCategoryCode.equals(budgetDetail.getBudgetCategoryCode()) && (budgetPeriod.getBudgetPeriodId() == budgetDetail.getPeriod().getBudgetPeriodId()) && !budgetCategoryCode.equals(Constants.BUDGET_CATEGORY_TYPE_CODE_BUDGET_SUMMARY_TOTAL)) {
						if (budgetDetail.getSponsorRequestedAmount() != null) {
							totalFundRequested = totalFundRequested.add(budgetDetail.getSponsorRequestedAmount());
						}
						if (budgetDetail.getLineItemCost() != null) {
							totalLineItemCost = totalLineItemCost.add(budgetDetail.getLineItemCost());
						}
						if (budgetDetail.getCostSharingAmount() != null) {
							if (budgetDetail.getSystemGeneratedCEType() != null && budgetDetail.getSystemGeneratedCEType().equals("BUDGET_OH_ON")) {
								totalUnderRecoveryAmount = budgetPeriod.getUnderrecoveryAmount();
								totalCostShareAmount = totalCostShareAmount.add(totalUnderRecoveryAmount).add(budgetDetail.getCostSharingAmount());
							} else {
								totalCostShareAmount = totalCostShareAmount.add(budgetDetail.getCostSharingAmount());
							}
						}
					}
				}
				totalLineItemCostSum = totalLineItemCostSum.add(totalLineItemCost);
				totalCostShareAmountSum = totalCostShareAmountSum.add(totalCostShareAmount);
				totalFundRequestedSum = totalFundRequestedSum.add(totalFundRequested);
				BudgetSummaryVO budgetSummaryVO = new BudgetSummaryVO();
				budgetSummaryVO.setPeriodNumber(budgetPeriod.getBudgetPeriod());
				budgetSummaryVO.setLineItemCost(totalLineItemCost);
				budgetSummaryVO.setTotalCostSharingAmount(totalCostShareAmount);
				budgetSummaryVO.setTotalFundRequested(totalFundRequested);
				totalLineItemCosts.add(budgetSummaryVO);
			}

			periodTotalSum = periodTotalSum.add(totalLineItemCostSum);
			periodCostShareTotalSum = periodCostShareTotalSum.add(totalCostShareAmountSum).setScale(2, RoundingMode.HALF_UP).setScale(2);
			periodTotalFundRequestedSum = periodTotalFundRequestedSum.add(totalFundRequestedSum);
			budgetSummaryTotals.setPeriodTotalSum(periodTotalSum);
			budgetSummaryTotals.setPeriodTotalFundRequestedSum(periodTotalFundRequestedSum);
			budgetSummaryTotals.setPeriodCostShareTotalSum(periodCostShareTotalSum);
			budgetPeriodSummary.setBudgetSummaryVOs(totalLineItemCosts);
			budgetPeriodSummary.setTotalLineItemCostSum(totalLineItemCostSum);
			budgetPeriodSummary.setTotalFundRequestedCostSum(totalFundRequestedSum);
			budgetPeriodSummary.setTotalCostShareAmountSum(totalCostShareAmountSum);
			budgetPeriodSummaries.add(budgetPeriodSummary);
		}
		return budgetSummaryTotals;
	}

	private BudgetSummaryVO setBudgetPeriodSummaryBasedOnCostElements(List<BudgetPeriodSummary> budgetPeriodSummaries, List<BudgetPeriod> budgetPeriods, Set<String> costElements, List<BudgetDetail> budgetDetails, BudgetSummaryVO budgetSummaryTotals) {
		BigDecimal periodTotalSum = budgetSummaryTotals.getPeriodTotalSum();
		BigDecimal periodCostShareTotalSum = budgetSummaryTotals.getPeriodCostShareTotalSum();
		BigDecimal periodTotalFundRequestedSum = budgetSummaryTotals.getPeriodTotalFundRequestedSum();
		for (String costElement : costElements) {
			BudgetPeriodSummary budgetPeriodSummary = new BudgetPeriodSummary();
			BigDecimal totalLineItemCost;
			BigDecimal totalCostShareAmount;
			BigDecimal totalFundRequested = BigDecimal.ZERO;
			BigDecimal totalLineItemCostSum = BigDecimal.ZERO;
			BigDecimal totalFundRequestedSum = BigDecimal.ZERO;
			BigDecimal totalCostShareAmountSum = BigDecimal.ZERO;
			BigDecimal totalUnderRecoveryAmount = BigDecimal.ZERO;
			List<BudgetSummaryVO> totalLineItemCosts = new ArrayList<>();
			budgetPeriodSummary.setBudgetCategory(budgetDao.fetchCostElementName(costElement));
			for (BudgetPeriod budgetPeriod : budgetPeriods) {
				totalUnderRecoveryAmount = BigDecimal.ZERO;
				totalLineItemCost = BigDecimal.ZERO;
				totalFundRequested = BigDecimal.ZERO;
				totalCostShareAmount = BigDecimal.ZERO;
				for (BudgetDetail budgetDetail : budgetDetails) {
					if (costElement.equals(budgetDetail.getCostElementCode()) && (budgetPeriod.getBudgetPeriodId() == budgetDetail.getPeriod().getBudgetPeriodId()) && budgetDetail.getBudgetCategoryCode().equals(Constants.BUDGET_CATEGORY_TYPE_CODE_BUDGET_SUMMARY_TOTAL)) {
						if (budgetDetail.getSponsorRequestedAmount() != null) {
							totalFundRequested = totalFundRequested.add(budgetDetail.getSponsorRequestedAmount());
						}
						if (budgetDetail.getLineItemCost() != null) {
							totalLineItemCost = totalLineItemCost.add(budgetDetail.getLineItemCost());
						}
						if (budgetDetail.getCostSharingAmount() != null) {
							if (budgetDetail.getSystemGeneratedCEType() != null && budgetDetail.getSystemGeneratedCEType().equals("BUDGET_OH_ON")) {
								totalUnderRecoveryAmount = budgetPeriod.getUnderrecoveryAmount();
								totalCostShareAmount = totalCostShareAmount.add(totalUnderRecoveryAmount).add(budgetDetail.getCostSharingAmount());
							} else {
								totalCostShareAmount = totalCostShareAmount.add(budgetDetail.getCostSharingAmount());
							}
						}
					}
				}
				totalLineItemCostSum = totalLineItemCostSum.add(totalLineItemCost);
				totalCostShareAmountSum = totalCostShareAmountSum.add(totalCostShareAmount);
				totalFundRequestedSum = totalFundRequestedSum.add(totalFundRequested);
				BudgetSummaryVO budgetSummaryVO = new BudgetSummaryVO();
				budgetSummaryVO.setPeriodNumber(budgetPeriod.getBudgetPeriod());
				budgetSummaryVO.setLineItemCost(totalLineItemCost);
				budgetSummaryVO.setTotalFundRequested(totalFundRequested);
				budgetSummaryVO.setTotalCostSharingAmount(totalCostShareAmount);
				totalLineItemCosts.add(budgetSummaryVO);
			}
			periodTotalSum = periodTotalSum.add(totalLineItemCostSum);
			periodTotalFundRequestedSum = periodTotalFundRequestedSum.add(totalFundRequestedSum);
			periodCostShareTotalSum = periodCostShareTotalSum.add(totalCostShareAmountSum).setScale(2, RoundingMode.HALF_UP).setScale(2);
			budgetSummaryTotals.setPeriodTotalSum(periodTotalSum);
			budgetSummaryTotals.setPeriodTotalFundRequestedSum(periodTotalFundRequestedSum);
			budgetSummaryTotals.setPeriodCostShareTotalSum(periodCostShareTotalSum);
			budgetPeriodSummary.setBudgetSummaryVOs(totalLineItemCosts);
			budgetPeriodSummary.setTotalLineItemCostSum(totalLineItemCostSum);
			budgetPeriodSummary.setTotalFundRequestedCostSum(totalFundRequestedSum);
			budgetPeriodSummary.setTotalCostShareAmountSum(totalCostShareAmountSum);
			budgetPeriodSummaries.add(budgetPeriodSummary);
		}
		return budgetSummaryTotals;
	}

	private List<BudgetPrintParameter> setBudgetPersonelList(List<BudgetPerson> budgetPersons) {
		List<BudgetPrintParameter> budgetPrintParameters = new ArrayList<BudgetPrintParameter>();
		if (budgetPersons != null && !budgetPersons.isEmpty()) {
			for (BudgetPerson budgetPerson : budgetPersons) {
				String effectiveDate = "";
				String personType = "";
				String personName = budgetPerson.getPersonName() != null ? budgetPerson.getPersonName() : "";
				String baseSalary = budgetPerson.getCalculationBase() != null ? ("" + budgetPerson.getCalculationBase()) : "";
				String appointmentType = budgetPerson.getAppointmentType() != null ? budgetPerson.getAppointmentType().getDescription() : "";
				String jobCode = budgetPerson.getJobCode() != null ? budgetPerson.getJobCode().getJobTitle() : "";
				if (budgetPerson.getEffectiveDate() != null) {
					effectiveDate = commonService.convertDateFormatBasedOnTimeZone(budgetPerson.getEffectiveDate().getTime(), Constants.DEFAULT_DATE_FORMAT);
				}
				if (budgetPerson.getPersonType().equals(Constants.EMPLOYEE_PERSON_TYPE)) {
					personType = "Employee";
				} else if (budgetPerson.getPersonType().equals(Constants.PROPOSAL_PERSON_TYPE)) {
					personType = "Proposal Persons";
				} else if (budgetPerson.getPersonType().equals(Constants.NON_EMPLOYEE_TYPE)) {
					personType = "Non Employee";
				} else if (budgetPerson.getPersonType().equals(Constants.TBN_PERSON_TYPE)) {
					personType = "To Be Named";
				}
				budgetPrintParameters.add(new BudgetPrintParameter(personType, personName, appointmentType, jobCode, effectiveDate, baseSalary));
			}
		} else {
			budgetPrintParameters.add(new BudgetPrintParameter("", "", "", "", "", ""));
		}
		return budgetPrintParameters;
	}

	private void prepareExcelSheetForBudgetSummary(BudgetPrintParameter budgetPrintParameter, List<BudgetPeriod> budgetPeriods, XSSFSheet sheet, XSSFWorkbook workbook, Proposal proposal, BudgetHeader budgetHeader) {
		int headingCellNumber = 0;
		Row budgetTableHeadingRow = sheet.createRow(8);
		XSSFCellStyle tableHeadStyle = workbook.createCellStyle();
		XSSFCellStyle tableBodyStyle = workbook.createCellStyle();
		sheet.addMergedRegion(new CellRangeAddress(0, 0, 0, 1));
		prepareHeadingRowForExcel(sheet, workbook, tableHeadStyle, tableBodyStyle, GENERAL_PROPOSAL_INFORMATION, 0);
		prepareProposalSummaryForExcel(sheet, 1, tableHeadStyle, tableBodyStyle, proposal, workbook);
		String heading = Boolean.TRUE.equals(commonDao.getParameterValueAsBoolean(Constants.ENABLE_PROPOSAL_BUDGET_VERSIONS)) ? "Budget Summary(Version " + budgetHeader.getVersionNumber() + ")" : BUDGET_SUMMARY;
		prepareHeadingRowForExcel(sheet, workbook, tableHeadStyle, tableBodyStyle, heading, 7);
		Boolean isShowInKind = commonDao.getParameterValueAsBoolean(Constants.IS_SHOW_IN_KIND);
		// Set table body data to each column.
		int rowNumber = 9;
		List<BudgetPeriodSummary> budgetPeriodSummaries = budgetPrintParameter.getBudgetPeriodSummaries();
		List<BudgetSummaryVO> budgetSummaryVOs = budgetPrintParameter.getBudgetSummaryVOs();

		Cell tableCell = budgetTableHeadingRow.createCell(0);
		tableCell.setCellValue(CATEGORY);
		tableCell.setCellStyle(tableHeadStyle);
		headingCellNumber++;

		for (BudgetPeriod budgetPeriod : budgetPeriods) {
			Cell cell = budgetTableHeadingRow.createCell(headingCellNumber++);
			cell.setCellValue("Period " + budgetPeriod.getBudgetPeriod() + " (" + Constants.DOLLAR_SYMBOL + ")");
			cell.setCellStyle(tableHeadStyle);
		}
		
		if (Boolean.TRUE.equals(isShowInKind)) {
			Cell tableCellTotalInKind = budgetTableHeadingRow.createCell(headingCellNumber);
            tableCellTotalInKind.setCellValue("IN_KIND_TOTAL (" + Constants.DOLLAR_SYMBOL + ")");
			tableCellTotalInKind.setCellStyle(tableHeadStyle);
			headingCellNumber++;
		}
		
		Cell tableCellTotalRequestedCost = budgetTableHeadingRow.createCell(headingCellNumber);
        tableCellTotalRequestedCost.setCellValue(TOTAL_REQUESTED_COST + " (" + Constants.DOLLAR_SYMBOL + ")");
		tableCellTotalRequestedCost.setCellStyle(tableHeadStyle);
		headingCellNumber++;

		Cell totalHeadingCell = budgetTableHeadingRow.createCell(headingCellNumber);
		totalHeadingCell.setCellValue("Total Cost (" + Constants.DOLLAR_SYMBOL + ")");
		totalHeadingCell.setCellStyle(tableHeadStyle);
		sheet.addMergedRegion(new CellRangeAddress(7, 7, 0, headingCellNumber));
		prepareBudgetSummaryForExcel(sheet, rowNumber, budgetPeriodSummaries, tableBodyStyle, budgetPrintParameter, budgetSummaryVOs, workbook);
		autoSizeColumns(workbook, rowNumber);
	}

	@Override
	public void prepareHeadingRowForExcel(XSSFSheet sheet, XSSFWorkbook workbook, XSSFCellStyle tableHeadStyle, XSSFCellStyle tableBodyStyle, String headingName, int rowNumber) {
		Row headerRow = sheet.createRow(rowNumber);
		Cell headingCell = headerRow.createCell(0);
		headingCell.setCellValue(headingName);
		XSSFFont headerFont = workbook.createFont();
		headerFont.setBold(true);
		headerFont.setFontHeightInPoints((short) 15);
		XSSFCellStyle headerStyle = workbook.createCellStyle();
		headerStyle.setAlignment(HorizontalAlignment.CENTER);
		headerStyle.setFont(headerFont);
		headingCell.setCellStyle(headerStyle);
		// Table head style and font creation code.
		XSSFFont tableHeadFont = workbook.createFont();
		tableHeadFont.setBold(true);
		tableHeadFont.setFontHeightInPoints((short) 13);
		tableHeadStyle.setFont(tableHeadFont);
		// Table body style and font creation code.
		XSSFFont tableBodyFont = workbook.createFont();
		tableBodyFont.setFontHeightInPoints((short) 12);
		tableBodyStyle.setFont(tableBodyFont);
	}

	@Override
	public void autoSizeColumns(XSSFWorkbook workbook, int rowNumber) {
		XSSFSheet sheet = workbook.getSheetAt(0);
		if (sheet.getPhysicalNumberOfRows() > 0) {
			Row row = sheet.getRow(rowNumber);
			Iterator<Cell> cellIterator = row.cellIterator();
			while (cellIterator.hasNext()) {
				Cell cell = cellIterator.next();
				int columnIndex = cell.getColumnIndex();
				sheet.autoSizeColumn(columnIndex);
			}
		}
	}

	private void prepareBudgetSummaryForExcel(XSSFSheet sheet, int rowNumber, List<BudgetPeriodSummary> budgetPeriodSummaries, XSSFCellStyle tableBodyStyle, BudgetPrintParameter budgetPrintParameter, List<BudgetSummaryVO> budgetSummaryVOs, XSSFWorkbook workbook) {
		XSSFCellStyle budgetSummaryTotalRowStyle = prepareTableRowFont(workbook);
		Boolean isShowInKind = commonDao.getParameterValueAsBoolean(Constants.IS_SHOW_IN_KIND);
        List<BudgetPeriodSummary> sortedBudgetPeriodSummary = budgetPeriodSummaries.stream().sorted(Comparator.comparing(BudgetPeriodSummary::getSortOrder)).collect(Collectors.toList());
		for (BudgetPeriodSummary budgetPeriodSummary : sortedBudgetPeriodSummary) {
			Row row = sheet.createRow(rowNumber++);
			int cellNumber = 0;
			Cell categoryNameCell = row.createCell(cellNumber++);
			categoryNameCell.setCellStyle(tableBodyStyle);
			categoryNameCell.setCellValue(budgetPeriodSummary.getBudgetCategory());

			for (BudgetSummaryVO budgetSummaryVO : budgetPeriodSummary.getBudgetSummaryVOs()) {
				Cell periodCostCell = row.createCell(cellNumber++);
				periodCostCell.setCellStyle(tableBodyStyle);
				periodCostCell.setCellValue(budgetSummaryVO.getTotalFundRequested().toString());
			}
			if (Boolean.TRUE.equals(isShowInKind)) {
				Cell categoryInKindCell = row.createCell(cellNumber++);
				categoryInKindCell.setCellStyle(tableBodyStyle);
				categoryInKindCell.setCellValue(budgetPeriodSummary.getTotalCostShareAmountSum().toString());
			}
			
			Cell categoryFundRequestedCell = row.createCell(cellNumber++);
			categoryFundRequestedCell.setCellStyle(tableBodyStyle);
			categoryFundRequestedCell.setCellValue(budgetPeriodSummary.getTotalFundRequestedCostSum().toString());
			
			Cell categoryCostSumCell = row.createCell(cellNumber++);
			categoryCostSumCell.setCellStyle(tableBodyStyle);
			categoryCostSumCell.setCellValue(budgetPeriodSummary.getTotalLineItemCostSum().toString());
		}
		Row row = sheet.createRow(rowNumber);
		int cellNumber = 0;
		Cell categoryNameCell = row.createCell(cellNumber++);
		categoryNameCell.setCellStyle(budgetSummaryTotalRowStyle);
		categoryNameCell.setCellValue(GRAND_TOTAL);

		for (BudgetSummaryVO budgetSummaryVO : budgetSummaryVOs) {
			Cell periodTotalCostCell = row.createCell(cellNumber++);
			periodTotalCostCell.setCellStyle(budgetSummaryTotalRowStyle);
			periodTotalCostCell.setCellValue(budgetSummaryVO.getTotalFundRequestedAmount());
		}
		if (Boolean.TRUE.equals(isShowInKind)) {
			Cell periodTotalInKindCell = row.createCell(cellNumber++);
			periodTotalInKindCell.setCellStyle(budgetSummaryTotalRowStyle);
			periodTotalInKindCell.setCellValue(budgetPrintParameter.getPeriodTotalCostShareSum());
		}
		Cell periodTotalRequestedCell = row.createCell(cellNumber++);
		periodTotalRequestedCell.setCellStyle(budgetSummaryTotalRowStyle);
		periodTotalRequestedCell.setCellValue(budgetPrintParameter.getPeriodTotalFundRequestedSum());
		
		Cell periodCostsTotalSumCell = row.createCell(cellNumber++);
		periodCostsTotalSumCell.setCellStyle(budgetSummaryTotalRowStyle);
		periodCostsTotalSumCell.setCellValue(budgetPrintParameter.getPeriodCostsTotalSum());
	}

	private void prepareProposalSummaryForExcel(XSSFSheet sheet, int rowNumber, XSSFCellStyle tableHeadStyle, XSSFCellStyle tableBodyStyle, Proposal proposal, XSSFWorkbook workbook) {
		proposal.setProposalPersons(proposalModuleDao.fetchProposalPersonBasedOnProposalId(proposal.getProposalId()));
		Row proposalIdRow = sheet.createRow(rowNumber++);
		Cell proposalIdHeadingCell = proposalIdRow.createCell(0);
		proposalIdHeadingCell.setCellStyle(tableBodyStyle);
		proposalIdHeadingCell.setCellValue("Proposal ID and Title");

		Cell proposalIdValueCell = proposalIdRow.createCell(1);
		proposalIdValueCell.setCellStyle(tableHeadStyle);
		proposalIdValueCell.setCellValue(proposal.getProposalId() + " - " + proposal.getTitle());

		Row piRow = sheet.createRow(rowNumber++);
		Cell piHeadingCell = piRow.createCell(0);
		piHeadingCell.setCellStyle(tableBodyStyle);
		piHeadingCell.setCellValue("Principal Investigator");

		Cell piValueCell = piRow.createCell(1);
		piValueCell.setCellStyle(tableHeadStyle);
		piValueCell.setCellValue(proposal.getInvestigator() != null ? proposal.getInvestigator().getFullName() : "");

		Row fundingAgencyRow = sheet.createRow(rowNumber++);
		Cell fundingAgencyHeadingCell = fundingAgencyRow.createCell(0);
		fundingAgencyHeadingCell.setCellStyle(tableBodyStyle);
		fundingAgencyHeadingCell.setCellValue("Sponsor");

		Cell fundingAgencyValueCell = fundingAgencyRow.createCell(1);
		fundingAgencyValueCell.setCellStyle(tableHeadStyle);
        fundingAgencyValueCell.setCellValue(proposal.getSponsor() != null ? commonService.getSponsorFormatBySponsorDetail(proposal.getSponsor().getSponsorCode(), proposal.getSponsor().getSponsorName(), proposal.getSponsor().getAcronym()) : "");

		Row proposalStartDateRow = sheet.createRow(rowNumber++);
		Cell proposalStartDateHeadingCell = proposalStartDateRow.createCell(0);
		proposalStartDateHeadingCell.setCellStyle(tableBodyStyle);
		proposalStartDateHeadingCell.setCellValue("Proposed Start Date");

		Cell proposalStartDateValueCell = proposalStartDateRow.createCell(1);
		proposalStartDateValueCell.setCellStyle(tableHeadStyle);
		proposalStartDateValueCell
				.setCellValue(proposal.getStartDate() != null ? commonService.convertDateFormatBasedOnTimeZone(
						proposal.getStartDate().getTime(), Constants.DEFAULT_DATE_FORMAT) : "");

		Row proposalEndDateRow = sheet.createRow(rowNumber++);
		Cell proposalEndDateHeadingCell = proposalEndDateRow.createCell(0);
		proposalEndDateHeadingCell.setCellStyle(tableBodyStyle);
		proposalEndDateHeadingCell.setCellValue("Proposed End Date");

		Cell proposalEndDateValueCell = proposalEndDateRow.createCell(1);
		proposalEndDateValueCell.setCellStyle(tableHeadStyle);
		proposalEndDateValueCell.setCellValue(proposal.getEndDate() != null ? commonService
				.convertDateFormatBasedOnTimeZone(proposal.getEndDate().getTime(), Constants.DEFAULT_DATE_FORMAT) : "");
		sheet.addMergedRegion(new CellRangeAddress(rowNumber, rowNumber, 0, 1));
	}

	@Override
	public XSSFCellStyle prepareTableRowFont(XSSFWorkbook workbook) {
		XSSFFont tableRowFont = workbook.createFont();
		XSSFCellStyle tableRowStyle = workbook.createCellStyle();
		tableRowFont.setBold(true);
		tableRowFont.setFontHeightInPoints((short) 12);
		tableRowStyle.setFont(tableRowFont);
		return tableRowStyle;
	}

	private IContext prepareBudgetHeaderDetails(IContext context, BudgetHeader budgetHeader) {
		if (budgetHeader != null) {
			context.put("TOTAL_COST", budgetHeader.getTotalCost() != null ? Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetHeader.getTotalCost())
							: "");
			context.put("TOTAL_DIRECT_COST", budgetHeader.getTotalDirectCost() != null ? Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetHeader.getTotalDirectCost()) : "");
			context.put("TOTAL_INDIRECT_COST", budgetHeader.getTotalIndirectCost() != null ? Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetHeader.getTotalIndirectCost()) : "");
			context.put("COST_SHARING_AMOUNT", budgetHeader.getCostSharingAmount() != null ? Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetHeader.getCostSharingAmount()) : "");
			context.put("UNDERRECOVERY_AMOUNT", budgetHeader.getUnderrecoveryAmount() != null ? Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetHeader.getUnderrecoveryAmount()) : "");
			context.put("BUDGET_STATUS", budgetHeader.getBudgetStatus() != null ? budgetHeader.getBudgetStatus().getDescription() : "");
            Boolean isOverheadRateTypeEnabled = commonDao.getParameterValueAsBoolean(Constants.ENABLE_OVERHEAD_RATE_TYPE);
            context.put("OVER_HEAD_RATE_TYPE", Boolean.TRUE.equals(isOverheadRateTypeEnabled) && budgetHeader.getRateTypeCode() != null ? budgetHeader.getRateType().getDescription() : "");
			context.put("UNDER_RECOVERY_RATE_TYPE", budgetHeader.getUnderrecoveryRateClassCode() != null ? budgetHeader.getUnderrecoveryRateType().getDescription() : "");
            Boolean isCampusFlagEnabled = commonDao.getParameterValueAsBoolean(Constants.ENABLE_CAMPUS_FLAG_PROPOSAL);
			if (Boolean.TRUE.equals(isCampusFlagEnabled)) {
				if (budgetHeader.getCampusFlag() != null) {
					if (budgetHeader.getCampusFlag().equals("F")) {
						context.put("ON_OFF_CAMPUS_FLAG", "OFF" + "");
					} else if (budgetHeader.getCampusFlag().equals("N")) {
						context.put("ON_OFF_CAMPUS_FLAG", "ON" + "");
					} else if (budgetHeader.getCampusFlag().equals("D")) {
						context.put("ON_OFF_CAMPUS_FLAG", "BOTH" + "");
					}
				} else {
					context.put("ON_OFF_CAMPUS_FLAG", "");
				}
			}
			context.put("DESCRIPTION", budgetHeader.getComments() != null ? budgetHeader.getComments() : "");
			if (budgetHeader.getIsFinalBudget() != null) {
				if (budgetHeader.getIsFinalBudget()) {
					context.put("IS_BUDGET_FINAL", "Yes");
				} else {
					context.put("IS_BUDGET_FINAL", "No");
				}
			} else {
				context.put("IS_BUDGET_FINAL", "");
			}
			context.put("OVERHEAD_RATE_TYPE", budgetHeader.getRateType() != null ? budgetHeader.getRateType().getDescription() : "");
			context.put("MODIFIED_DIRECT_COST", budgetHeader.getTotalModifiedDirectCost() != null ? Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetHeader.getTotalModifiedDirectCost()) : "");
			context.put("VERSION_NUMBER", budgetHeader.getVersionNumber() != null ? budgetHeader.getVersionNumber().toString() : "");
		} else {
			context.put("TOTAL_COST", "");
			context.put("TOTAL_DIRECT_COST", "");
			context.put("TOTAL_INDIRECT_COST", "");
			context.put("COST_SHARING_AMOUNT", "");
			context.put("UNDERRECOVERY_AMOUNT", "");
			context.put("BUDGET_STATUS", "");
			context.put("OVER_HEAD_RATE_TYPE", "");
			context.put("UNDER_RECOVERY_RATE_TYPE", "");
			context.put("ON_OFF_CAMPUS_FLAG", "");
			context.put("DESCRIPTION", "");
			context.put("IS_BUDGET_FINAL", "");
			context.put("OVERHEAD_RATE_TYPE", "");
			context.put("MODIFIED_DIRECT_COST", "");
			context.put("VERSION_NUMBER", "");
		}
		return context;
	}

	private void prepareBudgetPeriodsInformation(List<BudgetPeriodPrintParameter> budgetPeriods) {
		BudgetPeriodPrintParameter budgetPeriodPrintParameter = new BudgetPeriodPrintParameter();
		budgetPeriodPrintParameter.setStartDate("");
		budgetPeriodPrintParameter.setEndDate("");
		budgetPeriodPrintParameter.setDirectCost("");
		budgetPeriodPrintParameter.setIndirectCost("");
		budgetPeriodPrintParameter.setTotalCost("");
		List<LineItemData> lineItemDatas = new ArrayList<>();
		LineItemData lineItemData = new LineItemData();
		lineItemData.setCostElement("");
		lineItemData.setLineItemDescription("");
		lineItemData.setQuantity("");
		lineItemData.setLineItemCost("");
		lineItemData.setCostSharingPercentage("");
		lineItemData.setCostSharingAmount("");
		lineItemData.setSponsorRequestedAmount("");
		lineItemData.setBudgetCategory("");
		lineItemData.setInternalOrderCode("");
		lineItemData.setPreviousLineItemCost("");
		lineItemData.setBalanceToDate("");
		lineItemData.setHasBudgetPerson(false);
		lineItemData.setIsBudgetCategoryNotSystemGenerated(true);
		lineItemData.getBudgetPersonalDetails().add(addBudgetPersonalDetail());
		lineItemDatas.add(lineItemData);
		budgetPeriodPrintParameter.setPeriod("");
		budgetPeriodPrintParameter.setLineItemList(lineItemDatas);
		budgetPeriods.add(budgetPeriodPrintParameter);
	}

	private void setAwardBudgetHeaderBasedOnVersionNumber(AwardBudgetHeader awardBudgetHeader) {
		List<AwardBudgetPeriod> updatedAwardBudgetPeriods = new ArrayList<>();
		for (AwardBudgetPeriod awardBudgetPeriod : awardBudgetHeader.getBudgetPeriods()) {
			List<AwardBudgetDetail> updatedAwardBudgetDetails = new ArrayList<>();
			for (AwardBudgetDetail awardBudgetDetail : awardBudgetPeriod.getBudgetDetails()) {
				awardBudgetService.calculateBalanceToDateValue(awardBudgetDetail, awardBudgetHeader.getFundCode());
				updatedAwardBudgetDetails.add(awardBudgetDetail);
			}
			awardBudgetPeriod.getBudgetDetails().clear();
			awardBudgetPeriod.getBudgetDetails().addAll(updatedAwardBudgetDetails);
			updatedAwardBudgetPeriods.add(awardBudgetPeriod);
		}
		awardBudgetHeader.getBudgetPeriods().clear();
		awardBudgetHeader.getBudgetPeriods().addAll(updatedAwardBudgetPeriods);
	}

	@Override
	public ResponseEntity<byte[]> generateQuestionnaireReport(HttpServletResponse response, CommonVO vo) {
		ResponseEntity<byte[]> attachmentData = null;
		byte[] bFile = null;
		byte[] mergedOutput = null;
		try {
			Integer moduleItemKey = vo.getModuleItemKey() == null ? null : Integer.parseInt(vo.getModuleItemKey());
			if (vo.getModuleCode().equals(Constants.DEV_PROPOSAL_MODULE_CODE)) {
				bFile = getTemplateData(Constants.PROPOSAL_QUESTIONNAIRE_LETTER_TEMPLATE_TYPE_CODE);
				mergedOutput = setMergePlaceHoldersOfProposal(bFile, moduleItemKey, vo.getPersonId(), vo.getUserName(), vo.getSubModuleCode(), vo.getSubModuleItemKey(), null);
			} else if (vo.getModuleCode().equals(Constants.AWARD_MODULE_CODE)) {
				if (vo.getSubModuleCode().equals(Constants.AWARD_SUBMODULE_CODE)) {
					bFile = getTemplateData(Constants.AWARD_QUESTIONNAIRE_LETTER_TEMPLATE_TYPE_CODE);
				} else if (vo.getSubModuleCode().equals(Constants.AWARD_DMP_SUBMODULE_CODE)) {
					bFile = getTemplateData(Constants.AWARD_DMP_QUESTIONNAIRE_LETTER_TEMPLATE_TYPE_CODE);
				}
				mergedOutput = mergePlaceHoldersOfAward(bFile, moduleItemKey, vo.getPersonId());
			} else if (vo.getModuleCode().equals(Constants.PROGRESS_REPORT_MODULE_CODE)) {
				bFile = getTemplateData(Constants.PROGRSS_REPORT_QUESTIONNAIRE_LETTER_TEMPLATE_TYPE_CODE);
				mergedOutput = mergePlaceHoldersOfProgressReport(bFile, moduleItemKey, vo.getPersonId());
            } else if (vo.getModuleCode().equals(Constants.AGREEMENT_MODULE_CODE)) {
                bFile = getTemplateData(Constants.AGREEMENT_QUESTIONNAIRE_LETTER_TEMPLATE_TYPE_CODE);
                mergedOutput = agreementPrintService.setMergePlaceHoldersOfAgreementSummary(bFile, moduleItemKey, vo.getPersonId(), vo.getUserName());
			}else if (vo.getModuleCode().equals(Constants.SERVICE_REQUEST_MODULE_CODE)) {
				bFile = getTemplateData(Constants.SERVICE_REQUEST_QUESTIONNAIRE_LETTER_TEMPLATE_TYPE_CODE);
			    mergedOutput = setMergePlaceHoldersOfServiceRequest(bFile, moduleItemKey, vo.getPersonId(), vo.getUserName(), vo.getSubModuleCode(), vo.getSubModuleItemKey());
		    }
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
			logger.error("Exception in generateQuestionnaireReport : {}", e.getMessage());
		}
		return attachmentData;
	}

	private byte[] mergePlaceHoldersOfProgressReport(byte[] data, Integer moduleItemKey, String personId) {
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		try {
			InputStream myInputStream = new ByteArrayInputStream(data);
			IXDocReport report = XDocReportRegistry.getRegistry().loadReport(myInputStream,
					TemplateEngineKind.Velocity);
			FieldsMetadata fieldsMetadata = report.createFieldsMetadata();
			AwardProgressReport progressReport = progressReportDao.loadAwardProgressReport(moduleItemKey);
			Award award = progressReport.getAward();
			fieldsMetadata.load("awardPersons", AwardPrintParameter.class, true);
			List<AwardPrintParameter> awardPersons = setAwardPersonDetails(award);
			fieldsMetadata.load("questionnaires", QuestionnairePrintParameter.class, true);
			List<QuestionnairePrintParameter> questionnaires = setQuestionnaireDetails(null, personId,
					Constants.PROGRESS_REPORT_MODULE_CODE, moduleItemKey, Constants.PROGRESS_REPORT_SUBMODULE_CODE, Constants.SUBMODULE_ITEM_KEY);			
			IContext context = report.createContext();
			context.put("awardPersons", awardPersons);
			context.put("questionnaires", questionnaires);
			context = setPRPlaceHolderData(context, award, progressReport);
			Options options = Options.getTo(ConverterTypeTo.PDF).via(ConverterTypeVia.XWPF);
			report.convert(context, options, baos);
		} catch (Exception e) {
			logger.error("Exception in mergePlaceHoldersOfProgressReport : {}", e.getMessage());
		}
		return baos.toByteArray();
	}

	@Override
	public ResponseEntity<byte[]> setAttachmentContent(String fileName, byte[] data) {
		HttpHeaders headers = new HttpHeaders();
		headers.setContentType(MediaType.parseMediaType("application/octet-stream"));
		headers.setContentDispositionFormData(fileName, fileName);
		headers.setContentLength(data.length);
		headers.setCacheControl("must-revalidate, post-check=0, pre-check=0");
		headers.setPragma("public");
		return new ResponseEntity<>(data, headers, HttpStatus.OK);
	}

	@Override
    public Integer prepareAwardGeneralInformationForExcel(XSSFSheet sheet, int rowNumber, String awardId, XSSFWorkbook workbook, String timesheetPersonId, Boolean isAwardTimesheetPrint) {
		Award award = awardDao.fetchAwardByAwardId(awardId);
		// Excel sheet heading style and font creation code.
		Row headerRow = sheet.createRow(rowNumber++);
		Cell headingCell = headerRow.createCell(0);
		headingCell.setCellValue("General Award Information");
		XSSFFont headerFont = workbook.createFont();
		headerFont.setBold(true);
		headerFont.setFontHeightInPoints((short) 15);
		XSSFCellStyle headerStyle = workbook.createCellStyle();
		headerStyle.setAlignment(HorizontalAlignment.CENTER);
		headerStyle.setFont(headerFont);
		headingCell.setCellStyle(headerStyle);
		// Table head style and font creation code.
		XSSFCellStyle tableHeadStyle = workbook.createCellStyle();
        tableHeadStyle.setWrapText(true);
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
        tableBodyStyle.setWrapText(true);
		tableBodyStyle.setBorderTop(BorderStyle.HAIR);
		tableBodyStyle.setBorderBottom(BorderStyle.HAIR);
		tableBodyStyle.setBorderLeft(BorderStyle.HAIR);
		tableBodyStyle.setBorderRight(BorderStyle.HAIR);
		XSSFFont tableBodyFont = workbook.createFont();
		tableBodyFont.setFontHeightInPoints((short) 12);
		tableBodyStyle.setFont(tableBodyFont);

		Row awardTitleRow = sheet.createRow(rowNumber++);
		Cell awardHeadingCell = awardTitleRow.createCell(0);
		awardHeadingCell.setCellStyle(tableBodyStyle);
		awardHeadingCell.setCellValue("Award Title");

		Cell awardValueCell = awardTitleRow.createCell(1);
        sheet.addMergedRegion(new CellRangeAddress(awardTitleRow.getRowNum(), awardTitleRow.getRowNum(), awardValueCell.getColumnIndex(), awardValueCell.getColumnIndex() + 3));
        setBordersToMergedCells(sheet, new CellRangeAddress(awardTitleRow.getRowNum(), awardTitleRow.getRowNum(), awardValueCell.getColumnIndex(), awardValueCell.getColumnIndex() + 3));
		awardValueCell.setCellStyle(tableHeadStyle);
        awardValueCell.setCellValue(convertStringToParagraph(award.getTitle()));
        awardTitleRow.setHeightInPoints(awardValueCell.getStringCellValue().split("\n").length * sheet.getDefaultRowHeightInPoints());

		Row awardStartDateRow = sheet.createRow(rowNumber++);
		Cell awardStartDateHeadingCell = awardStartDateRow.createCell(0);
		awardStartDateHeadingCell.setCellStyle(tableBodyStyle);
		awardStartDateHeadingCell.setCellValue("Start Date");

		Cell awardStartDateValueCell = awardStartDateRow.createCell(1);
        sheet.addMergedRegion(new CellRangeAddress(awardStartDateRow.getRowNum(), awardStartDateRow.getRowNum(), awardStartDateValueCell.getColumnIndex(), awardStartDateValueCell.getColumnIndex() + 3));
        setBordersToMergedCells(sheet, new CellRangeAddress(awardStartDateRow.getRowNum(), awardStartDateRow.getRowNum(), awardStartDateValueCell.getColumnIndex(), awardStartDateValueCell.getColumnIndex() + 3));
		awardStartDateValueCell.setCellValue(
				award.getBeginDate() != null
						? commonService.convertDateFormatBasedOnTimeZone(award.getBeginDate().getTime(),
								Constants.DEFAULT_DATE_FORMAT)
						: "");
        awardStartDateValueCell.setCellStyle(tableHeadStyle);

		Row awardEndDateRow = sheet.createRow(rowNumber++);
		Cell awardEndDateHeadingCell = awardEndDateRow.createCell(0);
		awardEndDateHeadingCell.setCellStyle(tableBodyStyle);
		awardEndDateHeadingCell.setCellValue("End Date");

		Cell proposalEndDateValueCell = awardEndDateRow.createCell(1);
		proposalEndDateValueCell.setCellStyle(tableHeadStyle);
        sheet.addMergedRegion(new CellRangeAddress(awardEndDateRow.getRowNum(), awardEndDateRow.getRowNum(), proposalEndDateValueCell.getColumnIndex(), proposalEndDateValueCell.getColumnIndex() + 3));
        setBordersToMergedCells(sheet, new CellRangeAddress(awardEndDateRow.getRowNum(), awardEndDateRow.getRowNum(), proposalEndDateValueCell.getColumnIndex(), proposalEndDateValueCell.getColumnIndex() + 3));
		proposalEndDateValueCell.setCellValue(award.getFinalExpirationDate() != null ? commonService.convertDateFormatBasedOnTimeZone(
						award.getFinalExpirationDate().getTime(), Constants.DEFAULT_DATE_FORMAT) : "");

		Row fundingAgencyRow = sheet.createRow(rowNumber++);
		Cell fundingAgencyHeadingCell = fundingAgencyRow.createCell(0);
		fundingAgencyHeadingCell.setCellStyle(tableBodyStyle);
		fundingAgencyHeadingCell.setCellValue("Sponsor");

		Cell fundingAgencyValueCell = fundingAgencyRow.createCell(1);
		fundingAgencyValueCell.setCellStyle(tableHeadStyle);
        sheet.addMergedRegion(new CellRangeAddress(fundingAgencyRow.getRowNum(), fundingAgencyRow.getRowNum(), fundingAgencyValueCell.getColumnIndex(), fundingAgencyValueCell.getColumnIndex() + 3));
        setBordersToMergedCells(sheet, new CellRangeAddress(fundingAgencyRow.getRowNum(), fundingAgencyRow.getRowNum(), fundingAgencyValueCell.getColumnIndex(), fundingAgencyValueCell.getColumnIndex() + 3));
        fundingAgencyValueCell.setCellValue(award.getSponsor() != null ? commonService.getSponsorFormatBySponsorDetail(award.getSponsor().getSponsorCode(), award.getSponsor().getSponsorName(), award.getSponsor().getAcronym()) : "");

		Row leadUnitRow = sheet.createRow(rowNumber++);
		Cell leadUnitHeadingCell = leadUnitRow.createCell(0);
		leadUnitHeadingCell.setCellStyle(tableBodyStyle);
		leadUnitHeadingCell.setCellValue("Lead Unit");

		Cell leadUnitValueCell = leadUnitRow.createCell(1);
		leadUnitValueCell.setCellStyle(tableHeadStyle);
        sheet.addMergedRegion(new CellRangeAddress(leadUnitRow.getRowNum(), leadUnitRow.getRowNum(), leadUnitValueCell.getColumnIndex(), leadUnitValueCell.getColumnIndex() + 3));
        setBordersToMergedCells(sheet, new CellRangeAddress(leadUnitRow.getRowNum(), leadUnitRow.getRowNum(), leadUnitValueCell.getColumnIndex(), leadUnitValueCell.getColumnIndex() + 3));
		leadUnitValueCell.setCellValue(award.getLeadUnit().getUnitName() != null ? award.getLeadUnit().getUnitName() : "");

		Row sponsorAwardRow = sheet.createRow(rowNumber++);
		Cell sponsorAwardHeadingCell = sponsorAwardRow.createCell(0);
		sponsorAwardHeadingCell.setCellStyle(tableBodyStyle);
		sponsorAwardHeadingCell.setCellValue("Sponsor Award Number");

		Cell sponsorAwardValueCell = sponsorAwardRow.createCell(1);
		sponsorAwardValueCell.setCellStyle(tableHeadStyle);
        sheet.addMergedRegion(new CellRangeAddress(sponsorAwardRow.getRowNum(), sponsorAwardRow.getRowNum(), sponsorAwardValueCell.getColumnIndex(), sponsorAwardValueCell.getColumnIndex() + 3));
        setBordersToMergedCells(sheet, new CellRangeAddress(sponsorAwardRow.getRowNum(), sponsorAwardRow.getRowNum(), sponsorAwardValueCell.getColumnIndex(), sponsorAwardValueCell.getColumnIndex() + 3));
		sponsorAwardValueCell.setCellValue(award.getSponsorAwardNumber() != null ? award.getSponsorAwardNumber() : "");

		Row pricipalInvestigatorRow = sheet.createRow(rowNumber++);
        Cell pricipalInvestigatorCell = pricipalInvestigatorRow.createCell(0);
		pricipalInvestigatorCell.setCellStyle(tableBodyStyle);
		pricipalInvestigatorCell.setCellValue("Lead Principal Investigator");

        Cell pricipalInvestigatorValueCell = pricipalInvestigatorRow.createCell(1);
		pricipalInvestigatorValueCell.setCellStyle(tableHeadStyle);
        sheet.addMergedRegion(new CellRangeAddress(pricipalInvestigatorRow.getRowNum(), pricipalInvestigatorRow.getRowNum(), pricipalInvestigatorValueCell.getColumnIndex(), pricipalInvestigatorValueCell.getColumnIndex() + 3));
        setBordersToMergedCells(sheet, new CellRangeAddress(pricipalInvestigatorRow.getRowNum(), pricipalInvestigatorRow.getRowNum(), pricipalInvestigatorValueCell.getColumnIndex(), pricipalInvestigatorValueCell.getColumnIndex() + 3));
		pricipalInvestigatorValueCell.setCellValue(award.getPrincipalInvestigator() != null ? award.getPrincipalInvestigator() : "");

        if (Boolean.TRUE.equals(isAwardTimesheetPrint)) {
        	String personName = null;
        	if (award.getAwardPersons() != null && !award.getAwardPersons().isEmpty()) {
    			for (AwardPerson awardPerson : award.getAwardPersons()) {
    				if (awardPerson.getPersonId() != null && timesheetPersonId.equals(awardPerson.getPersonId()) && awardPerson.getPersonRoleId().equals(Constants.COI_ROLE_CODE)) {
    					personName = awardPerson.getFullName();
    				}
    			}
    		}
        	if (personName != null) {
        		Row roleRow = sheet.createRow(rowNumber++);
                Cell roleHeadingCell = roleRow.createCell(0);
                roleHeadingCell.setCellStyle(tableBodyStyle);
                roleHeadingCell.setCellValue("Co-Investigator");

                Cell roleValueCell = roleRow.createCell(1);
                roleValueCell.setCellStyle(tableHeadStyle);
                sheet.addMergedRegion(new CellRangeAddress(roleRow.getRowNum(), roleRow.getRowNum(), roleValueCell.getColumnIndex(), roleValueCell.getColumnIndex() + 3));
                setBordersToMergedCells(sheet, new CellRangeAddress(roleRow.getRowNum(), roleRow.getRowNum(), roleValueCell.getColumnIndex(), roleValueCell.getColumnIndex() + 3));
                roleValueCell.setCellValue(personName != null ? personName : "");
        	}
        }
		return rowNumber;
	}

    @Override
	public String convertStringToParagraph(String description) {
    	StringBuilder descriptionDetail = new StringBuilder("");
    	if (description != null) {
			String[] words = description.split("\\s+");
	        Integer lineSperator = 10;
	        for(int i = 0; i < words.length; i++) {
	        	if(i < lineSperator) {
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

	@Override
	public ResponseEntity<byte[]> generateProposalDetailedBudgetExcelReport(HttpServletResponse response, Integer budgetId) {
		ResponseEntity<byte[]> attachmentData = null;
		try {
			XSSFWorkbook workbook = new XSSFWorkbook();
			BudgetHeader budgetHeader = budgetDao.fetchBudgetByBudgetId(budgetId);
			Proposal proposal = proposalDao.fetchProposalById(budgetHeader.getProposalId());
			XSSFSheet sheet = workbook.createSheet("Proposal Detailed Budget");
            commonService.addDetailsInHeader(workbook, sheet);
			prepareExcelSheetForProposalDetailedBudget(budgetHeader, proposal, sheet, workbook, budgetId);
			autoSizeColumns(workbook);
			ByteArrayOutputStream bos = new ByteArrayOutputStream();
			workbook.write(bos);
			attachmentData = getResponseEntityForExcelDownload(bos.toByteArray());
		} catch (Exception e) {
			logger.error("Exception in generateProposalDetailedBudgetExcelReport : {} ", e.getMessage());
		}
		return attachmentData;
	}

	private void prepareExcelSheetForProposalDetailedBudget(BudgetHeader budgetHeader, Proposal proposal, XSSFSheet sheet, XSSFWorkbook workbook, Integer budgetId) {
		XSSFCellStyle tableHeadStyle = workbook.createCellStyle();
		XSSFCellStyle tableBodyStyle = workbook.createCellStyle();
		sheet.addMergedRegion(new CellRangeAddress(0, 0, 0, 1));
		prepareHeadingRowForExcel(sheet, workbook, tableHeadStyle, tableBodyStyle, GENERAL_PROPOSAL_INFORMATION, 0);
		prepareProposalSummaryForExcel(sheet, 1, tableHeadStyle, tableBodyStyle, proposal, workbook);
		String heading = "Detailed Budget (" + budgetHeader.getBudgetStatus().getDescription() + ")";
		int totalColumnCount = 9;
		int rowNumber = 7;
		prepareExcelSheetHeading(sheet, heading, workbook, totalColumnCount, rowNumber);
		List<BudgetPeriod> budgetPeriods = budgetHeader.getBudgetPeriods();
		for (BudgetPeriod budgetPeriod : budgetPeriods) {
			rowNumber = rowNumber + 1;
            Object[] tableHeadingPeriod = {"Period " + budgetPeriod.getBudgetPeriod()};
			prepareExcelSheetHeader(sheet, tableHeadingPeriod, workbook, tableBodyStyle, rowNumber++);
			rowNumber = rowNumber + 1;
            Object[] tableHeadingDateAndCost = {"Start Date", "End Date",
					"Direct Cost (" + Constants.DOLLAR_SYMBOL + ")", "Indirect Cost (" + Constants.DOLLAR_SYMBOL + ")",
					"Cost Share (" + Constants.DOLLAR_SYMBOL + ")",
					"Under Recovery (" + Constants.DOLLAR_SYMBOL + ")",
                    "Total Cost (" + Constants.DOLLAR_SYMBOL + ")"};
			prepareExcelSheetHeader(sheet, tableHeadingDateAndCost, workbook, tableBodyStyle, rowNumber++);
			Row outerRow = sheet.createRow(rowNumber++);
			int cellNumber = 0;
			Cell cell1 = assignCell(cellNumber, tableBodyStyle, outerRow);
			if (budgetPeriod.getStartDate() != null) {
				Date date = (Date) budgetPeriod.getStartDate();
				String dateValue = commonService.convertDateFormatBasedOnTimeZone(date.getTime(),
						Constants.DEFAULT_DATE_FORMAT);
				cell1.setCellValue((String) dateValue);
			} else
				cell1.setCellValue(" ");
			cellNumber++;

			Cell cell2 = assignCell(cellNumber, tableBodyStyle, outerRow);
			if (budgetPeriod.getEndDate() != null) {
				Date date = budgetPeriod.getEndDate();
				String dateValue = commonService.convertDateFormatBasedOnTimeZone(date.getTime(),
						Constants.DEFAULT_DATE_FORMAT);
				cell2.setCellValue(dateValue);
			} else
				cell2.setCellValue(" ");
			cellNumber++;

			Cell cell3 = assignCell(cellNumber, tableBodyStyle, outerRow);
			if (budgetPeriod.getTotalDirectCost() != null)
				cell3.setCellValue(decimalFormat.format(budgetPeriod.getTotalDirectCost()));
			else
				cell3.setCellValue(" ");
			cellNumber++;

			Cell cell4 = assignCell(cellNumber, tableBodyStyle, outerRow);
			if (budgetPeriod.getTotalIndirectCost() != null)
				cell4.setCellValue(decimalFormat.format(budgetPeriod.getTotalIndirectCost()));
			else
				cell4.setCellValue(" ");
			cellNumber++;

			Cell cell5 = assignCell(cellNumber, tableBodyStyle, outerRow);
			if (budgetPeriod.getCostSharingAmount() != null)
				cell5.setCellValue(decimalFormat.format(budgetPeriod.getCostSharingAmount()));
			else
				cell5.setCellValue(" ");
			cellNumber++;

			Cell cell6 = assignCell(cellNumber, tableBodyStyle, outerRow);
			if (budgetPeriod.getUnderrecoveryAmount() != null)
				cell6.setCellValue(decimalFormat.format(budgetPeriod.getUnderrecoveryAmount()));
			else
				cell6.setCellValue(" ");
			cellNumber++;

			Cell cell7 = assignCell(cellNumber, tableBodyStyle, outerRow);
			if (budgetPeriod.getTotalCost() != null)
				cell7.setCellValue(decimalFormat.format(budgetPeriod.getTotalCost()));
			else
				cell7.setCellValue(" ");
			cellNumber++;

			rowNumber = rowNumber + 1;
			if (budgetPeriod.getBudgetDetails() != null && !budgetPeriod.getBudgetDetails().isEmpty()) {
                Object[] tableHeadingCostElement = {"Cost Element", "Line Item Description", "Quantity",
						"Line Item Cost (" + Constants.DOLLAR_SYMBOL + ")",
						"Cost Share(" + Constants.DOLLAR_SYMBOL + ")",
						"Cost Share Amount (" + Constants.DOLLAR_SYMBOL + ")",
                        "Fund Requested (" + Constants.DOLLAR_SYMBOL + ")"};
				prepareExcelSheetHeader(sheet, tableHeadingCostElement, workbook, tableBodyStyle, rowNumber++);
				for (BudgetDetail budgetDetail : budgetPeriod.getBudgetDetails()) {
					List<BudgetPersonalDetails> budgetPersonalDetails = new ArrayList<>();
					cellNumber = 0;
					Row innerRow = sheet.createRow(rowNumber++);
					Cell cell8 = assignCell(cellNumber, tableBodyStyle, innerRow);
					if (budgetDetail.getCostElementCode() != null)
						cell8.setCellValue(budgetDetail.getCostElementCode() + " - "
								+ budgetDetail.getCostElement().getDescription());
					else
						cell8.setCellValue(" ");
					cellNumber++;

					Cell cell9 = assignCell(cellNumber, tableBodyStyle, innerRow);
					if (budgetDetail.getLineItemDescription() != null)
						cell9.setCellValue(budgetDetail.getLineItemDescription());
					else
						cell9.setCellValue(" ");
					cellNumber++;

					Cell cell10 = assignCell(cellNumber, tableBodyStyle, innerRow);
					if (budgetDetail.getQuantity() != null)
						cell10.setCellValue(decimalFormat.format(budgetDetail.getQuantity()));
					else
						cell10.setCellValue(" ");
					cellNumber++;

					Cell cell11 = assignCell(cellNumber, tableBodyStyle, innerRow);
					if (budgetDetail.getLineItemCost() != null)
						cell11.setCellValue(decimalFormat.format(budgetDetail.getLineItemCost()));
					else
						cell11.setCellValue(" ");
					cellNumber++;

					Cell cell12 = assignCell(cellNumber, tableBodyStyle, innerRow);
					if (budgetDetail.getCostSharingPercentage() != null)
						cell12.setCellValue(budgetDetail.getCostSharingPercentage().doubleValue());
					else
						cell12.setCellValue(" ");
					cellNumber++;

					Cell cell13 = assignCell(cellNumber, tableBodyStyle, innerRow);
					if (budgetDetail.getCostSharingAmount() != null)
						cell13.setCellValue(decimalFormat.format(budgetDetail.getCostSharingAmount()));
					else
						cell13.setCellValue(" ");
					cellNumber++;

					Cell cell14 = assignCell(cellNumber, tableBodyStyle, innerRow);
					if (budgetDetail.getSponsorRequestedAmount() != null)
						cell14.setCellValue(decimalFormat.format(budgetDetail.getSponsorRequestedAmount()));
					else
						cell14.setCellValue(" ");
					cellNumber++;

					if (budgetDetail.getBudgetCategory().getBudgetCategoryTypeCode().equals("P")) {
						budgetPersonalDetails = budgetDetail.getPersonsDetails();
						if (budgetPersonalDetails != null && !budgetPersonalDetails.isEmpty()) {
                            Object[] tableHeadingPerson = {"Persons", "Start Date", "End Date",
									"Base Salary (" + Constants.DOLLAR_SYMBOL + ")", "Effort %",
									"Requested Salary (" + Constants.DOLLAR_SYMBOL + ")", "Cost-Share %",
									"Cost-Share (" + Constants.DOLLAR_SYMBOL + ")",
                                    "Fund Requested (" + Constants.DOLLAR_SYMBOL + ")"};
							prepareExcelSheetHeader(sheet, tableHeadingPerson, workbook, tableBodyStyle, rowNumber++);
							for (BudgetPersonalDetails budgetPersonalDetail : budgetPersonalDetails) {
								Row personRow = sheet.createRow(rowNumber++);
								cellNumber = 0;
								Cell cell15 = assignCell(cellNumber, tableBodyStyle, personRow);
								if (budgetPersonalDetail.getBudgetPerson().getPersonName() != null)
									cell15.setCellValue(budgetPersonalDetail.getBudgetPerson().getPersonName());
								else if (budgetPersonalDetail.getBudgetPerson().getTbnId() != null)
									cell15.setCellValue(
											budgetPersonalDetail.getBudgetPerson().getTbnPerson().getPersonName());
								else
									cell15.setCellValue(" ");
								cellNumber++;

								Cell cell16 = assignCell(cellNumber, tableBodyStyle, personRow);
								if (budgetPersonalDetail.getStartDate() != null) {
									Date date = budgetPersonalDetail.getStartDate();
									String dateValue = commonService.convertDateFormatBasedOnTimeZone(date.getTime(),
											Constants.DEFAULT_DATE_FORMAT);
									cell16.setCellValue(dateValue);
								} else
									cell16.setCellValue(" ");
								cellNumber++;

								Cell cell17 = assignCell(cellNumber, tableBodyStyle, personRow);
								if (budgetPersonalDetail.getEndDate() != null) {
									Date date = budgetPersonalDetail.getEndDate();
									String dateValue = commonService.convertDateFormatBasedOnTimeZone(date.getTime(),
											Constants.DEFAULT_DATE_FORMAT);
									cell17.setCellValue(dateValue);
								} else
									cell17.setCellValue(" ");
								cellNumber++;

								Cell cell18 = assignCell(cellNumber, tableBodyStyle, personRow);
								if (budgetPersonalDetail.getSalary() != null)
									cell18.setCellValue(decimalFormat.format(budgetPersonalDetail.getSalary()));
								else
									cell18.setCellValue(" ");
								cellNumber++;

								Cell cell19 = assignCell(cellNumber, tableBodyStyle, personRow);
								if (budgetPersonalDetail.getPercentageEffort() != null)
									cell19.setCellValue(budgetPersonalDetail.getPercentageEffort().doubleValue());
								else
									cell19.setCellValue(" ");
								cellNumber++;

								Cell cell20 = assignCell(cellNumber, tableBodyStyle, personRow);
								if (budgetPersonalDetail.getSalaryRequested() != null)
									cell20.setCellValue(
											decimalFormat.format(budgetPersonalDetail.getSalaryRequested()));
								else
									cell20.setCellValue(" ");
								cellNumber++;

								Cell cell21 = assignCell(cellNumber, tableBodyStyle, personRow);
								if (budgetPersonalDetail.getCostSharingPercentage() != null)
									cell21.setCellValue(budgetPersonalDetail.getCostSharingPercentage().doubleValue());
								else
									cell21.setCellValue(" ");
								cellNumber++;

								Cell cell22 = assignCell(cellNumber, tableBodyStyle, personRow);
								if (budgetPersonalDetail.getCostSharingAmount() != null)
									cell22.setCellValue(
											decimalFormat.format(budgetPersonalDetail.getCostSharingAmount()));
								else
									cell22.setCellValue(" ");
								cellNumber++;

								Cell cell23 = assignCell(cellNumber, tableBodyStyle, personRow);
								if (budgetPersonalDetail.getSponsorRequestedAmount() != null)
									cell23.setCellValue(
											decimalFormat.format(budgetPersonalDetail.getSponsorRequestedAmount()));
								else
									cell23.setCellValue(" ");
								cellNumber++;
							}
							rowNumber = rowNumber + 1;
						}
					}
				}
			}
		}
	}

	@Override
	public XSSFWorkbook prepareExcelSheetHeading(XSSFSheet sheet, String heading, XSSFWorkbook workbook, int columnNumber, int rowNumber) {
		Row headerRow = sheet.createRow(rowNumber);
		Cell headingCell = headerRow.createCell(0);
		headingCell.setCellValue((String) heading);
		sheet.addMergedRegion(new CellRangeAddress(rowNumber, rowNumber, 0, columnNumber - 1));
		XSSFFont headerFont = workbook.createFont();
		headerFont.setBold(true);
		headerFont.setFontHeightInPoints((short) 15);
		XSSFCellStyle headerStyle = workbook.createCellStyle();
		headerStyle.setAlignment(HorizontalAlignment.CENTER);
		headerStyle.setFont(headerFont);
		headingCell.setCellStyle(headerStyle);
		return workbook;
	}

	@Override
	public XSSFWorkbook prepareExcelSheetHeader(XSSFSheet sheet, Object[] tableHeadingRow, XSSFWorkbook workbook, XSSFCellStyle tableBodyStyle, int rowNumber) {
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
		autoSizeColumns(workbook, rowNumber);
		return workbook;
	}

	private Cell assignCell(int cellNumber, XSSFCellStyle tableBodyStyle, Row row) {
		Cell cell = row.createCell(cellNumber);
		cell.setCellStyle(tableBodyStyle);
		return cell;
	}

	@Override
	public ResponseEntity<byte[]> generateProposalSimpleBudgetExcelReport(HttpServletResponse response, Integer budgetId) {
		ResponseEntity<byte[]> attachmentData = null;
		try {
			XSSFWorkbook workbook = new XSSFWorkbook();
			BudgetHeader budgetHeader = budgetDao.fetchBudgetByBudgetId(budgetId);
			List<SimpleBudgetVO> simpleBudgetVOs = budgetService.prepareSimpleBudget(budgetId);
			if (simpleBudgetVOs != null && !simpleBudgetVOs.isEmpty()) {
				Collections.sort(simpleBudgetVOs, new SimpleBudgetDetailComparatorByBudgetCategoryCode());
				Collections.sort(simpleBudgetVOs, new SimpleBudgetDetailComparatorBySystemGenerated());
			}
			Proposal proposal = proposalDao.fetchProposalById(budgetHeader.getProposalId());
			XSSFSheet sheet = workbook.createSheet("Proposal Simple Budget");
            commonService.addDetailsInHeader(workbook, sheet);
			prepareExcelSheetForProposalSimpleBudget(simpleBudgetVOs, budgetHeader, proposal, sheet, workbook);
			autoSizeColumns(workbook);
			ByteArrayOutputStream bos = new ByteArrayOutputStream();
			workbook.write(bos);
			attachmentData = getResponseEntityForExcelDownload(bos.toByteArray());
		} catch (Exception e) {
			logger.error("Exception in generateProposalSimpleBudgetExcelReport : {} ", e.getMessage());
		}
		return attachmentData;
	}

	private void prepareExcelSheetForProposalSimpleBudget(List<SimpleBudgetVO> simpleBudgetVOs, BudgetHeader budgetHeader, Proposal proposal, XSSFSheet sheet, XSSFWorkbook workbook) {
		XSSFCellStyle tableHeadStyle = workbook.createCellStyle();
		XSSFCellStyle tableBodyStyle = workbook.createCellStyle();
		sheet.addMergedRegion(new CellRangeAddress(0, 0, 0, 1));
		prepareHeadingRowForExcel(sheet, workbook, tableHeadStyle, tableBodyStyle, GENERAL_PROPOSAL_INFORMATION, 0);
		prepareProposalSummaryForExcel(sheet, 1, tableHeadStyle, tableBodyStyle, proposal, workbook);
		List<BudgetPeriod> budgetPeriods = new ArrayList<>();
		if (budgetHeader.getBudgetPeriods() != null && !budgetHeader.getBudgetPeriods().isEmpty()) {
			budgetPeriods = budgetHeader.getBudgetPeriods();
		}
		String tableHeadingBudget = "Simple Budget (" + budgetHeader.getBudgetStatus().getDescription() + ")";
		int totalColumnCount = 4 + budgetPeriods.size();
		int rowNumber = 7;
		prepareExcelSheetHeading(sheet, tableHeadingBudget, workbook, totalColumnCount, rowNumber++);
        Object[] tableHeadingCost = {"Total Cost : " + Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetHeader.getTotalCost())};
		prepareExcelSheetHeader(sheet, tableHeadingCost, workbook, tableBodyStyle, rowNumber++);
		rowNumber = rowNumber + 1;
        Object[] tableHeading = {"Budget Category", "Cost Element", "Quantity"};
		int periodNumber = 1;
		for (int i = 0; i < budgetPeriods.size(); i++) {
			tableHeading = appendHeader(tableHeading, "Period " + periodNumber++ + "(" + Constants.DOLLAR_SYMBOL + ")");
		}
		tableHeading = appendHeader(tableHeading, "Justification");
		prepareExcelSheetHeader(sheet, tableHeading, workbook, tableBodyStyle, rowNumber++);
		if (simpleBudgetVOs != null && !simpleBudgetVOs.isEmpty()) {
			for (SimpleBudgetVO simpleBudget : simpleBudgetVOs) {
				int cellNumber = 0;
				Row row = sheet.createRow(rowNumber++);
				Cell cell1 = assignCell(cellNumber, tableBodyStyle, row);
				if (simpleBudget.getCategoryName() != null)
					cell1.setCellValue(simpleBudget.getCategoryName());
				else
					cell1.setCellValue(" ");
				cellNumber++;

				if (simpleBudget.getLineItemList() != null && !simpleBudget.getLineItemList().isEmpty()) {
					Boolean firstTime = true;
					for (SimpleBudgetLineItemVO simpleBudgetLineItem : simpleBudget.getLineItemList()) {
						cellNumber = 1;
						if (!firstTime) {
							row = sheet.createRow(rowNumber++);
						}
						Cell cell2 = assignCell(cellNumber, tableBodyStyle, row);
						if (simpleBudgetLineItem.getCostElement() != null)
							cell2.setCellValue(simpleBudgetLineItem.getCostElement().getDescription());
						else
							cell2.setCellValue(" ");
						cellNumber++;

						Cell cell3 = assignCell(cellNumber, tableBodyStyle, row);
						if (simpleBudgetLineItem.getQuantity() != null)
							cell3.setCellValue(decimalFormat.format(simpleBudgetLineItem.getQuantity()));
						else
							cell3.setCellValue(" ");
						cellNumber++;

						if (simpleBudgetLineItem.getPeriodCostsList() != null
								&& !simpleBudgetLineItem.getPeriodCostsList().isEmpty()) {
							for (PeriodCost periodCost : simpleBudgetLineItem.getPeriodCostsList()) {
								Cell cell4 = assignCell(cellNumber, tableBodyStyle, row);
								if (periodCost.getCost() != null)
									cell4.setCellValue(decimalFormat.format(periodCost.getCost()));
								else
									cell4.setCellValue(" ");
								cellNumber++;
							}
						}
						Cell cell5 = assignCell(cellNumber, tableBodyStyle, row);
						if (simpleBudgetLineItem.getLineItemDescription() != null)
							cell5.setCellValue(simpleBudgetLineItem.getLineItemDescription());
						else
							cell5.setCellValue(" ");
						cellNumber++;

						firstTime = false;
					}
				}
			}
		}
	}

	private Object[] appendHeader(Object[] headers, Object header) {
		ArrayList<Object> newHeader = new ArrayList<Object>(Arrays.asList(headers));
		newHeader.add(header);
		return newHeader.toArray();
	}

	@Override
	public ResponseEntity<byte[]> generateAwardDetailedBudgetExcelReport(HttpServletResponse response, Integer budgetId) {
		ResponseEntity<byte[]> attachmentData = null;
		try {
			XSSFWorkbook workbook = new XSSFWorkbook();
			AwardBudgetHeader awardBudgetHeader = awardBudgetDao.fetchBudgetByBudgetId(budgetId);
			awardBudgetHeader
					.setBudgetPeriods(awardBudgetDao.getAwardBudgetPeriodsByBudgetId(awardBudgetHeader.getBudgetId()));
			for (AwardBudgetPeriod awardBudgetPeriod : awardBudgetHeader.getBudgetPeriods()) {
				awardBudgetPeriod.setBudgetDetails(
						awardBudgetDao.fetchAwardBudgetDetailByPeriodId(awardBudgetPeriod.getBudgetPeriodId()));
			}
			XSSFSheet sheet = workbook.createSheet("Award Detailed Budget");
            commonService.addDetailsInHeader(workbook, sheet);
			prepareExcelSheetForAwardDetailedBudget(awardBudgetHeader, sheet, workbook);
			autoSizeColumns(workbook);
			ByteArrayOutputStream bos = new ByteArrayOutputStream();
			workbook.write(bos);
			attachmentData = getResponseEntityForExcelDownload(bos.toByteArray());
		} catch (Exception e) {
			logger.error("Exception in generateAwardDetailedBudgetExcelReport : {} ", e.getMessage());
		}
		return attachmentData;
	}

	private void prepareExcelSheetForAwardDetailedBudget(AwardBudgetHeader awardBudgetHeader, XSSFSheet sheet, XSSFWorkbook workbook) {
		XSSFCellStyle tableBodyStyle = workbook.createCellStyle();
		sheet.addMergedRegion(new CellRangeAddress(0, 0, 0, 1));
		Boolean isNonPersonalLineItemEnabled = commonDao.getParameterValueAsBoolean(Constants.BUDGET_ASSOCIATED_WITH_NON_PERSONAL_SUBITEM_ENABLED);
		int rowNumber = 0;
        rowNumber = prepareAwardGeneralInformationForExcel(sheet, rowNumber, awardBudgetHeader.getAwardId().toString(), workbook, null, Boolean.FALSE);
		String heading = "Detailed Budget (" + awardBudgetHeader.getBudgetStatus().getDescription() + ")";
		int totalColumnCount = 7;
		Boolean budgetVariationFlag = false;
		if (awardBudgetHeader.getBudgetTypeCode().equals(Constants.BUDGET_TYPE_REBUDGET) && (awardBudgetHeader.getBudgetStatusCode().equals(Constants.AWARD_BUDGET_STATUS_CODE_INPROGRESS))) {
			budgetVariationFlag = true;
			++totalColumnCount;
		}
		rowNumber = rowNumber + 1;
		prepareExcelSheetHeading(sheet, heading, workbook, totalColumnCount, rowNumber);
		List<AwardBudgetPeriod> awardBudgetPeriods = awardBudgetHeader.getBudgetPeriods();
		for (AwardBudgetPeriod awardBudgetPeriod : awardBudgetPeriods) {
			rowNumber = rowNumber + 1;
            Object[] tableHeadingPeriod = {"Period " + awardBudgetPeriod.getBudgetPeriod()};
			prepareExcelSheetHeader(sheet, tableHeadingPeriod, workbook, tableBodyStyle, rowNumber++);
			rowNumber = rowNumber + 1;
            Object[] tableHeadingDateAndCost = {"Start Date", "End Date", "Direct Cost (" + Constants.DOLLAR_SYMBOL + ")", "Indirect Cost (" + Constants.DOLLAR_SYMBOL + ")",
                    "Total Cost (" + Constants.DOLLAR_SYMBOL + ")"};
			prepareExcelSheetHeader(sheet, tableHeadingDateAndCost, workbook, tableBodyStyle, rowNumber++);
			Row outerRow = sheet.createRow(rowNumber++);
			int cellNumber = 0;
			Cell cell1 = assignCell(cellNumber, tableBodyStyle, outerRow);
			if (awardBudgetPeriod.getStartDate() != null) {
				Date date = awardBudgetPeriod.getStartDate();
				String dateValue = commonService.convertDateFormatBasedOnTimeZone(date.getTime(), Constants.DEFAULT_DATE_FORMAT);
				cell1.setCellValue(dateValue);
			} else
				cell1.setCellValue(" ");
			cellNumber++;

			Cell cell2 = assignCell(cellNumber, tableBodyStyle, outerRow);
			if (awardBudgetPeriod.getEndDate() != null) {
				Date date = awardBudgetPeriod.getEndDate();
				String dateValue = commonService.convertDateFormatBasedOnTimeZone(date.getTime(), Constants.DEFAULT_DATE_FORMAT);
				cell2.setCellValue(dateValue);
			} else
				cell2.setCellValue(" ");
			cellNumber++;

			Cell cell3 = assignCell(cellNumber, tableBodyStyle, outerRow);
			if (awardBudgetPeriod.getTotalDirectCost() != null)
				cell3.setCellValue(decimalFormat.format(awardBudgetPeriod.getTotalDirectCost()));
			else
				cell3.setCellValue(" ");
			cellNumber++;

			Cell cell4 = assignCell(cellNumber, tableBodyStyle, outerRow);
			if (awardBudgetPeriod.getTotalIndirectCost() != null)
				cell4.setCellValue(decimalFormat.format(awardBudgetPeriod.getTotalIndirectCost()));
			else
				cell4.setCellValue(" ");
			cellNumber++;

			Cell cell5 = assignCell(cellNumber, tableBodyStyle, outerRow);
			if (awardBudgetPeriod.getTotalCost() != null)
				cell5.setCellValue(decimalFormat.format(awardBudgetPeriod.getTotalCost()));
			else
				cell5.setCellValue(" ");
			cellNumber++;

			rowNumber = rowNumber + 1;
			if (awardBudgetPeriod.getBudgetDetails() != null && !awardBudgetPeriod.getBudgetDetails().isEmpty()) {
                Object[] tableHeadingCostElement = {"Budget Category", "Cost Element", "WBS Number", "Quantity"};
				if (budgetVariationFlag) {
                    Object[] tableHeading = {"Latest Approved Budget (" + Constants.DOLLAR_SYMBOL + ")",
							"Balance To Date (" + Constants.DOLLAR_SYMBOL + ")",
                            "Revised Budget (" + Constants.DOLLAR_SYMBOL + ")", "Line Item Description"};
					tableHeadingCostElement = mergeHeaders(tableHeadingCostElement, tableHeading);
				} else {
                    Object[] tableHeading = {"Line Item Cost (" + Constants.DOLLAR_SYMBOL + ")",
                            "Line Item Description"};
					tableHeadingCostElement = mergeHeaders(tableHeadingCostElement, tableHeading);
				}
				prepareExcelSheetHeader(sheet, tableHeadingCostElement, workbook, tableBodyStyle, rowNumber++);
				for (AwardBudgetDetail awardBudgetDetail : awardBudgetPeriod.getBudgetDetails()) {
					List<AwardBudgetPersonalDetail> awardBudgetPersonalDetails = new ArrayList<>();
					List<AwardBudgetNonPersonDetail> awardBudgetNonPersonalDetails = new ArrayList<>();
					cellNumber = 0;
					Row innerRow = sheet.createRow(rowNumber++);
					Cell cell6 = assignCell(cellNumber, tableBodyStyle, innerRow);
					if (awardBudgetDetail.getBudgetCategory().getDescription() != null)
						cell6.setCellValue(awardBudgetDetail.getBudgetCategory().getDescription());
					else
						cell6.setCellValue(" ");
					cellNumber++;

					Cell cell7 = assignCell(cellNumber, tableBodyStyle, innerRow);
					if (awardBudgetDetail.getCostElementCode() != null)
						cell7.setCellValue(awardBudgetDetail.getCostElementCode() + " - "
								+ awardBudgetDetail.getCostElement().getDescription());
					else
						cell7.setCellValue(" ");
					cellNumber++;

					Cell cell8 = assignCell(cellNumber, tableBodyStyle, innerRow);
					if (awardBudgetDetail.getInternalOrderCode() != null)
						cell8.setCellValue(awardBudgetDetail.getInternalOrderCode());
					else
						cell8.setCellValue(" ");
					cellNumber++;

					Cell cell9 = assignCell(cellNumber, tableBodyStyle, innerRow);
					if (awardBudgetDetail.getQuantity() != null)
						cell9.setCellValue(decimalFormat.format(awardBudgetDetail.getQuantity()));
					else
						cell9.setCellValue(" ");
					cellNumber++;

					if (budgetVariationFlag) {
						Cell cell10 = assignCell(cellNumber, tableBodyStyle, innerRow);
						if (awardBudgetDetail.getPrevLineItemCost() != null)
							cell10.setCellValue(decimalFormat.format(awardBudgetDetail.getPrevLineItemCost()));
						else
							cell10.setCellValue(" ");
						cellNumber++;

						awardBudgetService.calculateBalanceToDateValue(awardBudgetDetail,
								awardBudgetHeader.getFundCode());
						Cell cell11 = assignCell(cellNumber, tableBodyStyle, innerRow);
						if (awardBudgetDetail.getBalanceToDate() != null)
							cell11.setCellValue(decimalFormat.format(awardBudgetDetail.getBalanceToDate()));
						else
							cell11.setCellValue(" ");
						cellNumber++;
					}

					Cell cell12 = assignCell(cellNumber, tableBodyStyle, innerRow);
					if (awardBudgetDetail.getLineItemCost() != null)
						cell12.setCellValue(decimalFormat.format(awardBudgetDetail.getLineItemCost()));
					else
						cell12.setCellValue(" ");
					cellNumber++;

					Cell cell13 = assignCell(cellNumber, tableBodyStyle, innerRow);
					if (awardBudgetDetail.getLineItemDescription() != null)
						cell13.setCellValue(awardBudgetDetail.getLineItemDescription());
					else
						cell13.setCellValue(" ");
					cellNumber++;

					if (awardBudgetDetail.getBudgetCategory().getBudgetCategoryTypeCode().equals("P")) {
						awardBudgetPersonalDetails = awardBudgetDetail.getPersonsDetails();
						if (awardBudgetPersonalDetails != null && !awardBudgetPersonalDetails.isEmpty()) {
                            Object[] tablePersonHeading = {"Persons", "Start Date", "End Date", "WBS Number", "Salary (" + Constants.DOLLAR_SYMBOL + ")", "Effort %",
                                    "Applied Salary (" + Constants.DOLLAR_SYMBOL + ")"};
							prepareExcelSheetHeader(sheet, tablePersonHeading, workbook, tableBodyStyle, rowNumber++);
							for (AwardBudgetPersonalDetail awardBudgetPersonalDetail : awardBudgetPersonalDetails) {
								Row personRow = sheet.createRow(rowNumber++);
								cellNumber = 0;
								Cell cell14 = assignCell(cellNumber, tableBodyStyle, personRow);
								if (awardBudgetPersonalDetail.getBudgetPerson().getBudgetPersonName() != null)
									cell14.setCellValue(
											awardBudgetPersonalDetail.getBudgetPerson().getBudgetPersonName());
								else
									cell14.setCellValue(" ");
								cellNumber++;

								Cell cell15 = assignCell(cellNumber, tableBodyStyle, personRow);
								if (awardBudgetPersonalDetail.getStartDate() != null) {
									Date date = awardBudgetPersonalDetail.getStartDate();
									String dateValue = commonService.convertDateFormatBasedOnTimeZone(date.getTime(),
											Constants.DEFAULT_DATE_FORMAT);
									cell15.setCellValue(dateValue);
								} else
									cell15.setCellValue(" ");
								cellNumber++;

								Cell cell16 = assignCell(cellNumber, tableBodyStyle, personRow);
								if (awardBudgetPersonalDetail.getEndDate() != null) {
									Date date = awardBudgetPersonalDetail.getEndDate();
									String dateValue = commonService.convertDateFormatBasedOnTimeZone(date.getTime(),
											Constants.DEFAULT_DATE_FORMAT);
									cell16.setCellValue(dateValue);
								} else
									cell16.setCellValue(" ");
								cellNumber++;

								Cell cell17 = assignCell(cellNumber, tableBodyStyle, personRow);
								if (awardBudgetPersonalDetail.getInternalOrderCode() != null)
									cell17.setCellValue(awardBudgetPersonalDetail.getInternalOrderCode());
								else
									cell17.setCellValue(" ");
								cellNumber++;

								Cell cell18 = assignCell(cellNumber, tableBodyStyle, personRow);
								if (awardBudgetPersonalDetail.getTotalSalary() != null)
									cell18.setCellValue(decimalFormat.format(awardBudgetPersonalDetail.getTotalSalary()));
								else
									cell18.setCellValue(" ");
								cellNumber++;

								Cell cell19 = assignCell(cellNumber, tableBodyStyle, personRow);
								if (awardBudgetPersonalDetail.getPercentageEffort() != null)
									cell19.setCellValue(awardBudgetPersonalDetail.getPercentageEffort().doubleValue());
								else
									cell19.setCellValue(" ");
								cellNumber++;

								Cell cell20 = assignCell(cellNumber, tableBodyStyle, personRow);
								if (awardBudgetPersonalDetail.getSalaryRequested() != null)
									cell20.setCellValue(
											decimalFormat.format(awardBudgetPersonalDetail.getSalaryRequested()));
								else
									cell20.setCellValue(" ");
								cellNumber++;
							}
							rowNumber = rowNumber + 1;
						}
                    } else if (awardBudgetDetail.getIsSystemGeneratedCostElement().equals(IS_NOT_SYSTEM_DENERATED_COST_ELEMENT) && (Boolean.TRUE.equals(isNonPersonalLineItemEnabled))) {
						awardBudgetNonPersonalDetails = awardBudgetDetail.getNonPersonsDetails();
						if (awardBudgetNonPersonalDetails != null && !awardBudgetNonPersonalDetails.isEmpty()) {
                            Object[] tableNonPersonHeading = {"Sub Line Item", "IO Code", "Line Item Cost (" + Constants.DOLLAR_SYMBOL + ")"};
							prepareExcelSheetHeader(sheet, tableNonPersonHeading, workbook, tableBodyStyle, rowNumber++);
							for (AwardBudgetNonPersonDetail awardBudgetNonPersonalDetail : awardBudgetNonPersonalDetails) {
								Row nonPersonRow = sheet.createRow(rowNumber++);
								cellNumber = 0;
								Cell cell14 = assignCell(cellNumber, tableBodyStyle, nonPersonRow);
								if (awardBudgetNonPersonalDetail.getDescription() != null)
									cell14.setCellValue(awardBudgetNonPersonalDetail.getDescription());
								else
									cell14.setCellValue(" ");
								cellNumber++;
								Cell cell15 = assignCell(cellNumber, tableBodyStyle, nonPersonRow);
								if (awardBudgetNonPersonalDetail.getInternalOrderCode() != null)
									cell15.setCellValue(awardBudgetNonPersonalDetail.getInternalOrderCode());
								else
									cell15.setCellValue(" ");
								cellNumber++;
								Cell cell16 = assignCell(cellNumber, tableBodyStyle, nonPersonRow);
								if (awardBudgetNonPersonalDetail.getLineItemCost() != null)
									cell16.setCellValue(
											decimalFormat.format(awardBudgetNonPersonalDetail.getLineItemCost()));
								else
									cell15.setCellValue(" ");
								cellNumber++;
							}
							rowNumber = rowNumber + 1;
						}
					}
				}
			}
		}
	}

	public static Object[] mergeHeaders(Object[] firstHeader, Object[] secondHeader) {
		Object[] mergedHeader = new Object[firstHeader.length + secondHeader.length];
		System.arraycopy(firstHeader, 0, mergedHeader, 0, firstHeader.length);
		for (int i = 0; i < secondHeader.length; i++) {
			mergedHeader[firstHeader.length + i] = secondHeader[i];
		}
		return mergedHeader;
	}

	private List<AwardPrintParameter> setResearchAreas(Integer awardId) {
		List<AwardResearchArea> awardResearchAreas = awardDao.fetchAwardResearchAreaBasedOnAwardId(awardId);
		List<AwardPrintParameter> awardResearchAreaList = new ArrayList<AwardPrintParameter>();
		if (awardResearchAreas != null && !awardResearchAreas.isEmpty()) {
			// Set initial capacity of array list based on size of award Research Area List
			awardResearchAreaList = new ArrayList<>(awardResearchAreas.isEmpty() ? 1 : awardResearchAreas.size());
			for (AwardResearchArea awardResearchArea : awardResearchAreas) {
				addAwardResearchArea(awardResearchArea, awardResearchAreaList);
			}
		} else {
			addAwardResearchArea(null, awardResearchAreaList);
		}
		return awardResearchAreaList;
	}

	private void addAwardResearchArea(AwardResearchArea awardResearchArea, List<AwardPrintParameter> awardResearchAreaList) {
		AwardPrintParameter awardResearchData = new AwardPrintParameter();
		if (awardResearchArea != null) {
			ResearchType researchType = awardResearchArea.getResearchType();
			awardResearchData.setAwardResearchType(researchType != null ? researchType.getDescription() : "");
			if (awardResearchArea.getResearchTypeArea() != null) {
				awardResearchData.setAwardResearchArea(awardResearchArea.getResearchTypeArea() != null ? awardResearchArea.getResearchTypeArea().getDescription() : "");
				awardResearchData.setAwardResearchSubArea(awardResearchArea.getResearchTypeSubArea() != null ? awardResearchArea.getResearchTypeSubArea().getDescription() : "");
			}
		} else {
			awardResearchData.setAwardResearchType("");
			awardResearchData.setAwardResearchArea("");
			awardResearchData.setAwardResearchSubArea("");
		}
		awardResearchAreaList.add(awardResearchData);
	}

	public ByteArrayInputStream generateBudgetPDF(PrintVO vo) throws DocumentException, ParseException {
		Proposal pdfData = proposalDao.fetchProposalById(vo.getProposalId());
		pdfData.setProposalPersons(proposalModuleDao.fetchProposalPersonBasedOnProposalId(pdfData.getProposalId()));
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
			Paragraph pdfHeading = new Paragraph(BUDGET_SUMMARY, headFont);
			pdfHeading.setAlignment(Paragraph.ALIGN_CENTER);
			pdfHeading.setSpacingBefore(5f);
			pdfHeading.setSpacingAfter(7f);
			document.add(pdfHeading);
			prepareProposalOverviewDetails(document, subHeadFont, bodyFont, bodyTextFont, pdfData);
			BudgetHeader budgetHeader = budgetDao.fetchBudgetByBudgetId(vo.getBudgetId());
			Boolean isShowInKind = commonDao.getParameterValueAsBoolean(Constants.IS_SHOW_IN_KIND);
			Boolean isShowCostShareUnderRecovery = commonDao.getParameterValueAsBoolean(Constants.IS_SHOW_COST_SHARE_AND_UNDERRECOVERY);
			Boolean isEnableCostShareStatus = commonDao.getParameterValueAsBoolean(Constants.ENABLE_COST_SHARE_STATUS);
			if (budgetHeader != null) {
				prepareBudgetOverviewDetails(document, subHeadFont, bodyFont, bodyTextFont, budgetHeader, isShowInKind, isShowCostShareUnderRecovery, isEnableCostShareStatus);
				preparePeriodsAndTotalDetails(document, subHeadFont, bodyFont, tableLabelFont, budgetHeader, isShowInKind, isShowCostShareUnderRecovery);
				vo.setIsSimpleBudgetPrint(commonDao.getParameterValueAsBoolean(Constants.IS_SIMPLE_BUDGET_ENABLED) ? "Y" : "N");
				vo.setIsDetailedBudgetPrint(commonDao.getParameterValueAsBoolean(Constants.IS_DETAILED_BUDGET_ENABLED) ? "Y" : "N");
				vo.setIsBudgetSummaryPrint(commonDao.getParameterValueAsBoolean(Constants.IS_BUDGET_SUMMARY_ENABLED) ? "Y" : "N");
				vo.setIsPersonnelBudgetPrint(commonDao.getParameterValueAsBoolean(Constants.IS_DETAILED_BUDGET_ENABLED) ? "Y" : "N");
				if (vo.getIsPersonnelBudgetPrint() != null && vo.getIsPersonnelBudgetPrint().equals("Y")) {
					preparePersonnelBudget(document, subHeadFont, bodyFont, tableLabelFont, budgetHeader);
				}
				if (vo.getIsSimpleBudgetPrint().equals("Y")) {
					prepareSimpleBudgetDetail(document, subHeadFont, bodyFont, tableLabelFont, budgetHeader);
				}
				if (vo.getIsDetailedBudgetPrint().equals("Y")) {
					prepareDetailedBudgetDetail(document, subHeadFont, bodyFont, tableLabelFont, budgetHeader, isShowInKind, isShowCostShareUnderRecovery);
				}
				if (vo.getIsBudgetSummaryPrint().equals("Y")) {
					prepareBudgetSummaryDetail(document, subHeadFont, bodyFont, tableLabelFont, budgetHeader, isShowInKind);
				}
			}
			document.close();
		} catch (DocumentException ex) {
		}
		return new ByteArrayInputStream(out.toByteArray());
	}

	private void prepareProposalOverviewDetails(Document document, Font subHeadFont, Font bodyFont, Font bodyTextFont, Proposal pdfData) throws DocumentException {
		PdfPTable proposalOverviewHeading = new PdfPTable(1);
		proposalOverviewHeading.setWidthPercentage(100);
        proposalOverviewHeading.setWidths(new int[]{12});
		proposalOverviewHeading.setSpacingBefore(5f);
		proposalOverviewHeading.addCell(getSubHeadingCell("Proposal Overview", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, subHeadFont));
		document.add(proposalOverviewHeading);

		PdfPTable generalDetailsTable = new PdfPTable(3);
		generalDetailsTable.setWidthPercentage(100);
        generalDetailsTable.setWidths(new int[]{2, 1, 9});

		generalDetailsTable.addCell(getGeneralDetailsCell("Proposal ID & Title ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
		generalDetailsTable.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
		generalDetailsTable.addCell(getGeneralDetailsCell(pdfData.getProposalId() + " - " + pdfData.getTitle(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
		generalDetailsTable.addCell(getGeneralDetailsCell("Principal Investigator ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
		generalDetailsTable.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
		generalDetailsTable.addCell(getGeneralDetailsCell(pdfData.getInvestigator() != null ? pdfData.getInvestigator().getFullName() : "", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
		generalDetailsTable.addCell(getGeneralDetailsCell("Funding Agency ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
		generalDetailsTable.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
        generalDetailsTable.addCell(getGeneralDetailsCell(pdfData.getSponsor() != null ? commonService.getSponsorFormatBySponsorDetail(pdfData.getSponsor().getSponsorCode(), pdfData.getSponsor().getSponsorName(), pdfData.getSponsor().getAcronym()) : "", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
		document.add(generalDetailsTable);
	}

	private void prepareBudgetOverviewDetails(Document document, Font subHeadFont, Font bodyFont, Font bodyTextFont, BudgetHeader budgetHeader, Boolean isShowInKind, Boolean isShowCostShareUnderRecovery,Boolean isEnableCostShareStatus) throws DocumentException {
		Boolean isShowModifiedCost = commonDao.getParameterValueAsBoolean(Constants.IS_SHOW_MODIFIED_DIRECT_COST);
		Boolean isShowBudgetOHRatePercentage = commonDao.getParameterValueAsBoolean(Constants.SHOW_BUDGET_OH_RATE_PERCENTAGE);
		PdfPTable budgetOverviewHeading = new PdfPTable(1);
		budgetOverviewHeading.setWidthPercentage(100);
        budgetOverviewHeading.setWidths(new int[]{12});
		budgetOverviewHeading.setSpacingBefore(20f);
		budgetOverviewHeading.addCell(getSubHeadingCell("Budget Overview", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, subHeadFont));
		document.add(budgetOverviewHeading);

		PdfPTable budgetOverviewDetails = new PdfPTable(6);
		budgetOverviewDetails.setWidthPercentage(100);
        budgetOverviewDetails.setWidths(new int[]{5, 1, 5 , 5, 1, 5});
		if (budgetHeader != null) {
			budgetOverviewDetails.addCell(getGeneralDetailsCell("Version Number", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(budgetHeader.getVersionNumber() != null ? budgetHeader.getVersionNumber().toString() : "", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
//          budgetOverviewDetails.addCell(getGeneralDetailsCell("Start Date ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
//          budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
//          budgetOverviewDetails.addCell(getGeneralDetailsCell(budgetHeader.getStartDate() != null ? commonService.convertDateFormatBasedOnTimeZone(budgetHeader.getStartDate().getTime(), Constants.DEFAULT_DATE_FORMAT) : " ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
//          budgetOverviewDetails.addCell(getGeneralDetailsCell("End Date ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
//          budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
//          budgetOverviewDetails.addCell(getGeneralDetailsCell(budgetHeader.getEndDate() != null ? commonService.convertDateFormatBasedOnTimeZone(budgetHeader.getEndDate().getTime(), Constants.DEFAULT_DATE_FORMAT) : " ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(BUDGET_STATUS, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
            budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
            budgetOverviewDetails.addCell(getGeneralDetailsCell(budgetHeader.getBudgetStatus() != null ? budgetHeader.getBudgetStatus().getDescription() : "", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
            Boolean isCampusFlagEnabled = commonDao.getParameterValueAsBoolean(Constants.ENABLE_CAMPUS_FLAG_PROPOSAL);
            if (Boolean.TRUE.equals(isCampusFlagEnabled)) {
                budgetOverviewDetails.addCell(getGeneralDetailsCell("Campus Flag", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
                budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
                if (budgetHeader.getCampusFlag() != null) {
                    if (budgetHeader.getCampusFlag().equals("F")) {
                        budgetOverviewDetails.addCell(getGeneralDetailsCell("OFF" + "", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
                    } else if (budgetHeader.getCampusFlag().equals("N")) {
                        budgetOverviewDetails.addCell(getGeneralDetailsCell("ON" + "", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
                    } else if (budgetHeader.getCampusFlag().equals("D")) {
                        budgetOverviewDetails.addCell(getGeneralDetailsCell("BOTH" + "", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
                    }
                } else {
                    budgetOverviewDetails.addCell(getGeneralDetailsCell("", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
                }
            }
            Boolean isOverheadRateTypeEnabled = commonDao.getParameterValueAsBoolean(Constants.ENABLE_OVERHEAD_RATE_TYPE);
            if (Boolean.TRUE.equals(isOverheadRateTypeEnabled)) {
            	budgetOverviewDetails.addCell(getGeneralDetailsCell(OVER_HEAD_RATE_TYPE, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
            	budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
                budgetOverviewDetails.addCell(getGeneralDetailsCell(budgetHeader.getRateTypeCode() != null ? budgetHeader.getRateType().getDescription() : "", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
            }
			budgetOverviewDetails.addCell(getGeneralDetailsCell(DIRECT_COST, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(budgetHeader.getTotalDirectCost() != null ? Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetHeader.getTotalDirectCost()) : "", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
			if (Boolean.TRUE.equals(isShowModifiedCost)) {
				budgetOverviewDetails.addCell(getGeneralDetailsCell(MODIFIED_DIRECT_COST, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
				budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
				budgetOverviewDetails.addCell(getGeneralDetailsCell(budgetHeader.getTotalModifiedDirectCost() != null ? Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetHeader.getTotalModifiedDirectCost()) : "", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
			}
			budgetOverviewDetails.addCell(getGeneralDetailsCell(INDIRECT_COST, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(budgetHeader.getTotalIndirectCost() != null ? Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetHeader.getTotalIndirectCost()) : "", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
			if (Boolean.TRUE.equals(isShowCostShareUnderRecovery)) {
				budgetOverviewDetails.addCell(getGeneralDetailsCell("Cost Share  ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
				budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
				budgetOverviewDetails.addCell(getGeneralDetailsCell(budgetHeader.getCostSharingAmount() != null ? Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetHeader.getCostSharingAmount()) : "", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
				budgetOverviewDetails.addCell(getGeneralDetailsCell("Under Recovery", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
				budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
				budgetOverviewDetails.addCell(getGeneralDetailsCell(budgetHeader.getUnderrecoveryAmount() != null ? Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetHeader.getUnderrecoveryAmount()) : "", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
			}
			budgetOverviewDetails.addCell(getGeneralDetailsCell(TOTAL_REQUESTED_COST, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(budgetHeader.getTotalCost() != null ? Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetHeader.getTotalCost()) : "", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
			if (Boolean.TRUE.equals(isShowInKind)) {
				budgetOverviewDetails.addCell(getGeneralDetailsCell(TOTAL_IN_KIND, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
				budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
				budgetOverviewDetails.addCell(getGeneralDetailsCell(budgetHeader.getTotalInKind() != null ? Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetHeader.getTotalInKind()) : "", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
			}
			budgetOverviewDetails.addCell(getGeneralDetailsCell(TOTAL_COST, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(budgetHeader.getTotalOfTotalCost() != null ? Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetHeader.getTotalOfTotalCost()) : "", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
            budgetOverviewDetails.addCell(getGeneralDetailsCell("Final  ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			if (budgetHeader.getIsFinalBudget() != null) {
				if (Boolean.TRUE.equals(budgetHeader.getIsFinalBudget())) {
					budgetOverviewDetails.addCell(getGeneralDetailsCell("Yes", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
				} else {
					budgetOverviewDetails.addCell(getGeneralDetailsCell("No", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
				}
			} else {
				budgetOverviewDetails.addCell(getGeneralDetailsCell("", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			}
			
			if (Boolean.TRUE.equals(isShowCostShareUnderRecovery)) {
				budgetOverviewDetails.addCell(getGeneralDetailsCell("Under Recovery Rate Type ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
				budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
				budgetOverviewDetails.addCell(getGeneralDetailsCell(budgetHeader.getUnderrecoveryRateClassCode() != null ? budgetHeader.getUnderrecoveryRateType().getDescription() : "", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
			}
            budgetOverviewDetails.addCell(getGeneralDetailsCell("Budget Template  ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
            budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
            budgetOverviewDetails.addCell(getGeneralDetailsCell(budgetHeader.getBudgetTemplateTypeId() != null ? budgetDao.fetchBudgetTemplateTypeById(budgetHeader.getBudgetTemplateTypeId()).getDescription() : "", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(DESCRIPTION, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(budgetHeader.getComments() != null ? budgetHeader.getComments() : "", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
			if(Boolean.TRUE.equals(isEnableCostShareStatus)) {
				budgetOverviewDetails.addCell(getGeneralDetailsCell("Cost Sharing", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
				budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
				budgetOverviewDetails.addCell(getGeneralDetailsCell(budgetHeader.getCostSharingTypeCode() != null ? budgetHeader.getCostSharingType().getDescription() : "", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
			}
			if(Boolean.TRUE.equals(isShowBudgetOHRatePercentage)) {
				budgetOverviewDetails.addCell(getGeneralDetailsCell("On-Campus Rate", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
				budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
				budgetOverviewDetails.addCell(getGeneralDetailsCell(budgetHeader.getOnCampusRates() != null ? budgetHeader.getOnCampusRates() : "", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
				budgetOverviewDetails.addCell(getGeneralDetailsCell("Off-Campus Rate", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
				budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
				budgetOverviewDetails.addCell(getGeneralDetailsCell(budgetHeader.getOffCampusRates() != null ? budgetHeader.getOffCampusRates() : "", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
			}
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
		} else {
			budgetOverviewDetails.addCell(getGeneralDetailsCell("Version Number  ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(DIRECT_COST, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(MODIFIED_DIRECT_COST, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(INDIRECT_COST, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			if (Boolean.TRUE.equals(isShowCostShareUnderRecovery)) {
				budgetOverviewDetails.addCell(getGeneralDetailsCell("Cost Share ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
				budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
				budgetOverviewDetails.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
				budgetOverviewDetails.addCell(getGeneralDetailsCell("Under Recovery ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
				budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
				budgetOverviewDetails.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			}
			budgetOverviewDetails.addCell(getGeneralDetailsCell(TOTAL_REQUESTED_COST, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			if (Boolean.TRUE.equals(isShowInKind)) {
				budgetOverviewDetails.addCell(getGeneralDetailsCell(TOTAL_IN_KIND, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
				budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
				budgetOverviewDetails.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			}
			budgetOverviewDetails.addCell(getGeneralDetailsCell(TOTAL_COST, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell("Is Final  ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(BUDGET_STATUS, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell("Campus Flag ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
            budgetOverviewDetails.addCell(getGeneralDetailsCell(OVER_HEAD_RATE_TYPE, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
            budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
            budgetOverviewDetails.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			if (Boolean.TRUE.equals(isShowCostShareUnderRecovery)) {
				budgetOverviewDetails.addCell(getGeneralDetailsCell("Under Recovery Rate Type ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
				budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
				budgetOverviewDetails.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			}
            budgetOverviewDetails.addCell(getGeneralDetailsCell("Budget Template  ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
            budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
            budgetOverviewDetails.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(DESCRIPTION, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			if(isShowBudgetOHRatePercentage) {
				budgetOverviewDetails.addCell(getGeneralDetailsCell("On-Campus Rate", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
				budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
				budgetOverviewDetails.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
				budgetOverviewDetails.addCell(getGeneralDetailsCell("Off-Campus Rate", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
				budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
				budgetOverviewDetails.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			}
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
		}
		document.add(budgetOverviewDetails);
	}

	private void preparePeriodsAndTotalDetails(Document document, Font subHeadFont, Font bodyFont, Font tableLabelFont, BudgetHeader budgetHeader, Boolean isShowInKind, Boolean isShowCostShareUnderRecovery) throws DocumentException {
		Boolean isShowModifiedCost = commonDao.getParameterValueAsBoolean(Constants.IS_SHOW_MODIFIED_DIRECT_COST);
		PdfPTable periodsAndTotalHeading = new PdfPTable(1);
		periodsAndTotalHeading.setWidthPercentage(100);
        periodsAndTotalHeading.setWidths(new int[]{12});
		periodsAndTotalHeading.setSpacingBefore(10f);
		periodsAndTotalHeading.addCell(getSubHeadingCell("Periods and Total", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, subHeadFont));
		document.add(periodsAndTotalHeading);

		if (budgetHeader != null) {
			List<BudgetPeriod> periodList = budgetHeader.getBudgetPeriods();
			if (periodList != null && !periodList.isEmpty()) {
				List<BudgetPrintParameter> budgetPeriodsAndTotalList = setBudgetPeriodsAndTotalList(periodList);
				int numberOfColumns = 11;
				PdfPTable budgetPeriodsAndTotal = prepareNumberOfColumns(numberOfColumns, isShowInKind, isShowCostShareUnderRecovery, isShowModifiedCost);
				budgetPeriodsAndTotal.setWidthPercentage(100);
				budgetPeriodsAndTotal.addCell(getTableHeadingCell("Period #", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
				budgetPeriodsAndTotal.addCell(getTableHeadingCell("Period Start Date", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
				budgetPeriodsAndTotal.addCell(getTableHeadingCell("Period End Date", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
				budgetPeriodsAndTotal.addCell(getTableHeadingCell(DIRECT_COST, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
				if (Boolean.TRUE.equals(isShowModifiedCost)) {
					budgetPeriodsAndTotal.addCell(getTableHeadingCell(MODIFIED_DIRECT_COST, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
				}
				budgetPeriodsAndTotal.addCell(getTableHeadingCell(INDIRECT_COST, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
				if (Boolean.TRUE.equals(isShowCostShareUnderRecovery)) {
					budgetPeriodsAndTotal.addCell(getTableHeadingCell("Cost Share Amount", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
					budgetPeriodsAndTotal.addCell(getTableHeadingCell("Under Recovery Amount", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
				}
				if (Boolean.TRUE.equals(isShowInKind)) {
					budgetPeriodsAndTotal.addCell(getTableHeadingCell(TOTAL_IN_KIND, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
				}
				budgetPeriodsAndTotal.addCell(getTableHeadingCell(TOTAL_REQUESTED_COST, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
				budgetPeriodsAndTotal.addCell(getTableHeadingCell(TOTAL_COST, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
				if (!budgetPeriodsAndTotalList.isEmpty()) {
					for (BudgetPrintParameter periodsAndTotal : budgetPeriodsAndTotalList) {
						budgetPeriodsAndTotal.addCell(getTableCell("Period " + periodsAndTotal.getPeriod(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
						budgetPeriodsAndTotal.addCell(getTableCell(periodsAndTotal.getPeriodStartDate(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
						budgetPeriodsAndTotal.addCell(getTableCell(periodsAndTotal.getPeriodEndDate(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
						budgetPeriodsAndTotal.addCell(getTableCell(periodsAndTotal.getDirectCost(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
						if (Boolean.TRUE.equals(isShowModifiedCost)) {
							budgetPeriodsAndTotal.addCell(getTableCell(periodsAndTotal.getTotalModifiedDirectCost(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
						}
						budgetPeriodsAndTotal.addCell(getTableCell(periodsAndTotal.getIndirectCost(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
						if (Boolean.TRUE.equals(isShowCostShareUnderRecovery)) {
							budgetPeriodsAndTotal.addCell(getTableCell(periodsAndTotal.getCostSharing(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
							budgetPeriodsAndTotal.addCell(getTableCell(periodsAndTotal.getUnderRecoveryAmount(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
						}
						if (Boolean.TRUE.equals(isShowInKind)) {
							budgetPeriodsAndTotal.addCell(getTableCell(periodsAndTotal.getTotalInKind(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
						}
						budgetPeriodsAndTotal.addCell(getTableCell(periodsAndTotal.getTotalCost(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
						budgetPeriodsAndTotal.addCell(getTableCell(periodsAndTotal.getTotalOfTotal(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
					}
				}
				document.add(budgetPeriodsAndTotal);
			}
		}
	}

	private void preparePersonnelBudget(Document document, Font subHeadFont, Font bodyFont, Font tableLabelFont, BudgetHeader budgetHeader) throws DocumentException {
		List<BudgetPrintParameter> budgetPersonList = new ArrayList<>();
		List<BudgetPerson> budgetPersons = budgetDao.getBudgetPersons(budgetHeader.getBudgetId());
		if (budgetPersons != null && !budgetPersons.isEmpty()) {
			budgetPersonList = setBudgetPersonelList(budgetPersons);
		}
		PdfPTable personnelBudgetHeading = new PdfPTable(1);
		personnelBudgetHeading.setWidthPercentage(100);
        personnelBudgetHeading.setWidths(new int[]{12});
		personnelBudgetHeading.setSpacingBefore(20f);
		personnelBudgetHeading.addCell(getSubHeadingCell("Budget Persons", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, subHeadFont));
		document.add(personnelBudgetHeading);

		PdfPTable personsDetailsTable = new PdfPTable(5);
		personsDetailsTable.setWidthPercentage(100);
		personsDetailsTable.addCell(getTableHeadingCell("Person Type", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		personsDetailsTable.addCell(getTableHeadingCell("Name", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		personsDetailsTable.addCell(getTableHeadingCell("Job Type", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		personsDetailsTable.addCell(getTableHeadingCell("Appointment Type", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		personsDetailsTable.addCell(getTableHeadingCell("Effective Date", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		if (budgetPersonList != null && !budgetPersonList.isEmpty()) {
			for (BudgetPrintParameter budgetPerson : budgetPersonList) {
				personsDetailsTable.addCell(getTableCell(budgetPerson.getPersonType(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
				personsDetailsTable.addCell(getTableCell(budgetPerson.getName(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
				personsDetailsTable.addCell(getTableCell(budgetPerson.getJobType(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
				personsDetailsTable.addCell(getTableCell(budgetPerson.getAppointmentType(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
				personsDetailsTable.addCell(getTableCell(budgetPerson.getEffectiveDate(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
			}
		}
		document.add(personsDetailsTable);
	}

	private void prepareSimpleBudgetDetail(Document document, Font subHeadFont, Font bodyFont, Font tableLabelFont, BudgetHeader budgetHeader) throws DocumentException {
		PdfPTable simpleBudgetHeading = new PdfPTable(1);
		simpleBudgetHeading.setWidthPercentage(100);
        simpleBudgetHeading.setWidths(new int[]{12});
		simpleBudgetHeading.setSpacingBefore(20f);
		simpleBudgetHeading.addCell(getSubHeadingCell("Simple Budget", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, subHeadFont));
		document.add(simpleBudgetHeading);

		List<SimpleBudgetVO> simpleBudgetVOs = budgetService.prepareSimpleBudget(budgetHeader.getBudgetId());
		if (simpleBudgetVOs != null && !simpleBudgetVOs.isEmpty()) {
			Collections.sort(simpleBudgetVOs, new SimpleBudgetDetailComparatorByBudgetCategoryCode());
			Collections.sort(simpleBudgetVOs, new SimpleBudgetDetailComparatorBySystemGenerated());
		}
		List<BudgetPeriod> budgetPeriods = budgetHeader.getBudgetPeriods();
		Integer numberOfColumns = budgetPeriods.size() + 3;
		PdfPTable simpleBudgetOuterRow = new PdfPTable(1);
		simpleBudgetOuterRow.setWidthPercentage(100);
		PdfPTable simpleBudgetInnerRow = new PdfPTable(2);
		simpleBudgetInnerRow.setWidthPercentage(100);
		simpleBudgetInnerRow.addCell(getGeneralDetailsCell(Boolean.TRUE.equals(commonDao.getParameterValueAsBoolean(Constants.ENABLE_PROPOSAL_BUDGET_VERSIONS)) ? "Budget Details - All Periods : Version " + budgetHeader.getVersionNumber() : "Budget Details - All Periods", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, tableLabelFont));
		simpleBudgetInnerRow.addCell(getGeneralDetailsCell("Total Cost : (" + Constants.DOLLAR_SYMBOL + ")" + decimalFormat.format(budgetHeader.getTotalCost()), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_RIGHT, tableLabelFont));
		simpleBudgetOuterRow.addCell(getNestedTableCell(simpleBudgetInnerRow, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
		document.add(simpleBudgetOuterRow);

		PdfPTable simpleBudgetLabel = new PdfPTable(numberOfColumns);
		simpleBudgetLabel.setWidthPercentage(100);
		simpleBudgetLabel.addCell(getTableHeadingCell("Items", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		simpleBudgetLabel.addCell(getTableHeadingCell("Quantity", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		for (BudgetPeriod budgetPeriod : budgetPeriods) {
			simpleBudgetLabel.addCell(getTableHeadingCell("Period " + budgetPeriod.getBudgetPeriod(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		}
		simpleBudgetLabel.addCell(getTableHeadingCell("Total", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		document.add(simpleBudgetLabel);
		for (SimpleBudgetVO simpleBudgetVO : simpleBudgetVOs) {
			PdfPTable budgetCategoryOuterRow = new PdfPTable(1);
			budgetCategoryOuterRow.setWidthPercentage(100);
			PdfPTable budgetCategoryInnerRow = new PdfPTable(2);
			budgetCategoryInnerRow.setWidthPercentage(100);
			budgetCategoryInnerRow.addCell(getGeneralDetailsCell(simpleBudgetVO.getCategoryName(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, tableLabelFont));
			budgetCategoryInnerRow.addCell(getGeneralDetailsCell("Total : (" + Constants.DOLLAR_SYMBOL + ")" + decimalFormat.format(simpleBudgetVO.getTotalCategoryCost()).toString(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_RIGHT, tableLabelFont));
			budgetCategoryOuterRow.addCell(getNestedTableCell(budgetCategoryInnerRow, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			document.add(budgetCategoryOuterRow);
			for (SimpleBudgetLineItemVO budgetLineItem : simpleBudgetVO.getLineItemList()) {
				PdfPTable lineItem = new PdfPTable(numberOfColumns);
				lineItem.setWidthPercentage(100);
				if (budgetLineItem.getCostElement() != null) {
					lineItem.addCell(getTableCell(budgetLineItem.getCostElement().getDescription(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
				} else
					lineItem.addCell(getTableCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
				if (budgetLineItem.getQuantity() != null) {
					lineItem.addCell(getTableCell(decimalFormat.format(budgetLineItem.getQuantity()), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
				} else
					lineItem.addCell(getTableCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));

				for (PeriodCost periodCost : budgetLineItem.getPeriodCostsList()) {
					if (periodCost.getCost() != null) {
						lineItem.addCell(getTableCell(Constants.DOLLAR_SYMBOL + decimalFormat.format(periodCost.getCost()).toString(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
					} else
						lineItem.addCell(getTableCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
				}
				if (budgetLineItem.getTotalLineItemCost() != null) {
					lineItem.addCell(getTableCell(Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetLineItem.getTotalLineItemCost()).toString(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
				} else
					lineItem.addCell(getTableCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
				document.add(lineItem);
				if (budgetLineItem.getIsSystemGeneratedCostElement().equals(IS_NOT_SYSTEM_DENERATED_COST_ELEMENT)) {
					PdfPTable justificationDetailOuter = new PdfPTable(1);
					justificationDetailOuter.setWidthPercentage(100);
					PdfPTable justificationDetailInner = new PdfPTable(2);
					justificationDetailInner.setWidthPercentage(100);
                    justificationDetailInner.setWidths(new int[]{2, 10});
					justificationDetailInner.addCell(getGeneralDetailsCell("Justification :", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
					if (budgetLineItem.getLineItemDescription() != null) {
						justificationDetailInner.addCell(getGeneralDetailsCell(budgetLineItem.getLineItemDescription(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
					} else
						justificationDetailInner.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
                    justificationDetailOuter.addCell(getNestedTableCell(justificationDetailInner, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
					document.add(justificationDetailOuter);
				}
			}
		}
	}

	private void prepareDetailedBudgetDetail(Document document, Font subHeadFont, Font bodyFont, Font tableLabelFont, BudgetHeader budgetHeader, Boolean isShowInKind, Boolean isShowCostShareUnderRecovery) throws DocumentException {
		Boolean isShowModifiedCost = commonDao.getParameterValueAsBoolean(Constants.IS_SHOW_MODIFIED_DIRECT_COST);
		PdfPTable detailedBudgetHeading = new PdfPTable(1);
		detailedBudgetHeading.setWidthPercentage(100);
        detailedBudgetHeading.setWidths(new int[]{12});
		detailedBudgetHeading.setSpacingBefore(20f);
		detailedBudgetHeading.addCell(getSubHeadingCell("Detailed Budget", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, subHeadFont));
		document.add(detailedBudgetHeading);

		List<BudgetPeriod> budgetPeriods = budgetHeader.getBudgetPeriods();
		for (BudgetPeriod budgetPeriod : budgetPeriods) {
			PdfPTable detailedBudget = new PdfPTable(1);
			detailedBudget.setWidthPercentage(100);
			detailedBudget.addCell(getSubTableCell("Period " + budgetPeriod.getBudgetPeriod(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, tableLabelFont));
			PdfPTable detailedBudgetOuterRow = new PdfPTable(1);
			detailedBudgetOuterRow.setWidthPercentage(100);
			int numberOfColumns = 10;
			PdfPTable detailedBudgetInnerRow = prepareNumberOfColumns(numberOfColumns, isShowInKind, isShowCostShareUnderRecovery, isShowModifiedCost);
			detailedBudgetInnerRow.setWidthPercentage(100);
			detailedBudgetInnerRow.addCell(getGeneralDetailsCell("Start Date", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, tableLabelFont));
			detailedBudgetInnerRow.addCell(getGeneralDetailsCell("End Date", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, tableLabelFont));
			detailedBudgetInnerRow.addCell(getGeneralDetailsCell(DIRECT_COST, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, tableLabelFont));
			if (Boolean.TRUE.equals(isShowModifiedCost)) {
				detailedBudgetInnerRow.addCell(getGeneralDetailsCell(MODIFIED_DIRECT_COST, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, tableLabelFont));
			}
			detailedBudgetInnerRow.addCell(getGeneralDetailsCell(INDIRECT_COST, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, tableLabelFont));
			if (Boolean.TRUE.equals(isShowCostShareUnderRecovery)) {
				detailedBudgetInnerRow.addCell(getGeneralDetailsCell("Cost Share", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, tableLabelFont));
			}
			if (Boolean.TRUE.equals(isShowInKind)) {
				detailedBudgetInnerRow.addCell(getGeneralDetailsCell(TOTAL_IN_KIND, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, tableLabelFont));
			}
			if (Boolean.TRUE.equals(isShowCostShareUnderRecovery)) {
				detailedBudgetInnerRow.addCell(getGeneralDetailsCell("Under Recovery", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, tableLabelFont));
			}
			detailedBudgetInnerRow.addCell(getGeneralDetailsCell(TOTAL_REQUESTED_COST, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, tableLabelFont));
			detailedBudgetInnerRow.addCell(getGeneralDetailsCell(TOTAL_COST, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, tableLabelFont));
			if (budgetPeriod.getStartDate() != null) {
				Date date = (Date) budgetPeriod.getStartDate();
				String dateValue = commonService.convertDateFormatBasedOnTimeZone(date.getTime(), Constants.DEFAULT_DATE_FORMAT);
				detailedBudgetInnerRow.addCell(getGeneralDetailsCell(dateValue, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			} else
				detailedBudgetInnerRow.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			if (budgetPeriod.getEndDate() != null) {
				Date date = (Date) budgetPeriod.getEndDate();
				String dateValue = commonService.convertDateFormatBasedOnTimeZone(date.getTime(), Constants.DEFAULT_DATE_FORMAT);
				detailedBudgetInnerRow.addCell(getGeneralDetailsCell(dateValue, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			} else
				detailedBudgetInnerRow.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			if (budgetPeriod.getTotalDirectCost() != null)
				detailedBudgetInnerRow.addCell(getGeneralDetailsCell(Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetPeriod.getTotalDirectCost()).toString(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			else
				detailedBudgetInnerRow.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			if (Boolean.TRUE.equals(isShowModifiedCost)) {
				if (budgetPeriod.getTotalModifiedDirectCost() != null)
					detailedBudgetInnerRow.addCell(getGeneralDetailsCell(Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetPeriod.getTotalModifiedDirectCost()).toString(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
				else
					detailedBudgetInnerRow.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			}
			if (budgetPeriod.getTotalIndirectCost() != null)
				detailedBudgetInnerRow.addCell(getGeneralDetailsCell(Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetPeriod.getTotalIndirectCost()).toString(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			else
				detailedBudgetInnerRow.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			if (Boolean.TRUE.equals(isShowCostShareUnderRecovery)) {
				if (budgetPeriod.getCostSharingAmount() != null)
					detailedBudgetInnerRow.addCell(getGeneralDetailsCell(Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetPeriod.getCostSharingAmount()).toString(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
				else
					detailedBudgetInnerRow.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			}
			if (Boolean.TRUE.equals(isShowInKind)) {
				if (budgetPeriod.getTotalInKind() != null)
					detailedBudgetInnerRow.addCell(getGeneralDetailsCell(Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetPeriod.getTotalInKind()).toString(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
				else
					detailedBudgetInnerRow.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			}
			if (Boolean.TRUE.equals(isShowCostShareUnderRecovery)) {
				if (budgetPeriod.getUnderrecoveryAmount() != null)
					detailedBudgetInnerRow.addCell(getGeneralDetailsCell(Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetPeriod.getUnderrecoveryAmount()).toString(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
				else
					detailedBudgetInnerRow.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			}
			if (budgetPeriod.getTotalCost() != null)
				detailedBudgetInnerRow.addCell(getGeneralDetailsCell(Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetPeriod.getTotalCost()).toString(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			else
				detailedBudgetInnerRow.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			if (budgetPeriod.getTotalOfTotalCost() != null)
				detailedBudgetInnerRow.addCell(getGeneralDetailsCell(Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetPeriod.getTotalOfTotalCost()).toString(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			else
				detailedBudgetInnerRow.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			document.add(detailedBudget);
			detailedBudgetOuterRow.addCell(getNestedTableCell(detailedBudgetInnerRow, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			document.add(detailedBudgetOuterRow);

			if (budgetPeriod.getBudgetDetails() != null && !budgetPeriod.getBudgetDetails().isEmpty()) {
				PdfPTable detailedBudgetLabel = new PdfPTable(7);
				detailedBudgetLabel.setWidthPercentage(100);
				detailedBudgetLabel.addCell(getTableHeadingCell("Cost Element", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, tableLabelFont));
				detailedBudgetLabel.addCell(getTableHeadingCell("Line Item Description", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, tableLabelFont));
				detailedBudgetLabel.addCell(getTableHeadingCell("Quantity", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, tableLabelFont));
				detailedBudgetLabel.addCell(getTableHeadingCell("Line Item Cost (" + Constants.DOLLAR_SYMBOL + ")", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, tableLabelFont));
				if (Boolean.TRUE.equals(isShowInKind)) {
					detailedBudgetLabel.addCell(getTableHeadingCell("In-Kind %", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, tableLabelFont));
					detailedBudgetLabel.addCell(getTableHeadingCell("In-Kind Amount (" + Constants.DOLLAR_SYMBOL + ")", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, tableLabelFont));
				} else {
					detailedBudgetLabel.addCell(getTableHeadingCell("Cost Share %", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, tableLabelFont));
					detailedBudgetLabel.addCell(getTableHeadingCell("Cost Share Amount (" + Constants.DOLLAR_SYMBOL + ")", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, tableLabelFont));
				}
				detailedBudgetLabel.addCell(getTableHeadingCell("Fund Requested", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, tableLabelFont));
				document.add(detailedBudgetLabel);
				for (BudgetDetail budgetDetail : budgetPeriod.getBudgetDetails()) {
					PdfPTable budgetCostElements = new PdfPTable(7);
					budgetCostElements.setWidthPercentage(100);
					List<BudgetPersonalDetails> budgetPersonalDetails = new ArrayList<>();
					if (budgetDetail.getCostElementCode() != null)
						budgetCostElements.addCell(getTableCell(budgetDetail.getCostElementCode() + " - " + budgetDetail.getCostElement().getDescription(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
					else
						budgetCostElements.addCell(getTableCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
					if (budgetDetail.getLineItemDescription() != null)
						budgetCostElements.addCell(getTableCell(budgetDetail.getLineItemDescription(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
					else
						budgetCostElements.addCell(getTableCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
					if (budgetDetail.getQuantity() != null)
						budgetCostElements.addCell(getTableCell(decimalFormat.format(budgetDetail.getQuantity()), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
					else
						budgetCostElements.addCell(getTableCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
					if (budgetDetail.getLineItemCost() != null)
						budgetCostElements.addCell(getTableCell(Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetDetail.getLineItemCost()).toString(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
					else
						budgetCostElements.addCell(getTableCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
					if (budgetDetail.getCostSharingPercentage() != null)
						budgetCostElements.addCell(getTableCell(budgetDetail.getCostSharingPercentage().toString(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
					else
						budgetCostElements.addCell(getTableCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
					if (budgetDetail.getCostSharingAmount() != null)
						budgetCostElements.addCell(getTableCell(Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetDetail.getCostSharingAmount()).toString(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
					else
						budgetCostElements.addCell(getTableCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
					if (budgetDetail.getSponsorRequestedAmount() != null)
						budgetCostElements.addCell(getTableCell(Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetDetail.getSponsorRequestedAmount()).toString(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
					else
						budgetCostElements.addCell(getTableCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
					document.add(budgetCostElements);
					PdfPTable budgetCategoryDetail = new PdfPTable(1);
					budgetCategoryDetail.setWidthPercentage(100);
					budgetCategoryDetail.addCell(getTableCell(budgetDetail.getBudgetCategory().getDescription(), PdfPCell.ALIGN_MIDDLE, PdfPCell.LEFT, bodyFont));
					document.add(budgetCategoryDetail);

					if (budgetDetail.getBudgetCategory().getBudgetCategoryTypeCode().equals(Constants.BUDGET_CATEGORY_CODE_TYPE_PERSONNEL)) {
						budgetPersonalDetails = budgetDetail.getPersonsDetails();
						if (budgetPersonalDetails != null && !budgetPersonalDetails.isEmpty()) {
							PdfPTable budgetPersonalDetail = new PdfPTable(9);
							budgetPersonalDetail.setWidthPercentage(100);
							budgetPersonalDetail.addCell(getTablePersonHeadingCell("Persons", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
							budgetPersonalDetail.addCell(getTablePersonHeadingCell("Start Date", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
							budgetPersonalDetail.addCell(getTablePersonHeadingCell("End Date", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
							budgetPersonalDetail.addCell(getTablePersonHeadingCell("Base Salary (" + Constants.DOLLAR_SYMBOL + ")", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
							budgetPersonalDetail.addCell(getTablePersonHeadingCell("Effort %", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
							budgetPersonalDetail.addCell(getTablePersonHeadingCell("Requested Salary (" + Constants.DOLLAR_SYMBOL + ")", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
							if (Boolean.TRUE.equals(isShowInKind)) {
								budgetPersonalDetail.addCell(getTablePersonHeadingCell("In-Kind %", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
								budgetPersonalDetail.addCell(getTablePersonHeadingCell("In-Kind (" + Constants.DOLLAR_SYMBOL + ")", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
							} else {
								budgetPersonalDetail.addCell(getTablePersonHeadingCell("Cost Share %", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
								budgetPersonalDetail.addCell(getTablePersonHeadingCell("Cost Share (" + Constants.DOLLAR_SYMBOL + ")", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
							}
							budgetPersonalDetail.addCell(getTablePersonHeadingCell("Fund Requested (" + Constants.DOLLAR_SYMBOL + ")", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));

							for (BudgetPersonalDetails personalDetail : budgetPersonalDetails) {
								if (personalDetail.getBudgetPerson().getPersonName() != null)
									budgetPersonalDetail.addCell(getTablePersonCell(personalDetail.getBudgetPerson().getPersonName(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
								else if (personalDetail.getBudgetPerson().getTbnId() != null)
									budgetPersonalDetail.addCell(getTablePersonCell(personalDetail.getBudgetPerson().getTbnPerson().getPersonName(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
								else
									budgetPersonalDetail.addCell(getTablePersonCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
								if (personalDetail.getStartDate() != null) {
									Date date = (Date) personalDetail.getStartDate();
									String dateValue = commonService.convertDateFormatBasedOnTimeZone(date.getTime(), Constants.DEFAULT_DATE_FORMAT);
									budgetPersonalDetail.addCell(getTablePersonCell((String) dateValue, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
								} else
									budgetPersonalDetail.addCell(getTablePersonCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
								if (personalDetail.getEndDate() != null) {
									Date date = (Date) personalDetail.getEndDate();
									String dateValue = commonService.convertDateFormatBasedOnTimeZone(date.getTime(), Constants.DEFAULT_DATE_FORMAT);
									budgetPersonalDetail.addCell(getTablePersonCell((String) dateValue, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
								} else
									budgetPersonalDetail.addCell(getTablePersonCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
								if (personalDetail.getSalary() != null)
									budgetPersonalDetail.addCell(getTablePersonCell(Constants.DOLLAR_SYMBOL + decimalFormat.format(personalDetail.getSalary()).toString(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
								else
									budgetPersonalDetail.addCell(getTablePersonCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
								if (personalDetail.getPercentageEffort() != null)
									budgetPersonalDetail.addCell(getTablePersonCell(personalDetail.getPercentageEffort().toString(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
								else
									budgetPersonalDetail.addCell(getTablePersonCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
								if (personalDetail.getSalaryRequested() != null)
									budgetPersonalDetail.addCell(getTablePersonCell(Constants.DOLLAR_SYMBOL + decimalFormat.format(personalDetail.getSalaryRequested()).toString(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
								else
									budgetPersonalDetail.addCell(getTablePersonCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
								if (personalDetail.getCostSharingPercentage() != null)
									budgetPersonalDetail.addCell(getTablePersonCell(personalDetail.getCostSharingPercentage().toString(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
								else
									budgetPersonalDetail.addCell(getTablePersonCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
								if (personalDetail.getCostSharingAmount() != null)
									budgetPersonalDetail.addCell(getTablePersonCell(Constants.DOLLAR_SYMBOL + decimalFormat.format(personalDetail.getCostSharingAmount()).toString(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
								else
									budgetPersonalDetail.addCell(getTablePersonCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
								if (personalDetail.getSponsorRequestedAmount() != null)
									budgetPersonalDetail.addCell(getTablePersonCell(Constants.DOLLAR_SYMBOL + decimalFormat.format(personalDetail.getSponsorRequestedAmount()).toString(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
								else
									budgetPersonalDetail.addCell(getTablePersonCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
							}
							document.add(budgetPersonalDetail);
						}
					}
				}
			}
		}
	}

	private void prepareBudgetSummaryDetail(Document document, Font subHeadFont, Font bodyFont, Font tableLabelFont, BudgetHeader budgetHeader, Boolean isShowInKind) throws DocumentException {
		PdfPTable budgetSummaryHeading = new PdfPTable(1);
		budgetSummaryHeading.setWidthPercentage(100);
        budgetSummaryHeading.setWidths(new int[]{12});
		budgetSummaryHeading.setSpacingBefore(20f);
		budgetSummaryHeading.addCell(getSubHeadingCell(BUDGET_SUMMARY, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, subHeadFont));
		document.add(budgetSummaryHeading);

		List<BudgetPeriod> budgetPeriods = budgetHeader.getBudgetPeriods();
		BudgetPrintParameter budgetPrintParameter = setBudgetSummaryDetailsForExcelPrint(budgetPeriods);
		List<BudgetPeriodSummary> budgetPeriodSummaries = budgetPrintParameter.getBudgetPeriodSummaries();
		List<BudgetSummaryVO> budgetSummaryVOs = budgetPrintParameter.getBudgetSummaryVOs();
		Integer numberOfColumns;
		if (Boolean.TRUE.equals(isShowInKind)) {
			numberOfColumns = budgetPeriods.size() + 4;
		} else {
			numberOfColumns = budgetPeriods.size() + 3;
		}
		PdfPTable budgetSummary = new PdfPTable(numberOfColumns);
		budgetSummary.setWidthPercentage(100);
		budgetSummary.addCell(getTableHeadingCell(CATEGORY, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		for (BudgetPeriod budgetPeriod : budgetPeriods) {
            budgetSummary.addCell(getTableHeadingCell("Period " + budgetPeriod.getBudgetPeriod() + " (" + Constants.DOLLAR_SYMBOL + ")", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		}
		if (Boolean.TRUE.equals(isShowInKind)) {
            budgetSummary.addCell(getTableHeadingCell(TOTAL_IN_KIND + " (" + Constants.DOLLAR_SYMBOL + ")", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		}
        budgetSummary.addCell(getTableHeadingCell(TOTAL_REQUESTED_COST + " (" + Constants.DOLLAR_SYMBOL + ")", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
        budgetSummary.addCell(getTableHeadingCell(TOTAL_COST + " (" + Constants.DOLLAR_SYMBOL + ")", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
        budgetPeriodSummaries.stream().sorted(Comparator.comparing(BudgetPeriodSummary::getSortOrder)).collect(Collectors.toList()).forEach(budgetPeriodSummary -> {
			budgetSummary.addCell(getTableCell(budgetPeriodSummary.getBudgetCategory(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
            budgetPeriodSummary.getBudgetSummaryVOs().stream().forEach(budgetSummaryVO -> budgetSummary.addCell(getTableCell(decimalFormat.format(budgetSummaryVO.getTotalFundRequested()), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont)));
			if (Boolean.TRUE.equals(isShowInKind)) {
                budgetSummary.addCell(getTableCell(decimalFormat.format(budgetPeriodSummary.getTotalCostShareAmountSum()), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
			}
            budgetSummary.addCell(getTableCell(decimalFormat.format(budgetPeriodSummary.getTotalFundRequestedCostSum()), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
            budgetSummary.addCell(getTableCell(decimalFormat.format(budgetPeriodSummary.getTotalLineItemCostSum()), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
			
		});
		budgetSummary.addCell(getTableCell("Grand Total", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		for (BudgetSummaryVO budgetSummaryVO : budgetSummaryVOs) {
            budgetSummary.addCell(getTableCell(decimalFormat.format(budgetSummaryVO.getTotalFundRequested()), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		}
		if (Boolean.TRUE.equals(isShowInKind)) {
			budgetSummary.addCell(getTableCell(budgetPrintParameter.getPeriodTotalCostShareSum(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		}
		budgetSummary.addCell(getTableCell(budgetPrintParameter.getPeriodTotalFundRequestedSum(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		budgetSummary.addCell(getTableCell(budgetPrintParameter.getPeriodCostsTotalSum(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		document.add(budgetSummary);
	}

	public ByteArrayInputStream generateAwardBudgetPDF(PrintVO vo) throws DocumentException, ParseException {
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
			pdfHeading.setAlignment(Paragraph.ALIGN_CENTER);
			pdfHeading.setSpacingBefore(5f);
			pdfHeading.setSpacingAfter(7f);
			document.add(pdfHeading);
			AwardBudgetHeader awardBudgetHeader = awardBudgetDao.fetchBudgetByBudgetId(vo.getBudgetId());
			Award award = awardDao.fetchAwardByAwardId(vo.getAwardId().toString());
			awardBudgetHeader.setFundCode(award.getAccountNumber());
			awardBudgetHeader.setFundCenter(award.getFundCenter());
			awardBudgetService.fetchAwardBudgetPeriods(awardBudgetHeader);
			// prepareAwardOverviewDetails(document, subHeadFont, bodyFont, bodyTextFont,
			// award);
            prepareAwardGeneralInfo(document, subHeadFont, bodyFont, bodyTextFont, award, Boolean.FALSE, null, null);
			prepareKeyPersonnelDetails(document, subHeadFont, bodyFont, tableLabelFont, award.getAwardPersons());
			if (awardBudgetHeader != null) {
				prepareAwardBudgetOverviewDetails(document, subHeadFont, bodyFont, bodyTextFont, awardBudgetHeader);
				prepareAwardPeriodsAndTotalDetails(document, subHeadFont, bodyFont, tableLabelFont, awardBudgetHeader);
				if (vo.getIsDetailedBudgetPrint().equals("Y")) {
					prepareAwardDetailedBudgetDetail(document, subHeadFont, bodyFont, tableLabelFont, awardBudgetHeader);
				}
				if (vo.getIsBudgetSummaryPrint().equals("Y")) {
					prepareAwardBudgetSummaryDetail(document, subHeadFont, bodyFont, tableLabelFont, awardBudgetHeader);
				}
			}
			document.close();
		} catch (DocumentException ex) {
		}
		return new ByteArrayInputStream(out.toByteArray());
	}

	private void prepareAwardBudgetOverviewDetails(Document document, Font subHeadFont, Font bodyFont, Font bodyTextFont, AwardBudgetHeader awardBudgetHeader) throws DocumentException {
		PdfPTable budgetOverviewHeading = new PdfPTable(1);
		budgetOverviewHeading.setWidthPercentage(100);
        budgetOverviewHeading.setWidths(new int[]{12});
		budgetOverviewHeading.setSpacingBefore(20f);
		budgetOverviewHeading.addCell(getSubHeadingCell("Budget Overview", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, subHeadFont));
		document.add(budgetOverviewHeading);

		PdfPTable budgetOverviewDetails = new PdfPTable(6);
		budgetOverviewDetails.setWidthPercentage(100);
        budgetOverviewDetails.setWidths(new int[]{5, 1, 5 , 5, 1, 5});
		Boolean isShowBudgetOHRatePercentage = commonDao.getParameterValueAsBoolean(Constants.SHOW_BUDGET_OH_RATE_PERCENTAGE);
		Boolean isEnableCostShareStatus = commonDao.getParameterValueAsBoolean(Constants.ENABLE_COST_SHARE_STATUS);
		if (awardBudgetHeader != null) {
			budgetOverviewDetails.addCell(getGeneralDetailsCell("Type ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(awardBudgetHeader.getBudgetTypeCode() != null ? awardBudgetHeader.getBudgetType().getDescription() : " ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell("Version Number ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(awardBudgetHeader.getVersionNumber() != null ? awardBudgetHeader.getVersionNumber().toString() : " ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell("Budget Status ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(awardBudgetHeader.getBudgetStatus() != null ? awardBudgetHeader.getBudgetStatus().getDescription() : " ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell("Start Date ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(awardBudgetHeader.getStartDate() != null ? commonService.convertDateFormatBasedOnTimeZone(awardBudgetHeader.getStartDate().getTime(), Constants.DEFAULT_DATE_FORMAT) : " ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell("End Date ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(awardBudgetHeader.getEndDate() != null ? commonService.convertDateFormatBasedOnTimeZone(awardBudgetHeader.getEndDate().getTime(), Constants.DEFAULT_DATE_FORMAT) : " ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
            Boolean isCampusFlagEnabled = commonDao.getParameterValueAsBoolean(Constants.ENABLE_CAMPUS_FLAG_AWARD);
			if (Boolean.TRUE.equals(isCampusFlagEnabled)) {
			budgetOverviewDetails.addCell(getGeneralDetailsCell("Campus Flag ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			if (awardBudgetHeader.getOnOffCampusFlag() != null) {
				if (awardBudgetHeader.getOnOffCampusFlag().equals("N")) {
					budgetOverviewDetails.addCell(getGeneralDetailsCell("OFF", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
				} else if (awardBudgetHeader.getOnOffCampusFlag().equals("Y")) {
					budgetOverviewDetails.addCell(getGeneralDetailsCell("ON", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
				} else if (awardBudgetHeader.getOnOffCampusFlag().equals("D")) {
					budgetOverviewDetails.addCell(getGeneralDetailsCell("BOTH", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
				}
			}
			}
            budgetOverviewDetails.addCell(getGeneralDetailsCell("F & A Rate Type ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(awardBudgetHeader.getRateTypeCode() != null ? awardBudgetHeader.getRateType().getDescription() : " ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
            if (awardBudgetHeader.getAvailableFundType() != null) {
            	 AwardBudgetFundType fundType = awardBudgetDao.getBudgetFundTypeByCode(awardBudgetHeader.getAvailableFundType());
                 budgetOverviewDetails.addCell(getGeneralDetailsCell("Remaining Fund based on ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
                 budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
                 budgetOverviewDetails.addCell(getGeneralDetailsCell(fundType != null && fundType.getFundType() != null ? fundType.getFundType() : "", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
            }
			budgetOverviewDetails.addCell(getGeneralDetailsCell("Direct Cost ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(awardBudgetHeader.getTotalDirectCost() != null ? Constants.DOLLAR_SYMBOL + decimalFormat.format(awardBudgetHeader.getTotalDirectCost()) : " ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell("Indirect Cost ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(awardBudgetHeader.getTotalIndirectCost() != null ? Constants.DOLLAR_SYMBOL + decimalFormat.format(awardBudgetHeader.getTotalIndirectCost()) : " ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell("Total Cost ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(awardBudgetHeader.getTotalCost() != null ? Constants.DOLLAR_SYMBOL + decimalFormat.format(awardBudgetHeader.getTotalCost()) : " ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
			/* SMU SPECIFIC FUND CENTER
            budgetOverviewDetails.addCell(getGeneralDetailsCell("Fund Centre ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(awardBudgetHeader.getFundCenter() != null ? awardBudgetHeader.getFundCenter() : " ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));*/
            budgetOverviewDetails.addCell(getGeneralDetailsCell("Account Number", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
            budgetOverviewDetails.addCell(getGeneralDetailsCell(awardBudgetHeader.getFundCode() != null ? awardBudgetHeader.getFundCode() : " ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
            budgetOverviewDetails.addCell(getGeneralDetailsCell("Budget Template  ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
            budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
            budgetOverviewDetails.addCell(getGeneralDetailsCell(awardBudgetHeader.getBudgetTemplateTypeId() != null ? budgetDao.fetchBudgetTemplateTypeById(awardBudgetHeader.getBudgetTemplateTypeId()).getDescription() : "", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell("Description ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(awardBudgetHeader.getComments() != null ? awardBudgetHeader.getComments() : " ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
			if (Boolean.TRUE.equals(isEnableCostShareStatus)) {
				budgetOverviewDetails.addCell(getGeneralDetailsCell("Cost Sharing", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
				budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
				budgetOverviewDetails.addCell(getGeneralDetailsCell(awardBudgetHeader.getCostSharingTypeCode() != null ? awardBudgetHeader.getCostSharingType().getDescription() : "", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
			}
			if (Boolean.TRUE.equals(isShowBudgetOHRatePercentage)) {
				budgetOverviewDetails.addCell(getGeneralDetailsCell("On-Campus Rate", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
				budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
				budgetOverviewDetails.addCell(getGeneralDetailsCell(awardBudgetHeader.getOnCampusRates() != null ? awardBudgetHeader.getOnCampusRates() : "", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
				budgetOverviewDetails.addCell(getGeneralDetailsCell("Off-Campus Rate", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
				budgetOverviewDetails.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
				budgetOverviewDetails.addCell(getGeneralDetailsCell(awardBudgetHeader.getOffCampusRates() != null ? awardBudgetHeader.getOffCampusRates() : "", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
			}
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			budgetOverviewDetails.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			document.add(budgetOverviewDetails);
		}
	}

	private void prepareAwardPeriodsAndTotalDetails(Document document, Font subHeadFont, Font bodyFont, Font tableLabelFont, AwardBudgetHeader awardBudgetHeader) throws DocumentException {
		PdfPTable periodsAndTotalHeading = new PdfPTable(1);
		periodsAndTotalHeading.setWidthPercentage(100);
        periodsAndTotalHeading.setWidths(new int[]{12});
		periodsAndTotalHeading.setSpacingBefore(20f);
		periodsAndTotalHeading.addCell(getSubHeadingCell("Periods and Total", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, subHeadFont));
		document.add(periodsAndTotalHeading);

		List<AwardBudgetPeriod> awardBudgetPeriodsData = awardBudgetDao.getAwardBudgetPeriodsByBudgetId(awardBudgetHeader.getBudgetId());
		List<BudgetPrintParameter> awardBudgetPeriodsAndTotal = prepareAwardBudgetPeriodsAndTotalData(awardBudgetPeriodsData);
		PdfPTable budgetPeriodsAndTotal = new PdfPTable(6);
		budgetPeriodsAndTotal.setWidthPercentage(100);

		budgetPeriodsAndTotal.addCell(getTableHeadingCell("Period #", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		budgetPeriodsAndTotal.addCell(getTableHeadingCell("Period Start Date", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		budgetPeriodsAndTotal.addCell(getTableHeadingCell("Period End Date", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		budgetPeriodsAndTotal.addCell(getTableHeadingCell("Direct Cost", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		budgetPeriodsAndTotal.addCell(getTableHeadingCell("Indirect Cost", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		budgetPeriodsAndTotal.addCell(getTableHeadingCell("Total Cost", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		for (BudgetPrintParameter periodsAndTotal : awardBudgetPeriodsAndTotal) {
			budgetPeriodsAndTotal.addCell(getTableCell("Period " + periodsAndTotal.getPeriod(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
			budgetPeriodsAndTotal.addCell(getTableCell(periodsAndTotal.getPeriodStartDate(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
			budgetPeriodsAndTotal.addCell(getTableCell(periodsAndTotal.getPeriodEndDate(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
			budgetPeriodsAndTotal.addCell(getTableCell(periodsAndTotal.getDirectCost(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
			budgetPeriodsAndTotal.addCell(getTableCell(periodsAndTotal.getIndirectCost(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
			budgetPeriodsAndTotal.addCell(getTableCell(periodsAndTotal.getTotalCost(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
		}
		document.add(budgetPeriodsAndTotal);
	}

	private void prepareAwardDetailedBudgetDetail(Document document, Font subHeadFont, Font bodyFont, Font tableLabelFont, AwardBudgetHeader awardBudgetHeader) throws DocumentException {
		PdfPTable detailedBudgetHeading = new PdfPTable(1);
		detailedBudgetHeading.setWidthPercentage(100);
        detailedBudgetHeading.setWidths(new int[]{12});
		detailedBudgetHeading.setSpacingBefore(20f);
		detailedBudgetHeading.addCell(getSubHeadingCell("Detailed Budget", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, subHeadFont));
		document.add(detailedBudgetHeading);
		Boolean budgetVariationFlag = false;
		Boolean isNonPersonalLineItemEnabled = commonDao.getParameterValueAsBoolean(Constants.BUDGET_ASSOCIATED_WITH_NON_PERSONAL_SUBITEM_ENABLED);
		if (awardBudgetHeader.getBudgetTypeCode().equals(Constants.BUDGET_TYPE_REBUDGET) && (awardBudgetHeader.getBudgetStatusCode().equals(Constants.AWARD_BUDGET_STATUS_CODE_INPROGRESS))) {
			budgetVariationFlag = true;
		}
		List<AwardBudgetPeriod> budgetPeriods = awardBudgetHeader.getBudgetPeriods();
		for (AwardBudgetPeriod budgetPeriod : budgetPeriods) {
			PdfPTable detailedBudget = new PdfPTable(1);
			detailedBudget.setWidthPercentage(100);
			detailedBudget.addCell(getSubTableCell("Period " + budgetPeriod.getBudgetPeriod(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, tableLabelFont));
			PdfPTable detailedBudgetOuterRow = new PdfPTable(1);
			detailedBudgetOuterRow.setWidthPercentage(100);
			PdfPTable detailedBudgetInnerRow = new PdfPTable(5);
			detailedBudgetInnerRow.setWidthPercentage(100);
			detailedBudgetInnerRow.addCell(getGeneralDetailsCell("Start Date", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, tableLabelFont));
			detailedBudgetInnerRow.addCell(getGeneralDetailsCell("End Date", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, tableLabelFont));
			detailedBudgetInnerRow.addCell(getGeneralDetailsCell("Direct Cost", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, tableLabelFont));
			detailedBudgetInnerRow.addCell(getGeneralDetailsCell("Indirect Cost", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, tableLabelFont));
			detailedBudgetInnerRow.addCell(getGeneralDetailsCell("Total Cost", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, tableLabelFont));
			if (budgetPeriod.getStartDate() != null) {
				Date date = (Date) budgetPeriod.getStartDate();
				String dateValue = commonService.convertDateFormatBasedOnTimeZone(date.getTime(), Constants.DEFAULT_DATE_FORMAT);
				detailedBudgetInnerRow.addCell(getGeneralDetailsCell(dateValue, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			} else
				detailedBudgetInnerRow.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			if (budgetPeriod.getEndDate() != null) {
				Date date = (Date) budgetPeriod.getEndDate();
				String dateValue = commonService.convertDateFormatBasedOnTimeZone(date.getTime(), Constants.DEFAULT_DATE_FORMAT);
				detailedBudgetInnerRow.addCell(getGeneralDetailsCell(dateValue, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			} else
				detailedBudgetInnerRow.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			if (budgetPeriod.getTotalDirectCost() != null)
				detailedBudgetInnerRow.addCell(getGeneralDetailsCell(Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetPeriod.getTotalDirectCost()).toString(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			else
				detailedBudgetInnerRow.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));

			if (budgetPeriod.getTotalIndirectCost() != null)
                detailedBudgetInnerRow.addCell(getGeneralDetailsCell(Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetPeriod.getTotalIndirectCost()).toString(),
								PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			else
				detailedBudgetInnerRow.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			if (budgetPeriod.getTotalCost() != null)
				detailedBudgetInnerRow.addCell(getGeneralDetailsCell(Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetPeriod.getTotalCost()).toString(),
								PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			else
				detailedBudgetInnerRow.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			document.add(detailedBudget);
			detailedBudgetOuterRow.addCell(getNestedTableCell(detailedBudgetInnerRow, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
			document.add(detailedBudgetOuterRow);
			if (budgetPeriod.getBudgetDetails() != null && !budgetPeriod.getBudgetDetails().isEmpty()) {
				PdfPTable detailedBudgetLabel;
				if (budgetVariationFlag) {
					detailedBudgetLabel = new PdfPTable(7);
					detailedBudgetLabel.setWidthPercentage(100);
				} else {
					detailedBudgetLabel = new PdfPTable(5);
					detailedBudgetLabel.setWidthPercentage(100);
				}
				detailedBudgetLabel.addCell(getTableHeadingCell("Budget Category", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
				detailedBudgetLabel.addCell(getTableHeadingCell("Cost Element", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
				detailedBudgetLabel.addCell(getTableHeadingCell("WBS Number (for OFIN use)", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
				detailedBudgetLabel.addCell(getTableHeadingCell("Quantity", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
				if (budgetVariationFlag) {
					detailedBudgetLabel.addCell(getTableHeadingCell("Latest Approved Budget (" + Constants.DOLLAR_SYMBOL + ")", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
					detailedBudgetLabel.addCell(getTableHeadingCell("Balance To Date (" + Constants.DOLLAR_SYMBOL + ")", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
					detailedBudgetLabel.addCell(getTableHeadingCell("Revised Budget (" + Constants.DOLLAR_SYMBOL + ")", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
				} else {
					detailedBudgetLabel.addCell(getTableHeadingCell("Line Item Cost (" + Constants.DOLLAR_SYMBOL + ")", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
				}
				document.add(detailedBudgetLabel);
				for (AwardBudgetDetail budgetDetail : budgetPeriod.getBudgetDetails()) {
					List<AwardBudgetPersonalDetail> awardBudgetPersonalDetails = new ArrayList<AwardBudgetPersonalDetail>();
					List<AwardBudgetNonPersonDetail> awardBudgetNonPersonalDetails = new ArrayList<AwardBudgetNonPersonDetail>();
					PdfPTable budgetCostElements;
					if (budgetVariationFlag) {
						budgetCostElements = new PdfPTable(7);
						budgetCostElements.setWidthPercentage(100);
					} else {
						budgetCostElements = new PdfPTable(5);
						budgetCostElements.setWidthPercentage(100);
					}
					if (budgetDetail.getBudgetCategory() != null)
						budgetCostElements.addCell(getTableCell(budgetDetail.getBudgetCategory().getDescription(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
					else
						budgetCostElements.addCell(getTableCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
					if (budgetDetail.getCostElementCode() != null)
						budgetCostElements.addCell(getTableCell(
								budgetDetail.getCostElementCode() + " - "
										+ budgetDetail.getCostElement().getDescription(),
								PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
					else
						budgetCostElements
								.addCell(getTableCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
					if (budgetDetail.getInternalOrderCode() != null)
						budgetCostElements.addCell(getTableCell(budgetDetail.getInternalOrderCode(),
								PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
					else
						budgetCostElements
								.addCell(getTableCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
					if (budgetDetail.getQuantity() != null)
						budgetCostElements.addCell(getTableCell(decimalFormat.format(budgetDetail.getQuantity()),
								PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
					else
						budgetCostElements
								.addCell(getTableCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));

					if (budgetVariationFlag) {
						if (budgetDetail.getPrevLineItemCost() != null)
							budgetCostElements.addCell(getTableCell(
									Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetDetail.getPrevLineItemCost()).toString(),
									PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
						else
							budgetCostElements
									.addCell(getTableCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
						awardBudgetService.calculateBalanceToDateValue(budgetDetail, awardBudgetHeader.getFundCode());
						if (budgetDetail.getBalanceToDate() != null)
							budgetCostElements.addCell(
									getTableCell(Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetDetail.getBalanceToDate()).toString(),
											PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
						else
							budgetCostElements
									.addCell(getTableCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
					}
					if (budgetDetail.getLineItemCost() != null)
						budgetCostElements.addCell(
								getTableCell(Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetDetail.getLineItemCost()).toString(),
										PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
					else
						budgetCostElements
								.addCell(getTableCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
					document.add(budgetCostElements);
					if (budgetDetail.getIsSystemGeneratedCostElement().equals(IS_NOT_SYSTEM_DENERATED_COST_ELEMENT)) {
						PdfPTable justificationDetailOuter = new PdfPTable(1);
						justificationDetailOuter.setWidthPercentage(100);
						PdfPTable justificationDetailInner = new PdfPTable(2);
						justificationDetailInner.setWidthPercentage(100);
                        justificationDetailInner.setWidths(new int[]{2, 10});
						justificationDetailInner.addCell(getGeneralDetailsCell("Line Item Description :",
								PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
						if (budgetDetail.getLineItemDescription() != null) {
							justificationDetailInner
									.addCell(getGeneralDetailsCell(budgetDetail.getLineItemDescription(),
											PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
						} else
							justificationDetailInner.addCell(
									getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
						justificationDetailOuter.addCell(getNestedTableCell(justificationDetailInner,
								PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
						document.add(justificationDetailOuter);
					}
					if (budgetDetail.getBudgetCategory().getBudgetCategoryTypeCode().equals("P")) {
						awardBudgetPersonalDetails = budgetDetail.getPersonsDetails();
						if (awardBudgetPersonalDetails != null && !awardBudgetPersonalDetails.isEmpty()) {
							PdfPTable personDetail = new PdfPTable(7);
							personDetail.setWidthPercentage(100);
							personDetail.addCell(getTablePersonHeadingCell("Persons", PdfPCell.ALIGN_MIDDLE,
									PdfPCell.ALIGN_CENTER, tableLabelFont));
							personDetail.addCell(getTablePersonHeadingCell("Start Date", PdfPCell.ALIGN_MIDDLE,
									PdfPCell.ALIGN_CENTER, tableLabelFont));
							personDetail.addCell(getTablePersonHeadingCell("End Date", PdfPCell.ALIGN_MIDDLE,
									PdfPCell.ALIGN_CENTER, tableLabelFont));
							personDetail.addCell(getTablePersonHeadingCell("WBS Number (for OFIN use)",
									PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
							personDetail.addCell(getTablePersonHeadingCell("Salary("+ Constants.DOLLAR_SYMBOL +")", PdfPCell.ALIGN_MIDDLE,
									PdfPCell.ALIGN_CENTER, tableLabelFont));
							personDetail.addCell(getTablePersonHeadingCell("Effort%", PdfPCell.ALIGN_MIDDLE,
									PdfPCell.ALIGN_CENTER, tableLabelFont));
							personDetail.addCell(getTablePersonHeadingCell("Applied Salary ("+Constants.DOLLAR_SYMBOL +")", PdfPCell.ALIGN_MIDDLE,
									PdfPCell.ALIGN_CENTER, tableLabelFont));
							for (AwardBudgetPersonalDetail awardBudgetPersonalDetail : awardBudgetPersonalDetails) {
								if (awardBudgetPersonalDetail.getBudgetPerson().getBudgetPersonName() != null)
									personDetail.addCell(getTablePersonCell(
											awardBudgetPersonalDetail.getBudgetPerson().getBudgetPersonName(),
											PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
								else
									personDetail.addCell(getTablePersonCell(" ", PdfPCell.ALIGN_MIDDLE,
											PdfPCell.ALIGN_CENTER, bodyFont));
								if (awardBudgetPersonalDetail.getStartDate() != null) {
									Date date = (Date) awardBudgetPersonalDetail.getStartDate();
									String dateValue = commonService.convertDateFormatBasedOnTimeZone(date.getTime(),
											Constants.DEFAULT_DATE_FORMAT);
									personDetail.addCell(getTablePersonCell(dateValue, PdfPCell.ALIGN_MIDDLE,
											PdfPCell.ALIGN_CENTER, bodyFont));
								} else
									personDetail.addCell(getTablePersonCell(" ", PdfPCell.ALIGN_MIDDLE,
											PdfPCell.ALIGN_CENTER, bodyFont));
								if (awardBudgetPersonalDetail.getEndDate() != null) {
									Date date = (Date) awardBudgetPersonalDetail.getEndDate();
									String dateValue = commonService.convertDateFormatBasedOnTimeZone(date.getTime(),
											Constants.DEFAULT_DATE_FORMAT);
									personDetail.addCell(getTablePersonCell(dateValue, PdfPCell.ALIGN_MIDDLE,
											PdfPCell.ALIGN_CENTER, bodyFont));
								} else
									personDetail.addCell(getTablePersonCell(" ", PdfPCell.ALIGN_MIDDLE,
											PdfPCell.ALIGN_CENTER, bodyFont));
								if (awardBudgetPersonalDetail.getInternalOrderCode() != null)
									personDetail.addCell(
											getTablePersonCell(awardBudgetPersonalDetail.getInternalOrderCode(),
													PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
								else
									personDetail.addCell(getTablePersonCell(" ", PdfPCell.ALIGN_MIDDLE,
											PdfPCell.ALIGN_CENTER, bodyFont));
								if (awardBudgetPersonalDetail.getTotalSalary() != null)
									personDetail.addCell(getTablePersonCell(
											Constants.DOLLAR_SYMBOL
													+ decimalFormat.format(awardBudgetPersonalDetail.getTotalSalary()).toString(),
											PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
								else
									personDetail.addCell(getTablePersonCell(" ", PdfPCell.ALIGN_MIDDLE,
											PdfPCell.ALIGN_CENTER, bodyFont));
								if (awardBudgetPersonalDetail.getPercentageEffort() != null)
									personDetail.addCell(getTablePersonCell(
											awardBudgetPersonalDetail.getPercentageEffort().toString(),
											PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
								else
									personDetail.addCell(getTablePersonCell(" ", PdfPCell.ALIGN_MIDDLE,
											PdfPCell.ALIGN_CENTER, bodyFont));
								if (awardBudgetPersonalDetail.getSalaryRequested() != null)
									personDetail.addCell(getTablePersonCell(
											Constants.DOLLAR_SYMBOL
													+ decimalFormat.format(awardBudgetPersonalDetail.getSalaryRequested()).toString(),
											PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
								else
									personDetail.addCell(getTablePersonCell(" ", PdfPCell.ALIGN_MIDDLE,
											PdfPCell.ALIGN_CENTER, bodyFont));
							}
							document.add(personDetail);
						}
					} else if (budgetDetail.getIsSystemGeneratedCostElement()
							.equals(IS_NOT_SYSTEM_DENERATED_COST_ELEMENT) && (isNonPersonalLineItemEnabled)) {
						awardBudgetNonPersonalDetails = budgetDetail.getNonPersonsDetails();
						if (awardBudgetNonPersonalDetails != null && !awardBudgetNonPersonalDetails.isEmpty()) {
							PdfPTable nonPersonDetail = new PdfPTable(3);
							nonPersonDetail.setWidthPercentage(100);
							nonPersonDetail.addCell(getTablePersonHeadingCell("Sub Line Item", PdfPCell.ALIGN_MIDDLE,
									PdfPCell.ALIGN_CENTER, tableLabelFont));
							nonPersonDetail.addCell(getTablePersonHeadingCell("WBS Number (for OFIN use)",
									PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
							nonPersonDetail.addCell(getTablePersonHeadingCell("Line Item Cost ("+Constants.DOLLAR_SYMBOL +")",
									PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
							for (AwardBudgetNonPersonDetail awardBudgetNonPersonalDetail : awardBudgetNonPersonalDetails) {
								if (awardBudgetNonPersonalDetail.getDescription() != null)
									nonPersonDetail
											.addCell(getTablePersonCell(awardBudgetNonPersonalDetail.getDescription(),
													PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
								else
									nonPersonDetail.addCell(getTablePersonCell(" ", PdfPCell.ALIGN_MIDDLE,
											PdfPCell.ALIGN_CENTER, bodyFont));
								if (awardBudgetNonPersonalDetail.getInternalOrderCode() != null)
									nonPersonDetail.addCell(
											getTablePersonCell(awardBudgetNonPersonalDetail.getInternalOrderCode(),
													PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
								else
									nonPersonDetail.addCell(getTablePersonCell(" ", PdfPCell.ALIGN_MIDDLE,
											PdfPCell.ALIGN_CENTER, bodyFont));
								if (awardBudgetNonPersonalDetail.getLineItemCost() != null)
									nonPersonDetail.addCell(getTablePersonCell(
											Constants.DOLLAR_SYMBOL
													+ decimalFormat.format(awardBudgetNonPersonalDetail.getLineItemCost()).toString(),
											PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
								else
									nonPersonDetail.addCell(getTablePersonCell(" ", PdfPCell.ALIGN_MIDDLE,
											PdfPCell.ALIGN_CENTER, bodyFont));
							}
							document.add(nonPersonDetail);
						}
					}
				}
			}
		}
	}

	private void prepareAwardBudgetSummaryDetail(Document document, Font subHeadFont, Font bodyFont, Font tableLabelFont, AwardBudgetHeader awardBudgetHeader) throws DocumentException {
		PdfPTable awardBudgetSummaryHeading = new PdfPTable(1);
		awardBudgetSummaryHeading.setWidthPercentage(100);
        awardBudgetSummaryHeading.setWidths(new int[]{12});
		awardBudgetSummaryHeading.setSpacingBefore(20f);
		awardBudgetSummaryHeading.addCell(getSubHeadingCell(BUDGET_SUMMARY, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, subHeadFont));
		document.add(awardBudgetSummaryHeading);
		List<AwardBudgetPeriod> budgetPeriods = awardBudgetHeader.getBudgetPeriods();
		BudgetPrintParameter budgetPrintParameter = setAwardBudgetSummaryForPrint(budgetPeriods);
		List<AwardBudgetPeriodSummary> budgetPeriodSummaries = budgetPrintParameter.getAwardBudgetPeriodSummaries();
		List<AwardBudgetSummaryVO> budgetSummaryVOs = budgetPrintParameter.getAwardBudgetSummaryVOs();
		Integer numberOfColumns = budgetPeriods.size() + 2;

		PdfPTable budgetSummary = new PdfPTable(numberOfColumns);
		budgetSummary.setWidthPercentage(100);
		budgetSummary.addCell(getTableHeadingCell("Category", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		for (AwardBudgetPeriod budgetPeriod : budgetPeriods) {
            budgetSummary.addCell(getTableHeadingCell("Period " + budgetPeriod.getBudgetPeriod() + " (" + Constants.DOLLAR_SYMBOL + ")", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		}
        budgetSummary.addCell(getTableHeadingCell("Total (" + Constants.DOLLAR_SYMBOL + ")", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		for (AwardBudgetPeriodSummary budgetPeriodSummary : budgetPeriodSummaries) {
			budgetSummary.addCell(getTableCell(budgetPeriodSummary.getBudgetCategory(), PdfPCell.ALIGN_MIDDLE,
					PdfPCell.ALIGN_CENTER, bodyFont));
			for (AwardBudgetSummaryVO budgetSummaryVO : budgetPeriodSummary.getBudgetSummaryVOs()) {
				budgetSummary.addCell(getTableCell(budgetSummaryVO.getLineItemCostValue(), PdfPCell.ALIGN_MIDDLE,
						PdfPCell.ALIGN_CENTER, bodyFont));
			}
			budgetSummary.addCell(getTableCell(budgetPeriodSummary.getTotalFundRequestedCost(), PdfPCell.ALIGN_MIDDLE,
					PdfPCell.ALIGN_CENTER, bodyFont));
		}
		budgetSummary
				.addCell(getTableCell(GRAND_TOTAL, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		for (AwardBudgetSummaryVO budgetSummaryVO : budgetSummaryVOs) {
			budgetSummary.addCell(getTableCell(budgetSummaryVO.getLineItemCostValue(), PdfPCell.ALIGN_MIDDLE,
					PdfPCell.ALIGN_CENTER, tableLabelFont));
		}
		budgetSummary.addCell(getTableCell(budgetPrintParameter.getPeriodCostsTotalSum(), PdfPCell.ALIGN_MIDDLE,
				PdfPCell.ALIGN_CENTER, tableLabelFont));
		document.add(budgetSummary);
	}

	private BudgetPrintParameter setAwardBudgetSummaryForPrint(List<AwardBudgetPeriod> budgetPeriods) {
		BudgetPrintParameter budgetPrintParameter = new BudgetPrintParameter();
		Set<String> budgetCategoryCodes = new HashSet<>();
		Set<String> costElements = new HashSet<>();
		List<AwardBudgetSummaryVO> awardBudgetSummaryVOs = new ArrayList<>();
		List<AwardBudgetPeriodSummary> budgetPeriodSummaries = new ArrayList<>();
		List<AwardBudgetDetail> budgetDetails = new ArrayList<>();
		List<Integer> budgetPeriodIds = new ArrayList<>();
		BigDecimal periodTotalSum = BigDecimal.ZERO;
		periodTotalSum = prepareAwardBudgetSummaryDetails(budgetPeriodSummaries, awardBudgetSummaryVOs, budgetPeriods, budgetPeriodIds, budgetDetails, costElements, budgetCategoryCodes, periodTotalSum);
		budgetPrintParameter.setAwardBudgetSummaryVOs(awardBudgetSummaryVOs);
		budgetPrintParameter.setPeriodCostsTotalSum(decimalFormat.format(periodTotalSum));
		budgetPrintParameter.setAwardBudgetPeriodSummaries(budgetPeriodSummaries);
		return budgetPrintParameter;
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

	public static PdfPCell getTablePersonHeadingCell(String text, int verticalAlignment, int horizontalAlignmentt, Font bodyFont) {
		PdfPCell cell = new PdfPCell(new Phrase(text, bodyFont));
		cell.setPadding(5);
		cell.setVerticalAlignment(verticalAlignment);
		cell.setHorizontalAlignment(horizontalAlignmentt);
		cell.setBackgroundColor(new BaseColor(173, 216, 230));
		return cell;
	}

	public static PdfPCell getTablePersonCell(String text, int verticalAlignment, int horizontalAlignmentt, Font bodyFont) {
		PdfPCell cell = new PdfPCell(new Phrase(text, bodyFont));
		cell.setPadding(10);
		cell.setVerticalAlignment(verticalAlignment);
		cell.setHorizontalAlignment(horizontalAlignmentt);
		cell.setBackgroundColor(new BaseColor(173, 216, 230));
		return cell;
	}

	public static PdfPCell getNestedTableCell(PdfPTable detailedBudgetRow2, int verticalAlignment, int horizontalAlignmentt, Font bodyFont) {
		PdfPCell cell = new PdfPCell();
		cell.setPadding(10);
		cell.setVerticalAlignment(verticalAlignment);
		cell.setHorizontalAlignment(horizontalAlignmentt);
		cell.addElement(detailedBudgetRow2);
		return cell;
	}

	public static PdfPCell getSubTableCell(String text, int verticalAlignment, int horizontalAlignmentt, Font bodyFont) {
		PdfPCell cell = new PdfPCell(new Phrase(text, bodyFont));
		cell.setPadding(5);
		cell.setVerticalAlignment(verticalAlignment);
		cell.setHorizontalAlignment(horizontalAlignmentt);
		cell.setBackgroundColor(new BaseColor(211, 211, 211));
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

	@Override
	public ResponseEntity<byte[]> generateAwardBudgetSummaryExcelReport(HttpServletResponse response, Integer awardId, Integer budgetId) {
		ResponseEntity<byte[]> attachmentData = null;
		try {
			XSSFWorkbook workbook = new XSSFWorkbook();
			Award award = awardDao.fetchAwardByAwardId(awardId.toString());
			AwardBudgetHeader awardBudgetHeader = awardBudgetDao.fetchBudgetByBudgetId(budgetId);
			awardBudgetHeader.setBudgetPeriods(awardBudgetDao.getAwardBudgetPeriodsByBudgetId(awardBudgetHeader.getBudgetId()));
			for (AwardBudgetPeriod awardBudgetPeriod : awardBudgetHeader.getBudgetPeriods()) {
				awardBudgetPeriod.setBudgetDetails(awardBudgetDao.fetchAwardBudgetDetailByPeriodId(awardBudgetPeriod.getBudgetPeriodId()));
			}
			BudgetPrintParameter budgetPrintParameter = setAwardBudgetSummaryForPrint(awardBudgetHeader.getBudgetPeriods());
			XSSFSheet sheet = workbook.createSheet(BUDGET_SUMMARY);
            commonService.addDetailsInHeader(workbook, sheet);
			prepareExcelSheetForAwardBudgetSummary(budgetPrintParameter, awardBudgetHeader.getBudgetPeriods(), sheet, workbook, award, awardBudgetHeader);
			autoSizeColumns(workbook);
			ByteArrayOutputStream bos = new ByteArrayOutputStream();
			workbook.write(bos);
			attachmentData = getResponseEntityForExcelDownload(bos.toByteArray());
		} catch (Exception e) {
			logger.error("Exception in generateAwardDetailedBudgetExcelReport : {} ", e.getMessage());
			e.printStackTrace();
		}
		return attachmentData;
	}

	private void prepareExcelSheetForAwardBudgetSummary(BudgetPrintParameter budgetPrintParameter, List<AwardBudgetPeriod> budgetPeriods, XSSFSheet sheet, XSSFWorkbook workbook, Award award, AwardBudgetHeader awardBudgetHeader) {
		XSSFCellStyle tableBodyStyle = workbook.createCellStyle();
		sheet.addMergedRegion(new CellRangeAddress(0, 0, 0, 1));
		int rowNumber = 0;
        rowNumber = prepareAwardGeneralInformationForExcel(sheet, rowNumber, awardBudgetHeader.getAwardId().toString(), workbook, null, Boolean.FALSE);
		String heading = "Budget Summary(Version " + awardBudgetHeader.getVersionNumber() + ")";
		rowNumber = rowNumber + 1;
		int totalColumnCount = 2 + budgetPeriods.size();
		prepareExcelSheetHeading(sheet, heading, workbook, totalColumnCount, rowNumber);
		rowNumber = rowNumber + 1;
        Object[] tableHeading = {"Category"};
		int periodNumber = 1;
		for (int i = 0; i < budgetPeriods.size(); i++) {
			tableHeading = appendHeader(tableHeading, "Period " + periodNumber++ + " (" + Constants.DOLLAR_SYMBOL + ")");
		}
		tableHeading = appendHeader(tableHeading, "Total (" + Constants.DOLLAR_SYMBOL + ")");
		prepareExcelSheetHeader(sheet, tableHeading, workbook, tableBodyStyle, rowNumber++);
		List<AwardBudgetPeriodSummary> budgetPeriodSummaries = budgetPrintParameter.getAwardBudgetPeriodSummaries();
		List<AwardBudgetSummaryVO> budgetSummaryVOs = budgetPrintParameter.getAwardBudgetSummaryVOs();
		prepareAwardBudgetSummaryForExcel(sheet, rowNumber, budgetPeriodSummaries, tableBodyStyle, budgetPrintParameter, budgetSummaryVOs, workbook);
		autoSizeColumns(workbook, rowNumber);
	}

	private void prepareAwardBudgetSummaryForExcel(XSSFSheet sheet, int rowNumber, List<AwardBudgetPeriodSummary> budgetPeriodSummaries, XSSFCellStyle tableBodyStyle, BudgetPrintParameter budgetPrintParameter, List<AwardBudgetSummaryVO> budgetSummaryVOs, XSSFWorkbook workbook) {
		XSSFCellStyle budgetSummaryTotalRowStyle = prepareTableRowFont(workbook);
		for (AwardBudgetPeriodSummary budgetPeriodSummary : budgetPeriodSummaries) {
			Row row = sheet.createRow(rowNumber++);
			int cellNumber = 0;
			Cell categoryNameCell = row.createCell(cellNumber++);
			categoryNameCell.setCellStyle(tableBodyStyle);
			categoryNameCell.setCellValue(budgetPeriodSummary.getBudgetCategory());

			for (AwardBudgetSummaryVO budgetSummaryVO : budgetPeriodSummary.getBudgetSummaryVOs()) {
				Cell periodCostCell = row.createCell(cellNumber++);
				periodCostCell.setCellStyle(tableBodyStyle);
				periodCostCell.setCellValue(budgetSummaryVO.getLineItemCostValue());
			}

			Cell categoryCostSumCell = row.createCell(cellNumber++);
			categoryCostSumCell.setCellStyle(tableBodyStyle);
			categoryCostSumCell.setCellValue(budgetPeriodSummary.getTotalFundRequestedCost());
		}

		Row row = sheet.createRow(rowNumber);
		int cellNumber = 0;
		Cell categoryNameCell = row.createCell(cellNumber++);
		categoryNameCell.setCellStyle(budgetSummaryTotalRowStyle);
		categoryNameCell.setCellValue(GRAND_TOTAL);

		for (AwardBudgetSummaryVO budgetSummaryVO : budgetSummaryVOs) {
			Cell periodTotalCostCell = row.createCell(cellNumber++);
			periodTotalCostCell.setCellStyle(budgetSummaryTotalRowStyle);
			periodTotalCostCell.setCellValue(budgetSummaryVO.getLineItemCostValue());
		}

		Cell periodCostsTotalSumCell = row.createCell(cellNumber++);
		periodCostsTotalSumCell.setCellStyle(budgetSummaryTotalRowStyle);
		periodCostsTotalSumCell.setCellValue(budgetPrintParameter.getPeriodCostsTotalSum());
	}

	@Override
	public ResponseEntity<byte[]> exportCurrentAndPending(HttpServletResponse response, Integer moduleCode,
			String personId, String moduleItemKey) {
		ResponseEntity<byte[]> attachmentData = null;
		try {
			byte[] data = getLetterTemplate(Constants.CP_LETTER_TEMPLATE_TYPE_CODE);
			byte[] mergedOutput = setMergePlaceHoldersOfCurrentAndPending(data, moduleCode, personId, moduleItemKey);
			String generatedFileName = RESULT + System.nanoTime() + ".pdf";
			setHttpHeaderAndHttpResponseData(response, generatedFileName, mergedOutput, attachmentData);
		} catch (Exception e) {
			logger.error("Exception in generateAgreementSummary : {}", e.getMessage());
		}
		return attachmentData;
	}

	private byte[] setMergePlaceHoldersOfCurrentAndPending(byte[] data, Integer moduleCode, String personId, String moduleItemKey) {
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		try {
			InputStream myInputStream = new ByteArrayInputStream(data);
			IXDocReport report = XDocReportRegistry.getRegistry().loadReport(myInputStream,
					TemplateEngineKind.Velocity);
			FieldsMetadata fieldsMetadata = report.createFieldsMetadata();
			IContext context = report.createContext();
			CurrentAndPendingVO currentAndPendingVO = new CurrentAndPendingVO();
			currentAndPendingVO.setModuleCode(moduleCode);
			currentAndPendingVO.setModuleItemKey(moduleItemKey);
			currentAndPendingVO.setPersonId(personId);
			List<CurrentAndPendingPersonDTO> currentAndPendingDto = currentAndPendingService
					.preparePersonCurrentAndPendingDetails(currentAndPendingVO);
			List<CurrentAndPendingPersonDTO> currentAndPendingPersonDTO = setCurrentAndPendingPrint(
					currentAndPendingDto);
			fieldsMetadata.load("currentAndPendingPersonDTO", CurrentAndPendingPersonDTO.class, true);
			context.put("currentAndPendingPersonDTO", currentAndPendingPersonDTO);
			if (moduleCode.equals(Constants.DEV_PROPOSAL_MODULE_CODE)) {
				Proposal proposal = proposalDao.fetchProposalById(Integer.parseInt(moduleItemKey));
				context = setCPPlaceHolderData(context, proposal);
			}
			Options options = Options.getTo(ConverterTypeTo.PDF).via(ConverterTypeVia.XWPF);
			report.convert(context, options, baos);
		} catch (Exception e) {
			logger.error("Exception in setMergePlaceHoldersOfCurrentAndPending : {}", e.getMessage());
		}
		return baos.toByteArray();
	}

	private List<CurrentAndPendingPersonDTO> setCurrentAndPendingPrint(List<CurrentAndPendingPersonDTO> currentAndPendings) {
		List<CurrentAndPendingPersonDTO> currentAndPendingDto = new ArrayList<>();
		for (CurrentAndPendingPersonDTO currentAndPending : currentAndPendings) {
			CurrentAndPendingPersonDTO currentAndPendingData = new CurrentAndPendingPersonDTO();
			currentAndPendingData
					.setPersonName(currentAndPending.getPersonName() != null ? currentAndPending.getPersonName() : "");
			currentAndPendingData
					.setRoleName(currentAndPending.getRoleName() != null ? currentAndPending.getRoleName() : "");
			List<CurrentAndPendingModulePrintDTO> currentAwards = new ArrayList<>();
			List<CurrentAndPendingModulePrintDTO> pendingProposals = new ArrayList<>();
			if (currentAndPending.getCurrentAwards() != null && !currentAndPending.getCurrentAwards().isEmpty()) {
				for (CurrentAndPendingModuleDTO module : currentAndPending.getCurrentAwards()) {
					if (Boolean.FALSE.equals(module.getIsExcluded())) {
						CurrentAndPendingModulePrintDTO moduleDetails = new CurrentAndPendingModulePrintDTO();
						CPReportProjectDetailDTO reportDetail = new CPReportProjectDetailDTO();
						moduleDetails.setModuleItemId(
								module.getModuleItemId() != null ? module.getModuleItemId().toString() : null);
						moduleDetails
								.setModuleItemKey(module.getModuleItemKey() != null ? module.getModuleItemKey() : null);
						moduleDetails
								.setModuleStatus(module.getModuleStatus() != null ? module.getModuleStatus() : null);
						moduleDetails.setTitle(module.getTitle() != null ? module.getTitle() : null);
						moduleDetails.setSponsorAwardNumber(
								module.getSponsorAwardNumber() != null ? module.getSponsorAwardNumber() : null);
						moduleDetails.setDepartment(module.getDepartment() != null ? module.getDepartment() : null);
						moduleDetails.setLeadPrincipalInvestigator(
								module.getLeadPrincipalInvestigator() != null ? module.getLeadPrincipalInvestigator()
										: null);
						moduleDetails
								.setStartDate(
										module.getStartDate() != null
												? commonService.convertDateFormatBasedOnTimeZone(
														module.getStartDate().getTime(), Constants.DEFAULT_DATE_FORMAT)
												: null);
						moduleDetails
								.setEndDate(
										module.getEndDate() != null
												? commonService.convertDateFormatBasedOnTimeZone(
														module.getEndDate().getTime(), Constants.DEFAULT_DATE_FORMAT)
												: null);
                        String sponsorAcronym = module.getSponsor() != null && module.getSponsor().getAcronym() != null ? " (" + module.getSponsor().getAcronym() + ")" : "";
                        moduleDetails.setSponsorName(module.getSponsor() != null && module.getSponsor().getSponsorName() != null ? module.getSponsor().getSponsorCode() + " - " + module.getSponsor().getSponsorName() + sponsorAcronym : null);
						moduleDetails.setTotalAwardAmount(module.getTotalAwardAmount() != null
								? decimalFormat.format(module.getTotalAwardAmount()) + ""
								: null);
						moduleDetails.setIsExcluded(module.getIsExcluded());
						moduleDetails.setLeadPIPersonId(
								module.getLeadPIPersonId() != null ? module.getLeadPIPersonId() : null);
						moduleDetails.setLeadPiNonEmployeeFlag(module.getLeadPiNonEmployeeFlag());
						moduleDetails.setAnnualDirectCost(module.getAnnualDirectCost() != null
								? decimalFormat.format(module.getAnnualDirectCost()) + ""
								: null);
						moduleDetails.setIsManuallyAdded(module.getIsManuallyAdded());
						if (module.getCpReportProjectDetailExt() != null) {
							reportDetail.setPercentageOfEffort(
									module.getCpReportProjectDetailExt().getPercentageOfEffort() != null
											? module.getCpReportProjectDetailExt().getPercentageOfEffort() + ""
											: null);
							reportDetail.setAnnualDirectCost(
									module.getCpReportProjectDetailExt().getAnnualDirectCost() != null
											? Constants.DOLLAR_SYMBOL + " "
													+ decimalFormat.format(
															module.getCpReportProjectDetailExt().getAnnualDirectCost())
													+ ""
											: null);
							reportDetail.setTotalAwardAmount(
									module.getCpReportProjectDetailExt().getTotalAwardAmount() != null
											? Constants.DOLLAR_SYMBOL + " "
													+ decimalFormat.format(
															module.getCpReportProjectDetailExt().getTotalAwardAmount())
													+ ""
											: null);
							reportDetail.setComments(module.getCpReportProjectDetailExt().getComments() != null
									? module.getCpReportProjectDetailExt().getComments()
									: null);
							reportDetail.setProjectGoals(module.getCpReportProjectDetailExt().getProjectGoals() != null
									? module.getCpReportProjectDetailExt().getProjectGoals()
									: null);
							reportDetail.setSpecificAims(module.getCpReportProjectDetailExt().getSpecificAims() != null
									? module.getCpReportProjectDetailExt().getSpecificAims()
									: null);
							reportDetail
									.setProjectSummary(module.getCpReportProjectDetailExt().getProjectSummary() != null
											? module.getCpReportProjectDetailExt().getProjectSummary()
											: null);
							reportDetail
									.setRolePlayed((module.getCpReportProjectDetailExt().getProposalPersonRole() != null
											&& module.getCpReportProjectDetailExt().getProposalPersonRole()
													.getDescription() != null)
															? module.getCpReportProjectDetailExt()
																	.getProposalPersonRole().getDescription()
															: null);
							moduleDetails.setcPReportProjectDetailDTO(reportDetail);
						} else {
							reportDetail.setPercentageOfEffort(
									module.getPercentageEffort() != null ? module.getPercentageEffort() + "" : null);
							if (module.getTotalAwardAmount() != null
									&& (module.getCurrency() == null || (module.getCurrency() != null
											&& module.getCurrency().getCurrencySymbol() == null))) {
								reportDetail.setTotalAwardAmount(
										Constants.DOLLAR_SYMBOL + decimalFormat.format(module.getTotalAwardAmount()));
							} else if (module.getTotalAwardAmount() != null && module.getCurrency() != null
									&& module.getCurrency().getCurrencySymbol() != null) {
								reportDetail.setTotalAwardAmount(module.getCurrency().getCurrencySymbol() + " "
										+ decimalFormat.format(module.getTotalAwardAmount()) + "");
							} else {
								reportDetail.setTotalAwardAmount(Constants.DOLLAR_SYMBOL + "0.00");
							}
							reportDetail.setRolePlayed((module.getProposalPersonRole() != null
									&& module.getProposalPersonRole().getDescription() != null)
											? module.getProposalPersonRole().getDescription()
											: null);
							reportDetail.setGrantCallName(
									module.getGrantCallName() != null ? module.getGrantCallName() + "" : null);
							reportDetail.setFundingType(
									module.getSponsorType() != null ? module.getSponsorType().getDescription() + ""
											: null);
							moduleDetails.setcPReportProjectDetailDTO(reportDetail);
						}
						currentAwards.add(moduleDetails);
					}
				}
			}
			currentAndPendingData.getCurrentPrintAwards().addAll(currentAwards);
			if (currentAndPending.getPendingProposals() != null && !currentAndPending.getPendingProposals().isEmpty()) {
				for (CurrentAndPendingModuleDTO module : currentAndPending.getPendingProposals()) {
					if (Boolean.FALSE.equals(module.getIsExcluded())) {
						CurrentAndPendingModulePrintDTO moduleDetails = new CurrentAndPendingModulePrintDTO();
						CPReportProjectDetailDTO reportDetail = new CPReportProjectDetailDTO();
						moduleDetails.setModuleItemId(
								module.getModuleItemId() != null ? module.getModuleItemId().toString() : null);
						moduleDetails
								.setModuleItemKey(module.getModuleItemKey() != null ? module.getModuleItemKey() : null);
						moduleDetails
								.setModuleStatus(module.getModuleStatus() != null ? module.getModuleStatus() : null);
						moduleDetails
								.setApplicationId(module.getApplicationId() != null ? module.getApplicationId() : null);
						moduleDetails.setTitle(
								module.getTitle() != null && module.getModuleItemId() != null ? module.getTitle()
										: null);
						moduleDetails.setSponsorAwardNumber(
								module.getSponsorAwardNumber() != null ? module.getSponsorAwardNumber() : null);
						moduleDetails.setDepartment(module.getDepartment() != null ? module.getDepartment() : null);
						moduleDetails.setLeadPrincipalInvestigator(
								module.getLeadPrincipalInvestigator() != null ? module.getLeadPrincipalInvestigator()
										: null);
						moduleDetails
								.setStartDate(
										module.getStartDate() != null
												? commonService.convertDateFormatBasedOnTimeZone(
														module.getStartDate().getTime(), Constants.DEFAULT_DATE_FORMAT)
												: null);
						moduleDetails
								.setEndDate(
										module.getEndDate() != null
												? commonService.convertDateFormatBasedOnTimeZone(
														module.getEndDate().getTime(), Constants.DEFAULT_DATE_FORMAT)
												: null);
                        String sponsorAcronym = module.getSponsor() != null && module.getSponsor().getAcronym() != null ? " (" + module.getSponsor().getAcronym() + ")" : "";
                        moduleDetails.setSponsorName(module.getSponsor() != null && module.getSponsor().getSponsorName() != null ? module.getSponsor().getSponsorCode() + " - " + module.getSponsor().getSponsorName() + sponsorAcronym : null);
						moduleDetails.setTotalAwardAmount(module.getTotalAwardAmount() != null
								? decimalFormat.format(module.getTotalAwardAmount()) + ""
								: null);
						moduleDetails.setIsExcluded(module.getIsExcluded());
						moduleDetails.setLeadPIPersonId(
								module.getLeadPIPersonId() != null ? module.getLeadPIPersonId() : null);
						moduleDetails.setLeadPiNonEmployeeFlag(module.getLeadPiNonEmployeeFlag());
						moduleDetails.setAnnualDirectCost(module.getAnnualDirectCost() != null
								? decimalFormat.format(module.getAnnualDirectCost()) + ""
								: null);
						moduleDetails.setIsManuallyAdded(module.getIsManuallyAdded());
						if (module.getCpReportProjectDetailExt() != null) {
							reportDetail.setPercentageOfEffort(
									module.getCpReportProjectDetailExt().getPercentageOfEffort() != null
											? module.getCpReportProjectDetailExt().getPercentageOfEffort() + ""
											: null);
							reportDetail.setAnnualDirectCost(
									module.getCpReportProjectDetailExt().getAnnualDirectCost() != null
											? Constants.DOLLAR_SYMBOL + " "
													+ decimalFormat.format(
															module.getCpReportProjectDetailExt().getAnnualDirectCost())
													+ ""
											: null);
							reportDetail.setTotalAwardAmount(
									module.getCpReportProjectDetailExt().getTotalAwardAmount() != null
											? Constants.DOLLAR_SYMBOL + " "
													+ decimalFormat.format(
															module.getCpReportProjectDetailExt().getTotalAwardAmount())
													+ ""
											: null);
							reportDetail.setComments(module.getCpReportProjectDetailExt().getComments() != null
									? module.getCpReportProjectDetailExt().getComments()
									: null);
							reportDetail.setProjectGoals(module.getCpReportProjectDetailExt().getProjectGoals() != null
									? module.getCpReportProjectDetailExt().getProjectGoals()
									: null);
							reportDetail.setSpecificAims(module.getCpReportProjectDetailExt().getSpecificAims() != null
									? module.getCpReportProjectDetailExt().getSpecificAims()
									: null);
							reportDetail
									.setProjectSummary(module.getCpReportProjectDetailExt().getProjectSummary() != null
											? module.getCpReportProjectDetailExt().getProjectSummary()
											: null);
							reportDetail
									.setRolePlayed((module.getCpReportProjectDetailExt().getProposalPersonRole() != null
											&& module.getCpReportProjectDetailExt().getProposalPersonRole()
													.getDescription() != null)
															? module.getCpReportProjectDetailExt()
																	.getProposalPersonRole().getDescription()
															: null);
							moduleDetails.setcPReportProjectDetailDTO(reportDetail);
						} else {
							reportDetail.setPercentageOfEffort(
									module.getPercentageEffort() != null ? module.getPercentageEffort() + "" : null);
							if (module.getTotalAwardAmount() != null
									&& (module.getCurrency() == null || (module.getCurrency() != null
											&& module.getCurrency().getCurrencySymbol() == null))) {
								reportDetail.setTotalAwardAmount(
										Constants.DOLLAR_SYMBOL + decimalFormat.format(module.getTotalAwardAmount()));
							} else if (module.getTotalAwardAmount() != null && module.getCurrency() != null
									&& module.getCurrency().getCurrencySymbol() != null) {
								reportDetail.setTotalAwardAmount(module.getCurrency().getCurrencySymbol() + " "
										+ decimalFormat.format(module.getTotalAwardAmount()) + "");
							} else {
								reportDetail.setTotalAwardAmount(Constants.DOLLAR_SYMBOL + "0.00");
							}
							reportDetail.setRolePlayed((module.getProposalPersonRole() != null
									&& module.getProposalPersonRole().getDescription() != null)
											? module.getProposalPersonRole().getDescription()
											: null);
							reportDetail.setGrantCallName(
									module.getGrantCallName() != null ? module.getGrantCallName() + "" : null);
							reportDetail.setFundingType(
									module.getSponsorType() != null ? module.getSponsorType().getDescription() + ""
											: null);
							moduleDetails.setcPReportProjectDetailDTO(reportDetail);
						}
						pendingProposals.add(moduleDetails);
					}
				}
			}
			currentAndPendingData.getPendingPrintProposals().addAll(pendingProposals);
			currentAndPendingDto.add(currentAndPendingData);
		}
		return currentAndPendingDto;
	}

	private IContext setCPPlaceHolderData(IContext context, Proposal proposal) {
		context.put("PROPOSAL_ID", proposal.getProposalId());
		if (commonDao.getParameterValueAsBoolean(Constants.IS_APPLICATION_ID_REQUIRED)) {
			context.put("PROPOSAL_ID", proposal.getApplicationId() != null ? proposal.getApplicationId() : "");
		} else {
			context.put("PROPOSAL_ID", proposal.getProposalId() != null ? proposal.getProposalId() : "");
		}
		context.put("TITLE", proposal.getTitle() != null ? proposal.getTitle() : "");
		return context;
	}

	@Override
	public ResponseEntity<byte[]> generateAwardExpenseExcelReport(PrintVO vo) throws Exception {
		ResponseEntity<byte[]> attachmentData = null;
		try {
			XSSFWorkbook workbook = new XSSFWorkbook();
			if (vo.getIsExpenseOrPurchase().equals("Y")) {
                String internalOrderCodes = (vo.getInternalOrderCodes() != null && !vo.getInternalOrderCodes().isEmpty()) ? String.join(",", vo.getInternalOrderCodes()) : null;
				List<AwardExpenseTransaction> expenseDetails = awardExpenseService.getExpenseTransactions(
						vo.getAccountNumber(), internalOrderCodes, vo.getActualOrCommittedFlag(),
						vo.getAwardNumber(), vo.getFmPostingStartDate(), vo.getFmPostingEndDate(), vo.getIsShowAllTransactions());
				XSSFSheet sheet = workbook.createSheet("Expense Details");
                commonService.addDetailsInHeader(workbook, sheet);
				Object[] tableHeadingRow = null;
				if (vo.getActualOrCommittedFlag().equals("A")) {
                    tableHeadingRow = new Object[]{"WBS Number (for OFIN use)", "GL Account", "Bank Clearing Date", "Document Date",
							"Source Doc Ref", "Posting Date", "System Doc Ref",
                            "Amount(" + Constants.DOLLAR_SYMBOL + ")", "Vendor", "Description"};
				} else {
//                    tableHeadingRow = new Object[]{"WBS Number (for OFIN use)", "GL Account", "PO / PR Date", "Source Doc ref",
//                            "Amount(" + Constants.DOLLAR_SYMBOL + ")", "Transaction Type", "Vendor", "Description"};
					  tableHeadingRow = new Object[]{"WBS Number (for OFIN use)", "GL Account", "PO / PR Date", "System Doc ref", "Amount(" + Constants.DOLLAR_SYMBOL + ")", "Transaction Type", "Vendor", "Description"};
				}
				prepareExcelSheetForExpenseAndPurchaseDetails(expenseDetails, sheet, tableHeadingRow, workbook, vo);
			}
			if (vo.getIsRevenue().equals("Y")) {
				List<AwardRevenueTransactions> revenueDetails = awardRevenueDao.fetchRevenueTransactionsByParams(
                        vo.getAwardNumber(), vo.getAccountNumber(), vo.getInternalOrderCodes(), vo.getFiPostingStartDate(), vo.getFiPostingEndDate());
				XSSFSheet sheet = workbook.createSheet("Revenue Details");
                commonService.addDetailsInHeader(workbook, sheet);
                Object[] tableHeadingRow = {"WBS Number (for OFIN use)", "GL Account", "Bank Clearing Date", "Fiscal Year", "Payment Doc Ref", "Document Date", "Source Doc Ref",
						"Posting Date", "System Doc Ref", "Amount(" + Constants.DOLLAR_SYMBOL + ")", "Customer",
                        "Description"};
				prepareExcelSheetForRevenueDetails(revenueDetails, sheet, tableHeadingRow, workbook, vo);
				addDetailsInHeader(workbook, sheet);
			}
			autoSizeColumns(workbook);
			ByteArrayOutputStream bos = new ByteArrayOutputStream();
			workbook.write(bos);
			attachmentData = getResponseEntityForExcelDownload(bos.toByteArray());
		} catch (Exception e) {
			logger.error("Exception in generateProposalDetailedBudgetExcelReport : {} ", e.getMessage());
		}
		return attachmentData;
	}

	@Override
	public ResponseEntity<byte[]> generateAwardExpensePDFReport(HttpServletResponse response, PrintVO vo) throws Exception {
		ResponseEntity<byte[]> attachmentData = null;
		try {
			ByteArrayInputStream bis = generatePDFReport(vo);
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
			logger.error("Exception in generateAwardExpensePDFReport : {}", e.getMessage());
		}
		return attachmentData;
	}

	private void prepareExcelSheetForRevenueDetails(List<AwardRevenueTransactions> revenueDetails, XSSFSheet sheet, Object[] tableHeadingRow, XSSFWorkbook workbook, PrintVO vo) {
		XSSFCellStyle tableBodyStyle = workbook.createCellStyle();
		XSSFCellStyle currencyStyle = (XSSFCellStyle) workbook.createCellStyle();
		currencyStyle.setAlignment(HorizontalAlignment.RIGHT);
		sheet.addMergedRegion(new CellRangeAddress(0, 0, 0, 1));
		int rowNumber = 0;
		int cellNumber = 0;
        rowNumber = prepareAwardGeneralInformationForExcel(sheet, rowNumber, vo.getAwardId().toString(), workbook, null,Boolean.FALSE);
		String heading = vo.getTabHeading();
		++rowNumber;
		workbook = prepareExcelSheetHeading(sheet, heading, workbook, tableHeadingRow.length, rowNumber);
		++rowNumber;
		workbook = prepareExcelSheetHeader(sheet, tableHeadingRow, workbook, tableBodyStyle, rowNumber++);
		for (AwardRevenueTransactions revenueDetail : revenueDetails) {
			Row row = sheet.createRow(rowNumber++);
			cellNumber = 0;

			Cell cell1 = assignCell(cellNumber, tableBodyStyle, row);
			if (revenueDetail.getInternalOrderCode() != null)
				cell1.setCellValue(revenueDetail.getInternalOrderCode());
			else
				cell1.setCellValue(" ");
			cellNumber++;

			Cell cell2 = assignCell(cellNumber, tableBodyStyle, row);
			if (revenueDetail.getFiGlAccount() != null)
				cell2.setCellValue(revenueDetail.getFiGlAccount());
			else
				cell2.setCellValue(" ");
			cellNumber++;

			Cell cell3 = assignCell(cellNumber, tableBodyStyle, row);
            if (revenueDetail.getPaymentDate() != null) {
                cell3.setCellValue(commonService.convertDateFormatBasedOnTimeZone(revenueDetail.getPaymentDate().getTime(), Constants.DEFAULT_DATE_FORMAT));
            } else
				cell3.setCellValue(" ");
			cellNumber++;

			Cell cell4 = assignCell(cellNumber, tableBodyStyle, row);
            if (revenueDetail.getPaymentFiscalYear() != null) {
                cell4.setCellValue(revenueDetail.getPaymentFiscalYear());
            } else
				cell4.setCellValue(" ");
			cellNumber++;

			Cell cell5 = assignCell(cellNumber, tableBodyStyle, row);
            if (revenueDetail.getPaymentDocNumber() != null) {
                cell5.setCellValue(revenueDetail.getPaymentDocNumber());
			} else
				cell5.setCellValue(" ");
			cellNumber++;

			Cell cell6 = assignCell(cellNumber, tableBodyStyle, row);
            if (revenueDetail.getDocumentDate() != null)
                cell6.setCellValue(commonService.convertDateFormatBasedOnTimeZone(revenueDetail.getDocumentDate().getTime(), Constants.DEFAULT_DATE_FORMAT));
            else
				cell6.setCellValue(" ");
			cellNumber++;

			Cell cell7 = assignCell(cellNumber, tableBodyStyle, row);
            if (revenueDetail.getTransactionReferenceNumber() != null) {
                cell7.setCellValue(revenueDetail.getTransactionReferenceNumber());
			} else
				cell7.setCellValue(" ");
			cellNumber++;

			Cell cell8 = assignCell(cellNumber, tableBodyStyle, row);
            if (revenueDetail.getFiPostingDate() != null) {
                cell8.setCellValue(commonService.convertDateFormatBasedOnTimeZone(revenueDetail.getFiPostingDate().getTime(), Constants.DEFAULT_DATE_FORMAT));
			} else
				cell8.setCellValue(" ");
			cellNumber++;

			Cell cell9 = assignCell(cellNumber, tableBodyStyle, row);
            if (revenueDetail.getGmiaDocnr() != null) {
                cell9.setCellValue(revenueDetail.getGmiaDocnr());
			} else
				cell9.setCellValue(" ");
			cellNumber++;

			Cell cell10 = assignCell(cellNumber, tableBodyStyle, row);
            cell8.setCellStyle(currencyStyle);
            if (revenueDetail.getAmountInFmaCurrency() != null) {
                cell10.setCellValue(
                        Constants.DOLLAR_SYMBOL + decimalFormat.format(revenueDetail.getAmountInFmaCurrency()));
			} else
				cell10.setCellValue(" ");
			cellNumber++;

            Cell cell11 = assignCell(cellNumber, tableBodyStyle, row);
            if (revenueDetail.getBpName() != null) {
                cell11.setCellValue(revenueDetail.getBpName());
            } else
                cell11.setCellValue(" ");
            cellNumber++;

            Cell cell12 = assignCell(cellNumber, tableBodyStyle, row);
            if (revenueDetail.getFiGlDescription() != null) {
                cell12.setCellValue(revenueDetail.getRemarks());
            } else
                cell12.setCellValue(" ");
            cellNumber++;
		}
	}

	public ByteArrayInputStream generatePDFReport(PrintVO vo) throws DocumentException, ParseException {
		Document document = new Document();
		document.setPageSize(PageSize.A4.rotate());
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
		subHeadFont.setSize(14);

		try {
			Paragraph paragraph = new Paragraph(vo.getDocumentHeading(), pdfTitleFont);
			paragraph.setAlignment(Paragraph.ALIGN_CENTER);
			document.add(paragraph);

			Award award = awardDao.fetchAwardByAwardId(vo.getAwardId().toString());
            prepareAwardGeneralInfo(document, subHeadFont, bodyFont, bodyTextFont, award, Boolean.FALSE, null, null);
			if (vo.getIsExpenseOrPurchase().equals("Y")) {
				prepareAwardExpenseTrackingDetail(document, subHeadFont, bodyFont, tableLabelFont, vo);
			}
			if (vo.getIsRevenue().equals("Y")) {
				prepareAwardRevenueDetail(document, subHeadFont, bodyFont, tableLabelFont, vo);
			}
			document.close();
		} catch (DocumentException ex) {
		}
		return new ByteArrayInputStream(out.toByteArray());
	}

    public void prepareAwardGeneralInfo(Document document, Font subHeadFont, Font bodyFont, Font bodyTextFont, Award award, Boolean isAwardTimesheetPrint, String awardPersonName, String awardId) throws DocumentException {
		PdfPTable awardOverviewHeading = new PdfPTable(1);
		awardOverviewHeading.setWidthPercentage(100);
        awardOverviewHeading.setWidths(new int[]{12});
		awardOverviewHeading.setSpacingBefore(10f);
        awardOverviewHeading.addCell(getSubHeadingCell("General Award Information", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, subHeadFont));
		document.add(awardOverviewHeading);

		PdfPTable generalDetailsTable = new PdfPTable(3);
		generalDetailsTable.setWidthPercentage(100);
        generalDetailsTable.setWidths(new int[]{2, 1, 9});
        generalDetailsTable.addCell(getGeneralDetailsCell("Award Title ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
		generalDetailsTable.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
        if (award == null) {
        	award = awardDao.fetchAwardByAwardId(awardId);
        }
		if (award.getTitle() != null) {
            generalDetailsTable.addCell(getGeneralDetailsCell(award.getTitle(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
		} else
            generalDetailsTable.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
        	generalDetailsTable.addCell(getGeneralDetailsCell("Start Date ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
		generalDetailsTable.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
		if (award.getBeginDate() != null) {
            generalDetailsTable.addCell(getGeneralDetailsCell(commonService.convertDateFormatBasedOnTimeZone(award.getBeginDate().getTime(), Constants.DEFAULT_DATE_FORMAT), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
		} else
            generalDetailsTable.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
        	generalDetailsTable.addCell(getGeneralDetailsCell("End Date ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
		generalDetailsTable.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
		if (award.getFinalExpirationDate() != null) {
            generalDetailsTable.addCell(getGeneralDetailsCell(commonService.convertDateFormatBasedOnTimeZone(award.getFinalExpirationDate().getTime(), Constants.DEFAULT_DATE_FORMAT), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
		} else
            generalDetailsTable.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
        	generalDetailsTable.addCell(getGeneralDetailsCell("Sponsor ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
		generalDetailsTable.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
        if (award.getSponsor() != null) {
            generalDetailsTable.addCell(getGeneralDetailsCell(commonService.getSponsorFormatBySponsorDetail(award.getSponsor().getSponsorCode(), award.getSponsor().getSponsorName(), award.getSponsor().getAcronym()),
					PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
		} else
            generalDetailsTable.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
        	generalDetailsTable.addCell(getGeneralDetailsCell("Lead Unit ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
		generalDetailsTable.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
		if (award.getLeadUnit().getUnitName() != null) {
            generalDetailsTable.addCell(getGeneralDetailsCell(award.getLeadUnit().getUnitName(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
		} else
            generalDetailsTable.addCell(getGeneralDetailsCell(" ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
        	generalDetailsTable.addCell(getGeneralDetailsCell("Sponsor Award Number", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
		generalDetailsTable.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
        	generalDetailsTable.addCell(getGeneralDetailsCell(award.getSponsorAwardNumber() != null ? award.getSponsorAwardNumber() : " ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
        	generalDetailsTable.addCell(getGeneralDetailsCell("Lead Principal Investigator", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
		generalDetailsTable.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
        	generalDetailsTable.addCell(getGeneralDetailsCell(award.getPrincipalInvestigator() != null ? award.getPrincipalInvestigator() : " ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
        if (Boolean.TRUE.equals(isAwardTimesheetPrint) && awardPersonName != null) {
        	 generalDetailsTable.addCell(getGeneralDetailsCell("Co-Investigator", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
             generalDetailsTable.addCell(getGeneralDetailsCell(" : ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyFont));
             generalDetailsTable.addCell(getGeneralDetailsCell(awardPersonName != null ? awardPersonName : " ", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, bodyTextFont));
        }
		document.add(generalDetailsTable);
	}

	private void prepareAwardExpenseTrackingDetail(Document document, Font subHeadFont, Font bodyFont, Font tableLabelFont, PrintVO vo) throws DocumentException {
		BigDecimal totalAmountInFmacurrency = BigDecimal.ZERO;
		PdfPTable revenueHeading = new PdfPTable(1);
		revenueHeading.setWidthPercentage(100);
        revenueHeading.setWidths(new int[]{12});
		revenueHeading.setSpacingBefore(20f);
		revenueHeading.addCell(
				getSubHeadingCell(vo.getTabHeading(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, subHeadFont));
		document.add(revenueHeading);
        String internalOrderCodes = (vo.getInternalOrderCodes() != null && !vo.getInternalOrderCodes().isEmpty()) ? String.join(",", vo.getInternalOrderCodes()) : null;
		List<AwardExpenseTransaction> expenseDetails = awardExpenseService.getExpenseTransactions(vo.getAccountNumber(),
				internalOrderCodes, vo.getActualOrCommittedFlag(), vo.getAwardNumber(), vo.getFmPostingStartDate(), vo.getFmPostingEndDate(), vo.getIsShowAllTransactions());
		List<AwardExpenseTransaction> sapTransactionDetails = expenseDetails.stream()
				.filter(expenseDetail -> expenseDetail.getIsFromSap().equals("Y")).collect(Collectors.toList());
		List<AwardExpenseTransaction> nonSapTransactionDetails = expenseDetails.stream()
				.filter(expenseDetail -> expenseDetail.getIsFromSap().equals("N")).collect(Collectors.toList());
		if (vo.getActualOrCommittedFlag().equals("A")) {
			PdfPTable awardActualExpenseHeading = new PdfPTable(10);
			awardActualExpenseHeading.setWidthPercentage(100);
			awardActualExpenseHeading.addCell(
					getTableHeadingCell("WBS Number (for OFIN use)", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
			awardActualExpenseHeading.addCell(
					getTableHeadingCell("GL Account", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
			awardActualExpenseHeading.addCell(getTableHeadingCell("Bank Clearing Date", PdfPCell.ALIGN_MIDDLE,
					PdfPCell.ALIGN_CENTER, tableLabelFont));
			awardActualExpenseHeading.addCell(
					getTableHeadingCell("Document Date", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
			awardActualExpenseHeading.addCell(getTableHeadingCell("Source Doc Ref", PdfPCell.ALIGN_MIDDLE,
					PdfPCell.ALIGN_CENTER, tableLabelFont));
			awardActualExpenseHeading.addCell(
					getTableHeadingCell("Posting Date", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
			awardActualExpenseHeading.addCell(getTableHeadingCell("System Doc Ref", PdfPCell.ALIGN_MIDDLE,
					PdfPCell.ALIGN_CENTER, tableLabelFont));
			awardActualExpenseHeading.addCell(getTableHeadingCell("Amount(" + Constants.DOLLAR_SYMBOL + ")",
					PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
			awardActualExpenseHeading.addCell(
					getTableHeadingCell("Vendor", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
			awardActualExpenseHeading.addCell(
					getTableHeadingCell("Description", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
			document.add(awardActualExpenseHeading);
			if (sapTransactionDetails != null && !sapTransactionDetails.isEmpty()) {
				//prepareSAPFieldDataForPDF(document, EXPENSE_FROM_SAP, bodyFont);
				PdfPTable awardActualSAPExpenseDetails = new PdfPTable(10);
				awardActualSAPExpenseDetails.setWidthPercentage(100);
				for (AwardExpenseTransaction sapTransactionDetail : sapTransactionDetails) {
					totalAmountInFmacurrency = totalAmountInFmacurrency.add(sapTransactionDetail.getAmountInFmacurrency());
					prepareActualTransactionPDFDetails(awardActualSAPExpenseDetails, sapTransactionDetail, bodyFont);
				}
				awardActualSAPExpenseDetails.addCell(
						getTableCell("", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
				awardActualSAPExpenseDetails.addCell(
						getTableCell("", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
				awardActualSAPExpenseDetails.addCell(
						getTableCell("", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
				awardActualSAPExpenseDetails.addCell(
						getTableCell("", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
				awardActualSAPExpenseDetails.addCell(
						getTableCell("", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
				awardActualSAPExpenseDetails.addCell(
						getTableCell("", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
				awardActualSAPExpenseDetails.addCell(
						getTableCell("Total", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
				awardActualSAPExpenseDetails.addCell(getTableCell(Constants.DOLLAR_SYMBOL + decimalFormat.format(totalAmountInFmacurrency),
						PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
				awardActualSAPExpenseDetails.addCell(
						getTableCell("", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
				awardActualSAPExpenseDetails.addCell(
						getTableCell("", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
				document.add(awardActualSAPExpenseDetails);
			}
			if (nonSapTransactionDetails != null && !nonSapTransactionDetails.isEmpty()) {
				//prepareSAPFieldDataForPDF(document, EXPENSE_FROM_NON_SAP, bodyFont);
				PdfPTable awardActualNonSAPExpenseDetails = new PdfPTable(10);
				awardActualNonSAPExpenseDetails.setWidthPercentage(100);
				for (AwardExpenseTransaction nonSapTransactionDetail : nonSapTransactionDetails) {
					prepareActualTransactionPDFDetails(awardActualNonSAPExpenseDetails, nonSapTransactionDetail,
							bodyFont);
				}
				document.add(awardActualNonSAPExpenseDetails);
			}
		} else if (vo.getActualOrCommittedFlag().equals("C")) {
			PdfPTable awardCommittedExpenseHeading = new PdfPTable(8);
			awardCommittedExpenseHeading.setWidthPercentage(100);
			awardCommittedExpenseHeading.addCell(
					getTableHeadingCell("WBS Number (for OFIN use)", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
			awardCommittedExpenseHeading.addCell(
					getTableHeadingCell("GL Account", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
			awardCommittedExpenseHeading.addCell(
					getTableHeadingCell("PO / PR Date", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
//			awardCommittedExpenseHeading.addCell(getTableHeadingCell("Source Doc Ref", PdfPCell.ALIGN_MIDDLE,
//					PdfPCell.ALIGN_CENTER, tableLabelFont));
			awardCommittedExpenseHeading.addCell(getTableHeadingCell("System Doc Ref", PdfPCell.ALIGN_MIDDLE,
					PdfPCell.ALIGN_CENTER, tableLabelFont));
			awardCommittedExpenseHeading.addCell(getTableHeadingCell("Amount(" + Constants.DOLLAR_SYMBOL + ")",
					PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
			awardCommittedExpenseHeading.addCell(
					getTableHeadingCell("Transaction Type", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
			awardCommittedExpenseHeading.addCell(
					getTableHeadingCell("Vendor", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
			awardCommittedExpenseHeading.addCell(
					getTableHeadingCell("Description", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
			document.add(awardCommittedExpenseHeading);
			if (sapTransactionDetails != null && !sapTransactionDetails.isEmpty()) {				
				//prepareSAPFieldDataForPDF(document, EXPENSE_FROM_SAP, bodyFont);
				PdfPTable awardCommittedExpenseDetails = new PdfPTable(8);
				awardCommittedExpenseDetails.setWidthPercentage(100);
				for (AwardExpenseTransaction sapTransactionDetail : sapTransactionDetails) {
					totalAmountInFmacurrency = totalAmountInFmacurrency.add(sapTransactionDetail.getAmountInFmacurrency());
					prepareCommittedTransactionPDFDetails(awardCommittedExpenseDetails, sapTransactionDetail, bodyFont);
				}
				awardCommittedExpenseDetails.addCell(
						getTableCell("", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
				awardCommittedExpenseDetails.addCell(
						getTableCell("", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
				awardCommittedExpenseDetails.addCell(
						getTableCell("", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
				awardCommittedExpenseDetails.addCell(
						getTableCell("Total", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
				awardCommittedExpenseDetails.addCell(getTableCell(Constants.DOLLAR_SYMBOL + decimalFormat.format(totalAmountInFmacurrency),
						PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
				awardCommittedExpenseDetails.addCell(
						getTableCell("", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
				awardCommittedExpenseDetails.addCell(
						getTableCell("", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
				awardCommittedExpenseDetails.addCell(
						getTableCell("", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
				document.add(awardCommittedExpenseDetails);
			}
			if (nonSapTransactionDetails != null && !nonSapTransactionDetails.isEmpty()) {
				//prepareSAPFieldDataForPDF(document, EXPENSE_FROM_NON_SAP, bodyFont);
				PdfPTable awardCommittedExpenseDetails = new PdfPTable(8);
				awardCommittedExpenseDetails.setWidthPercentage(100);
				for (AwardExpenseTransaction nonSapTransactionDetail : nonSapTransactionDetails) {
					prepareCommittedTransactionPDFDetails(awardCommittedExpenseDetails, nonSapTransactionDetail,
							bodyFont);
				}
				document.add(awardCommittedExpenseDetails);
			}
		}
	}

	private void prepareAwardRevenueDetail(Document document, Font subHeadFont, Font bodyFont, Font tableLabelFont, PrintVO vo) throws DocumentException {
		PdfPTable revenueHeading = new PdfPTable(1);
		revenueHeading.setWidthPercentage(100);
        revenueHeading.setWidths(new int[]{12});
		revenueHeading.setSpacingBefore(20f);
		revenueHeading.addCell(getSubHeadingCell(vo.getTabHeading(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, subHeadFont));
		document.add(revenueHeading);

		List<AwardRevenueTransactions> revenueDetails = awardRevenueDao.fetchRevenueTransactionsByParams(vo.getAwardNumber(), vo.getAccountNumber(), vo.getInternalOrderCodes(), vo.getFiPostingStartDate(), vo.getFiPostingEndDate());
        PdfPTable awardRevenueDetails = new PdfPTable(12);
		awardRevenueDetails.setWidthPercentage(100);
		awardRevenueDetails.addCell(
				getTableHeadingCell("WBS Number (for OFIN use)", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		awardRevenueDetails.addCell(
				getTableHeadingCell("GL Account", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		awardRevenueDetails.addCell(
				getTableHeadingCell("Bank Clearing Date", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		awardRevenueDetails.addCell(
                getTableHeadingCell("Fiscal Year", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
        awardRevenueDetails.addCell(
                getTableHeadingCell("Payment Doc Ref", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
        awardRevenueDetails.addCell(
				getTableHeadingCell("Document Date", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		awardRevenueDetails.addCell(
				getTableHeadingCell("Source Doc Ref", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		awardRevenueDetails.addCell(
				getTableHeadingCell("Posting Date", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		awardRevenueDetails.addCell(
				getTableHeadingCell("System Doc Ref", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		awardRevenueDetails.addCell(getTableHeadingCell("Amount(" + Constants.DOLLAR_SYMBOL + ")",
				PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		awardRevenueDetails
				.addCell(getTableHeadingCell("Customer", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		awardRevenueDetails.addCell(
				getTableHeadingCell("Description", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		if (revenueDetails != null && !revenueDetails.isEmpty()) {
			for (AwardRevenueTransactions revenueDetail : revenueDetails) {
				awardRevenueDetails.addCell(getTableCell(revenueDetail.getInternalOrderCode(), PdfPCell.ALIGN_MIDDLE,
						PdfPCell.ALIGN_CENTER, bodyFont));
				awardRevenueDetails.addCell(getTableCell(revenueDetail.getFiGlAccount(), PdfPCell.ALIGN_MIDDLE,
						PdfPCell.ALIGN_CENTER, bodyFont));
                awardRevenueDetails.addCell(getTableCell(revenueDetail.getPaymentDate() != null ? commonService.convertDateFormatBasedOnTimeZone(revenueDetail.getPaymentDate().getTime(), Constants.DEFAULT_DATE_FORMAT) : "",
                        PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
                awardRevenueDetails.addCell(getTableCell(revenueDetail.getPaymentFiscalYear() != null ? revenueDetail.getPaymentFiscalYear() : "",
                        PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
                awardRevenueDetails.addCell(getTableCell(revenueDetail.getPaymentDocNumber() != null ? revenueDetail.getPaymentDocNumber() : "",
                        PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
				awardRevenueDetails.addCell(getTableCell(revenueDetail.getDocumentDate() != null ? commonService.convertDateFormatBasedOnTimeZone(revenueDetail.getDocumentDate().getTime(), Constants.DEFAULT_DATE_FORMAT) : "", PdfPCell.ALIGN_MIDDLE,
						PdfPCell.ALIGN_CENTER, bodyFont));
				awardRevenueDetails.addCell(getTableCell(revenueDetail.getTransactionReferenceNumber(),
						PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
				awardRevenueDetails.addCell(getTableCell(revenueDetail.getFiPostingDate() != null ? commonService.convertDateFormatBasedOnTimeZone(revenueDetail.getFiPostingDate().getTime(), Constants.DEFAULT_DATE_FORMAT) : "", PdfPCell.ALIGN_MIDDLE,
						PdfPCell.ALIGN_CENTER, bodyFont));
				awardRevenueDetails.addCell(getTableCell(revenueDetail.getGmiaDocnr(), PdfPCell.ALIGN_MIDDLE,
						PdfPCell.ALIGN_CENTER, bodyFont));
				awardRevenueDetails.addCell(getTableCell(
						Constants.DOLLAR_SYMBOL + decimalFormat.format(revenueDetail.getAmountInFmaCurrency()),
						PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
				awardRevenueDetails.addCell(getTableCell(revenueDetail.getBpName(), PdfPCell.ALIGN_MIDDLE,
						PdfPCell.ALIGN_CENTER, bodyFont));
				awardRevenueDetails.addCell(getTableCell(revenueDetail.getRemarks(), PdfPCell.ALIGN_MIDDLE,
						PdfPCell.ALIGN_CENTER, bodyFont));
			}
		}
		document.add(awardRevenueDetails);
	}

	private void prepareExcelSheetForExpenseAndPurchaseDetails(List<AwardExpenseTransaction> expenseDetails, XSSFSheet sheet, Object[] tableHeadingRow, XSSFWorkbook workbook, PrintVO vo) throws DocumentException {
		XSSFCellStyle tableBodyStyle = workbook.createCellStyle();
		sheet.addMergedRegion(new CellRangeAddress(0, 0, 0, 1));
		BigDecimal totalAmountInFmacurrency = BigDecimal.ZERO;
		int rowNumber = 0;
		int cellNumber = 0;
        rowNumber = prepareAwardGeneralInformationForExcel(sheet, rowNumber, vo.getAwardId().toString(), workbook, null, Boolean.FALSE);
		String heading = vo.getTabHeading();
		++rowNumber;
		workbook = prepareExcelSheetHeading(sheet, heading, workbook, tableHeadingRow.length, rowNumber);
		++rowNumber;
		workbook = prepareExcelSheetHeader(sheet, tableHeadingRow, workbook, tableBodyStyle, rowNumber++);
		List<AwardExpenseTransaction> sapTransactionDetails = expenseDetails.stream()
				.filter(expenseDetail -> expenseDetail.getIsFromSap().equals("Y")).collect(Collectors.toList());
		List<AwardExpenseTransaction> nonSapTransactionDetails = expenseDetails.stream()
				.filter(expenseDetail -> expenseDetail.getIsFromSap().equals("N")).collect(Collectors.toList());
		if (vo.getActualOrCommittedFlag().equals("A")) {
			
			if (sapTransactionDetails != null && !sapTransactionDetails.isEmpty()) {
				/* workbook = prepareSAPFieldDataForExcel(sheet, EXPENSE_FROM_SAP, rowNumber++,
				   tableHeadingRow.length, workbook);*/
				for (AwardExpenseTransaction sapTransactionDetail : sapTransactionDetails) {
					totalAmountInFmacurrency = prepareActualTransactionDetails(sheet, rowNumber, cellNumber, tableBodyStyle, sapTransactionDetail, workbook, totalAmountInFmacurrency);
					rowNumber++;
				}
			}
			if (nonSapTransactionDetails != null && !nonSapTransactionDetails.isEmpty()) {
				/* workbook = prepareSAPFieldDataForExcel(sheet, EXPENSE_FROM_NON_SAP, rowNumber++, tableHeadingRow.length,
						workbook);*/
				for (AwardExpenseTransaction nonSapTransactionDetail : nonSapTransactionDetails) {
					totalAmountInFmacurrency = prepareActualTransactionDetails(sheet, rowNumber, cellNumber, tableBodyStyle, nonSapTransactionDetail, workbook, totalAmountInFmacurrency);
					rowNumber++;
				}
			}
		} else if (vo.getActualOrCommittedFlag().equals("C")) {
			if (sapTransactionDetails != null && !sapTransactionDetails.isEmpty()) {
				/*workbook = prepareSAPFieldDataForExcel(sheet, EXPENSE_FROM_SAP, rowNumber++, tableHeadingRow.length,
						workbook);*/
				for (AwardExpenseTransaction sapTransactionDetail : sapTransactionDetails) {
					totalAmountInFmacurrency = prepareCommittedTransactionDetails(sheet, rowNumber, cellNumber, tableBodyStyle, sapTransactionDetail, workbook, totalAmountInFmacurrency);
					rowNumber++;
				}
			}
			if (nonSapTransactionDetails != null && !nonSapTransactionDetails.isEmpty()) {
				/*workbook = prepareSAPFieldDataForExcel(sheet, EXPENSE_FROM_NON_SAP, rowNumber++, tableHeadingRow.length,
						workbook);*/
				for (AwardExpenseTransaction nonSapTransactionDetail : nonSapTransactionDetails) {
					totalAmountInFmacurrency = prepareCommittedTransactionDetails(sheet, rowNumber, cellNumber, tableBodyStyle, nonSapTransactionDetail, workbook, totalAmountInFmacurrency);
					rowNumber++;
				}
			}
		}
	}

	private void prepareActualTransactionPDFDetails(PdfPTable awardActualExpenseDetails, AwardExpenseTransaction expenseDetail, Font bodyFont) throws DocumentException {
		awardActualExpenseDetails.addCell(
				getTableCell(expenseDetail.getInternalOrderCode(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
		awardActualExpenseDetails.addCell(
				getTableCell(expenseDetail.getFiGlAccount(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
		awardActualExpenseDetails.addCell(getTableCell(
				expenseDetail.getBankClearingDate() != null ? commonService.convertDateFormatBasedOnTimeZone(
						expenseDetail.getBankClearingDate().getTime(), Constants.DEFAULT_DATE_FORMAT) : "",
				PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
		awardActualExpenseDetails.addCell(getTableCell(
				expenseDetail.getDocumentDate() != null ? commonService.convertDateFormatBasedOnTimeZone(
						expenseDetail.getDocumentDate().getTime(), Constants.DEFAULT_DATE_FORMAT) : "",
				PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
		awardActualExpenseDetails.addCell(getTableCell(expenseDetail.getTransactionReferenceNumber(),
				PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
		awardActualExpenseDetails.addCell(getTableCell(
				expenseDetail.getFmPostingDate() != null ? commonService.convertDateFormatBasedOnTimeZone(
						expenseDetail.getFmPostingDate().getTime(), Constants.DEFAULT_DATE_FORMAT) : "",
				PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
		awardActualExpenseDetails.addCell(getTableCell(expenseDetail.getDocumentNumber(), PdfPCell.ALIGN_MIDDLE,
				PdfPCell.ALIGN_CENTER, bodyFont));
		awardActualExpenseDetails.addCell(
				getTableCell(Constants.DOLLAR_SYMBOL + decimalFormat.format(expenseDetail.getAmountInFmacurrency()),
						PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
		awardActualExpenseDetails
				.addCell(getTableCell(expenseDetail.getVendorCode() + " - " + expenseDetail.getVendorName(),
						PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
		awardActualExpenseDetails.addCell(getTableCell(expenseDetail.getRemarks(), PdfPCell.ALIGN_MIDDLE,
				PdfPCell.ALIGN_CENTER, bodyFont));
	}

	private void prepareCommittedTransactionPDFDetails(PdfPTable awardCommittedExpenseDetails, AwardExpenseTransaction expenseDetail, Font bodyFont) throws DocumentException {
		awardCommittedExpenseDetails.addCell(
				getTableCell(expenseDetail.getInternalOrderCode(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
		awardCommittedExpenseDetails.addCell(
				getTableCell(expenseDetail.getFiGlAccount(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
		awardCommittedExpenseDetails.addCell(getTableCell(expenseDetail.getPoPrFlag() != null &&
				expenseDetail.getPoPrFlag().equals("PR") ? 
                        (expenseDetail.getPrDate() != null ? commonService.convertDateFormatBasedOnTimeZone(
                                expenseDetail.getPrDate().getTime(), Constants.DEFAULT_DATE_FORMAT) : "") :
                        (expenseDetail.getPoDate() != null ? commonService.convertDateFormatBasedOnTimeZone(
                                expenseDetail.getPoDate().getTime(), Constants.DEFAULT_DATE_FORMAT) : ""),
				PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
//		awardCommittedExpenseDetails.addCell(getTableCell(expenseDetail.getInvoiceNumber(),
//				PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
		awardCommittedExpenseDetails.addCell(getTableCell(expenseDetail.getDocumentNumber(),
				PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
		awardCommittedExpenseDetails.addCell(
				getTableCell(Constants.DOLLAR_SYMBOL + decimalFormat.format(expenseDetail.getAmountInFmacurrency()),
						PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
		awardCommittedExpenseDetails.addCell(
				getTableCell(expenseDetail.getPoPrFlag(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
		awardCommittedExpenseDetails
				.addCell(getTableCell(expenseDetail.getVendorCode() + " - " + expenseDetail.getVendorName(),
						PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
		awardCommittedExpenseDetails.addCell(getTableCell(expenseDetail.getRemarks(), PdfPCell.ALIGN_MIDDLE,
				PdfPCell.ALIGN_CENTER, bodyFont));
	}

	private BigDecimal prepareActualTransactionDetails(XSSFSheet sheet, int rowNumber, int cellNumber, XSSFCellStyle tableBodyStyle, AwardExpenseTransaction expenseDetail, XSSFWorkbook workbook, BigDecimal totalAmountInFmacurrency) {
        XSSFCellStyle currencyStyle = (XSSFCellStyle) workbook.createCellStyle();
		currencyStyle.setAlignment(HorizontalAlignment.RIGHT);
		
		Row row = sheet.createRow(rowNumber++);
		cellNumber = 0;
		
		Cell cell1 = assignCell(cellNumber, tableBodyStyle, row);
		if (expenseDetail.getInternalOrderCode() != null)
			cell1.setCellValue(expenseDetail.getInternalOrderCode());
		else
			cell1.setCellValue(" ");
		cellNumber++;

		Cell cell2 = assignCell(cellNumber, tableBodyStyle, row);
		if (expenseDetail.getFiGlAccount() != null)
			cell2.setCellValue(expenseDetail.getFiGlAccount());
		else
			cell2.setCellValue(" ");
		cellNumber++;

		Cell cell3 = assignCell(cellNumber, tableBodyStyle, row);
		if (expenseDetail.getBankClearingDate() != null)
			cell3.setCellValue(commonService.convertDateFormatBasedOnTimeZone(
					expenseDetail.getBankClearingDate().getTime(), Constants.DEFAULT_DATE_FORMAT));
		else
			cell3.setCellValue(" ");
		cellNumber++;

		Cell cell4 = assignCell(cellNumber, tableBodyStyle, row);
		if (expenseDetail.getDocumentDate() != null)
			cell4.setCellValue(commonService.convertDateFormatBasedOnTimeZone(expenseDetail.getDocumentDate().getTime(),
					Constants.DEFAULT_DATE_FORMAT));
		else
			cell4.setCellValue(" ");
		cellNumber++;

		Cell cell5 = assignCell(cellNumber, tableBodyStyle, row);
		if (expenseDetail.getTransactionReferenceNumber() != null) {
			cell5.setCellValue(expenseDetail.getTransactionReferenceNumber());
		} else
			cell5.setCellValue(" ");
		cellNumber++;

		Cell cell6 = assignCell(cellNumber, tableBodyStyle, row);
		if (expenseDetail.getFmPostingDate() != null) {
			cell6.setCellValue(commonService.convertDateFormatBasedOnTimeZone(
					expenseDetail.getFmPostingDate().getTime(), Constants.DEFAULT_DATE_FORMAT));
		} else
			cell6.setCellValue(" ");
		cellNumber++;

		Cell cell7 = assignCell(cellNumber, tableBodyStyle, row);
		if (expenseDetail.getDocumentNumber() != null) {
			cell7.setCellValue(expenseDetail.getDocumentNumber());
		} else
			cell7.setCellValue(" ");
		cellNumber++;

		Cell cell8 = assignCell(cellNumber, tableBodyStyle, row);
		cell8.setCellStyle(currencyStyle);
		if (expenseDetail.getAmountInFmacurrency() != null) {
			totalAmountInFmacurrency = totalAmountInFmacurrency.add(expenseDetail.getAmountInFmacurrency());
			cell8.setCellValue(Constants.DOLLAR_SYMBOL + decimalFormat.format(expenseDetail.getAmountInFmacurrency()));
		} else
			cell8.setCellValue(" ");
		cellNumber++;

		Cell cell9 = assignCell(cellNumber, tableBodyStyle, row);
		if (expenseDetail.getVendorCode() != null || expenseDetail.getVendorName() != null) {
			cell9.setCellValue(expenseDetail.getVendorCode() + " - " + expenseDetail.getVendorName());
		} else
			cell9.setCellValue(" ");
		cellNumber++;

		Cell cell10 = assignCell(cellNumber, tableBodyStyle, row);
		if (expenseDetail.getRemarks() != null) {
			cell10.setCellValue(expenseDetail.getRemarks());
		} else
			cell10.setCellValue(" ");
		cellNumber++;
		
		cellNumber = 6;
		Row totalRow = sheet.createRow(rowNumber++);
		Cell cell11 = assignCell(cellNumber, tableBodyStyle, totalRow);
		cell11.setCellValue("TOTAL");
		cellNumber++;

		Cell cell12 = assignCell(cellNumber, tableBodyStyle, totalRow);
		cell12.setCellStyle(currencyStyle);
		cell12.setCellValue(Constants.DOLLAR_SYMBOL + decimalFormat.format(totalAmountInFmacurrency));
		cellNumber++;
		
		return totalAmountInFmacurrency;
	}

	private BigDecimal prepareCommittedTransactionDetails(XSSFSheet sheet, int rowNumber, int cellNumber, XSSFCellStyle tableBodyStyle, AwardExpenseTransaction expenseDetail, XSSFWorkbook workbook, BigDecimal totalAmountInFmacurrency) {
        XSSFCellStyle currencyStyle = (XSSFCellStyle) workbook.createCellStyle();
		currencyStyle.setAlignment(HorizontalAlignment.RIGHT);

		Row row = sheet.createRow(rowNumber++);
		cellNumber = 0;
		
		Cell cell1 = assignCell(cellNumber, tableBodyStyle, row);
		if (expenseDetail.getInternalOrderCode() != null)
			cell1.setCellValue(expenseDetail.getInternalOrderCode());
		else
			cell1.setCellValue(" ");
		cellNumber++;

		Cell cell2 = assignCell(cellNumber, tableBodyStyle, row);
		if (expenseDetail.getFiGlAccount() != null)
			cell2.setCellValue(expenseDetail.getFiGlAccount());
		else
			cell2.setCellValue(" ");
		cellNumber++;

		Cell cell3 = assignCell(cellNumber, tableBodyStyle, row);
		if (expenseDetail.getPoPrFlag() != null) {
			if (expenseDetail.getPoPrFlag().equals("PR"))
				cell3.setCellValue(expenseDetail.getPrDate() != null ? commonService.convertDateFormatBasedOnTimeZone(expenseDetail.getPrDate().getTime(),
						Constants.DEFAULT_DATE_FORMAT) : "");
			else
				cell3.setCellValue(expenseDetail.getPoDate() != null ? commonService.convertDateFormatBasedOnTimeZone(expenseDetail.getPoDate().getTime(),
						Constants.DEFAULT_DATE_FORMAT) : "");
        } else
			cell3.setCellValue(" ");
		cellNumber++;

		Cell cell4 = assignCell(cellNumber, tableBodyStyle, row);
//		if (expenseDetail.getInvoiceNumber() != null) {
//			cell4.setCellValue(expenseDetail.getInvoiceNumber());
		if (expenseDetail.getDocumentNumber() != null) {
			cell4.setCellValue(expenseDetail.getDocumentNumber());
        } else
			cell4.setCellValue(" ");
		cellNumber++;

		Cell cell5 = assignCell(cellNumber, tableBodyStyle, row);
		cell5.setCellStyle(currencyStyle);
		if (expenseDetail.getAmountInFmacurrency() != null) {
			totalAmountInFmacurrency = totalAmountInFmacurrency.add(expenseDetail.getAmountInFmacurrency());
			cell5.setCellValue(Constants.DOLLAR_SYMBOL + decimalFormat.format(expenseDetail.getAmountInFmacurrency()));
        } else
			cell5.setCellValue(" ");
		cellNumber++;

		Cell cell6 = assignCell(cellNumber, tableBodyStyle, row);
		if (expenseDetail.getPoPrFlag() != null) {
			cell6.setCellValue(expenseDetail.getPoPrFlag());
        } else
			cell6.setCellValue(" ");
		cellNumber++;

		Cell cell7 = assignCell(cellNumber, tableBodyStyle, row);
		if (expenseDetail.getVendorCode() != null || expenseDetail.getVendorName() != null) {
            cell7.setCellValue(expenseDetail.getVendorCode() + " - " + expenseDetail.getVendorName());
        } else
			cell7.setCellValue(" ");
		cellNumber++;

		Cell cell8 = assignCell(cellNumber, tableBodyStyle, row);
		if (expenseDetail.getRemarks() != null) {
			cell8.setCellValue(expenseDetail.getRemarks());
        } else
			cell8.setCellValue(" ");
		cellNumber++;
		
		
		cellNumber = 3;
		Row totalRow = sheet.createRow(rowNumber++);
		Cell cell9 = assignCell(cellNumber, tableBodyStyle, totalRow);
		cell9.setCellValue("TOTAL");
		cellNumber++;

		Cell cell10 = assignCell(cellNumber, tableBodyStyle, totalRow);
		cell10.setCellStyle(currencyStyle);
		cell10.setCellValue(Constants.DOLLAR_SYMBOL + decimalFormat.format(totalAmountInFmacurrency));
		cellNumber++;
		
		return totalAmountInFmacurrency;
	}

	private void prepareKeyPersonnelDetails(Document document, Font subHeadFont, Font bodyFont, Font tableLabelFont, List<AwardPerson> awardPersons) throws DocumentException {
		PdfPTable awardPersonHeading = new PdfPTable(1);
		awardPersonHeading.setWidthPercentage(100);
		awardPersonHeading.setSpacingBefore(10f);
        awardPersonHeading.addCell(getSubHeadingCell("Key Personnel", PdfPCell.ALIGN_MIDDLE,
				PdfPCell.ALIGN_LEFT, subHeadFont));
		document.add(awardPersonHeading);

		PdfPTable personnelDetails = new PdfPTable(3);
		personnelDetails.setWidthPercentage(100);
		personnelDetails.addCell(
				getTableHeadingCell("Name", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		personnelDetails.addCell(
				getTableHeadingCell("Role in Project", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		personnelDetails.addCell(
				getTableHeadingCell("Unit", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		for (AwardPerson awardPerson : awardPersons) {
			List<AwardPersonUnit> personUnits = awardPerson.getAwardPersonUnits();
			personnelDetails.addCell(getTableCell(awardPerson.getFullName(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
			personnelDetails.addCell(getTableCell(awardPerson.getProposalPersonRole().getDescription(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
			if (personUnits != null && !personUnits.isEmpty()) {
				personnelDetails.addCell(getTableCell(personUnits.get(0).getUnit().getUnitName(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
			} else {
				personnelDetails.addCell(getTableCell(awardPerson.getDepartment(), PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
			}	
		}
		document.add(personnelDetails);
	}

	public void addDetailsInHeader(XSSFWorkbook workbook, XSSFSheet sheet) {
		try {
			  Header header;
			  InputStream is;
			  byte[] bytes;
			  int pictureIdx;
			  header = sheet.getHeader();
			  header.setLeft("&G");
			  Resource resource = new ClassPathResource("logo_ntu.png");
			  is = resource.getInputStream();
			  bytes = IOUtils.toByteArray(is);
			  pictureIdx = workbook.addPicture(bytes, Workbook.PICTURE_TYPE_PNG);
			  is.close();
			  sheet.setMargin(org.apache.poi.ss.usermodel.Sheet.TopMargin, 1);
			  //create header picture from picture data of this workbook
			  createPictureForHeader(sheet, pictureIdx, "logo", 1, "LH");
			} catch (Exception e) {
				logger.info("Error Occured in createPictureForHeader : {}", e.getMessage());
			}
		}

	private void createPictureForHeader(XSSFSheet sheet, int pictureIdx, String pictureTitle, int vmlIdx, String headerPos) {
		try {
			OPCPackage opcpackage = sheet.getWorkbook().getPackage();
			// creating /xl/drawings/vmlDrawing1.vml
			PackagePartName partname = PackagingURIHelper.createPartName("/xl/drawings/vmlDrawing" + vmlIdx + ".vml");
			PackagePart part = opcpackage.createPart(partname, "application/vnd.openxmlformats-officedocument.vmlDrawing");
			// creating new VmlDrawing
			VmlDrawing vmldrawing = new VmlDrawing(part);
			// creating the relation to the picture in
			// /xl/drawings/_rels/vmlDrawing1.vml.rels
			XSSFPictureData picData = sheet.getWorkbook().getAllPictures().get(pictureIdx);
			String rIdPic = vmldrawing.addRelation(null, XSSFRelation.IMAGES, picData).getRelationship().getId();
			// get image dimension
			ByteArrayInputStream is = new ByteArrayInputStream(picData.getData());
			// setting the image width = 3cm and height = 1.5 cm in pixels
			java.awt.Dimension imageDimension = new java.awt.Dimension(162, 56);
			is.close();
			// updating the VmlDrawing
			vmldrawing.setRelationIdPic(rIdPic);
			vmldrawing.setPictureTitle(pictureTitle);
			vmldrawing.setImageDimension(imageDimension);
			vmldrawing.setHeaderPosition(headerPos);
			// creating the relation to /xl/drawings/vmlDrawing1.xml in
			String rIdExtLink = sheet.addRelation(null, XSSFRelation.VML_DRAWINGS, vmldrawing).getRelationship().getId();
			sheet.getCTWorksheet().addNewLegacyDrawingHF().setId(rIdExtLink);
		} catch (Exception e) {
			logger.info("Error Occured in createPictureForHeader : {}", e.getMessage());
		}
	}

	private PdfPTable prepareNumberOfColumns(int numberOfColumns, Boolean isShowInKind, Boolean isShowCostShareUnderRecovery, Boolean isShowModifiedCost) {
		PdfPTable budgetPeriodsAndTotal = new PdfPTable(numberOfColumns);
		if ((Boolean.FALSE.equals(isShowCostShareUnderRecovery)) && (Boolean.FALSE.equals(isShowInKind)) && (Boolean.FALSE.equals(isShowModifiedCost))) {
            budgetPeriodsAndTotal = new PdfPTable(numberOfColumns - 4);
		} else if (Boolean.FALSE.equals(isShowCostShareUnderRecovery)) {
			if (Boolean.TRUE.equals(isShowInKind) && Boolean.TRUE.equals(isShowModifiedCost)) {
                budgetPeriodsAndTotal = new PdfPTable(numberOfColumns - 2);
			} else if (Boolean.TRUE.equals(isShowInKind) && Boolean.FALSE.equals(isShowModifiedCost)) {
                budgetPeriodsAndTotal = new PdfPTable(numberOfColumns - 3);
			} else if (Boolean.FALSE.equals(isShowInKind) && Boolean.TRUE.equals(isShowModifiedCost)) {
                budgetPeriodsAndTotal = new PdfPTable(numberOfColumns - 3);
			}
		} else if (Boolean.FALSE.equals(isShowInKind)) {
			if (Boolean.TRUE.equals(isShowCostShareUnderRecovery) && Boolean.TRUE.equals(isShowModifiedCost)) {
                budgetPeriodsAndTotal = new PdfPTable(numberOfColumns - 1);
			} else if (Boolean.TRUE.equals(isShowCostShareUnderRecovery) && Boolean.FALSE.equals(isShowModifiedCost)) {
                budgetPeriodsAndTotal = new PdfPTable(numberOfColumns - 2);
			} else if (Boolean.FALSE.equals(isShowCostShareUnderRecovery) && Boolean.TRUE.equals(isShowModifiedCost)) {
                budgetPeriodsAndTotal = new PdfPTable(numberOfColumns - 3);
			}
		} else if (Boolean.FALSE.equals(isShowModifiedCost)) {
			if (Boolean.TRUE.equals(isShowCostShareUnderRecovery) && Boolean.TRUE.equals(isShowInKind)) {
                budgetPeriodsAndTotal = new PdfPTable(numberOfColumns - 1);
			} else if (Boolean.TRUE.equals(isShowCostShareUnderRecovery) && Boolean.FALSE.equals(isShowInKind)) {
                budgetPeriodsAndTotal = new PdfPTable(numberOfColumns - 2);
			} else if (Boolean.FALSE.equals(isShowCostShareUnderRecovery) && Boolean.TRUE.equals(isShowInKind)) {
                budgetPeriodsAndTotal = new PdfPTable(numberOfColumns - 3);
			}
		}
		return budgetPeriodsAndTotal;
	}

	private IContext setPRPlaceHolderData(IContext context, Award award, AwardProgressReport progressReport) {
		DecimalFormat decimalFormat = new DecimalFormat(Constants.SINGAPORE_NUMBER_FORMAT_WITH_DECIMAL);
		String grantCallTitle = null;
		if (award.getGrantHeaderId() != null) {
			grantCallTitle = grantCallDao.getGrantCallTitleByGrantId(award.getGrantHeaderId());
		}
		context.put("PROGRESS_REPORT_NUMBER", progressReport.getProgressReportNumber() == null ? "" : progressReport.getProgressReportNumber());
		context.put("ACCOUNT_NUMBER", award.getAccountNumber() == null ? "" : award.getAccountNumber());
		AccountType accountType = null;
		if (award.getAccountTypeCode() != null) {
			accountType = commonDao.fetchAccountTypeByAccountTypeCode(Integer.parseInt(award.getAccountTypeCode()));
		}
		context.put("ACCOUNT_TYPE", accountType == null ? "" : accountType.getDescription());
		AwardType awardType = null;
		if (award.getAwardTypeCode() != null) {
			awardType = awardDao.fetchAwardTypeByAwardTypeCode(award.getAwardTypeCode());
		}
		context.put(AWARD_TYPE, awardType == null ? "" : awardType.getDescription());
		ActivityType activityType = null;
		if (award.getActivityTypeCode() != null) {
			activityType = commonDao.fetchActivityTypeByActivityTypeCode(award.getActivityTypeCode());
		}
		context.put(ACTIVITY_TYPE, activityType == null ? "" : activityType.getDescription());
		context.put("AWARD_STATUS",
				award.getAwardStatus().getDescription() == null ? "" : award.getAwardStatus().getDescription());
		context.put("AWARD_TITLE", award.getTitle() == null ? "" : award.getTitle());
		context.put(LEAD_UNIT, award.getLeadUnit().getUnitName() == null ? "" : commonService.getUnitFormatByUnitDetail(award.getLeadUnit().getUnitNumber(),
				award.getLeadUnit().getUnitName()));
		context.put("SPONSOR_NAME",
                award.getSponsor() == null ? "" : commonService.getSponsorFormatBySponsorDetail(award.getSponsor().getSponsorCode(), award.getSponsor().getSponsorName(), award.getSponsor().getAcronym()));
		context.put("PRIME_SPONSOR_NAME",
                award.getPrimeSponsor() == null ? "" : commonService.getSponsorFormatBySponsorDetail(award.getPrimeSponsor().getSponsorCode(), award.getPrimeSponsor().getSponsorName(), award.getPrimeSponsor().getAcronym()));
		context.put("PROJECT_START_DATE",
				award.getBeginDate() == null ? ""
						: commonService.convertDateFormatBasedOnTimeZone(award.getBeginDate().getTime(),
								Constants.DEFAULT_DATE_FORMAT));
		context.put("PROJECT_END_DATE",
				award.getFinalExpirationDate() == null ? ""
						: commonService.convertDateFormatBasedOnTimeZone(award.getFinalExpirationDate().getTime(),
								Constants.DEFAULT_DATE_FORMAT));
		context.put("AWARD_EFFECTIVE_DATE",
				award.getAwardEffectiveDate() == null ? ""
						: commonService.convertDateFormatBasedOnTimeZone(award.getAwardEffectiveDate().getTime(),
								Constants.DEFAULT_DATE_FORMAT));
		setAwardKeywords(award, context);
		Double totalSubContractAmount = calculateTotalSubContractAmount(award.getAwardId());
		setTotalCommitmentAndCostShareAmount(award.getAwardId(), context);
		context.put("TOTAL_SUBCONTRACT_AMOUNT",
				Constants.DOLLAR_SYMBOL + decimalFormat.format(totalSubContractAmount).toString());
		context.put("AWARD_DURATION", award.getDuration());
		context.put(AWARD_ID, award.getAwardId().toString());
		context.put(AWARD_NUMBER, award.getAwardNumber());
		context.put("LEAD_PI", award.getPrincipalInvestigator() != null ? award.getPrincipalInvestigator() : "");
		context.put(RESEARCH_DESCRIPTION,
				award.getResearchDescription() == null ? "" : award.getResearchDescription());
		context.put(MULTI_DISCIPLINARY_DESCRIPTION,
				award.getMultiDisciplinaryDescription() == null ? "" : award.getMultiDisciplinaryDescription());
		context.put("SPONSOR_AWARD_NUMBER", award.getSponsorAwardNumber() != null ? award.getSponsorAwardNumber() : "");
		context.put("GRANT_CALL_TITLE", grantCallTitle != null ? grantCallTitle : "");		
		return context;
	}

	@Override
	public ResponseEntity<byte[]> generateAwardKeyPersonTimesheetReport(HttpServletResponse response, CommonVO vo) {
		ResponseEntity<byte[]> attachmentData = null;
		try {
        	TimesheetVO timesheetVO = new TimesheetVO();
        	timesheetVO.setAwardId(Integer.parseInt(vo.getAwardId()));
        	timesheetVO.setAwardPersonId(vo.getAwardPersonId());
        	timesheetVO = timesheetService.prepareKeyPersonTimesheetData(timesheetVO, timesheetVO.getAwardPersonId(), timesheetVO.getAwardId());
            Map<String, List<AwardKeyPersonTimesheet>> awardKeyPersonTimesheetDetails = timesheetVO.getAwardKeyPersonTimesheetDetails();
            if (vo.getExportType().equals("pdf")) {
                attachmentData = generateAwardKeyPersonTimesheetPDFReport(timesheetVO.getAwardKeyPersonTimesheetType(), awardKeyPersonTimesheetDetails, response, attachmentData, vo);
			} else {
                attachmentData = generateAwardKeyPersonTimesheetExcelReport(timesheetVO.getAwardKeyPersonTimesheetType(), awardKeyPersonTimesheetDetails, attachmentData, vo);
			}
		} catch (Exception e) {
			logger.error("Exception in generateAwardKeyPersonTimesheetReport : {} ", e.getMessage());
		}
		return attachmentData;
	}

    private ResponseEntity<byte[]> generateAwardKeyPersonTimesheetExcelReport(String timesheetType, Map<String, List<AwardKeyPersonTimesheet>> awardKeyPersonTimesheetDetails, ResponseEntity<byte[]> attachmentData, CommonVO vo) {
		try {
		XSSFWorkbook workbook = new XSSFWorkbook();
		XSSFSheet sheet = workbook.createSheet("Award Key Person Timesheet");
            commonService.addDetailsInHeader(workbook, sheet);
            prepareExcelSheetForAwardKeyPersonTimesheet(timesheetType, awardKeyPersonTimesheetDetails, sheet, workbook, Integer.parseInt(vo.getAwardId()), vo.getPersonId());
		ByteArrayOutputStream bos = new ByteArrayOutputStream();
		workbook.write(bos);
		attachmentData = getResponseEntityForExcelDownload(bos.toByteArray());
        } catch (Exception e) {
			logger.error("error in generateAwardKeyPersonTimesheetExcelReport {}", e.getMessage());
		}
		return attachmentData;
	}

    private ResponseEntity<byte[]> generateAwardKeyPersonTimesheetPDFReport(String timesheetType, Map<String, List<AwardKeyPersonTimesheet>> awardKeyPersonTimesheetDetails, HttpServletResponse response, ResponseEntity<byte[]> attachmentData, CommonVO vo) {
		try {
			ByteArrayInputStream bis = generateTimesheetPDFReport(timesheetType, awardKeyPersonTimesheetDetails, vo);
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
			logger.error("Exception in generateAwardExpensePDFReport : {}", e.getMessage());
		}
		return attachmentData;
	}
 
   public ByteArrayInputStream generateTimesheetPDFReport(String timesheetType, Map<String, List<AwardKeyPersonTimesheet>> awardKeyPersonTimesheetDetails, CommonVO vo) throws DocumentException, ParseException {
	   Document document = new Document();
		document.setPageSize(PageSize.A4.rotate());
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
		subHeadFont.setSize(14);

		try {
			Paragraph paragraph = new Paragraph(vo.getDocumentHeading(), pdfTitleFont);
			paragraph.setAlignment(Paragraph.ALIGN_CENTER);
			document.add(paragraph);
			Award award = awardDao.fetchAwardByAwardId(vo.getAwardId());
            String awardPersonName = null;
            if (award.getAwardPersons() != null && !award.getAwardPersons().isEmpty()) {
				for (AwardPerson awardPerson : award.getAwardPersons()) {
    				if (awardPerson.getPersonId() != null && vo.getPersonId() != null && vo.getPersonId().equals(awardPerson.getPersonId()) && awardPerson.getPersonRoleId().equals(Constants.COI_ROLE_CODE)) {
        				awardPersonName = awardPerson.getFullName();
    				}
    			}
			}
            prepareAwardGeneralInfo(document, subHeadFont, bodyFont, bodyTextFont, award, Boolean.TRUE, awardPersonName, null);
			prepareAwardKeyPersonTimesheetDetail(timesheetType, awardKeyPersonTimesheetDetails, document, subHeadFont, bodyFont, tableLabelFont);
			document.close();
		} catch (DocumentException ex) {
		}
		return new ByteArrayInputStream(out.toByteArray());
   }

	private void prepareAwardKeyPersonTimesheetDetail(String timesheetType, Map<String, List<AwardKeyPersonTimesheet>> awardKeyPersonTimesheetDetails, Document document, Font subHeadFont, Font bodyFont,
		Font tableLabelFont) {
		try {
		PdfPTable timesheetHeading = new PdfPTable(1);
		timesheetHeading.setWidthPercentage(100);
            timesheetHeading.setWidths(new int[]{12});
		timesheetHeading.setSpacingBefore(20f);
            timesheetHeading.addCell(getSubHeadingCell("Award Key Person Timesheet Detail", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_LEFT, subHeadFont));
		document.add(timesheetHeading);
		String year = null;
		PdfPTable quarterlyData = null;
		PdfPTable tableValues = null; 
		if (timesheetType != null && timesheetType.equals("QUARTERLY")) {
			quarterlyData = setPdfTableForQuarterly(tableLabelFont);
		} else if (timesheetType != null && timesheetType.equals("YEARLY")) {
			quarterlyData = setPdfTableForYearly(tableLabelFont);
		} else if (timesheetType != null && timesheetType.equals("HALFYEARLY")) {
			quarterlyData = setPdfTableForHalYearly(tableLabelFont);
		}
		document.add(quarterlyData);
		for (Map.Entry<String, List<AwardKeyPersonTimesheet>> entry : awardKeyPersonTimesheetDetails.entrySet()) {
			year = entry.getKey();
			List<Object> values = new ArrayList<>();
                Object[] arrayValue = entry.getValue().toArray();
			for (Object awardKeyPerson : arrayValue) {
				AwardKeyPersonTimesheet awardKeyPersonTimesheet = (AwardKeyPersonTimesheet) awardKeyPerson;
				values.add(awardKeyPersonTimesheet.getOrderNumber() + "-" + awardKeyPersonTimesheet.getValue());
			}
			 tableValues = prepareAwardTimesheetDetailForPDF(tableValues, quarterlyData, bodyFont, timesheetType, year, values.toArray());
			 document.add(tableValues);
		   }
        } catch (Exception e) {
            logger.error("prepareAwardKeyPersonTimesheetDetail {}", e.getMessage());
		}
    }

	private PdfPTable prepareAwardTimesheetDetailForPDF(PdfPTable tableValues, PdfPTable quarterlyData, Font bodyFont, String timesheetType, String year, Object[] value) throws DocumentException {
		String firstValue = null;
		String secondValue = null;
		String thirdValue = null;
		String fourthValue = null;
		for (int i = 0; i < value.length; i++) {
			String orderValue = value[i].toString().split("-")[0];
			String cellValue = value[i].toString().split("-")[1];
			if (orderValue.equals("1")) {
				firstValue = cellValue;
			} else if (orderValue.equals("2")) {
				secondValue = cellValue;
			} else if (orderValue.equals("3")) {
				thirdValue = cellValue;
			} else if (orderValue.equals("4")) {
				fourthValue = cellValue;
			}
		}
		if (timesheetType.equals("QUARTERLY")) {
				tableValues = new PdfPTable(5);
				tableValues.setWidthPercentage(100);
				tableValues.addCell(getTableCell(year != null ? year : "", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
				if (firstValue != null) {
					tableValues.addCell(getTableCell(firstValue, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
				} else {
					tableValues.addCell(getTableCell("", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
				}
            if (secondValue != null) {
					tableValues.addCell(getTableCell(secondValue, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
				} else {
					tableValues.addCell(getTableCell("", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
				}
            if (thirdValue != null) {
					tableValues.addCell(getTableCell(thirdValue, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
				} else {
					tableValues.addCell(getTableCell("", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
				}

            if (fourthValue != null) {
                tableValues.addCell(getTableCell(fourthValue, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
				} else {
					tableValues.addCell(getTableCell("", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
				}
			} else if (timesheetType.equals("YEARLY")) {
				tableValues = new PdfPTable(2);
				tableValues.setWidthPercentage(100);
				tableValues.addCell(getTableCell(year != null ? year : "", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
				if (firstValue != null) {
					tableValues.addCell(getTableCell(firstValue, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
				} else {
					tableValues.addCell(getTableCell("", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
				}
			} else if (timesheetType.equals("HALFYEARLY")) {
				tableValues = new PdfPTable(3);
				tableValues.setWidthPercentage(100);
				tableValues.addCell(getTableCell(year != null ? year : "", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
				if (firstValue != null) {
					tableValues.addCell(getTableCell(firstValue, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
				} else {
					tableValues.addCell(getTableCell("", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
				}
				if (secondValue != null) {
					tableValues.addCell(getTableCell(secondValue, PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
				} else {
					tableValues.addCell(getTableCell("", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, bodyFont));
				}
			}
		return tableValues;
	}

	private PdfPTable setPdfTableForHalYearly(Font tableLabelFont) throws DocumentException {
		PdfPTable quarterlyData = new PdfPTable(3);
		quarterlyData.setWidthPercentage(100);
		quarterlyData.addCell(
				getTableHeadingCell("Year", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		quarterlyData.addCell(
                getTableHeadingCell("1st Half (Jan - Jun) (%)", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		quarterlyData.addCell(
                getTableHeadingCell("2nd Half (Jul - Dec) (%)", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		return quarterlyData;
	}

	private PdfPTable setPdfTableForYearly(Font tableLabelFont) throws DocumentException {
		PdfPTable quarterlyData = new PdfPTable(2);
		quarterlyData.setWidthPercentage(100);
		quarterlyData.addCell(
				getTableHeadingCell("Year", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		quarterlyData.addCell(
                getTableHeadingCell("Yearly (Jan - Dec) (%)", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		return quarterlyData;
	}

	public PdfPTable setPdfTableForQuarterly(Font tableLabelFont) throws DocumentException {
		PdfPTable quarterlyData = new PdfPTable(5);
		quarterlyData.setWidthPercentage(100);
		quarterlyData.addCell(
				getTableHeadingCell("Year", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		quarterlyData.addCell(
                getTableHeadingCell("1st Quarter (Jan - Mar) (%)", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		quarterlyData.addCell(
                getTableHeadingCell("2nd Quarter (Apr - Jun) (%)", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		quarterlyData.addCell(
                getTableHeadingCell("3rd Quarter (Jul - Sep) (%)", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		quarterlyData.addCell(
                getTableHeadingCell("4th Quarter (Oct - Dec) (%)", PdfPCell.ALIGN_MIDDLE, PdfPCell.ALIGN_CENTER, tableLabelFont));
		return quarterlyData;
	}

	private void prepareExcelSheetForAwardKeyPersonTimesheet(String timesheetType, Map<String, List<AwardKeyPersonTimesheet>> awardKeyPersonTimesheetDetails, XSSFSheet sheet,
                                                             XSSFWorkbook workbook, Integer awardId, String timesheetPersonId) {
		try {
		XSSFCellStyle tableBodyStyle = workbook.createCellStyle();
            sheet.addMergedRegion(new CellRangeAddress(0, 0, 0, 4));
		int rowNumber = 0;
            rowNumber = prepareAwardGeneralInformationForExcel(sheet, rowNumber, awardId.toString(), workbook, timesheetPersonId, Boolean.TRUE);
            String heading = "Award Key Person Timesheet Detail";
		int totalColumnCount = 7;
		rowNumber = rowNumber + 1;
		String year = null;
		prepareExcelSheetHeading(sheet, heading, workbook, totalColumnCount, rowNumber);
		rowNumber = rowNumber + 1;
            if (timesheetType != null && timesheetType.equals("QUARTERLY")) {
                Object[] tableHeadingYear = {"Year ", "1st Quarter (Jan - Mar) (%)", "2nd Quarter (Apr - Jun) (%)", "3rd Quarter (Jul - Sep) (%)", "4th Quarter (Oct - Dec) (%)"};
			prepareExcelSheetHeader(sheet, tableHeadingYear, workbook, tableBodyStyle, rowNumber++);
		} else if (timesheetType != null && timesheetType.equals("YEARLY")) {
                Object[] tableHeadingYear = {"Year ", "Yearly (Jan - Dec) (%)"};
			prepareExcelSheetHeader(sheet, tableHeadingYear, workbook, tableBodyStyle, rowNumber++);
		} else if (timesheetType != null && timesheetType.equals("HALFYEARLY")) {
                Object[] tableHeadingYear = {"Year ", "1st Half (Jan - Jun)(%)", "2nd Half (Jul - Dec)(%)"};
			prepareExcelSheetHeader(sheet, tableHeadingYear, workbook, tableBodyStyle, rowNumber++);
		}
		for (Map.Entry<String, List<AwardKeyPersonTimesheet>> entry : awardKeyPersonTimesheetDetails.entrySet()) {
			year = entry.getKey();
			List<Object> values = new ArrayList<>();
                Object[] arrayValue = entry.getValue().toArray();
			for (Object awardKeyPerson : arrayValue) {
				AwardKeyPersonTimesheet awardKeyPersonTimesheet = (AwardKeyPersonTimesheet) awardKeyPerson;
                    values.add(awardKeyPersonTimesheet.getOrderNumber() + "-" + awardKeyPersonTimesheet.getValue());
			}
			rowNumber = prepareAwardTimesheet(timesheetType, year, rowNumber, values.toArray(), sheet, tableBodyStyle);
			++rowNumber;
		   }
        } catch (Exception e) {
			logger.error("error in prepareExcelSheetForAwardKeyPersonTimesheet {}", e.getMessage());
		}
	}
  
	private Integer prepareAwardTimesheet(String timesheetType, String year, int rowNumber, Object[] value, XSSFSheet sheet, XSSFCellStyle tableBodyStyle) {
		int cellNumber = 0;
		String firstValue = null;
		String secondValue = null;
		String thirdValue = null;
		String fourthValue = null;
		for (int i = 0; i < value.length; i++) {
			String orderValue = value[i].toString().split("-")[0];
			String cellValue = value[i].toString().split("-")[1];
			if (orderValue.equals("1")) {
				firstValue = cellValue;
			} else if (orderValue.equals("2")) {
				secondValue = cellValue;
			} else if (orderValue.equals("3")) {
				thirdValue = cellValue;
			} else if (orderValue.equals("4")) {
				fourthValue = cellValue;
			}
		}
		Row outerRow = sheet.createRow(rowNumber);
		if (timesheetType != null && timesheetType.equals("QUARTERLY")) {
		Cell cell1 = assignCell(cellNumber, tableBodyStyle, outerRow);
		if (year != null) {
			cell1.setCellValue(year);
		} else
			cell1.setCellValue(" ");
		cellNumber++;

		Cell cell2 = assignCell(cellNumber, tableBodyStyle, outerRow);
		if (firstValue != null) {
			cell2.setCellValue(firstValue);
		} else
			cell2.setCellValue(" ");
		cellNumber++;

		Cell cell3 = assignCell(cellNumber, tableBodyStyle, outerRow);
		if (secondValue != null) {
			cell3.setCellValue(secondValue);
		} else
			cell3.setCellValue(" ");
		cellNumber++;

		Cell cell4 = assignCell(cellNumber, tableBodyStyle, outerRow);
		if (thirdValue != null) {
			cell4.setCellValue(thirdValue);
		} else
			cell4.setCellValue(" ");
		cellNumber++;

		Cell cell5 = assignCell(cellNumber, tableBodyStyle, outerRow);
		if (fourthValue != null) {
			cell5.setCellValue(fourthValue);
		} else
			cell5.setCellValue(" ");
		cellNumber++;
        } else if (timesheetType != null && timesheetType.equals("HALFYEARLY")) {
			Cell cell1 = assignCell(cellNumber, tableBodyStyle, outerRow);
			if (year != null) {
				cell1.setCellValue(year);
			} else
				cell1.setCellValue(" ");
			cellNumber++;

			Cell cell2 = assignCell(cellNumber, tableBodyStyle, outerRow);
			if (firstValue != null) {
				cell2.setCellValue(firstValue);
			} else
				cell2.setCellValue(" ");
			cellNumber++;

			Cell cell3 = assignCell(cellNumber, tableBodyStyle, outerRow);
			if (secondValue != null) {
				cell3.setCellValue(secondValue);
			} else
				cell3.setCellValue(" ");
			cellNumber++;
		} else if (timesheetType != null && timesheetType.equals("YEARLY")) {
			Cell cell1 = assignCell(cellNumber, tableBodyStyle, outerRow);
			if (year != null) {
				cell1.setCellValue(year);
			} else
				cell1.setCellValue(" ");
			cellNumber++;

			Cell cell2 = assignCell(cellNumber, tableBodyStyle, outerRow);
			if (firstValue != null) {
				cell2.setCellValue(firstValue);
			} else
				cell2.setCellValue(" ");
			cellNumber++;
		}
		return outerRow.getRowNum();
	}

	private BudgetHeader findBudgetHeader(List<BudgetHeader> budgetHeaders) {
		BudgetHeader budgetHeader = null;
		if (budgetHeaders != null && !budgetHeaders.isEmpty()) {
			Boolean isApprovedBudget = false;
			Boolean isFinalBudget = false;
			for (BudgetHeader budgetHeaderData : budgetHeaders) {
				if (budgetHeaderData.getIsApprovedBudget()) {
					isApprovedBudget = true;
					budgetHeader = budgetHeaderData;
				} else if (budgetHeaderData.getIsFinalBudget()) {
					if (!isApprovedBudget) {
						isFinalBudget = true;
						budgetHeader = budgetHeaderData;
					}
				} else if (budgetHeaderData.getIsLatestVersion()) {
					if ((!isApprovedBudget) && (!isFinalBudget)) {
						budgetHeader = budgetHeaderData;
					}
				}
			}
		}
		return budgetHeader;
	}

	@Override
	public LetterTemplateType getFileNameByTemplateCode(String templateTypeCode) {
		LetterTemplateType letterTemplate = new LetterTemplateType();
		try {
			String query = QueryBuilder.selectFileNameFromLetterTemplate(templateTypeCode);
			ArrayList<HashMap<String, Object>> dataList = dbEngine.executeQuerySQL(new ArrayList<Parameter>(), query);
			if (!dataList.isEmpty()) {
				letterTemplate.setFileName((dataList.get(0).get("FILE_NAME")).toString());
				letterTemplate.setPrintFileType((dataList.get(0).get("PRINT_FILE_TYPE")).toString());
			}
		} catch (Exception e) {
			logger.error("Exception in getFileNameByTemplateCode : {}", e.getMessage());
		}
		return letterTemplate;
	}

	@Override
	public byte[] setMergePlaceHoldersOfServiceRequest(byte[] data, Integer serviceRequestId, String personId,
			String userName, Integer subModuleCode, String subModuleItemCode) throws IOException {
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		try {
			InputStream myInputStream = new ByteArrayInputStream(data);
			IXDocReport report = XDocReportRegistry.getRegistry().loadReport(myInputStream, TemplateEngineKind.Velocity);
			FieldsMetadata fieldsMetadata = report.createFieldsMetadata();
			ServiceRequest serviceRequests = serviceRequestDao.fetchServiceRequestById(serviceRequestId);

			fieldsMetadata.load("questionnaires", QuestionnairePrintParameter.class, true);
			List<QuestionnairePrintParameter> questionnaires = setQuestionnaireDetails(userName, personId, Constants.SERVICE_REQUEST_MODULE_CODE, 
			serviceRequestId, subModuleCode, subModuleItemCode);

			IContext context = report.createContext();
			context.put("questionnaires", questionnaires);
			context = setServiceRequestPlaceHolderData(context, serviceRequests);
			Options options = Options.getTo(ConverterTypeTo.PDF).via(ConverterTypeVia.XWPF);
			report.convert(context, options, baos);
		} catch (Exception e) {
			logger.error("Exception in mergePlaceHolders : {}", e.getMessage());
		}finally {
			baos.close();
		}
		return baos.toByteArray();
	}

	public IContext setServiceRequestPlaceHolderData(IContext context, ServiceRequest serviceRequests) {
		context.put(TITLE, serviceRequests.getSubject() != null ? serviceRequests.getSubject() : "");
		context.put(SR_ID, serviceRequests.getServiceRequestId() != null ? serviceRequests.getServiceRequestId() : "");
		return context;
	}

	List<CustomDataPrintParameter> setCustomDataPrintDetails(String moduleItemKey, Integer moduleCode) {
		List<CustomDataPrintParameter> customDataPrintParameters = new ArrayList<>();
		try {
			List<CustomDataElementUsage> usages = customDataElementDao.getApplicableCustomElement(moduleCode);
			for (CustomDataElementUsage usage : usages) {
				Integer answerVersion = null;
				if (usage.getCustomDataElement().getIsLatestVesrion().equals("Y") && usage.getCustomDataElement().getIsActive().equals("N")) {
					answerVersion = customDataElementDao.getAnswerVersion(usage.getCustomDataElement().getColumnId(), moduleCode, moduleItemKey);
				}
				if ((usage.getCustomDataElement().getIsLatestVesrion().equals("Y") && usage.getCustomDataElement().getIsActive().equals("Y")) || answerVersion != null) {
					CustomDataPrintParameter customDataPrintParameter = new CustomDataPrintParameter();
					List<String> answersList = new ArrayList<>();
					customDataPrintParameter.setColumnLabel(usage.getCustomDataElement().getColumnLabel());
					customDataPrintParameter.setCustomElementName(usage.getCustomDataElement().getCustomElementName());
					customDataElementDao.getCustomDataAnswers(moduleCode, moduleItemKey, usage.getCustomDataElement().getCustomElementId()).forEach(answer ->
							answersList.add(answer.getValue())
					);
					customDataPrintParameter.setAnswersList(answersList);
					customDataPrintParameters.add(customDataPrintParameter);
				}
			}
		} catch(Exception e) {
			logger.error("setCustomDataPrintDetails {}", e.getMessage());
		}
		return customDataPrintParameters;
	}

	@Override
	public ResponseEntity<ResponseData> getAllLetterTemplateTypes(Integer moduleCode) {
		try {
			List<LetterTemplateType> letterTemplateTypes = new ArrayList<>();
			commonDao.getAllLetterTemplateTypes(moduleCode).forEach(object -> {
				Object[] obj = (Object[]) object;
				LetterTemplateType letterTemplateType = new LetterTemplateType();
				letterTemplateType.setLetterTemplateTypeCode((String) obj[0]);
				letterTemplateType.setFileName((String) obj[1]);
				letterTemplateType.setContentType((String) obj[2]);
				letterTemplateType.setPrintFileType((String) obj[3]);
				letterTemplateType.setModuleCode((Integer) obj[4]);
				letterTemplateTypes.add(letterTemplateType);
			});
			return new ResponseEntity<>(new ResponseData(letterTemplateTypes, "Success", commonDao.getCurrentTimestamp(),
					true), HttpStatus.OK);
		} catch (Exception e) {
			logger.error(" Exception in getAllLetterTemplateTypes : {}", e.getMessage());
			return new ResponseEntity<>(new ResponseData(null, "Failed to load data", commonDao.getCurrentTimestamp(),
					false, e.getMessage()), HttpStatus.INTERNAL_SERVER_ERROR);
		}
	}
	
	@Override
	public void generateIpReport(HttpServletResponse response, PrintVO printVO) {
		try {
			if (printVO.getLetterTemplateTypeCodes().size() == 1) {
				generateIpReport(response, printVO.getLetterTemplateTypeCodes().get(0), printVO.getInstituteProposalId());
			} else {
				generateIpReport(response, printVO.getLetterTemplateTypeCodes(), printVO.getInstituteProposalId());
			}
		} catch (ApplicationException e) {
			throw e;
		} catch (Exception e) {
			throw new ApplicationException("error in generateIpReport", e, Constants.JAVA_ERROR);
		}
	}

	private void generateIpReport(HttpServletResponse response, String templateTypeCode, Integer instituteProposalId) throws SQLException, IOException {
		LetterTemplateType letterTemplate = commonDao.getLetterTemplate(templateTypeCode);
		byte[] bFile = letterTemplate.getCorrespondenceTemplate().getBytes(1l,
				(int) letterTemplate.getCorrespondenceTemplate().length());
		byte[] mergedOutput = setMergePlaceHoldersOfIp(bFile, instituteProposalId,letterTemplate);
		String generatedFileName = RESULT + System.nanoTime() + "." + letterTemplate.getPrintFileType();
		HttpHeaders headers = new HttpHeaders();
		headers.setContentType(MediaType.parseMediaType(CONTENT_TYPE));
		headers.setContentDispositionFormData(generatedFileName, generatedFileName);
		headers.setContentLength(mergedOutput.length);
		headers.setCacheControl(CACHE_CONTROL);
		headers.setPragma(PUBLIC);
		response.setCharacterEncoding(StandardCharsets.UTF_8.name());
		response.setContentType(CONTENT_TYPE);
		response.setContentLength(mergedOutput.length);
		response.setHeader(CONTENT_DISPOSITION, ATTACHMENT_FILENAME + generatedFileName + "\"");
		FileCopyUtils.copy(mergedOutput, response.getOutputStream());
	}

	private void generateIpReport(HttpServletResponse response, List<String> templateTypeCodes,Integer instituteProposalId) throws SQLException, IOException {
		String fileName = "IP_#" + instituteProposalId + System.nanoTime();
		response.setContentType("application/zip");
		response.setHeader("Content-Disposition", "attachment;filename=\"" + fileName + ".zip" + "\"");
		java.io.ByteArrayOutputStream baos = new java.io.ByteArrayOutputStream();
		ZipOutputStream zos = new ZipOutputStream(baos);
		for (String templateTypeCode : templateTypeCodes) {
			LetterTemplateType letterTemplate = commonDao.getLetterTemplate(templateTypeCode);
			byte[] bFile = letterTemplate.getCorrespondenceTemplate().getBytes(1l,
					(int) letterTemplate.getCorrespondenceTemplate().length());
			byte[] mergedOutput = setMergePlaceHoldersOfIp(bFile, instituteProposalId,letterTemplate);
			zos.putNextEntry(new ZipEntry("IP_" + templateTypeCode + "_" + System.nanoTime() + "."
					+ letterTemplate.getPrintFileType()));
			zos.write(mergedOutput);
		}
		zos.closeEntry();
		zos.flush();
		baos.flush();
		zos.close();
		baos.close();
		ServletOutputStream op = response.getOutputStream();
		op.write(baos.toByteArray());
		op.flush();
		op.close();
	}
	
	@Override
	public byte[] setMergePlaceHoldersOfIp(byte[] data, Integer instituteProposalId, LetterTemplateType letterTemplate) {
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		try {
			InputStream myInputStream = new ByteArrayInputStream(data);
			IXDocReport report = XDocReportRegistry.getRegistry().loadReport(myInputStream, TemplateEngineKind.Velocity);
			FieldsMetadata fieldsMetadata = report.createFieldsMetadata();
			InstituteProposal instProposal = institutionalProposalDao.fetchInstProposalById(instituteProposalId);
			
			List<InstituteProposalPerson> instituteProposalPersons = institutionalProposalDao.loadInstProposalPersonsByProposalId(instituteProposalId);
			fieldsMetadata.load("ipPersonList", ProposalPrintParameter.class, true);
			List<IpPrintParameter> ipPersonList = setIpPersonDetails(instituteProposalPersons);
			instProposal.setPrincipalInvestigator(getPrincipalInvestigator(instituteProposalPersons));
			
			List<InstituteProposalSpecialReview> instituteProposalSpecialReviews = institutionalProposalDao.loadInstProposalSpecialReviewByProposalId(instituteProposalId);
			fieldsMetadata.load("IpSpecialReviews", IpPrintParameter.class, true);
			List<IpPrintParameter> ipSpecialReviews = setIpSpecialReviewDetails(instituteProposalSpecialReviews);
			
			List<InstituteProposalResearchArea> instituteProposalResearchArea = institutionalProposalDao.loadInstProposalResearchAreaByProposalId(instituteProposalId);
			fieldsMetadata.load("ipResearchAreas", IpPrintParameter.class, true);
			List<IpPrintParameter> ipResearchAreas = setIpResearchAreaDetails(instituteProposalResearchArea);
			
			fieldsMetadata.load("customData", CustomDataPrintParameter.class, true);
			List<CustomDataPrintParameter> customData = setCustomDataPrintDetails(instituteProposalId.toString(),
					Constants.MODULE_CODE_INSTITUTE_PROPOSAL);
			
			List<InstituteProposalComment> institueProposalComments = institutionalProposalDao.fetchInstituteProposalCommentsByParams(instituteProposalId);
			fieldsMetadata.load("ipComment", IpPrintParameter.class, true);
			List<IpPrintParameter>  ipComment = setIpComments(institueProposalComments);
			
			InstituteProposalBudgetHeader budgetHeaders = institutionalProposalDao.loadInstPropBudgetDetailsByProposalId(instituteProposalId);
			fieldsMetadata.load("periodAndTotalList", ProposalPrintParameter.class, true);
			List<BudgetPeriodPrintParameter> periodAndTotalList =  setPeriodAndTotalList(budgetHeaders);

			IContext context = report.createContext();
			context.put("customData", customData);
			context.put("ipPersonList", ipPersonList);
			context.put("IpSpecialReviews", ipSpecialReviews);
			context.put("ipResearchAreas", ipResearchAreas);
		    context.put("ipComment", ipComment);
			context.put("periodAndTotalList", periodAndTotalList);
			context = setIpPlaceHolderData(context, instProposal, fieldsMetadata, budgetHeaders);

			if (letterTemplate != null && letterTemplate.getPrintFileType().equals(DOCX)) {
				report.process(context, baos);
			} else {
				Options options = Options.getTo(ConverterTypeTo.PDF).via(ConverterTypeVia.XWPF);
				report.convert(context, options, baos);
			}
		} catch (Exception e) {
			throw new ApplicationException("error in setMergePlaceHoldersOfIp", e, Constants.JAVA_ERROR);
		}
		return baos.toByteArray();
	}
	
	public IContext setIpPlaceHolderData(IContext context, InstituteProposal instProposal,FieldsMetadata fieldsMetadata, InstituteProposalBudgetHeader budgetHeaders) {
		fieldsMetadata.addFieldAsTextStyling(ABSTRACT, SyntaxKind.Html);
		context.put(TITLE, instProposal.getTitle());
		context.put("IP_ID", instProposal.getProposalNumber());
		context.put(LEAD_UNIT, commonService.getUnitFormatByUnitDetail(instProposal.getHomeUnitNumber(), instProposal.getHomeUnitName()));
		context.put(GRANT_CALL_TYPE,instProposal.getGrantCallType() != null ? instProposal.getGrantCallType().getDescription(): "");
		context.put("CATEGORY", instProposal.getActivityType().getDescription());
		context.put(SPONSOR_NAME, instProposal.getSponsor() != null ? commonService.getSponsorFormatBySponsorDetail(instProposal.getSponsor().getSponsorCode(), 
				instProposal.getSponsor().getSponsorName(), instProposal.getSponsor().getAcronym()):"");
		context.put(PRIME_SPONSOR, instProposal.getPrimeSponsor() != null ?  commonService.getSponsorFormatBySponsorDetail(instProposal.getPrimeSponsor().getSponsorCode(),
				instProposal.getPrimeSponsor().getSponsorName(), instProposal.getPrimeSponsor().getAcronym()): "");
		context.put(FUNDING_OPPORTUNITY_NUMBER, instProposal.getProgramAnnouncementNumber() != null ? instProposal.getProgramAnnouncementNumber() : "");
		context.put("IP_STATUS", instProposal.getInstProposalStatus() != null ? instProposal.getInstProposalStatus().getDescription() : "");
		context.put("IP_TYPE", instProposal.getInstProposalType() != null ? instProposal.getInstProposalType().getDescription() : "");
		context.put("PI", instProposal.getPrincipalInvestigator());
		context.put("GRANT_CALL_ID", instProposal.getGrantCallId() != null ? instProposal.getGrantCallId() :"");
		context.put("GRANT_CALL_NAME", instProposal.getGrantCall() != null ? instProposal.getGrantCall().getGrantCallName() :"");
		context.put(START_DATE,commonService.convertDateFormatBasedOnTimeZone(instProposal.getStartDate().getTime(),Constants.DEFAULT_DATE_FORMAT));
		context.put(ABSTRACT, instProposal.getAbstractDescription() != null ?(instProposal.getAbstractDescription()): "");
		context.put(END_DATE, commonService.convertDateFormatBasedOnTimeZone(instProposal.getEndDate().getTime(),Constants.DEFAULT_DATE_FORMAT));
		context.put(SUBMISSION_DATE, instProposal.getSubmissionDate() != null ?commonService.convertDateFormatBasedOnTimeZone(instProposal.getSubmissionDate().getTime(),
                Constants.DEFAULT_DATE_FORMAT)  : "");
		context.put(INTERNAL_DEAD_LINE_DATE, instProposal.getInternalDeadLineDate() != null ?commonService.convertDateFormatBasedOnTimeZone(instProposal.getInternalDeadLineDate().getTime(),
                Constants.DEFAULT_DATE_FORMAT)  : "");
		context.put(SPONSOR_DEAD_LINE_DATE, instProposal.getSponsorDeadlineDate() != null ?commonService.convertDateFormatBasedOnTimeZone(instProposal.getSponsorDeadlineDate().getTime(),
                Constants.DEFAULT_DATE_FORMAT)  : "");
		context.put(SPONSOR_PROPOSAL_NUMBER, instProposal.getSponsorProposalNumber() != null ?instProposal.getSponsorProposalNumber()  : "");
		context.put("IP_CFDA_NUMBER", instProposal.getCfdaNumber() != null ?instProposal.getCfdaNumber()  : "");
		context.put(DURATION,instProposal.getDuration()!= null ? instProposal.getDuration() :"");
		context.put(AWARD_TYPE,instProposal.getAwardType() != null ?instProposal.getAwardType().getDescription()  : "");
		context.put(RESEARCH_DESCRIPTION, instProposal.getResearchDescription() != null ? instProposal.getResearchDescription() :"");
		context.put(MULTI_DISCIPLINARY_DESCRIPTION, instProposal.getMultiDisciplinaryDescription() != null ? instProposal.getMultiDisciplinaryDescription() :"");
		context.put(MASTER_PROPOSAL, instProposal.getBaseProposalNumber() != null ? instProposal.getBaseProposalNumber() :"");
		String ipTitle = institutionalProposalDao.getIPTitleForMasterProposal(instProposal.getBaseProposalNumber());
		context.put("MASTER_PROPOSAL_TITLE", ipTitle != null ? ipTitle :"");

		context.put("TOTAL_COST", (budgetHeaders != null &&budgetHeaders.getTotalCost() != null) ?Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetHeaders.getTotalCost())  : "");
		context.put("TOTAL_DIRECT_COST", (budgetHeaders != null && budgetHeaders.getTotalDirectCost() != null) ? Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetHeaders.getTotalDirectCost()) :"");
		context.put("TOTAL_INDIRECT_COST", (budgetHeaders != null && budgetHeaders.getTotalIndirectCost() != null) ?Constants.DOLLAR_SYMBOL + decimalFormat.format(budgetHeaders.getTotalIndirectCost()) : "");
		context.put("BUDGET_STATUS",(budgetHeaders != null && budgetHeaders.getBudgetStatus() != null)? budgetHeaders.getBudgetStatus().getDescription() : "");
        context.put("ON_CAMPUS_RATE", (budgetHeaders != null && budgetHeaders.getOnCampusRates() != null) ? budgetHeaders.getOnCampusRates() : "");
		context.put("OFF_CAMPUS_RATE", (budgetHeaders != null && budgetHeaders.getOffCampusRates() != null) ? budgetHeaders.getOffCampusRates() : "");
		context.put("COST_SHARING", (budgetHeaders != null && budgetHeaders.getCostSharingType() != null) ? budgetHeaders.getCostSharingType().getDescription() : "");
		context.put("BUDGET_START_DATE", (budgetHeaders != null && budgetHeaders.getStartDate() != null) ?commonService.convertDateFormatBasedOnTimeZone(budgetHeaders.getStartDate().getTime(),
                Constants.DEFAULT_DATE_FORMAT) : "");
		context.put("BUDGET_END_DATE", (budgetHeaders != null && budgetHeaders.getEndDate() != null) ? commonService.convertDateFormatBasedOnTimeZone(budgetHeaders.getEndDate().getTime(),
                Constants.DEFAULT_DATE_FORMAT) : "");
		return context;
	}
	
	private String getPrincipalInvestigator(List<InstituteProposalPerson> instituteProposalPerson) {
		InstituteProposalPerson pi = null;
		if (instituteProposalPerson != null && instituteProposalPerson.isEmpty()) {
			return "";
		}
		for (InstituteProposalPerson person : instituteProposalPerson) {
			if (StringUtils.equals(person.getProposalPersonRole().getCode(), Constants.PRINCIPAL_INVESTIGATOR)) {
				pi = person;
				break;
			}
		}
	    return pi != null ? pi.getFullName() : null;
	}

	private List<IpPrintParameter> setIpPersonDetails(List<InstituteProposalPerson> instituteProposalPerson) {
		List<IpPrintParameter> instituteProposalPersonList = new ArrayList<>();
		if (!instituteProposalPerson.isEmpty()) {
			for (InstituteProposalPerson instProposalPerson : instituteProposalPerson) {
				StringBuilder units = new StringBuilder(0);
				if (instProposalPerson.getDepartment() != null && !instProposalPerson.getDepartment().equals("")) {
					units.append(instProposalPerson.getDepartment());
				}
				if (!instProposalPerson.getUnits().isEmpty()) {
					for (InstituteProposalPersonUnit unit : instProposalPerson.getUnits()) {
						if (instProposalPerson.getUnits().indexOf(unit) != 0 || instProposalPerson.getDepartment() != null && !instProposalPerson.getDepartment().equals("")) {
							units.append(", " + unit.getUnit().getUnitDetail());
						} else {
							units.append(unit.getUnit().getUnitDetail());
						}
					}
				}
				String percentageOfEffort = "";
				if (instProposalPerson.getPercentageOfEffort() != null) {
					percentageOfEffort = percentageOfEffort + instProposalPerson.getPercentageOfEffort();
				}
				String personType = EMPLOYEE;
				String organisation = "";
				if (instProposalPerson.getRolodexId() != null) {
					personType = NON_EMPLOYEE;
					Rolodex rolodex = rolodexDao.getRolodexDetailById(instProposalPerson.getRolodexId());
					organisation = rolodex.getOrganization() == null ? "" : rolodex.getOrganizations().getOrganizationName();
				}
				IpPrintParameter instProsalPrintParameters = setIpPersonDetail(instProposalPerson, percentageOfEffort, units.toString(), personType, organisation);
				instituteProposalPersonList.add(instProsalPrintParameters);
			}
		} else {
			IpPrintParameter proposalPrintParameters = setIpPersonDetail(null, "", "", "", "");
			instituteProposalPersonList.add(proposalPrintParameters);
		}
		return instituteProposalPersonList;
	}

	private IpPrintParameter setIpPersonDetail(InstituteProposalPerson instProposalPerson, String percentageOfEffort, String units, String personType, String organization) {
		IpPrintParameter instProsalPrintParameters = new IpPrintParameter();
		if (instProposalPerson != null) {
			if (instProposalPerson.getDesignation() != null) {
				instProsalPrintParameters.setDesignation(instProposalPerson.getDesignation());
			} else {
				instProsalPrintParameters.setDesignation("");
			}
			String projectRole = Boolean.TRUE.equals(instProposalPerson.getProposalPersonRole().getShowProjectRole()) && instProposalPerson.getProjectRole() != null ? " (" + instProposalPerson.getProjectRole()+ ")" : "";
			instProsalPrintParameters.setRole(instProposalPerson.getProposalPersonRole() == null ? "" :instProposalPerson.getProposalPersonRole().getDescription().concat(projectRole));
			instProsalPrintParameters.setFullName(instProposalPerson.getFullName());
			instProsalPrintParameters.setDesignation(instProposalPerson.getDesignation() == null ? "" : instProposalPerson.getDesignation());
			instProsalPrintParameters.setPercentageOfEffort(instProposalPerson.getPercentageOfEffort() == null ? "" : ("" +instProposalPerson.getPercentageOfEffort().toString()));
			
		} else {
			instProsalPrintParameters.setRole("");
			instProsalPrintParameters.setFullName("");
			instProsalPrintParameters.setDesignation("");
			instProsalPrintParameters.setPercentageOfEffort("");
			instProsalPrintParameters.setPersonType("");
		}
		instProsalPrintParameters.setPercentageOfEffort(percentageOfEffort);
		instProsalPrintParameters.setUnits(units);
		instProsalPrintParameters.setPersonType(personType);
		instProsalPrintParameters.setOrganization(organization);
		return instProsalPrintParameters;
	}
	
	private List<IpPrintParameter> setIpSpecialReviewDetails(List<InstituteProposalSpecialReview> instituteProposalSpecialReviews) {
		List<IpPrintParameter> ipSpecialReviewList = new ArrayList<>();
		if (instituteProposalSpecialReviews.isEmpty()) {
			ipSpecialReviewList.add(new IpPrintParameter("", "", "", "", "", "", ""));
		} else {
			Map<String, String> nonEmployees = new HashMap<>();
			Map<String, String> persons = new HashMap<>();
			for (InstituteProposalSpecialReview instituteProposalSpecialReview : instituteProposalSpecialReviews) {
				String specialReviewType = "";
				String approvalType = "";
				String protocolNumber = "";
				String applicationDate = "";
				String approvalDate = "";
				String expirationDate = "";
				String comment = "";
				instituteProposalSpecialReview = institutionalProposalService.setInsSpecialReviewDetail(persons, nonEmployees, instituteProposalSpecialReview);
				if (instituteProposalSpecialReview.getSpecialReviewType() != null) {
					specialReviewType = specialReviewType + instituteProposalSpecialReview.getSpecialReviewType().getDescription();
				}
				if (instituteProposalSpecialReview.getSpecialReviewApprovalType() != null) {
					approvalType = approvalType + instituteProposalSpecialReview.getSpecialReviewApprovalType().getDescription();
				}
				if (instituteProposalSpecialReview.getProtocolNumber() != null) {
					protocolNumber = protocolNumber + instituteProposalSpecialReview.getProtocolNumber();
				}
				if (instituteProposalSpecialReview.getApplicationDate() != null) {
					applicationDate = applicationDate + commonService.convertDateFormatBasedOnTimeZone(instituteProposalSpecialReview.getApplicationDate().getTime(), Constants.DEFAULT_DATE_FORMAT);
				}
				if (instituteProposalSpecialReview.getApprovalDate() != null) {
					approvalDate = approvalDate + commonService.convertDateFormatBasedOnTimeZone(instituteProposalSpecialReview.getApprovalDate().getTime(), Constants.DEFAULT_DATE_FORMAT);
				}
				if (instituteProposalSpecialReview.getExpirationDate() != null) {
					expirationDate = expirationDate + commonService.convertDateFormatBasedOnTimeZone(instituteProposalSpecialReview.getExpirationDate().getTime(), Constants.DEFAULT_DATE_FORMAT);
				}
				if (instituteProposalSpecialReview.getComments() != null) {
					comment = comment + instituteProposalSpecialReview.getComments();
				}
				ipSpecialReviewList.add(new IpPrintParameter(specialReviewType, approvalType, protocolNumber, applicationDate, approvalDate, expirationDate, comment));
			}
		}
		return ipSpecialReviewList;
	}
	
	private List<IpPrintParameter> setIpResearchAreaDetails(List<InstituteProposalResearchArea> instituteProposalResearchArea) {
		List<IpPrintParameter> projectTeams = new ArrayList<>();
		if (instituteProposalResearchArea.isEmpty()) {
			projectTeams.add(new IpPrintParameter("", "", "", ""));
		} else {
			for (InstituteProposalResearchArea researchArea : instituteProposalResearchArea) {
				if (researchArea.getResearchTypeArea() != null) {
					if (researchArea.getResearchTypeSubArea() != null) {
						projectTeams.add(new IpPrintParameter(researchArea.getResearchTypeAreaCode(),
								researchArea.getResearchType().getDescription(),
								researchArea.getResearchTypeArea().getDescription(),
								researchArea.getResearchTypeSubArea().getDescription()));
					} else {
						projectTeams.add(new IpPrintParameter(researchArea.getResearchTypeAreaCode(),
								researchArea.getResearchType().getDescription(),
								researchArea.getResearchTypeArea().getDescription(), ""));
					}
				}
			}
		}
		return projectTeams;
	}
	
	private List<BudgetPeriodPrintParameter> setPeriodAndTotalList(InstituteProposalBudgetHeader budgetHeaders) {
		List<BudgetPeriodPrintParameter> budgetPeriodDetails = new ArrayList<>();
		if(budgetHeaders != null) {
			List<InstituteProposalBudgetPeriod> budgetPeriods = budgetHeaders.getBudgetPeriods();
			if (budgetPeriods.isEmpty()) {
				budgetPeriodDetails.add(new BudgetPeriodPrintParameter("","","","","",""));
			} else {
				for (InstituteProposalBudgetPeriod budgetPeriod : budgetPeriods) {
					if(budgetPeriod.getBudgetPeriodId() != null) {
						budgetPeriodDetails.add(new BudgetPeriodPrintParameter(budgetPeriod.getBudgetPeriod().toString(),
								commonService.convertDateFormatBasedOnTimeZone(budgetPeriod.getStartDate().getTime(), Constants.DEFAULT_DATE_FORMAT),
								commonService.convertDateFormatBasedOnTimeZone(budgetPeriod.getEndDate().getTime(), Constants.DEFAULT_DATE_FORMAT),
								Constants.DOLLAR_SYMBOL + budgetPeriod.getTotalDirectCost().toString(),
								Constants.DOLLAR_SYMBOL + budgetPeriod.getTotalIndirectCost().toString(),
								Constants.DOLLAR_SYMBOL + budgetPeriod.getTotalCost().toString()
								));
					}
					
				}
			}
		} else {
			budgetPeriodDetails.add(new BudgetPeriodPrintParameter("","","","","",""));
		}
		return budgetPeriodDetails;
	}
	
	private List<IpPrintParameter> setIpComments(List<InstituteProposalComment> institueProposalComments) {
		List<IpPrintParameter> institueProposalCommentDetails = new ArrayList<>();
		if (institueProposalComments.isEmpty()) {
			institueProposalCommentDetails.add(new IpPrintParameter("",null));
		} else {
			institueProposalComments.stream().collect(Collectors.groupingBy(InstituteProposalComment::getCommentTypeCode)).entrySet().forEach(proposalComment -> {
	            List<String> comments = new ArrayList<>();
	            List<String> commentType = new ArrayList<>();
	            proposalComment.getValue().stream().forEach(comment -> {
					 String commentData = comment.getComment() + "  Commented by " + personDao.getUserFullNameByUserName(comment.getUpdateUser()) + " on " + commonService.convertDateFormatBasedOnTimeZone(comment.getUpdateTimestamp().getTime(), Constants.DEFAULT_DATE_FORMAT) ;
					comments.add(commentData);
				    commentType.add(comment.getCommentType().getDescription());
					});
	            institueProposalCommentDetails.add(new IpPrintParameter(commentType.get(0),comments));
			});
	  }
		return institueProposalCommentDetails;
	}

	List<ProposalOrganization> loadProposalOrganization(Integer proposalId) {
		if(commonDao.getParameterValueAsBoolean(Constants.ENABLE_PROPOSAL_ORGANIZATION)) {
			List<ProposalOrganization> proposalOrganization = proposalDao.loadProposalOrganization(proposalId);
			List<Integer> rolodexId = proposalOrganization.stream().filter(organization -> organization.getOrganization() != null).map(organization -> organization.getOrganization().getContactAddressId()).collect(Collectors.toList());
			if(!rolodexId.isEmpty()){
				List<Rolodex> rolodex = commonDao.getRolodexDetailByRolodexId(rolodexId);
				Map<Integer, String> collect = rolodex.stream().collect(Collectors.toMap(Rolodex :: getRolodexId, Rolodex :: getFullName));
				proposalOrganization.stream()
						.filter(item -> item.getOrganization() != null && item.getOrganization().getContactAddressId() != null && collect.containsKey(item.getOrganization().getContactAddressId()))
						.forEach(item -> item.getOrganization().setContactPersonName(collect.get(item.getOrganization().getContactAddressId())));
			}
			return proposalOrganization;
		}
		return null;
	}
}
