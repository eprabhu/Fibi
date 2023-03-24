package com.polus.fibicomp.proposal.print.service;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.math.BigDecimal;
import java.text.ParseException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
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
import com.itextpdf.text.Rectangle;
import com.itextpdf.text.pdf.PdfPCell;
import com.itextpdf.text.pdf.PdfPTable;
import com.itextpdf.text.pdf.PdfWriter;
import com.polus.fibicomp.budget.pojo.BudgetDetail;
import com.polus.fibicomp.budget.pojo.BudgetHeader;
import com.polus.fibicomp.budget.pojo.BudgetPeriod;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.common.service.DateTimeService;
import com.polus.fibicomp.compilance.pojo.ProposalSpecialReview;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.grantcall.dao.GrantCallDao;
import com.polus.fibicomp.grantcall.pojo.GrantCall;
import com.polus.fibicomp.proposal.dao.ProposalDao;
import com.polus.fibicomp.proposal.module.dao.ProposalModuleDao;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.proposal.pojo.ProposalKeyword;
import com.polus.fibicomp.proposal.pojo.ProposalPerson;
import com.polus.fibicomp.proposal.pojo.ProposalPersonUnit;
import com.polus.fibicomp.proposal.pojo.ProposalProjectTeam;
import com.polus.fibicomp.proposal.pojo.ProposalResearchArea;
import com.polus.fibicomp.proposal.pojo.ProposalSponsor;
import com.polus.fibicomp.proposal.print.dto.BudgetCategoryDTO;
import com.polus.fibicomp.proposal.print.dto.LineItem;
import com.polus.fibicomp.proposal.print.dto.PeriodCost;
import com.polus.fibicomp.questionnaire.dto.QuestionnaireDataBus;
import com.polus.fibicomp.questionnaire.service.QuestionnaireService;

@Transactional
@Service(value = "proposalPrintService")
public class ProposalPrintServiceImpl implements ProposalPrintService {

	@Autowired
	@Qualifier(value = "proposalDao")
	private ProposalDao proposalDao;

	@Autowired
	@Qualifier(value = "dateTimeService")
	private DateTimeService dateTimeService;

	@Autowired
	@Qualifier(value = "questionnaireService")
	private QuestionnaireService questionnaireService;

	@Autowired
	private GrantCallDao grantCallDao;

	@Autowired
	private ProposalModuleDao proposalModuleDao;
	
	@Autowired
	CommonService commonService;

	private static final String PROPOSAL_OVERVIEW = "Proposal Overview";
	private static final String TITLE = "Title: ";
	private static final String PRINCIPAL_INVESTIGATOR = "Principal Investigator: ";
	private static final String LEAD_UNIT = "Lead Unit: ";
	private static final String OH_BASE = "OH Base: ";
	private static final String START_DATE = "Start Date: ";
	private static final String BUDGET_SUMMARY = "Budget Summary";
	private static final String COST = Constants.DOLLAR_SYMBOL + "0.00";
	private static final String END_DATE = "End Date: ";
	private static final String DIRECT_COST = "Direct Cost: ";
	private static final String TOTAL_COST = "Total Cost: ";
	private static final String INDIRECT_COST = "Indirect Cost: ";
	private static final String NO_COST_ELEMENT_MESSAGE = "There are no cost elements present";

	@SuppressWarnings("unchecked")
	@Override
	public ByteArrayInputStream proposalPdfReport(Integer proposalId, String personId, String userName)
			throws DocumentException, ParseException {
		Proposal pdfData = proposalDao.fetchProposalById(proposalId);
		QuestionnaireDataBus questionnaireDataBusData = new QuestionnaireDataBus();
		questionnaireDataBusData.setActionPersonId(userName);
		questionnaireDataBusData.setActionUserId(personId);
		questionnaireDataBusData.setModuleItemCode(3);
		questionnaireDataBusData.setModuleItemKey(proposalId.toString());
		questionnaireDataBusData.setModuleSubItemCode(0);
		questionnaireDataBusData.setModuleSubItemKey("0");
		Document document = new Document();
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		PdfWriter.getInstance(document, out);
		document.open();
		Font pdfTitleFont = FontFactory.getFont(FontFactory.HELVETICA_BOLD);
		pdfTitleFont.setSize(13);
		Font headFont = FontFactory.getFont(FontFactory.HELVETICA, 10, Font.BOLD);
		Font bodyFont = FontFactory.getFont(FontFactory.HELVETICA, 10);
		Font subHeadFont = FontFactory.getFont(FontFactory.HELVETICA_BOLDOBLIQUE);
		subHeadFont.setSize(11);
		subHeadFont.setColor(new BaseColor(255, 255, 255));
		Font questionnaireHeadFont = FontFactory.getFont(FontFactory.HELVETICA_BOLD, 10);
		Font questionnaireBodyFont = FontFactory.getFont(FontFactory.HELVETICA, 9);
		Font questionnaireSubHeadFont = FontFactory.getFont(FontFactory.HELVETICA_BOLD, 9);
		Font questionnaireHeaderFont = FontFactory.getFont(FontFactory.HELVETICA_BOLDOBLIQUE, 11);
		questionnaireHeaderFont.setColor(new BaseColor(255, 255, 255));

		try {
			questionnaireDataBusData = questionnaireService.getApplicableQuestionnaire(questionnaireDataBusData);
			List<QuestionnaireDataBus> questionnaireList = questionnaireService
					.getQuestionnaireList(questionnaireDataBusData);
			Paragraph pdfHeading = new Paragraph("Proposal Summary", pdfTitleFont);
			pdfHeading.setAlignment(Element.ALIGN_CENTER);
			pdfHeading.setSpacingAfter(8);
			document.add(pdfHeading);

			PdfPTable proposalHeadingTable = new PdfPTable(1);
			setTableStyle(proposalHeadingTable);

			PdfPCell cell = new PdfPCell(new Phrase(PROPOSAL_OVERVIEW, subHeadFont));
			setTableHeadingStyle(cell);
			proposalHeadingTable.addCell(cell);
			document.add(proposalHeadingTable);

			PdfPTable proposalDetailsTable = new PdfPTable(1);
			proposalDetailsTable.setWidthPercentage(100);
			proposalDetailsTable.setWidths(new int[] { 12 });
			proposalDetailsTable.setSpacingAfter(2f);

			PdfPTable proposalDataTable = new PdfPTable(1);
			proposalDataTable.setWidthPercentage(100);
			proposalDataTable.setWidths(new int[] { 12 });
			proposalDataTable.setSpacingAfter(2f);

			PdfPTable proposalDetailsMutipleColumnTable = new PdfPTable(2);
			proposalDetailsMutipleColumnTable.setSpacingBefore(0);
			proposalDetailsMutipleColumnTable.setWidthPercentage(100);
			proposalDetailsMutipleColumnTable.setWidths(new int[] { 6, 6 });
			proposalDetailsMutipleColumnTable.setSpacingAfter(5f);

			proposalDetailsTable.addCell(getModifiedGeneralDetailsCell(TITLE, pdfData.getTitle(),
					Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));
			if (pdfData.getInvestigator() != null) {
				proposalDetailsMutipleColumnTable.addCell(
						getModifiedGeneralDetailsCell(PRINCIPAL_INVESTIGATOR, pdfData.getInvestigator().getFullName(),
								Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));
			}
			proposalDetailsMutipleColumnTable
					.addCell(getModifiedGeneralDetailsCell("Status: ", pdfData.getProposalStatus().getDescription(),
							Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));
			if (pdfData.getProposalType() != null) {
				proposalDetailsMutipleColumnTable.addCell(
						getModifiedGeneralDetailsCell("Proposal Type: ", pdfData.getProposalType().getDescription(),
								Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));
			} else {
				proposalDetailsMutipleColumnTable.addCell(getModifiedGeneralDetailsCell("Proposal Type: ", "",
						Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));
			}
			if (pdfData.getActivityType() != null) {
				proposalDetailsMutipleColumnTable.addCell(
						getModifiedGeneralDetailsCell("Activity Type: ", pdfData.getActivityType().getDescription(),
								Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));
			} else {
				proposalDetailsMutipleColumnTable.addCell(getModifiedGeneralDetailsCell("Activity Type: ", "",
						Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));
			}
			proposalDataTable.addCell(getModifiedGeneralDetailsCell(LEAD_UNIT,
					pdfData.getHomeUnitNumber() + " - " + pdfData.getHomeUnitName(), Element.ALIGN_MIDDLE,
					Element.ALIGN_LEFT, bodyFont, headFont));
			if (pdfData.getSponsor() != null) {
				proposalDetailsMutipleColumnTable.addCell(getModifiedGeneralDetailsCell("Sponsor: ", commonService.getSponsorFormatBySponsorDetail(pdfData.getSponsor().getSponsorCode(), pdfData.getSponsor().getSponsorName(), pdfData.getSponsor().getAcronym()), Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));
			} else {
				proposalDetailsMutipleColumnTable.addCell(getModifiedGeneralDetailsCell("Sponsor: ", "", Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));
			}
			proposalDetailsMutipleColumnTable
					.addCell(getModifiedGeneralDetailsCell("Sponsor Proposal Id: ", pdfData.getSponsorProposalNumber(),
							Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));
			String startDate = commonService.convertDateFormatBasedOnTimeZone(pdfData.getStartDate().getTime(),Constants.DEFAULT_DATE_FORMAT);
			proposalDetailsMutipleColumnTable.addCell(getModifiedGeneralDetailsCell("Project Start Date: ", startDate,
					Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));
			if (pdfData.getEndDate() != null) {
				String endDate = commonService.convertDateFormatBasedOnTimeZone(pdfData.getEndDate().getTime(),Constants.DEFAULT_DATE_FORMAT);
				proposalDetailsMutipleColumnTable.addCell(getModifiedGeneralDetailsCell("Project End Date: ", endDate,
						Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));
			} else {
				proposalDetailsMutipleColumnTable.addCell(getModifiedGeneralDetailsCell("Project End Date: ", "",
						Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));
			}
			proposalDataTable.addCell(getModifiedGeneralDetailsCell("Duration: ", pdfData.getDuration() != null ? pdfData.getDuration() : "", Element.ALIGN_MIDDLE,
					Element.ALIGN_LEFT, bodyFont, headFont));
			if (pdfData.getAwardType() != null) {
				proposalDetailsMutipleColumnTable.addCell(getModifiedGeneralDetailsCell("Anticipated Award Type: ",
						pdfData.getAwardType().getDescription(), Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont,
						headFont));
			} else {
				proposalDetailsMutipleColumnTable.addCell(getModifiedGeneralDetailsCell("Anticipated Award Type: ", "",
						Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));
			}
			proposalDetailsMutipleColumnTable.addCell(getModifiedGeneralDetailsCell("Master Proposal #: ",
					pdfData.getBaseProposalNumber(), Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));
			proposalDetailsMutipleColumnTable.addCell(getModifiedGeneralDetailsCell("Program Announcement Number: ",
					pdfData.getProgramAnnouncementNumber(), Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont,
					headFont));
			proposalDetailsMutipleColumnTable.addCell(getModifiedGeneralDetailsCell("CDFA Number: ",
					pdfData.getCfdaNumber(), Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));
			if (pdfData.getPrimeSponsor() != null) {
				proposalDetailsMutipleColumnTable.addCell(getModifiedGeneralDetailsCell("Prime Sponsor: ", commonService.getSponsorFormatBySponsorDetail(pdfData.getPrimeSponsor().getSponsorCode(), pdfData.getPrimeSponsor().getSponsorName(), pdfData.getPrimeSponsor().getAcronym()) , Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));
			} else {
				proposalDetailsMutipleColumnTable.addCell(getModifiedGeneralDetailsCell("Prime Sponsor: ", "", Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));
			}
			if (pdfData.getSubmissionDate() != null) {
				String sponsorDeadlineDate = commonService.convertDateFormatBasedOnTimeZone(pdfData.getSubmissionDate().getTime(),Constants.DEFAULT_DATE_FORMAT);
				proposalDetailsMutipleColumnTable.addCell(getModifiedGeneralDetailsCell("Sponsor Deadline: ",
						sponsorDeadlineDate, Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));
			} else {
				proposalDetailsMutipleColumnTable.addCell(getModifiedGeneralDetailsCell("Sponsor Deadline: ", "",
						Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));
			}
			if (!pdfData.getProposalKeywords().isEmpty()) {
				StringBuilder keywords = new StringBuilder();
				for (ProposalKeyword proposalKeyword : pdfData.getProposalKeywords()) {
					if (pdfData.getProposalKeywords().size() != pdfData.getProposalKeywords().indexOf(proposalKeyword)
							+ 1) {
						if (proposalKeyword.getScienceKeyword() != null) {
							keywords.append(proposalKeyword.getScienceKeyword().getDescription() + " , ");
						} else {
							keywords.append(proposalKeyword.getKeyword() + " , ");
						}
					} else {
						if (proposalKeyword.getScienceKeyword() != null) {
							keywords.append(proposalKeyword.getScienceKeyword().getDescription());
						} else {
							keywords.append(proposalKeyword.getKeyword());
						}
					}
				}
				proposalDataTable.addCell(getModifiedGeneralDetailsCell("Keywords of Research: ", keywords.toString(),
						Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));
			} else {
				proposalDataTable.addCell(getModifiedGeneralDetailsCell("Keywords of Research: ", "",
						Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));
			}
			proposalDataTable.addCell(getModifiedGeneralDetailsCell("Project Abstract: ",
					pdfData.getAbstractDescription(), Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));

			document.add(proposalDetailsTable);
			document.add(proposalDetailsMutipleColumnTable);
			document.add(proposalDataTable);

			// Grant call Details
			if (pdfData.getGrantCallId() != null) {
				GrantCall grantCall = grantCallDao.fetchGrantCallById(pdfData.getGrantCallId());
				PdfPTable grantCallHeadingTable = new PdfPTable(1);
				setTableStyle(grantCallHeadingTable);

				PdfPCell grantCallCell = new PdfPCell(new Phrase("Grant Call Details", subHeadFont));
				setTableHeadingStyle(grantCallCell);
				grantCallHeadingTable.addCell(grantCallCell);
				document.add(grantCallHeadingTable);

				PdfPTable grantCallTable = new PdfPTable(7);
				grantCallTable.setWidthPercentage(100);
				grantCallTable.setWidths(new float[] { 1, 1.5f, 2, 2, 2, 2, 1.5f });
				grantCallTable.setSpacingBefore(5f);
				grantCallTable.setSpacingAfter(10f);

				grantCallTable.addCell(getBudgetSummaryTableHeaderCell("Grant Call Id", Element.ALIGN_MIDDLE,
						Element.ALIGN_CENTER, headFont));
				grantCallTable.addCell(getBudgetSummaryTableHeaderCell("Type of Grant", Element.ALIGN_MIDDLE,
						Element.ALIGN_CENTER, headFont));
				grantCallTable.addCell(getBudgetSummaryTableHeaderCell("Name of Grant Call", Element.ALIGN_MIDDLE,
						Element.ALIGN_CENTER, headFont));
				grantCallTable.addCell(getBudgetSummaryTableHeaderCell("Name of Funding Agency", Element.ALIGN_MIDDLE,
						Element.ALIGN_CENTER, headFont));
				grantCallTable.addCell(getBudgetSummaryTableHeaderCell("Name of Funding Scheme", Element.ALIGN_MIDDLE,
						Element.ALIGN_CENTER, headFont));
				grantCallTable.addCell(getBudgetSummaryTableHeaderCell("Grant Call Theme", Element.ALIGN_MIDDLE,
						Element.ALIGN_CENTER, headFont));
				grantCallTable.addCell(getBudgetSummaryTableHeaderCell("Closing Date", Element.ALIGN_MIDDLE,
						Element.ALIGN_CENTER, headFont));
				grantCallTable.addCell(getBudgetSummaryTableCell(pdfData.getGrantCallId().toString(),
						Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
				grantCallTable.addCell(getBudgetSummaryTableCell(pdfData.getGrantCallType().getDescription(),
						Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
				grantCallTable.addCell(getBudgetSummaryTableCell(grantCall.getGrantCallName(),
						Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
				grantCallTable.addCell(getBudgetSummaryTableCell(grantCall.getSponsor().getSponsorName(),
						Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
				if (grantCall.getSponsorFundingScheme() != null) {
					grantCallTable.addCell(
							getBudgetSummaryTableCell(grantCall.getSponsorFundingScheme().getDescription(),
									Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
				} else {
					grantCallTable.addCell(
							getBudgetSummaryTableCell("", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
				}
				grantCallTable.addCell(getBudgetSummaryTableCell(grantCall.getGrantTheme(),
						Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
				if (grantCall.getClosingDate() != null) {
					String grantCallEndDate = commonService.convertDateFormatBasedOnTimeZone(grantCall.getClosingDate().getTime(),Constants.DEFAULT_DATE_FORMAT);
					grantCallTable.addCell(getBudgetSummaryTableCell(grantCallEndDate, Element.ALIGN_MIDDLE,
							Element.ALIGN_CENTER, bodyFont));
				} else {
					grantCallTable.addCell(
							getBudgetSummaryTableCell("", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
				}
				document.add(grantCallTable);
			}
			// Key Personnel Details
			PdfPTable projectPersonsTable = new PdfPTable(1);
			setTableStyle(projectPersonsTable);

			PdfPCell projectPersonsHeadingCell = new PdfPCell(new Phrase("Key Personnel", subHeadFont));
			setTableHeadingStyle(projectPersonsHeadingCell);
			projectPersonsTable.addCell(projectPersonsHeadingCell);
			document.add(projectPersonsTable);

			PdfPTable personTable = new PdfPTable(4);
			personTable.setWidthPercentage(100);
			personTable.setWidths(new float[] { 3.5f, 2.5f, 1.5f, 4.5f });
			personTable.setSpacingBefore(5f);
			personTable.setSpacingAfter(10f);
			personTable.getDefaultCell().setFixedHeight(10);
			personTable.addCell(getTableHeaderCell("Name", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, headFont));
			personTable.addCell(getTableHeaderCell("Role", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, headFont));
			personTable.addCell(getTableHeaderCell("% of Effort", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, headFont));
			personTable.addCell(getTableHeaderCell("Department", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, headFont));
			for (ProposalPerson persons : pdfData.getProposalPersons()) {
				personTable.addCell(getTableCell(persons.getFullName(), Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
				personTable.addCell(getTableCell(persons.getProposalPersonRole().getDescription(),
						Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
				if (persons.getPercentageOfEffort() != null) {
					personTable.addCell(getTableCell(persons.getPercentageOfEffort().toString(), Element.ALIGN_MIDDLE,
							Element.ALIGN_CENTER, bodyFont));
				} else {
					personTable.addCell(getTableCell("", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
				}
				if (!persons.getUnits().isEmpty()) {
					StringBuilder units = new StringBuilder();
					for (ProposalPersonUnit unit : persons.getUnits()) {
						if (persons.getUnits().size() - 1 < persons.getUnits().indexOf(unit)) {
							units.append(unit.getUnit().getUnitName() + " , ");
						} else {
							units.append(unit.getUnit().getUnitName());
						}
						personTable.addCell(getTableCell(units.toString(), Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
					}
				} else {
					personTable.addCell(getTableCell("", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
				}
			}
			document.add(personTable);
			// Project team details
			List<ProposalProjectTeam> proposalProjectTeams = proposalModuleDao.fetchProposalProjectTeamBasedOnProposalId(pdfData.getProposalId());
			if (!proposalProjectTeams.isEmpty()) {
				PdfPTable projectTeamHeaderTable = new PdfPTable(1);
				setTableStyle(projectTeamHeaderTable);
				PdfPCell projectTeamHeadingCell = new PdfPCell(new Phrase("Project Team", subHeadFont));
				setTableHeadingStyle(projectTeamHeadingCell);
				projectTeamHeaderTable.addCell(projectTeamHeadingCell);
				document.add(projectTeamHeaderTable);
				PdfPTable projectTeamTable = new PdfPTable(5);
				projectTeamTable.setWidthPercentage(100);
				projectTeamTable.setWidths(new float[] { 2.5f, 1.5f, 4, 2, 2 });
				projectTeamTable.setSpacingBefore(5f);
				projectTeamTable.setSpacingAfter(10f);
				projectTeamTable.addCell(getTableHeaderCell("Name", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, headFont));
				projectTeamTable
						.addCell(getTableHeaderCell("Role", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, headFont));
				projectTeamTable.addCell(
						getTableHeaderCell("Period of Active", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, headFont));
				projectTeamTable.addCell(
						getTableHeaderCell("% Charged", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, headFont));
				projectTeamTable
						.addCell(getTableHeaderCell("Active", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, headFont));
				for (ProposalProjectTeam proposalProjectTeam : proposalProjectTeams) {
					projectTeamTable.addCell(getTableCell(proposalProjectTeam.getFullName(), Element.ALIGN_MIDDLE,
							Element.ALIGN_CENTER, bodyFont));
					projectTeamTable.addCell(getTableCell(proposalProjectTeam.getProjectRole(), Element.ALIGN_MIDDLE,
							Element.ALIGN_CENTER, bodyFont));
					projectTeamTable
							.addCell(getTableCell(
									commonService.convertDateFormatBasedOnTimeZone(proposalProjectTeam.getStartDate().getTime(),Constants.DEFAULT_DATE_FORMAT)
											+ " to "
											+ commonService.convertDateFormatBasedOnTimeZone(
													proposalProjectTeam.getEndDate().getTime(),Constants.DEFAULT_DATE_FORMAT),
									Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
					if (proposalProjectTeam.getPercentageCharged() != null) {
						projectTeamTable.addCell(getTableCell(proposalProjectTeam.getPercentageCharged().toString(), Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
					} else {
						projectTeamTable.addCell(getTableCell("0.00", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
					}
					if (proposalProjectTeam.getIsActive().equals("Y")) {
						projectTeamTable
								.addCell(getTableCell("Yes", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
					} else {
						projectTeamTable
								.addCell(getTableCell("No", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
					}
				}
				document.add(projectTeamTable);
			}
			// Special Review details
			List<ProposalSpecialReview> proposalSpecialReviews = proposalModuleDao.fetchProposalSpecialReviewBasedOnProposalId(pdfData.getProposalId());
			if (!proposalSpecialReviews.isEmpty()) {
				PdfPTable specialReviewHeaderTable = new PdfPTable(1);
				setTableStyle(specialReviewHeaderTable);

				PdfPCell specialReviewHeadingCell = new PdfPCell(new Phrase("Special Review", subHeadFont));
				setTableHeadingStyle(specialReviewHeadingCell);
				specialReviewHeaderTable.addCell(specialReviewHeadingCell);
				document.add(specialReviewHeaderTable);

				PdfPTable specialReviewTable = new PdfPTable(7);
				specialReviewTable.setWidthPercentage(100);
				specialReviewTable.setWidths(new float[] { 1.8f, 1.8f, 1.8f, 1.8f, 1.8f, 1.8f, 1.2f });
				specialReviewTable.setSpacingBefore(5f);
				specialReviewTable.setSpacingAfter(10f);
				specialReviewTable.addCell(
						getTableHeaderCell("Review Type", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, headFont));
				specialReviewTable.addCell(
						getTableHeaderCell("Approval Status", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, headFont));
				specialReviewTable.addCell(
						getTableHeaderCell("Protocol Number", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, headFont));
				specialReviewTable.addCell(
						getTableHeaderCell("Application Date", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, headFont));
				specialReviewTable.addCell(
						getTableHeaderCell("Approval Date", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, headFont));
				specialReviewTable.addCell(
						getTableHeaderCell("Expiration Date", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, headFont));
				specialReviewTable
						.addCell(getTableHeaderCell("Comment", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, headFont));
				for (ProposalSpecialReview proposalSpecialReview : proposalSpecialReviews) {
					if (proposalSpecialReview.getSpecialReviewType() != null) {
						specialReviewTable
								.addCell(getTableCell(proposalSpecialReview.getSpecialReviewType().getDescription(),
										Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
					} else {
						specialReviewTable
								.addCell(getTableCell("", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
					}
					if (proposalSpecialReview.getApprovalType() != null) {
						specialReviewTable
								.addCell(getTableCell(proposalSpecialReview.getApprovalType().getDescription(),
										Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
					} else {
						specialReviewTable
								.addCell(getTableCell("", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
					}
					specialReviewTable.addCell(getTableCell(proposalSpecialReview.getProtocolNumber(),
							Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
					if (proposalSpecialReview.getApplicationDate() != null) {
						specialReviewTable.addCell(getTableCell(
								commonService.convertDateFormatBasedOnTimeZone(proposalSpecialReview.getApplicationDate().getTime(),Constants.DEFAULT_DATE_FORMAT),
								Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
					} else {
						specialReviewTable
								.addCell(getTableCell("", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
					}
					if (proposalSpecialReview.getApprovalDate() != null) {
						specialReviewTable.addCell(getTableCell(
								commonService.convertDateFormatBasedOnTimeZone(proposalSpecialReview.getApprovalDate().getTime(),Constants.DEFAULT_DATE_FORMAT),
								Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
					} else {
						specialReviewTable
								.addCell(getTableCell("", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
					}
					if (proposalSpecialReview.getExpirationDate() != null) {
						specialReviewTable.addCell(getTableCell(
								commonService.convertDateFormatBasedOnTimeZone(proposalSpecialReview.getExpirationDate().getTime(),Constants.DEFAULT_DATE_FORMAT),
								Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
					} else {
						specialReviewTable
								.addCell(getTableCell("", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
					}
					specialReviewTable.addCell(getTableCell(proposalSpecialReview.getComments(), Element.ALIGN_MIDDLE,
							Element.ALIGN_CENTER, bodyFont));
				}
				document.add(specialReviewTable);
			}

			// Area of Research Details
			List<ProposalResearchArea> proposalResearchAreas = proposalModuleDao.fetchProposalResearchAreaBasedOnProposalId(pdfData.getProposalId());
			if (!proposalResearchAreas.isEmpty()) {
				PdfPTable areaOfResearchHeaderTable = new PdfPTable(1);
				setTableStyle(areaOfResearchHeaderTable);

				PdfPCell areaOfResearchCell = new PdfPCell(new Phrase("Area of Research", subHeadFont));
				setTableHeadingStyle(areaOfResearchCell);
				areaOfResearchHeaderTable.addCell(areaOfResearchCell);
				document.add(areaOfResearchHeaderTable);

				PdfPTable areaOfResearchTable = new PdfPTable(4);
				areaOfResearchTable.setWidthPercentage(100);
				areaOfResearchTable.setWidths(new int[] { 2, 4, 4, 4 });
				areaOfResearchTable.setSpacingBefore(5f);
				areaOfResearchTable.setSpacingAfter(10f);

				areaOfResearchTable
						.addCell(getTableHeaderCell("Code", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, headFont));
				areaOfResearchTable.addCell(
						getTableHeaderCell("Research Area", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, headFont));
				areaOfResearchTable
						.addCell(getTableHeaderCell("Area", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, headFont));
				areaOfResearchTable.addCell(
						getTableHeaderCell("Sub Area", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, headFont));

				for (ProposalResearchArea researchArea : proposalResearchAreas) {
					if (researchArea.getResearchTypeArea() != null) {
						areaOfResearchTable.addCell(getTableCell(researchArea.getResearchTypeAreaCode(),
								Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
						areaOfResearchTable
								.addCell(getTableCell(researchArea.getResearchType().getDescription(),
										Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
						areaOfResearchTable.addCell(getTableCell(researchArea.getResearchTypeArea().getDescription(),
								Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
						if (researchArea.getResearchTypeSubArea() != null) {
							areaOfResearchTable.addCell(getTableCell(researchArea.getResearchTypeSubArea().getDescription(),
									Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
						} else {
							areaOfResearchTable
									.addCell(getTableCell("", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
						}
					}
				}
				PdfPCell moreInformationHeaderCell = new PdfPCell(new Phrase("More Information", headFont));
				setMoreInformationRowStyle(moreInformationHeaderCell, 4, Element.ALIGN_MIDDLE, Element.ALIGN_LEFT);
				moreInformationHeaderCell.setBackgroundColor(new BaseColor(230, 229, 229));
				moreInformationHeaderCell.setPadding(5);
				areaOfResearchTable.addCell(moreInformationHeaderCell);
				PdfPCell moreInformationDataCell1 = new PdfPCell(
						new Phrase(pdfData.getResearchDescription(), bodyFont));
				setMoreInformationRowStyle(moreInformationDataCell1, 4, Element.ALIGN_MIDDLE, Element.ALIGN_LEFT);
				moreInformationDataCell1.setPadding(5);
				areaOfResearchTable.addCell(moreInformationDataCell1);
				PdfPCell moreInformationDataCell2 = new PdfPCell(
						new Phrase(pdfData.getMultiDisciplinaryDescription(), bodyFont));
				setMoreInformationRowStyle(moreInformationDataCell2, 4, Element.ALIGN_MIDDLE, Element.ALIGN_LEFT);
				moreInformationDataCell2.setPadding(5);
				areaOfResearchTable.addCell(moreInformationDataCell2);
				document.add(areaOfResearchTable);
			}
			// Funding Support Details
			List<ProposalSponsor> proposalSponsors = proposalModuleDao.fetchProposalSponsorBasedOnProposalId(pdfData.getProposalId());
			if (!proposalSponsors.isEmpty()) {
				PdfPTable proposalSponsorTable = new PdfPTable(1);
				setTableStyle(proposalSponsorTable);

				PdfPCell proposalSponsorHeadingCell = new PdfPCell(new Phrase("Funding Support", subHeadFont));
				setTableHeadingStyle(proposalSponsorHeadingCell);
				proposalSponsorTable.addCell(proposalSponsorHeadingCell);
				document.add(proposalSponsorTable);

				PdfPTable sponsorTable = new PdfPTable(7);
				sponsorTable.setWidthPercentage(100);
				sponsorTable.setWidths(new float[] { 2, 2, 2, 1.5f, 1.5f, 1.5f, 1.5f });
				sponsorTable.setSpacingBefore(5f);
				sponsorTable.setSpacingAfter(10f);
				sponsorTable.getDefaultCell().setBorder(Rectangle.NO_BORDER);
				sponsorTable.addCell(getBudgetSummaryTableHeaderCell("Start Date", Element.ALIGN_MIDDLE,
						Element.ALIGN_CENTER, headFont));
				sponsorTable.addCell(getBudgetSummaryTableHeaderCell("End Date", Element.ALIGN_MIDDLE,
						Element.ALIGN_CENTER, headFont));
				for (ProposalSponsor sponsor : proposalSponsors) {
					if (sponsor.getStartDate() != null) {
						String fundingSupportStartDate = commonService.convertDateFormatBasedOnTimeZone(
								sponsor.getStartDate().getTime(),Constants.DEFAULT_DATE_FORMAT);
						sponsorTable.addCell(getBudgetSummaryTableCell(fundingSupportStartDate, Element.ALIGN_MIDDLE,
								Element.ALIGN_CENTER, bodyFont));
					} else {
						sponsorTable.addCell(
								getBudgetSummaryTableCell("", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
					}
					if (sponsor.getEndDate() != null) {
						String fundingSupportEndDate = commonService.convertDateFormatBasedOnTimeZone(sponsor.getEndDate().getTime(),Constants.DEFAULT_DATE_FORMAT);
						sponsorTable.addCell(getBudgetSummaryTableCell(fundingSupportEndDate, Element.ALIGN_MIDDLE,
								Element.ALIGN_CENTER, bodyFont));
					} else {
						sponsorTable.addCell(
								getBudgetSummaryTableCell("", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
					}
					sponsorTable.addCell(getBudgetSummaryTableCell(sponsor.getAmount().toString(),
							Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
				}
				document.add(sponsorTable);
			}
			// Budget Version Details
			List<BudgetHeader> budgetHeaders = proposalModuleDao.fetchBudgetHeaderBasedOnProposalId(pdfData.getProposalId());
			if (budgetHeaders != null && !budgetHeaders.isEmpty()) {
				PdfPTable budgetSummaryTable = new PdfPTable(1);
				setTableStyle(budgetSummaryTable);

				PdfPCell budgetSummaryHeadingCell = new PdfPCell(new Phrase(BUDGET_SUMMARY, subHeadFont));
				setTableHeadingStyle(budgetSummaryHeadingCell);
				budgetSummaryTable.addCell(budgetSummaryHeadingCell);
				document.add(budgetSummaryTable);

				PdfPTable budgetTable = new PdfPTable(8);
				budgetTable.setWidthPercentage(100);
				budgetTable.setWidths(new float[] { 1.2f, 1.7f, 1.7f, 1.5f, 1.5f, 1.5f, 1.5f, 1.4f });
				budgetTable.setSpacingBefore(5f);
				budgetTable.setSpacingAfter(2f);
				budgetTable.addCell(getBudgetSummaryTableHeaderCell("Version #", Element.ALIGN_MIDDLE,
						Element.ALIGN_CENTER, headFont));
				budgetTable.addCell(getBudgetSummaryTableHeaderCell("Budget Start", Element.ALIGN_MIDDLE,
						Element.ALIGN_CENTER, headFont));
				budgetTable.addCell(getBudgetSummaryTableHeaderCell("Budget End", Element.ALIGN_MIDDLE,
						Element.ALIGN_CENTER, headFont));
				budgetTable.addCell(getBudgetSummaryTableHeaderCell("Total Cost", Element.ALIGN_MIDDLE,
						Element.ALIGN_CENTER, headFont));
				budgetTable.addCell(getBudgetSummaryTableHeaderCell("Direct Cost", Element.ALIGN_MIDDLE,
						Element.ALIGN_CENTER, headFont));
				budgetTable.addCell(getBudgetSummaryTableHeaderCell("Indirect Cost", Element.ALIGN_MIDDLE,
						Element.ALIGN_CENTER, headFont));
				budgetTable.addCell(getBudgetSummaryTableHeaderCell("Status", Element.ALIGN_MIDDLE,
						Element.ALIGN_CENTER, headFont));
				budgetTable.addCell(getBudgetSummaryTableHeaderCell("Description", Element.ALIGN_MIDDLE,
						Element.ALIGN_CENTER, headFont));
				for (BudgetHeader budgetHeader : budgetHeaders) {
					budgetTable.addCell(getBudgetSummaryTableCell(budgetHeader.getVersionNumber().toString(),
							Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
					if (budgetHeader.getStartDate() != null) {
						String budgetStartDate = commonService.convertDateFormatBasedOnTimeZone(budgetHeader.getStartDate().getTime(),Constants.DEFAULT_DATE_FORMAT);
						budgetTable.addCell(getBudgetSummaryTableCell(budgetStartDate, Element.ALIGN_MIDDLE,
								Element.ALIGN_CENTER, bodyFont));
					} else {
						budgetTable.addCell(
								getBudgetSummaryTableCell("", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
					}
					if (budgetHeader.getEndDate() != null) {
						String budgetEndDate = commonService.convertDateFormatBasedOnTimeZone(budgetHeader.getEndDate().getTime(),Constants.DEFAULT_DATE_FORMAT);
						budgetTable.addCell(getBudgetSummaryTableCell(budgetEndDate, Element.ALIGN_MIDDLE,
								Element.ALIGN_CENTER, bodyFont));
					} else {
						budgetTable.addCell(
								getBudgetSummaryTableCell("", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
					}
					if (budgetHeader.getTotalCost() == null) {
						budgetTable.addCell(getBudgetSummaryTableCell(COST, Element.ALIGN_MIDDLE,
								Element.ALIGN_CENTER, bodyFont));
					} else {
						budgetTable.addCell(getBudgetSummaryTableCell(Constants.DOLLAR_SYMBOL + budgetHeader.getTotalCost().toString(),
								Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
					}
					if (budgetHeader.getTotalDirectCost() == null) {
						budgetTable.addCell(getBudgetSummaryTableCell(COST, Element.ALIGN_MIDDLE,
								Element.ALIGN_CENTER, bodyFont));
					} else {
						budgetTable
								.addCell(getBudgetSummaryTableCell(Constants.DOLLAR_SYMBOL + budgetHeader.getTotalDirectCost().toString(),
										Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
					}
					if (budgetHeader.getTotalIndirectCost() == null) {
						budgetTable.addCell(getBudgetSummaryTableCell(COST, Element.ALIGN_MIDDLE,
								Element.ALIGN_CENTER, bodyFont));
					} else {
						budgetTable
								.addCell(getBudgetSummaryTableCell(Constants.DOLLAR_SYMBOL + budgetHeader.getTotalIndirectCost().toString(),
										Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
					}
					budgetTable.addCell(getBudgetSummaryTableCell(budgetHeader.getBudgetStatus().getDescription(),
							Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
					budgetTable.addCell(getBudgetSummaryTableCell(budgetHeader.getComments(), Element.ALIGN_MIDDLE,
							Element.ALIGN_CENTER, bodyFont));
				}
				document.add(budgetTable);
			}

			// Questionnaire Details
			PdfPTable questionnaireHeadingTable = new PdfPTable(1);
			setTableStyle(questionnaireHeadingTable);

			PdfPCell questionnaireHeadingCell = new PdfPCell(new Phrase("Questionnaire", subHeadFont));
			setTableHeadingStyle(questionnaireHeadingCell);
			questionnaireHeadingTable.setSpacingAfter(5);
			questionnaireHeadingTable.setSpacingBefore(5);
			questionnaireHeadingTable.addCell(questionnaireHeadingCell);

			document.add(questionnaireHeadingTable);
			if (!questionnaireList.isEmpty()) {
				for (QuestionnaireDataBus questionnaireDataBus : questionnaireList) {
					if (questionnaireDataBus.getQuestionnaire() != null) {
						PdfPTable questionnaireTable = new PdfPTable(1);
						setTableStyle(questionnaireTable);
						StringBuilder questionnaireStatus = new StringBuilder(0);
						if (questionnaireDataBus.getQuestionnaireCompleteFlag().equals("Y")) {
							questionnaireStatus.append("Complete");
						} else {
							questionnaireStatus.append("In Complete");
						}
						PdfPCell questionnaireHeaderCell = new PdfPCell(
								new Phrase(questionnaireDataBus.getQuestionnaireName() + " ("
										+ questionnaireStatus.toString() + ")", questionnaireHeadFont));
						setRowStyle(questionnaireHeaderCell, Element.ALIGN_MIDDLE, Element.ALIGN_LEFT);
						questionnaireHeaderCell.setPadding(5);
						questionnaireHeaderCell.setBackgroundColor(new BaseColor(230, 229, 229));
						questionnaireTable.addCell(questionnaireHeaderCell);
						if (!questionnaireDataBus.getQuestionnaire().getQuestions().isEmpty()) {
							for (HashMap<String, Object> question : questionnaireDataBus.getQuestionnaire()
									.getQuestions()) {
								HashMap<String, Object> answerMap = (HashMap<String, Object>) question.get("ANSWERS");
								if (answerMap.get("1") != null && (!answerMap.get("1").toString().isEmpty())) {
										PdfPCell questionCell = new PdfPCell(new Paragraph(
												question.get("QUESTION").toString(), questionnaireSubHeadFont));
										setRowStyle(questionCell, Element.ALIGN_MIDDLE, Element.ALIGN_LEFT);
										questionCell.setPadding(5);
										questionnaireHeaderCell.setPaddingLeft(7);
										questionCell.setBorderWidthBottom(0);
										questionCell.setBorderColor(new BaseColor(255, 255, 255));
										questionnaireTable.addCell(questionCell);

										PdfPCell answerCell = new PdfPCell(
												new Phrase(answerMap.get("1").toString(), questionnaireBodyFont));
										setRowStyle(answerCell, Element.ALIGN_MIDDLE, Element.ALIGN_LEFT);
										answerCell.setPadding(5);
										answerCell.setBorderColor(new BaseColor(255, 255, 255));
										questionnaireHeaderCell.setPaddingLeft(20);
										questionnaireTable.addCell(answerCell);
								}
							}
						}
						document.add(questionnaireTable);
					}
				}
			}
			document.close();
		} catch (Exception ex) {
			Logger.getLogger(ProposalPrintServiceImpl.class.getName()).log(Level.SEVERE, null, ex);
		}
		return new ByteArrayInputStream(out.toByteArray());
	}

	@Override
	public ByteArrayInputStream proposalBudgetPdfReport(Integer proposalId, Integer budgetId)
			throws DocumentException, ParseException {
		Proposal budgetPdfData = proposalDao.fetchProposalById(proposalId);
		Document document = new Document();
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		PdfWriter.getInstance(document, out);
		document.open();
		Font pdfTitleFont = FontFactory.getFont(FontFactory.HELVETICA_BOLD);
		pdfTitleFont.setSize(13);
		Font headFont = FontFactory.getFont(FontFactory.HELVETICA_BOLD);
		headFont.setSize(10);
		Font bodyFont = FontFactory.getFont(FontFactory.HELVETICA);
		bodyFont.setSize(10);
		Font subHeadFont = FontFactory.getFont(FontFactory.HELVETICA_BOLDOBLIQUE);
		subHeadFont.setSize(11);

		try {
			Paragraph pdfHeading = new Paragraph(BUDGET_SUMMARY, pdfTitleFont);
			pdfHeading.setAlignment(Element.ALIGN_CENTER);
			pdfHeading.setSpacingAfter(8f);
			document.add(pdfHeading);
			Paragraph p1 = new Paragraph(PROPOSAL_OVERVIEW, subHeadFont);
			p1.setSpacingAfter(2);
			document.add(p1);

			PdfPTable proposalDetailsTable = new PdfPTable(2);
			proposalDetailsTable.setWidthPercentage(100);
			proposalDetailsTable.setWidths(new int[] { 6, 6 });
			proposalDetailsTable.setSpacingAfter(2f);
			PdfPTable proposalDetailsMutipleColumnTable = new PdfPTable(4);
			proposalDetailsMutipleColumnTable.setSpacingBefore(0);
			proposalDetailsMutipleColumnTable.setWidthPercentage(100);
			proposalDetailsMutipleColumnTable.setWidths(new int[] { 3, 3, 2, 4 });
			proposalDetailsMutipleColumnTable.setSpacingAfter(5f);

			proposalDetailsTable
					.addCell(getGeneralDetailsCell(TITLE, Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont));
			proposalDetailsTable.addCell(getGeneralDetailsCell(budgetPdfData.getTitle(), Element.ALIGN_MIDDLE,
					Element.ALIGN_LEFT, bodyFont));
			proposalDetailsTable.addCell(getGeneralDetailsCell(PRINCIPAL_INVESTIGATOR, Element.ALIGN_MIDDLE,
					Element.ALIGN_LEFT, bodyFont));
			if (budgetPdfData.getInvestigator() != null) {
				proposalDetailsTable.addCell(getGeneralDetailsCell(budgetPdfData.getInvestigator().getFullName(),
						Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont));
			}
			proposalDetailsTable.addCell(
					getGeneralDetailsCell(LEAD_UNIT, Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont));
			proposalDetailsTable.addCell(getGeneralDetailsCell(budgetPdfData.getHomeUnitName(), Element.ALIGN_MIDDLE,
					Element.ALIGN_LEFT, bodyFont));
			proposalDetailsMutipleColumnTable
					.addCell(getGeneralDetailsCell("Category: ", Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont));
			proposalDetailsMutipleColumnTable
					.addCell(getGeneralDetailsCell(budgetPdfData.getActivityType().getDescription(),
							Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont));
			proposalDetailsMutipleColumnTable.addCell(
					getGeneralDetailsCell(START_DATE, Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont));
			proposalDetailsMutipleColumnTable.addCell(
					getGeneralDetailsCell(dateTimeService.convertToSqlDate(budgetPdfData.getStartDate().toString()).toString(),
							Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont));
			proposalDetailsMutipleColumnTable
					.addCell(getGeneralDetailsCell(END_DATE, Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont));
			proposalDetailsMutipleColumnTable.addCell(
					getGeneralDetailsCell(dateTimeService.convertToSqlDate(budgetPdfData.getEndDate().toString()).toString(),
							Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont));
			document.add(proposalDetailsTable);
			document.add(proposalDetailsMutipleColumnTable);

			Paragraph p2 = new Paragraph("Budget Overview", subHeadFont);
			p1.setSpacingAfter(2);
			document.add(p2);

			List<BudgetHeader> budgetHeaders = proposalModuleDao.fetchBudgetHeaderBasedOnProposalId(budgetPdfData.getProposalId());
			if (budgetHeaders != null && !budgetHeaders.isEmpty()) {
				for (BudgetHeader budgetHeader : budgetHeaders) {
					if (budgetHeader.getBudgetId().equals(budgetId)) {
						PdfPTable budgetDetailsTable = new PdfPTable(4);
						budgetDetailsTable.setWidthPercentage(100);
						budgetDetailsTable.setWidths(new int[] { 3, 3, 2, 4 });
						budgetDetailsTable.setSpacingAfter(5f);

						budgetDetailsTable.addCell(getGeneralDetailsCell(START_DATE, Element.ALIGN_MIDDLE,
								Element.ALIGN_LEFT, bodyFont));
						budgetDetailsTable.addCell(getGeneralDetailsCell(
								dateTimeService.convertToSqlDate(budgetHeader.getStartDate().toString()).toString(),
								Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont));
						budgetDetailsTable.addCell(getGeneralDetailsCell(END_DATE, Element.ALIGN_MIDDLE,
								Element.ALIGN_LEFT, bodyFont));
						budgetDetailsTable.addCell(getGeneralDetailsCell(
								dateTimeService.convertToSqlDate(budgetHeader.getEndDate().toString()).toString(),
								Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont));
						budgetDetailsTable.addCell(getGeneralDetailsCell(DIRECT_COST, Element.ALIGN_MIDDLE,
								Element.ALIGN_LEFT, bodyFont));
						if (budgetHeader.getTotalDirectCost() == null) {
							budgetDetailsTable.addCell(getGeneralDetailsCell(COST, Element.ALIGN_MIDDLE,
									Element.ALIGN_LEFT, bodyFont));
						} else {
							budgetDetailsTable
									.addCell(getGeneralDetailsCell(budgetHeader.getTotalDirectCost().toString(),
											Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont));
						}
						budgetDetailsTable.addCell(getGeneralDetailsCell(INDIRECT_COST, Element.ALIGN_MIDDLE,
								Element.ALIGN_LEFT, bodyFont));
						if (budgetHeader.getTotalIndirectCost() == null) {
							budgetDetailsTable.addCell(getGeneralDetailsCell(COST, Element.ALIGN_MIDDLE,
									Element.ALIGN_LEFT, bodyFont));
						} else {
							budgetDetailsTable
									.addCell(getGeneralDetailsCell(budgetHeader.getTotalIndirectCost().toString(),
											Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont));
						}
						budgetDetailsTable.addCell(getGeneralDetailsCell(TOTAL_COST, Element.ALIGN_MIDDLE,
								Element.ALIGN_LEFT, bodyFont));
						if (budgetHeader.getTotalCost() == null) {
							budgetDetailsTable.addCell(getGeneralDetailsCell(COST, Element.ALIGN_MIDDLE,
									Element.ALIGN_LEFT, bodyFont));
						} else {
							budgetDetailsTable.addCell(getGeneralDetailsCell(budgetHeader.getTotalCost().toString(),
									Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont));
						}
						budgetDetailsTable.addCell(getGeneralDetailsCell(OH_BASE, Element.ALIGN_MIDDLE,
								Element.ALIGN_LEFT, bodyFont));
						if (budgetHeader.getRateType() != null) {
							budgetDetailsTable
									.addCell(getGeneralDetailsCell(budgetHeader.getRateType().getDescription(),
											Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont));
						} else {
							budgetDetailsTable.addCell(
									getGeneralDetailsCell("", Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont));
						}
						document.add(budgetDetailsTable);

						Paragraph p3 = new Paragraph("Periods And Total", subHeadFont);
						p1.setSpacingAfter(2);
						document.add(p3);

						PdfPTable periodsAndTotalTable = new PdfPTable(5);
						periodsAndTotalTable.setWidthPercentage(100);
						periodsAndTotalTable.setWidths(new int[] { 3, 3, 2, 2, 2 });
						periodsAndTotalTable.setSpacingBefore(10f);
						periodsAndTotalTable.setSpacingAfter(5f);

						periodsAndTotalTable.addCell(getTableCell("Period Start Date", Element.ALIGN_MIDDLE,
								Element.ALIGN_CENTER, headFont));
						periodsAndTotalTable.addCell(getTableCell("Period End Date", Element.ALIGN_MIDDLE,
								Element.ALIGN_CENTER, headFont));
						periodsAndTotalTable.addCell(
								getTableCell("Total Cost", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, headFont));
						periodsAndTotalTable.addCell(
								getTableCell("Direct Cost", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, headFont));
						periodsAndTotalTable.addCell(
								getTableCell("Indirect Cost", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, headFont));

						for (BudgetPeriod budgetPeriod : budgetHeader.getBudgetPeriods()) {
							periodsAndTotalTable.addCell(getTableCell(
									dateTimeService.convertToSqlDate(budgetPeriod.getStartDate().toString()).toString(),
									Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
							periodsAndTotalTable.addCell(
									getTableCell(dateTimeService.convertToSqlDate(budgetPeriod.getEndDate().toString()).toString(),
											Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
							if (budgetPeriod.getTotalCost() == null) {
								periodsAndTotalTable.addCell(
										getTableCell(COST, Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
							} else {
								periodsAndTotalTable.addCell(getTableCell(budgetPeriod.getTotalCost().toString(),
										Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
							}
							if (budgetPeriod.getTotalDirectCost() == null) {
								periodsAndTotalTable.addCell(
										getTableCell(COST, Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
							} else {
								periodsAndTotalTable.addCell(getTableCell(budgetPeriod.getTotalDirectCost().toString(),
										Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
							}
							if (budgetPeriod.getTotalIndirectCost() == null) {
								periodsAndTotalTable.addCell(
										getTableCell(COST, Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
							} else {
								periodsAndTotalTable
										.addCell(getTableCell(budgetPeriod.getTotalIndirectCost().toString(),
												Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
							}
						}
						periodsAndTotalTable
								.addCell(getTableCell("", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, headFont));
						periodsAndTotalTable.addCell(
								getTableCell("Total:", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, headFont));
						if (budgetHeader.getTotalCost() == null) {
							periodsAndTotalTable.addCell(
									getTableCell(COST, Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
						} else {
							periodsAndTotalTable.addCell(getTableCell(budgetHeader.getTotalCost().toString(),
									Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, headFont));
						}
						if (budgetHeader.getTotalDirectCost() == null) {
							periodsAndTotalTable.addCell(
									getTableCell(COST, Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
						} else {
							periodsAndTotalTable.addCell(getTableCell(budgetHeader.getTotalDirectCost().toString(),
									Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, headFont));
						}
						if (budgetHeader.getTotalIndirectCost() == null) {
							periodsAndTotalTable.addCell(
									getTableCell(COST, Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
						} else {
							periodsAndTotalTable.addCell(getTableCell(budgetHeader.getTotalIndirectCost().toString(),
									Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, headFont));
						}
						document.add(periodsAndTotalTable);

						Paragraph p4 = new Paragraph("Period details", subHeadFont);
						p4.setSpacingAfter(6);
						document.add(p4);
						Font periodFont = FontFactory.getFont(FontFactory.HELVETICA_BOLD);
						periodFont.setSize(10);
						for (BudgetPeriod budgetPeriod : budgetHeader.getBudgetPeriods()) {
							Paragraph p5 = new Paragraph("Period" + budgetPeriod.getBudgetPeriod(), periodFont);
							p5.setSpacingAfter(2);
							document.add(p5);
							PdfPTable periodDetailsTable = new PdfPTable(4);
							periodDetailsTable.setWidthPercentage(100);
							periodDetailsTable.setWidths(new int[] { 3, 3, 2, 4 });
							periodDetailsTable.setSpacingAfter(3f);

							periodDetailsTable.addCell(getGeneralDetailsCell("Period Start Date: ",
									Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont));
							periodDetailsTable.addCell(getGeneralDetailsCell(
									dateTimeService.convertToSqlDate(budgetPeriod.getStartDate().toString()).toString(),
									Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont));
							periodDetailsTable.addCell(getGeneralDetailsCell("Period End Date: ", Element.ALIGN_MIDDLE,
									Element.ALIGN_LEFT, bodyFont));
							periodDetailsTable.addCell(getGeneralDetailsCell(
									dateTimeService.convertToSqlDate(budgetPeriod.getEndDate().toString()).toString(),
									Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont));
							periodDetailsTable.addCell(getGeneralDetailsCell(DIRECT_COST, Element.ALIGN_MIDDLE,
									Element.ALIGN_LEFT, bodyFont));
							if (budgetPeriod.getTotalDirectCost() == null) {
								periodDetailsTable.addCell(getGeneralDetailsCell(COST, Element.ALIGN_MIDDLE,
										Element.ALIGN_LEFT, bodyFont));
							} else {
								periodDetailsTable
										.addCell(getGeneralDetailsCell(budgetPeriod.getTotalDirectCost().toString(),
												Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont));
							}
							periodDetailsTable.addCell(getGeneralDetailsCell(INDIRECT_COST, Element.ALIGN_MIDDLE,
									Element.ALIGN_LEFT, bodyFont));
							if (budgetPeriod.getTotalIndirectCost() == null) {
								periodDetailsTable.addCell(getGeneralDetailsCell(COST, Element.ALIGN_MIDDLE,
										Element.ALIGN_LEFT, bodyFont));
							} else {
								periodDetailsTable
										.addCell(getGeneralDetailsCell(budgetPeriod.getTotalIndirectCost().toString(),
												Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont));
							}
							periodDetailsTable.addCell(getGeneralDetailsCell(TOTAL_COST, Element.ALIGN_MIDDLE,
									Element.ALIGN_LEFT, bodyFont));
							if (budgetPeriod.getTotalCost() == null) {
								periodDetailsTable.addCell(getGeneralDetailsCell(COST, Element.ALIGN_MIDDLE,
										Element.ALIGN_LEFT, bodyFont));
							} else {
								periodDetailsTable.addCell(getGeneralDetailsCell(budgetPeriod.getTotalCost().toString(),
										Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont));
							}
							periodDetailsTable.addCell(
									getGeneralDetailsCell("", Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont));
							periodDetailsTable.addCell(
									getGeneralDetailsCell("", Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont));
							document.add(periodDetailsTable);
							List<BudgetDetail> budgetDetails = budgetPeriod.getBudgetDetails();
							if (!budgetDetails.isEmpty()) {
								PdfPTable lineItemTable = new PdfPTable(4);
								lineItemTable.setWidthPercentage(100);
								lineItemTable.setWidths(new int[] { 3, 3, 3, 3 });
								lineItemTable.setSpacingBefore(10f);
								lineItemTable.setSpacingAfter(5f);

								lineItemTable.addCell(getTableCell("Budget Category", Element.ALIGN_MIDDLE,
										Element.ALIGN_CENTER, headFont));
								lineItemTable.addCell(getTableCell("Cost Element", Element.ALIGN_MIDDLE,
										Element.ALIGN_CENTER, headFont));
								lineItemTable.addCell(getTableCell("Line Item Description", Element.ALIGN_MIDDLE,
										Element.ALIGN_CENTER, headFont));
								lineItemTable.addCell(getTableCell("Line Item Cost", Element.ALIGN_MIDDLE,
										Element.ALIGN_CENTER, headFont));

								for (BudgetDetail budgetDetail : budgetDetails) {
									lineItemTable
											.addCell(getTableCell(budgetDetail.getBudgetCategory().getDescription(),
													Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
									lineItemTable.addCell(getTableCell(budgetDetail.getCostElement().getDescription(),
											Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
									if (budgetDetail.getLineItemDescription() == null) {
										lineItemTable.addCell(getTableCell("", Element.ALIGN_MIDDLE,
												Element.ALIGN_CENTER, bodyFont));
									} else {
										lineItemTable.addCell(getTableCell(budgetDetail.getLineItemDescription(),
												Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
									}
									if (budgetDetail.getLineItemCost() == null) {
										lineItemTable.addCell(getTableCell(COST, Element.ALIGN_MIDDLE,
												Element.ALIGN_CENTER, bodyFont));
									} else {
										lineItemTable.addCell(getTableCell(budgetDetail.getLineItemCost().toString(),
												Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
									}
								}
								document.add(lineItemTable);
							}
						}
					}
				}
			}
			document.close();
		} catch (DocumentException ex) {
			Logger.getLogger(ProposalPrintServiceImpl.class.getName()).log(Level.SEVERE, null, ex);
		}
		return new ByteArrayInputStream(out.toByteArray());
	}

	public static PdfPCell getGeneralDetailsCell(String text, int verticalAlignment, int horizontalAlignment,
			Font bodyFont) {
		PdfPCell cell = new PdfPCell(new Phrase(text, bodyFont));
		cell.setPadding(5);
		cell.setVerticalAlignment(verticalAlignment);
		cell.setHorizontalAlignment(horizontalAlignment);
		cell.setBorder(Rectangle.NO_BORDER);
		return cell;
	}

	public static PdfPCell getTableCell(String text, int verticalAlignment, int horizontalAlignmentt, Font bodyFont) {
		PdfPCell cell = new PdfPCell(new Phrase(text, bodyFont));
		cell.setPadding(5);
		cell.setVerticalAlignment(verticalAlignment);
		cell.setHorizontalAlignment(horizontalAlignmentt);
		cell.setBorderColor(new BaseColor(230, 229, 229));
		return cell;
	}

	@Override
	public ByteArrayInputStream proposalBudgetDetailsPdfReport(Integer proposalId, Integer budgetId)
			throws DocumentException, ParseException {
		Proposal budgetPdfData = proposalDao.fetchProposalById(proposalId);
		Document document = new Document();
		int budgetPeriodLength = 0;
		List<BudgetHeader> budgetHeaders = proposalModuleDao.fetchBudgetHeaderBasedOnProposalId(budgetPdfData.getProposalId());
		if (budgetHeaders != null && !budgetHeaders.isEmpty()) {
			for (BudgetHeader budgetHeader : budgetHeaders) {
				List<BudgetPeriod> budgetPeriod =budgetHeader.getBudgetPeriods();
				if (budgetHeader.getBudgetId().equals(budgetId)) {
					budgetPeriodLength = budgetPeriod.size();
				}
			}
		}
		if (budgetPeriodLength > 3) {
			document.setPageSize(PageSize.A4.rotate());
		}
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		PdfWriter.getInstance(document, out);
		document.open();
		Font pdfTitleFont = FontFactory.getFont(FontFactory.HELVETICA_BOLD);
		pdfTitleFont.setSize(13);
		Font headFont = FontFactory.getFont(FontFactory.HELVETICA, 10, Font.BOLD);
		Font bodyFont = FontFactory.getFont(FontFactory.HELVETICA, 10);
		Font subHeadFont = FontFactory.getFont(FontFactory.HELVETICA_BOLDOBLIQUE);
		subHeadFont.setSize(11);
		subHeadFont.setColor(new BaseColor(255, 255, 255));
		try {
			Paragraph pdfHeading = new Paragraph(BUDGET_SUMMARY, pdfTitleFont);
			pdfHeading.setAlignment(Element.ALIGN_CENTER);
			pdfHeading.setSpacingAfter(8f);
			document.add(pdfHeading);
			PdfPTable proposalHeadingTable = new PdfPTable(1);
			setTableStyle(proposalHeadingTable);

			PdfPCell cell = new PdfPCell(new Phrase(PROPOSAL_OVERVIEW, subHeadFont));
			setTableHeadingStyle(cell);
			proposalHeadingTable.addCell(cell);
			document.add(proposalHeadingTable);

			PdfPTable proposalDetailsTable = new PdfPTable(1);
			proposalDetailsTable.setWidthPercentage(100);
			proposalDetailsTable.setWidths(new int[] { 12 });
			proposalDetailsTable.setSpacingAfter(2f);

			PdfPTable proposalDetailsMutipleColumnTable = new PdfPTable(2);
			proposalDetailsMutipleColumnTable.setSpacingBefore(0);
			proposalDetailsMutipleColumnTable.setWidthPercentage(100);
			proposalDetailsMutipleColumnTable.setWidths(new int[] { 6, 6 });
			proposalDetailsMutipleColumnTable.setSpacingAfter(5f);

			proposalDetailsTable.addCell(getModifiedGeneralDetailsCell(TITLE, budgetPdfData.getTitle(),
					Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));
			if (budgetPdfData.getInvestigator() != null) {
				proposalDetailsMutipleColumnTable.addCell(
						getModifiedGeneralDetailsCell(PRINCIPAL_INVESTIGATOR, budgetPdfData.getInvestigator().getFullName(),
								Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));
			}
			if (budgetPdfData.getSponsor() != null) {
				proposalDetailsMutipleColumnTable.addCell(getModifiedGeneralDetailsCell("Sponsor Name: ", commonService.getSponsorFormatBySponsorDetail(budgetPdfData.getSponsor().getSponsorCode(), budgetPdfData.getSponsor().getSponsorName(), budgetPdfData.getSponsor().getAcronym()), Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));
			} else {
				proposalDetailsMutipleColumnTable.addCell(getModifiedGeneralDetailsCell("Sponsor Name: ", "", Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));
			}
			proposalDetailsMutipleColumnTable.addCell(getModifiedGeneralDetailsCell(LEAD_UNIT,
					budgetPdfData.getHomeUnitNumber() + " - " + budgetPdfData.getHomeUnitName(), Element.ALIGN_MIDDLE,
					Element.ALIGN_LEFT, bodyFont, headFont));
			proposalDetailsMutipleColumnTable.addCell(
					getModifiedGeneralDetailsCell("Category: ", budgetPdfData.getActivityType().getDescription(),
							Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));
			String startDate = commonService.convertDateFormatBasedOnTimeZone(budgetPdfData.getStartDate().getTime(),Constants.DEFAULT_DATE_FORMAT);
			proposalDetailsMutipleColumnTable.addCell(getModifiedGeneralDetailsCell(START_DATE, startDate,
					Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));
			String endDate = commonService.convertDateFormatBasedOnTimeZone(budgetPdfData.getEndDate().getTime(),Constants.DEFAULT_DATE_FORMAT);
			proposalDetailsMutipleColumnTable.addCell(getModifiedGeneralDetailsCell(END_DATE, endDate,
					Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));
			document.add(proposalDetailsTable);
			document.add(proposalDetailsMutipleColumnTable);

			PdfPTable budgetHeadingTable = new PdfPTable(1);
			setTableStyle(budgetHeadingTable);

			PdfPCell budgetHeadingCell = new PdfPCell(new Phrase("Budget Overview", subHeadFont));
			setTableHeadingStyle(budgetHeadingCell);
			budgetHeadingTable.addCell(budgetHeadingCell);
			document.add(budgetHeadingTable);

			if (budgetHeaders != null && !budgetHeaders.isEmpty()) {
				for (BudgetHeader budgetHeader : budgetHeaders) {
					if (budgetHeader.getBudgetId().equals(budgetId)) {
						List<BudgetCategoryDTO> budgetCategoryDTOList = new ArrayList<>();
						PdfPTable budgetDetailsTable = new PdfPTable(3);
						budgetDetailsTable.setWidthPercentage(100);
						budgetDetailsTable.setWidths(new int[] { 4, 4, 4 });
						budgetDetailsTable.setSpacingAfter(5f);

						String budgetStartDate = commonService.convertDateFormatBasedOnTimeZone(budgetPdfData.getStartDate().getTime(),Constants.DEFAULT_DATE_FORMAT);
						budgetDetailsTable.addCell(getModifiedGeneralDetailsCell(START_DATE, budgetStartDate,
								Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));
						String budgetEndDate = commonService.convertDateFormatBasedOnTimeZone(budgetPdfData.getEndDate().getTime(),Constants.DEFAULT_DATE_FORMAT);
						budgetDetailsTable.addCell(getModifiedGeneralDetailsCell(END_DATE, budgetEndDate,
								Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));
						if (budgetHeader.getRateType() != null) {
							budgetDetailsTable.addCell(getModifiedGeneralDetailsCell(OH_BASE,
									budgetHeader.getRateType().getDescription(), Element.ALIGN_MIDDLE,
									Element.ALIGN_LEFT, bodyFont, headFont));
						} else {
							budgetDetailsTable.addCell(getModifiedGeneralDetailsCell(OH_BASE, "",
									Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));
						}
						if (budgetHeader.getTotalDirectCost() == null) {
							budgetDetailsTable.addCell(getModifiedGeneralDetailsCell(DIRECT_COST, COST,
									Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));
						} else {
							budgetDetailsTable.addCell(getModifiedGeneralDetailsCell(DIRECT_COST,
									budgetHeader.getTotalDirectCost().toString(), Element.ALIGN_MIDDLE,
									Element.ALIGN_LEFT, bodyFont, headFont));
						}
						if (budgetHeader.getTotalIndirectCost() == null) {
							budgetDetailsTable.addCell(getModifiedGeneralDetailsCell(INDIRECT_COST, COST,
									Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));
						} else {
							budgetDetailsTable.addCell(getModifiedGeneralDetailsCell(INDIRECT_COST,
									budgetHeader.getTotalIndirectCost().toString(), Element.ALIGN_MIDDLE,
									Element.ALIGN_LEFT, bodyFont, headFont));
						}
						if (budgetHeader.getTotalCost() == null) {
							budgetDetailsTable.addCell(getModifiedGeneralDetailsCell(TOTAL_COST, COST,
									Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));
						} else {
							budgetDetailsTable.addCell(getModifiedGeneralDetailsCell(TOTAL_COST,
									budgetHeader.getTotalCost().toString(), Element.ALIGN_MIDDLE, Element.ALIGN_LEFT,
									bodyFont, headFont));
						}
						document.add(budgetDetailsTable);

						PdfPTable budgetDescriptionTable = new PdfPTable(1);
						setTableStyle(budgetDescriptionTable);

						PdfPCell budgetDescriptionCell = new PdfPCell(new Phrase("Budget Details", subHeadFont));
						setTableHeadingStyle(budgetDescriptionCell);
						budgetDescriptionTable.addCell(budgetDescriptionCell);
						document.add(budgetDescriptionTable);

						List<BudgetPeriod> budgetPeriods = budgetHeader.getBudgetPeriods();
						// Calculate and set budget period values and total cost
						calculateBudgetValues(budgetPeriods, budgetCategoryDTOList);

						if (!budgetPeriods.isEmpty()) {
							if (budgetPeriods.size() == 1) {
								PdfPTable budgetDetailsHeaderTable = new PdfPTable(4);
								budgetDetailsHeaderTable.setWidthPercentage(100);
								budgetDetailsHeaderTable.setWidths(new int[] { 3, 3, 3, 3 });
								createBudgetDetailTableHeader(budgetDetailsHeaderTable, budgetPeriods, headFont,
										document, budgetCategoryDTOList);
							} else if (budgetPeriods.size() == 2) {
								PdfPTable budgetDetailsHeaderTable = new PdfPTable(5);
								budgetDetailsHeaderTable.setWidthPercentage(100);
								budgetDetailsHeaderTable.setWidths(new int[] { 3, 2, 2, 2, 3 });
								createBudgetDetailTableHeader(budgetDetailsHeaderTable, budgetPeriods, headFont,
										document, budgetCategoryDTOList);
							} else if (budgetPeriods.size() == 3) {
								PdfPTable budgetDetailsHeaderTable = new PdfPTable(6);
								budgetDetailsHeaderTable.setWidthPercentage(100);
								budgetDetailsHeaderTable.setWidths(new int[] { 2, 2, 2, 2, 2, 2 });
								createBudgetDetailTableHeader(budgetDetailsHeaderTable, budgetPeriods, headFont,
										document, budgetCategoryDTOList);
							} else if (budgetPeriods.size() == 4) {
								PdfPTable budgetDetailsHeaderTable = new PdfPTable(7);
								budgetDetailsHeaderTable.setWidthPercentage(100);
								budgetDetailsHeaderTable
										.setWidths(new float[] { 2, 1.5f, 1.6f, 1.6f, 1.6f, 1.6f, 1.7f });
								createBudgetDetailTableHeader(budgetDetailsHeaderTable, budgetPeriods, headFont,
										document, budgetCategoryDTOList);
							} else if (budgetPeriods.size() == 5) {
								PdfPTable budgetDetailsHeaderTable = new PdfPTable(8);
								budgetDetailsHeaderTable.setWidthPercentage(100);
								budgetDetailsHeaderTable
										.setWidths(new float[] { 2f, 1.5f, 1.4f, 1.4f, 1.4f, 1.4f, 1.4f, 1.5f });
								createBudgetDetailTableHeader(budgetDetailsHeaderTable, budgetPeriods, headFont,
										document, budgetCategoryDTOList);
							}

							// Table creation code for year wise budget details
							createBudgetDetailsTable(budgetCategoryDTOList, budgetPeriods, document, bodyFont);
						}
					}
				}
			}
			document.close();
		} catch (DocumentException ex) {
			Logger.getLogger(ProposalPrintServiceImpl.class.getName()).log(Level.SEVERE, null, ex);
		}
		return new ByteArrayInputStream(out.toByteArray());
	}

	private void createBudgetDetailTableHeader(PdfPTable budgetDetailsHeaderTable, List<BudgetPeriod> budgetPeriods,
			Font headFont, Document document, List<BudgetCategoryDTO> budgetCategoryDTOList) {
		try {
			budgetDetailsHeaderTable.setSpacingBefore(10f);
			budgetDetailsHeaderTable.addCell(
					getCategoryHeaderTableCell("Items", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, headFont));
			budgetDetailsHeaderTable.addCell(
					getCategoryHeaderTableCell("Quantity", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, headFont));
			for (BudgetPeriod budgetPeriod : budgetPeriods) {
				budgetDetailsHeaderTable.addCell(getCategoryHeaderTableCell("Year " + budgetPeriod.getBudgetPeriod(),
						Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, headFont));
			}
			budgetDetailsHeaderTable.addCell(
					getCategoryHeaderTableCell("Total", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, headFont));
			if (budgetCategoryDTOList.isEmpty()) {
				if (budgetPeriods.size() == 1) {
					budgetDetailsHeaderTable.addCell(getCategoryEmptyCell(NO_COST_ELEMENT_MESSAGE,
							Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, headFont, 4));
				} else if (budgetPeriods.size() == 2) {
					budgetDetailsHeaderTable.addCell(getCategoryEmptyCell(NO_COST_ELEMENT_MESSAGE,
							Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, headFont, 5));
				} else if (budgetPeriods.size() == 3) {
					budgetDetailsHeaderTable.addCell(getCategoryEmptyCell(NO_COST_ELEMENT_MESSAGE,
							Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, headFont, 6));
				} else if (budgetPeriods.size() == 4) {
					budgetDetailsHeaderTable.addCell(getCategoryEmptyCell(NO_COST_ELEMENT_MESSAGE,
							Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, headFont, 7));
				} else if (budgetPeriods.size() == 5) {
					budgetDetailsHeaderTable.addCell(getCategoryEmptyCell(NO_COST_ELEMENT_MESSAGE,
							Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, headFont, 8));
				}
			}
			document.add(budgetDetailsHeaderTable);
		} catch (DocumentException ex) {
			Logger.getLogger(ProposalPrintServiceImpl.class.getName()).log(Level.SEVERE, null, ex);
		}
	}

	private void createBudgetLineItemTable(PdfPTable budgetLineItemTable,
			Font bodyFont, Document document, BudgetCategoryDTO budgetCategoryDTO) {
		try {
			for (LineItem lineItem : budgetCategoryDTO.getLineItemList()) {
				budgetLineItemTable
						.addCell(getBudgetTableCell(lineItem.getBudgetDetail().getCostElement().getDescription(),
								Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
				if (lineItem.getBudgetDetail().getQuantity() != null) {
					budgetLineItemTable.addCell(getBudgetTableCell(lineItem.getBudgetDetail().getQuantity().toString(),
							Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
				} else {
					budgetLineItemTable
							.addCell(getBudgetTableCell("", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
				}
				for (PeriodCost periodCost : lineItem.getPeriodCosts()) {
					if (periodCost.getCost() != null) {
						budgetLineItemTable.addCell(getBudgetTableCell(periodCost.getCost().toString(),
								Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
					} else {
						budgetLineItemTable.addCell(
								getBudgetTableCell("", Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
					}
				}
				budgetLineItemTable.addCell(getBudgetTableCell(lineItem.getTotalLineItemCost().toString(),
						Element.ALIGN_MIDDLE, Element.ALIGN_CENTER, bodyFont));
				if (budgetLineItemTable.getNumberOfColumns() == 4) {
					setJustificationFieldValue(bodyFont, lineItem, budgetLineItemTable, 1, 3);
				} else if (budgetLineItemTable.getNumberOfColumns() == 5) {
					setJustificationFieldValue(bodyFont, lineItem, budgetLineItemTable, 1, 4);
				} else if (budgetLineItemTable.getNumberOfColumns() == 6) {
					setJustificationFieldValue(bodyFont, lineItem, budgetLineItemTable, 1, 5);
				} else if (budgetLineItemTable.getNumberOfColumns() == 7) {
					setJustificationFieldValue(bodyFont, lineItem, budgetLineItemTable, 1, 6);
				} else if (budgetLineItemTable.getNumberOfColumns() == 8) {
					setJustificationFieldValue(bodyFont, lineItem, budgetLineItemTable, 1, 7);
				}
			}
			document.add(budgetLineItemTable);
		} catch (Exception ex) {
			Logger.getLogger(ProposalPrintServiceImpl.class.getName()).log(Level.SEVERE, null, ex);
		}
	}

	private void setJustificationFieldValue(Font bodyFont, LineItem lineItem, PdfPTable budgetLineItemTable,
			int colSpanValue1, int colSpanValue2) {
		PdfPCell cell = new PdfPCell(new Phrase("Justification", bodyFont));
		cell.setColspan(colSpanValue1);
		cell.setVerticalAlignment(Element.ALIGN_MIDDLE);
		cell.setHorizontalAlignment(Element.ALIGN_CENTER);
		cell.setPadding(5);
		cell.setPaddingBottom(10);
		cell.setBorderColor(new BaseColor(230, 229, 229));
		budgetLineItemTable.addCell(cell);
		PdfPCell cellValue = new PdfPCell(new Phrase(lineItem.getLineItemDescription(), bodyFont));
		cellValue.setColspan(colSpanValue2);
		cellValue.setVerticalAlignment(Element.ALIGN_MIDDLE);
		cellValue.setHorizontalAlignment(Element.ALIGN_JUSTIFIED);
		cellValue.setPadding(5);
		cellValue.setPaddingBottom(10);
		cellValue.setBorderColor(new BaseColor(230, 229, 229));
		budgetLineItemTable.addCell(cellValue);
	}

	public static PdfPCell getCategoryHeaderTableCell(String text, int verticalAlignment, int horizontalAlignment,
			Font bodyFont) {
		PdfPCell cell = new PdfPCell(new Phrase(text, bodyFont));
		cell.setPadding(5);
		cell.setVerticalAlignment(verticalAlignment);
		cell.setHorizontalAlignment(horizontalAlignment);
		cell.setBorderColor(new BaseColor(230, 229, 229));
		cell.setBackgroundColor(new BaseColor(230, 229, 229));
		return cell;
	}

	public static PdfPCell getCategoryEmptyCell(String text, int verticalAlignment, int horizontalAlignment,
			Font bodyFont, int colSpanValue) {
		PdfPCell cell = new PdfPCell(new Phrase(text, bodyFont));
		cell.setPadding(5);
		cell.setVerticalAlignment(verticalAlignment);
		cell.setHorizontalAlignment(horizontalAlignment);
		cell.setBorderColor(new BaseColor(230, 229, 229));
		cell.setColspan(colSpanValue);
		return cell;
	}

	public static PdfPCell getBudgetTableCell(String text, int verticalAlignment, int horizontalAlignment,
			Font bodyFont) {
		PdfPCell cell = new PdfPCell(new Phrase(text, bodyFont));
		cell.setPadding(5);
		cell.setVerticalAlignment(verticalAlignment);
		cell.setHorizontalAlignment(horizontalAlignment);
		cell.setBorderColor(new BaseColor(230, 229, 229));
		return cell;
	}

	private void calculateBudgetValues(List<BudgetPeriod> budgetPeriods,
			List<BudgetCategoryDTO> budgetCategoryDTODetails) {
		// Set Budget Category List
		for (BudgetPeriod budgetPeriod : budgetPeriods) {
			for (BudgetDetail budgetDetail : budgetPeriod.getBudgetDetails()) {
				BudgetCategoryDTO budgetCategoryDTO = new BudgetCategoryDTO();
				if (budgetCategoryDTODetails.isEmpty()) {
					budgetCategoryDTO.setCategory(budgetDetail.getBudgetCategory().getDescription());
					budgetCategoryDTO.setCategoryCode(budgetDetail.getBudgetCategoryCode());
					budgetCategoryDTO.setTotalCategoryCost(BigDecimal.ZERO);
					budgetCategoryDTODetails.add(budgetCategoryDTO);
				} else {
					String isCategoryPresent = "N";
					for (BudgetCategoryDTO budgetCategoryData : budgetCategoryDTODetails) {
						if (budgetCategoryData.getCategory()
								.equals(budgetDetail.getBudgetCategory().getDescription())) {
							isCategoryPresent = "Y";
						}
					}
					if (isCategoryPresent.equals("N")) {
						budgetCategoryDTO.setCategory(budgetDetail.getBudgetCategory().getDescription());
						budgetCategoryDTO.setCategoryCode(budgetDetail.getBudgetCategoryCode());
						budgetCategoryDTO.setTotalCategoryCost(BigDecimal.ZERO);
						budgetCategoryDTODetails.add(budgetCategoryDTO);
					}
				}
			}
		}

		// Set budget line items
		for (BudgetPeriod budgetPeriod : budgetPeriods) {
			for (BudgetDetail budgetDetail : budgetPeriod.getBudgetDetails()) {
				if (!budgetCategoryDTODetails.isEmpty()) {
					for (BudgetCategoryDTO budgetCategoryData : budgetCategoryDTODetails) {
						if (budgetCategoryData.getCategoryCode().equals(budgetDetail.getBudgetCategoryCode())) {
							LineItem lineItem = new LineItem();
							lineItem.setBudgetDetail(budgetDetail);
							lineItem.setLineItemDescription(budgetDetail.getLineItemDescription());
							lineItem.setTotalLineItemCost(BigDecimal.ZERO);
							budgetCategoryData.getLineItemList().add(lineItem);
						}
					}
				}
			}
		}

		// Set budget period costs
		for (BudgetPeriod budgetPeriod : budgetPeriods) {
			for (BudgetDetail budgetDetail : budgetPeriod.getBudgetDetails()) {
				if (!budgetCategoryDTODetails.isEmpty()) {
					for (BudgetCategoryDTO budgetCategoryData : budgetCategoryDTODetails) {
						for (LineItem lineItem : budgetCategoryData.getLineItemList()) {
							if (lineItem.getBudgetDetail().getCostElement().getBudgetCategory().getCode()
									.equals(budgetDetail.getCostElement().getBudgetCategory().getCode())) {
								if (lineItem.getPeriodCosts().isEmpty() && lineItem.getBudgetDetail()
										.getCostElementCode().equals(budgetDetail.getCostElementCode())) {
									PeriodCost periodCost = new PeriodCost();
									periodCost.setBudgetPeriod(budgetDetail.getBudgetPeriod());
									periodCost.setCost(budgetDetail.getLineItemCost());
									lineItem.getPeriodCosts().add(periodCost);
								} else {
									if (lineItem.getBudgetDetail().getCostElementCode()
											.equals(budgetDetail.getCostElementCode())
											&& !lineItem.getBudgetDetail().getBudgetDetailId().equals(budgetDetail
													.getBudgetDetailId())) {
										PeriodCost periodCost = new PeriodCost();
										periodCost.setBudgetPeriod(budgetDetail.getBudgetPeriod());
										periodCost.setCost(budgetDetail.getLineItemCost());
										lineItem.getPeriodCosts().add(periodCost);
									}
								}
							}
						}
					}
				}
			}
		}

		// For avoiding duplicate entries of line item in each category
		if (!budgetCategoryDTODetails.isEmpty()) {
			for (BudgetCategoryDTO budgetCategoryData : budgetCategoryDTODetails) {
				List<LineItem> filteredLineItems = new ArrayList<>();
				for (LineItem lineItem : budgetCategoryData.getLineItemList()) {
					if (filteredLineItems.isEmpty()) {
						filteredLineItems.add(lineItem);
					} else {
						String hasDuplicateLineItem = "N";
						for (LineItem lineItemData : filteredLineItems) {
							if (lineItemData.getBudgetDetail().getCostElementCode()
									.equals(lineItem.getBudgetDetail().getCostElementCode())) {
								hasDuplicateLineItem = "Y";
							}
						}
						if (hasDuplicateLineItem.equals("N")) {
							filteredLineItems.add(lineItem);
						}
					}
				}
				budgetCategoryData.setLineItemList(filteredLineItems);
			}
		}

		// Calculate total cost of each line item and each category
		for (BudgetCategoryDTO budgetCategoryData : budgetCategoryDTODetails) {
			BigDecimal totalCategoryCost = budgetCategoryData.getTotalCategoryCost();
			for (LineItem lineItem : budgetCategoryData.getLineItemList()) {
				BigDecimal totalLineItemCost = lineItem.getTotalLineItemCost();
				for (PeriodCost periodCost : lineItem.getPeriodCosts()) {
					if (periodCost.getCost() != null) {
						totalLineItemCost = totalLineItemCost.add(periodCost.getCost());
					}
				}
				lineItem.setTotalLineItemCost(totalLineItemCost);
				totalCategoryCost = totalCategoryCost.add(totalLineItemCost);
			}
			budgetCategoryData.setTotalCategoryCost(totalCategoryCost);
		}
	}

	private void createBudgetDetailsTable(List<BudgetCategoryDTO> budgetCategoryDTOList,
			List<BudgetPeriod> budgetPeriods, Document document, Font bodyFont) {
		try {
			for (BudgetCategoryDTO budgetCategoryDTO : budgetCategoryDTOList) {
				PdfPTable budgetCategoryTable = new PdfPTable(2);
				budgetCategoryTable.setWidthPercentage(100);
				budgetCategoryTable.setWidths(new int[] { 8, 4 });
				Font categoryFont = FontFactory.getFont(FontFactory.HELVETICA_BOLD);
				categoryFont.setSize(10);
				PdfPCell cellForCategory = new PdfPCell(new Phrase(budgetCategoryDTO.getCategory(), categoryFont));
				cellForCategory.setBorderWidthRight(0);
				cellForCategory.setPadding(5);
				cellForCategory.setBackgroundColor(new BaseColor(230, 241, 255));
				cellForCategory.setBorderColor(new BaseColor(230, 229, 229));
				PdfPCell cellForTotal = new PdfPCell(
						new Phrase("Total: " + Constants.DOLLAR_SYMBOL + budgetCategoryDTO.getTotalCategoryCost().toString(), categoryFont));
				cellForTotal.setHorizontalAlignment(Element.ALIGN_RIGHT);
				cellForTotal.setPadding(5);
				cellForTotal.setBorderWidthLeft(0);
				cellForTotal.setBackgroundColor(new BaseColor(230, 241, 255));
				cellForTotal.setBorderColor(new BaseColor(230, 229, 229));
				cellForTotal.setBorderWidthRight(0.5f);
				budgetCategoryTable.addCell(cellForCategory);
				budgetCategoryTable.addCell(cellForTotal);
				document.add(budgetCategoryTable);

				if (budgetPeriods.size() == 1) {
					PdfPTable budgetLineItemTable = new PdfPTable(4);
					budgetLineItemTable.setWidthPercentage(100);
					budgetLineItemTable.setWidths(new int[] { 3, 3, 3, 3 });
					createBudgetLineItemTable(budgetLineItemTable, bodyFont, document,
							budgetCategoryDTO);
				} else if (budgetPeriods.size() == 2) {
					PdfPTable budgetLineItemTable = new PdfPTable(5);
					budgetLineItemTable.setWidthPercentage(100);
					budgetLineItemTable.setWidths(new int[] { 3, 2, 2, 2, 3 });
					createBudgetLineItemTable(budgetLineItemTable, bodyFont, document,
							budgetCategoryDTO);
				} else if (budgetPeriods.size() == 3) {
					PdfPTable budgetLineItemTable = new PdfPTable(6);
					budgetLineItemTable.setWidthPercentage(100);
					budgetLineItemTable.setWidths(new int[] { 2, 2, 2, 2, 2, 2 });
					createBudgetLineItemTable(budgetLineItemTable, bodyFont, document,
							budgetCategoryDTO);
				} else if (budgetPeriods.size() == 4) {
					PdfPTable budgetLineItemTable = new PdfPTable(7);
					budgetLineItemTable.setWidthPercentage(100);
					budgetLineItemTable.setWidths(new float[] { 2, 1.5f, 1.6f, 1.6f, 1.6f, 1.6f, 1.7f });
					createBudgetLineItemTable(budgetLineItemTable, bodyFont, document,
							budgetCategoryDTO);
				} else if (budgetPeriods.size() == 5) {
					PdfPTable budgetLineItemTable = new PdfPTable(8);
					budgetLineItemTable.setWidthPercentage(100);
					budgetLineItemTable.setWidths(new float[] { 2f, 1.5f, 1.4f, 1.4f, 1.4f, 1.4f, 1.4f, 1.5f });
					createBudgetLineItemTable(budgetLineItemTable, bodyFont, document,
							budgetCategoryDTO);
				}
			}
		} catch (DocumentException ex) {
			Logger.getLogger(ProposalPrintServiceImpl.class.getName()).log(Level.SEVERE, null, ex);
		}
	}

	public static PdfPCell getBudgetSummaryTableCell(String text, int verticalAlignment, int horizontalAlignmentt,
			Font bodyFont) {
		PdfPCell cell = new PdfPCell(new Phrase(text, bodyFont));
		cell.setPaddingRight(1);
		cell.setPaddingLeft(1);
		cell.setPaddingTop(5);
		cell.setPaddingBottom(5);
		cell.setVerticalAlignment(verticalAlignment);
		cell.setHorizontalAlignment(horizontalAlignmentt);
		cell.setBorderColor(new BaseColor(230, 229, 229));
		return cell;
	}

	public static PdfPCell getModifiedGeneralDetailsCell(String labelText, String contentText, int verticalAlignment,
			int horizontalAlignmentt, Font labelFont, Font bodyFont) {
		PdfPCell cell = new PdfPCell(new Phrase(new Chunk(labelText, bodyFont)));
		Phrase phrase = new Phrase();
		phrase.add(new Chunk(labelText, labelFont));
		if (contentText != null) {
			phrase.add(new Chunk(contentText, bodyFont));
		}
		cell.addElement(phrase);
		cell.setPadding(5);
		cell.setVerticalAlignment(verticalAlignment);
		cell.setHorizontalAlignment(horizontalAlignmentt);
		cell.setBorder(Rectangle.NO_BORDER);
		return cell;
	}

	public void setTableHeadingStyle(PdfPCell cell) {
		cell.setPadding(5);
		cell.setPaddingTop(2);
		cell.setVerticalAlignment(Element.ALIGN_MIDDLE);
		cell.setHorizontalAlignment(Element.ALIGN_LEFT);
		cell.setBorder(Rectangle.NO_BORDER);
		cell.setBackgroundColor(new BaseColor(32, 88, 160));
	}

	public void setTableStyle(PdfPTable table) {
		try {
			table.setWidthPercentage(100);
			table.setWidths(new int[] { 12 });
			table.setSpacingAfter(2f);
		} catch (Exception ex) {
			Logger.getLogger(ProposalPrintServiceImpl.class.getName()).log(Level.SEVERE, null, ex);
		}
	}

	public static PdfPCell getTableHeaderCell(String text, int verticalAlignment, int horizontalAlignmentt,
			Font bodyFont) {
		PdfPCell cell = new PdfPCell(new Phrase(text, bodyFont));
		cell.setPadding(5);
		cell.setVerticalAlignment(verticalAlignment);
		cell.setHorizontalAlignment(horizontalAlignmentt);
		cell.setBackgroundColor(new BaseColor(230, 229, 229));
		cell.setBorderColor(new BaseColor(230, 229, 229));
		return cell;
	}

	public static PdfPCell getBudgetSummaryTableHeaderCell(String text, int verticalAlignment, int horizontalAlignmentt,
			Font bodyFont) {
		PdfPCell cell = new PdfPCell(new Phrase(text, bodyFont));
		cell.setPaddingRight(1);
		cell.setPaddingLeft(1);
		cell.setPaddingTop(5);
		cell.setPaddingBottom(5);
		cell.setVerticalAlignment(verticalAlignment);
		cell.setHorizontalAlignment(horizontalAlignmentt);
		cell.setBackgroundColor(new BaseColor(230, 229, 229));
		cell.setBorderColor(new BaseColor(230, 229, 229));
		return cell;
	}

	private void setMoreInformationRowStyle(PdfPCell cell, int colSpanValue, int verticalAlignment,
			int horizontalAlignment) {
		cell.setColspan(colSpanValue);
		cell.setVerticalAlignment(verticalAlignment);
		cell.setHorizontalAlignment(horizontalAlignment);
		cell.setBorderColor(new BaseColor(230, 229, 229));
	}

	private void setRowStyle(PdfPCell cell, int verticalAlignment, int horizontalAlignment) {
		cell.setVerticalAlignment(verticalAlignment);
		cell.setHorizontalAlignment(horizontalAlignment);
		cell.setBorderColor(new BaseColor(230, 229, 229));
	}

}
