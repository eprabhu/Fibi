package com.polus.fibicomp.questionnaire.print.service;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.text.ParseException;
import java.util.HashMap;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import com.itextpdf.text.BaseColor;
import com.itextpdf.text.Chunk;
import com.itextpdf.text.Document;
import com.itextpdf.text.DocumentException;
import com.itextpdf.text.Element;
import com.itextpdf.text.Font;
import com.itextpdf.text.FontFactory;
import com.itextpdf.text.Paragraph;
import com.itextpdf.text.Phrase;
import com.itextpdf.text.Rectangle;
import com.itextpdf.text.pdf.PdfPCell;
import com.itextpdf.text.pdf.PdfPTable;
import com.itextpdf.text.pdf.PdfWriter;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.proposal.print.service.ProposalPrintServiceImpl;
import com.polus.fibicomp.questionnaire.dto.QuestionnaireDataBus;

@Transactional
@Service(value = "questionnairePrintService")
public class QuestionnairePrintServiceImpl implements QuestionnairePrintService {

	@Autowired
	private CommonService commonService;

	@SuppressWarnings("unchecked")
	@Override
	public ByteArrayInputStream questionnairePdfReport(List<QuestionnaireDataBus> questionnaireList, Proposal proposal)
			throws DocumentException, ParseException {
		Document document = new Document();
		ByteArrayOutputStream out = new ByteArrayOutputStream();
		PdfWriter.getInstance(document, out);
		document.open();
		Font pdfTitleFont = FontFactory.getFont(FontFactory.HELVETICA_BOLD, 13);
		Font headFont = FontFactory.getFont(FontFactory.HELVETICA_BOLD, 10);
		Font bodyFont = FontFactory.getFont(FontFactory.HELVETICA, 9);
		Font subHeadFont = FontFactory.getFont(FontFactory.HELVETICA_BOLD, 9);
		Font headerFont = FontFactory.getFont(FontFactory.HELVETICA_BOLDOBLIQUE, 11);
		headerFont.setColor(new BaseColor(255, 255, 255));
		try {
			Paragraph pdfHeading = new Paragraph("Questionnaire Summary", pdfTitleFont);
			pdfHeading.setAlignment(Element.ALIGN_CENTER);
			pdfHeading.setSpacingAfter(8);
			document.add(pdfHeading);
			PdfPTable proposalHeadingTable = new PdfPTable(1);
			setTableStyle(proposalHeadingTable);
			PdfPCell cell = new PdfPCell(new Phrase("Proposal Overview", headerFont));
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
			proposalDetailsTable.addCell(getModifiedGeneralDetailsCell("Title: ", proposal.getTitle(),
					Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));
			proposalDetailsMutipleColumnTable
					.addCell(getModifiedGeneralDetailsCell("Proposal Id: ", proposal.getProposalId().toString(),
							Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));
			proposalDetailsMutipleColumnTable
					.addCell(getModifiedGeneralDetailsCell("Status: ", proposal.getProposalStatus().getDescription(),
							Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));
			String startDate = commonService.convertDateFormatBasedOnTimeZone(proposal.getStartDate().getTime(),Constants.DEFAULT_DATE_FORMAT);
			proposalDetailsMutipleColumnTable.addCell(getModifiedGeneralDetailsCell("Start Date: ", startDate,
					Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));
			if (proposal.getEndDate() != null) {
				String endDate = commonService.convertDateFormatBasedOnTimeZone(proposal.getEndDate().getTime(),Constants.DEFAULT_DATE_FORMAT);
				proposalDetailsMutipleColumnTable.addCell(getModifiedGeneralDetailsCell("End Date: ", endDate,
						Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));
			} else {
				proposalDetailsMutipleColumnTable.addCell(getModifiedGeneralDetailsCell("End Date: ", "",
						Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));
			}
			if (proposal.getActivityType() != null) {
				proposalDetailsMutipleColumnTable.addCell(
						getModifiedGeneralDetailsCell("Category: ", proposal.getActivityType().getDescription(),
								Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));
			} else {
				proposalDetailsMutipleColumnTable.addCell(getModifiedGeneralDetailsCell("Category: ", "",
						Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));
			}
			if (proposal.getProposalType() != null) {
				proposalDetailsMutipleColumnTable.addCell(
						getModifiedGeneralDetailsCell("Proposal Type: ", proposal.getProposalType().getDescription(),
								Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));
			} else {
				proposalDetailsMutipleColumnTable.addCell(getModifiedGeneralDetailsCell("Proposal Type: ", "",
						Element.ALIGN_MIDDLE, Element.ALIGN_LEFT, bodyFont, headFont));
			}
			document.add(proposalDetailsTable);
			document.add(proposalDetailsMutipleColumnTable);
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
										+ questionnaireStatus.toString() + ")", headFont));
						setRowStyle(questionnaireHeaderCell, Element.ALIGN_MIDDLE, Element.ALIGN_LEFT);
						questionnaireHeaderCell.setPadding(5);
						questionnaireHeaderCell.setBackgroundColor(new BaseColor(230, 229, 229));
						questionnaireTable.addCell(questionnaireHeaderCell);
						if (!questionnaireDataBus.getQuestionnaire().getQuestions().isEmpty()) {
							for (HashMap<String, Object> question : questionnaireDataBus.getQuestionnaire().getQuestions()) {
								HashMap<String, Object> answerMap = (HashMap<String, Object>) question.get("ANSWERS");
									if (answerMap.get("1") != null && !answerMap.get("1").toString().isEmpty()) {
										PdfPCell questionCell = new PdfPCell(new Paragraph(question.get("QUESTION").toString(), subHeadFont));
										setRowStyle(questionCell, Element.ALIGN_MIDDLE, Element.ALIGN_LEFT);
										questionCell.setPadding(5);
										questionnaireHeaderCell.setPaddingLeft(7);
										questionCell.setBorderWidthBottom(0);
										questionCell.setBorderColor(new BaseColor(255, 255, 255));
										questionnaireTable.addCell(questionCell);
										PdfPCell answerCell = new PdfPCell(new Phrase(answerMap.get("1").toString(), bodyFont));
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
		} catch (DocumentException ex) {
			Logger.getLogger(QuestionnairePrintServiceImpl.class.getName()).log(Level.SEVERE, null, ex);
		}
		return new ByteArrayInputStream(out.toByteArray());
	}

	private void setTableStyle(PdfPTable table) {
		try {
			table.setWidthPercentage(100);
			table.setWidths(new int[] { 12 });
			table.setSpacingBefore(5f);
			table.setSpacingAfter(5f);
		} catch (Exception ex) {
			Logger.getLogger(ProposalPrintServiceImpl.class.getName()).log(Level.SEVERE, null, ex);
		}
	}

	private void setRowStyle(PdfPCell cell, int verticalAlignment, int horizontalAlignment) {
		cell.setVerticalAlignment(verticalAlignment);
		cell.setHorizontalAlignment(horizontalAlignment);
		cell.setBorderColor(new BaseColor(230, 229, 229));
	}

	private void setTableHeadingStyle(PdfPCell cell) {
		cell.setPadding(5);
		cell.setPaddingTop(2);
		cell.setVerticalAlignment(Element.ALIGN_MIDDLE);
		cell.setHorizontalAlignment(Element.ALIGN_LEFT);
		cell.setBorder(Rectangle.NO_BORDER);
		cell.setBackgroundColor(new BaseColor(32, 88, 160));
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

}
