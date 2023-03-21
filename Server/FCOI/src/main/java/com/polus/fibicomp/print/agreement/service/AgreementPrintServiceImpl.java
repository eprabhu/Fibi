package com.polus.fibicomp.print.agreement.service;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.nio.charset.StandardCharsets;
import java.text.DecimalFormat;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.servlet.http.HttpServletResponse;
import javax.transaction.Transactional;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.poi.xwpf.usermodel.XWPFDocument;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.util.FileCopyUtils;

import com.polus.fibicomp.agreements.dao.AgreementDao;
import com.polus.fibicomp.agreements.dto.AgreementClausesGroup;
import com.polus.fibicomp.agreements.pojo.AgreementAttachment;
import com.polus.fibicomp.agreements.pojo.AgreementClauses;
import com.polus.fibicomp.agreements.pojo.AgreementHeader;
import com.polus.fibicomp.agreements.pojo.AgreementPeople;
import com.polus.fibicomp.agreements.pojo.AgreementPlaceHolder;
import com.polus.fibicomp.agreements.pojo.AgreementSponsor;
import com.polus.fibicomp.agreements.pojo.AgreementTypeTemplate;
import com.polus.fibicomp.agreements.service.AgreementService;
import com.polus.fibicomp.agreements.vo.AgreementVO;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.dbengine.DBEngine;
import com.polus.fibicomp.dbengine.Parameter;
import com.polus.fibicomp.notification.pojo.NotificationRecipient;
import com.polus.fibicomp.person.dao.PersonDao;
import com.polus.fibicomp.pojo.Sponsor;
import com.polus.fibicomp.print.dto.AgreementPrintParameter;
import com.polus.fibicomp.print.dto.AgreementSponsorContactParameter;
import com.polus.fibicomp.print.dto.QuestionAndAnswer;
import com.polus.fibicomp.print.dto.QuestionnairePrintParameter;
import com.polus.fibicomp.questionnaire.dto.QuestionnaireDataBus;
import com.polus.fibicomp.questionnaire.service.QuestionnaireService;
import com.polus.fibicomp.utils.QueryBuilder;

import fr.opensagres.poi.xwpf.converter.pdf.PdfConverter;
import fr.opensagres.poi.xwpf.converter.pdf.PdfOptions;
import fr.opensagres.xdocreport.converter.ConverterTypeTo;
import fr.opensagres.xdocreport.converter.ConverterTypeVia;
import fr.opensagres.xdocreport.converter.Options;
import fr.opensagres.xdocreport.core.io.internal.ByteArrayOutputStream;
import fr.opensagres.xdocreport.document.IXDocReport;
import fr.opensagres.xdocreport.document.registry.XDocReportRegistry;
import fr.opensagres.xdocreport.template.IContext;
import fr.opensagres.xdocreport.template.TemplateEngineKind;
import fr.opensagres.xdocreport.template.formatter.FieldsMetadata;

@Transactional
@Service(value = "agreementPrintService")
public class AgreementPrintServiceImpl implements AgreementPrintService {

	protected static Logger logger = LogManager.getLogger(AgreementPrintServiceImpl.class.getName());

	@Autowired
	private QuestionnaireService questionnaireService;

	@Autowired
	private DBEngine dbEngine;

	@Autowired
	private AgreementDao agreementDao;

	@Autowired
	private AgreementService agreementService;

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private CommonService commonService;

	@Autowired
	private PersonDao personDao;

	private static final String PUBLIC = "public";
	private static final String RESULT = "Result";
	private static final String CONTENT_TYPE = "application/octet-stream";
	private static final String CACHE_CONTROL = "must-revalidate, post-check=0, pre-check=0";
	private static final String CONTENT_DISPOSITION = "Content-Disposition";
	private static final String ATTACHMENT_FILENAME = "attachment; filename=\"";
	DecimalFormat decimalFormat = new DecimalFormat(Constants.NUMBER_FORMAT_WITH_DECIMAL);
	private static final String ANSWER_TYPE = "ANSWER_TYPE";
	private static final String QUESTION = "QUESTION";

	@Override
	public byte[] getTemplateData(String templateTypeCode) {
		byte[] data = null;
		try {
			data = getLetterTemplate(templateTypeCode);
		} catch (Exception e) {
			logger.error("Exception in getTemplateData" + e.getMessage());
		}
		return data;
	}

	private byte[] getLetterTemplate(String templateTypeCode) {
		byte[] data = null;
		try {
			String query = QueryBuilder.selectAgreementTemplate(templateTypeCode);
			ArrayList<HashMap<String, Object>> dataList = dbEngine.executeQuerySQL(new ArrayList<Parameter>(), query);
			if (!dataList.isEmpty()) {
				java.io.ByteArrayOutputStream baos = (java.io.ByteArrayOutputStream) dataList.get(0)
						.get("TEMPLATE");
				data = baos.toByteArray();
			}
		} catch (Exception e) {
			logger.error("Exception in getLetterTemplate" + e.getMessage());
		}
		return data;
	}

	public ResponseEntity<byte[]> setHttpHeaderAndHttpResponseData(HttpServletResponse response, String generatedFileName, byte[] mergedOutput, ResponseEntity<byte[]> attachmentData) {
		try {
			HttpHeaders headers = new HttpHeaders();
			headers.setContentType(MediaType.parseMediaType("application/pdf"));
			headers.setContentDispositionFormData(generatedFileName, generatedFileName);
			headers.setContentLength(mergedOutput.length);
			headers.setCacheControl("must-revalidate, post-check=0, pre-check=0");
			headers.setPragma("public");
			attachmentData = new ResponseEntity<byte[]>(mergedOutput, headers, HttpStatus.OK);
			response.setCharacterEncoding(StandardCharsets.UTF_8.name());
			response.setContentType("application/pdf");
			response.setContentLength(mergedOutput.length);
			response.setHeader("Content-Disposition", "attachment; filename=\"" + generatedFileName + "\"");
			FileCopyUtils.copy(mergedOutput, response.getOutputStream());
		} catch (Exception e) {
			logger.error("Exception in setHttpHeaderAndHttpResponseData: {} " ,  e.getMessage());
		}
		return attachmentData;
	}

	@Override
	public String generateAgreementReport(AgreementVO agreementVO, HttpServletResponse response) {
		try {
			Integer versionNumber = agreementDao.fetchMaxVersionAgreementTemplateBasedOnParams(agreementVO.getAgreementTypeCode());
			if (versionNumber != 0) {
				Boolean isAgreementCreatedOnce = agreementVO.getIsAgreementCreatedOnce();
				AgreementTypeTemplate agreementTypeTemplate = agreementDao.getAgreementTypeTemplateBasedOnParams(agreementVO.getAgreementTypeCode(), versionNumber);
				byte[] mergedOutput = mergePlaceHoldersOfAgreement(agreementTypeTemplate.getTemplate(), agreementVO.getAgreementRequestId());
				agreementService.saveTemplateAsAttachments(agreementVO, mergedOutput);
				if (Boolean.TRUE.equals(isAgreementCreatedOnce)) {
					Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
					agreementService.sendNotificationForAgreement(agreementVO, Constants.REGENERATE_AGREEMENT_NOTIFICATION_CODE, dynamicEmailrecipients);
				} else {
					Set<NotificationRecipient> dynamicEmailrecipients = new HashSet<>();
					agreementService.sendNotificationForAgreement(agreementVO, Constants.GENERATE_AGREEMENT_NOTIFICATION_CODE, dynamicEmailrecipients);
				}
			} else {
				agreementVO.setMessage("There is no template for this agreement type.");
			}
			if (agreementVO.getAgreementAttachments() != null && !agreementVO.getAgreementAttachments().isEmpty()) {
				for (AgreementAttachment agreementAttachment : agreementVO.getAgreementAttachments()) {
					if (agreementAttachment.getUpdateUser() != null) {
						agreementAttachment.setUpdateUserFullName(personDao.getUserFullNameByUserName(agreementAttachment.getUpdateUser()));
					}
				}
			}
		} catch (Exception e) {
			logger.error("Exception in generateAgreementReport: {} ", e.getMessage());
		}
		return commonDao.convertObjectToJSON(agreementVO);
	}

	@Override
	public byte[] mergePlaceHoldersOfAgreement(byte[] data, Integer agreementRequestId) {
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		try {
			InputStream myInputStream = new ByteArrayInputStream(data);
			IXDocReport report = XDocReportRegistry.getRegistry().loadReport(myInputStream, TemplateEngineKind.Velocity);
			IContext context = report.createContext();
			context = setPlaceHoldersOfAgrementHeader(context, agreementRequestId);
			context = setPlaceHoldersOfAgrementClause(context, agreementRequestId);
			context = setPlaceHoldersOfQuestionnaire(context, agreementRequestId);
			report.process(context, baos);
		} catch (Exception e) {
			e.printStackTrace();
			logger.error("Exception in mergePlaceHoldersOfAgreement: {} ", e.getMessage());
		}
		return baos.toByteArray();
	}

	private IContext setPlaceHoldersOfAgrementHeader(IContext context, Integer agreementRequestId) {
		Map<String, String> contextDatas = preparePlaceHoldersOfAgreementHeader(agreementRequestId);
		for (Map.Entry<String, String> contextData : contextDatas.entrySet()) {
			context.put(contextData.getKey(), contextData.getValue());
		}
		return context;
	}

	private IContext setPlaceHoldersOfAgrementClause(IContext context, Integer agreementRequestId) {
		Map<String, List<String>> clauses =  preparePlaceHoldersOfAgreementClauses(agreementRequestId);
		for (Map.Entry<String, List<String>> clause : clauses.entrySet()) {
			StringBuilder subClause = new StringBuilder(0);
			for (String subclauseData : clause.getValue()) {
				subClause.append("\n" +subclauseData + "\n");
			}
			context.put(clause.getKey(), subClause.toString());
		}
		return context;
	}

	private IContext setPlaceHoldersOfQuestionnaire(IContext context, Integer agreementRequestId) {
		List<HashMap<String, Object>> questionnaires = agreementDao.fetchAgreementQuestionnaireBasedOnAgreementId(agreementRequestId);
		for (HashMap<String, Object> questionnaire : questionnaires) {
			String question = "";
			String answer = "";
			for (Map.Entry<String, Object> questionnaireData : questionnaire.entrySet()) {
				if (questionnaireData.getKey().equals("QUESTION_NUMBER")) {
					question = "QN-" + questionnaireData.getValue().toString();
				} else if (questionnaireData.getKey().equals("ANSWER")) {
					if (questionnaireData.getValue() != null)
					answer = questionnaireData.getValue().toString();
				}
			}
			context.put(question, answer);
		}
		return context;
	}

	private Map<String, String> preparePlaceHoldersOfAgreementHeader(Integer agreementRequestId) {
		List<AgreementPlaceHolder> agreementPlaceHolderData = agreementDao.getAllAgreementPlaceHolders();
		HashMap<String, Object> agreementView = agreementDao.fetchAgreementViewBasedOnAgreementId(agreementRequestId);
		Map<String, String> agreementPlaceHolders = new HashMap<>();
		if (agreementView != null) {
			for (AgreementPlaceHolder agreementPlaceHolder : agreementPlaceHolderData) {
				if (agreementView.containsKey(agreementPlaceHolder.getViewColumn())) {
					if (agreementView.get(agreementPlaceHolder.getViewColumn()) != null) {
						agreementPlaceHolders.put(agreementPlaceHolder.getPlaceHolderName(), agreementView.get(agreementPlaceHolder.getViewColumn()).toString());
					} else {
						agreementPlaceHolders.put(agreementPlaceHolder.getPlaceHolderName(), "");
					}
					if (agreementPlaceHolder.getPlaceHolderName().equals("DATE")) {
						agreementPlaceHolders.put(agreementPlaceHolder.getPlaceHolderName(), commonService.convertDateFormatBasedOnTimeZone(commonDao.getCurrentTimestamp().getTime(), Constants.DEFAULT_DATE_FORMAT));
					}
				}
			}
		}
		return agreementPlaceHolders;
	}

	private Map<String, List<String>> preparePlaceHoldersOfAgreementClauses(Integer agreementRequestId) {
		Map<String, List<String>> agreementClauses = new LinkedHashMap<>();
		List<AgreementClauses> agreementClausesDetails = agreementDao.fetchAgreementClausesBasedOnAgreementId(agreementRequestId);
		for (AgreementClauses agreementClause : agreementClausesDetails) {
			if (agreementClauses.containsKey("CL-" + agreementClause.getClausesGroupCode())) {
				agreementClauses.get("CL-" + agreementClause.getClausesGroupCode()).add(agreementClause.getClauses());
			} else {
				List<String> clauses = new ArrayList<>();
//				if (agreementClause.getClausesGroup() != null) {
//					clauses.add(agreementClause.getClausesGroup().getDescription());
//				}
				clauses.add(agreementClause.getClauses());
				agreementClauses.put("CL-" + agreementClause.getClausesGroupCode(), clauses);
			}
		}
//		//Numbering group and sub clauses 
//		Map<String, List<String>> numberedAgreementClauses = new LinkedHashMap<>();
//		int clauseNumber = 0;
//		for (Map.Entry<String, List<String>> clause : agreementClauses.entrySet()) {
//			int subClauseNumber = 0;
//			clauseNumber = clauseNumber + 1;
//			List<String> numberedClauseData = new ArrayList<>();
//			for (String clauseData : clause.getValue()) {
//				if (clause.getValue().indexOf(clauseData) == 0) {
//					numberedClauseData.add(clauseNumber + ". " + clauseData);
//				} else {
//					subClauseNumber = subClauseNumber + 1;
//					numberedClauseData.add(clauseNumber + "." + subClauseNumber + "\t" + clauseData);
//				}
//			}
//			numberedAgreementClauses.put(clause.getKey(), numberedClauseData);
//		}
		return agreementClauses;
	}

	@Override
	public ResponseEntity<byte[]> previewAgreementDocument(AgreementVO agreementVO) {
		Integer versionNumber = 0;
		if (agreementVO.getVersionNumber() != null) {
			versionNumber = agreementVO.getVersionNumber();
		} else {
			versionNumber = agreementDao.fetchMaxVersionAgreementTemplateBasedOnParams(agreementVO.getAgreementTypeCode());
		}
		ResponseEntity<byte[]> attachmentData = null;
		try {
			if (versionNumber != 0) {
				AgreementTypeTemplate agreementTypeTemplate = agreementDao.getAgreementTypeTemplateBasedOnParams(agreementVO.getAgreementTypeCode(), versionNumber);
				byte[] mergedOutput = mergePlaceHoldersOfAgreement(agreementTypeTemplate.getTemplate(), agreementVO.getAgreementRequestId());
				String filename = "previewDocument";
				HttpHeaders headers = new HttpHeaders();
				InputStream isFromFirstData = new ByteArrayInputStream(mergedOutput);
				XWPFDocument document = new XWPFDocument(isFromFirstData);
				PdfOptions options = PdfOptions.create();
				ByteArrayOutputStream pdf = new ByteArrayOutputStream();
				PdfConverter.getInstance().convert(document, pdf, options);
				document.write(pdf);
				document.close();
				byte[] datas = pdf.toByteArray();
				headers.setContentType(MediaType.parseMediaType("application/octet-stream"));
				headers.setContentLength(datas.length);
				headers.setContentDispositionFormData("ab.pdf", filename);
				headers.setCacheControl("must-revalidate, post-check=0, pre-check=0");
				headers.setPragma("public");
				attachmentData = new ResponseEntity<>(datas, headers, HttpStatus.OK);
			}
		} catch (Exception e) {
			e.printStackTrace();
		}
		return attachmentData;
	}

	private List<QuestionnairePrintParameter> setQuestionnaireDetails(String userName, String personId, Integer moduleItemCode, Integer moduleItemKey, Integer subModuleCode, String subModuleItemKey) {
		List<QuestionnairePrintParameter> proposalQuestionnaireList = new ArrayList<>();
		QuestionnaireDataBus questionnaireDataBusData = new QuestionnaireDataBus();
		List<QuestionnaireDataBus> questionnaireList = new ArrayList<>();
		questionnaireDataBusData.setActionPersonId(personId);
		questionnaireDataBusData.setActionUserId(userName);
		questionnaireDataBusData.setModuleItemCode(moduleItemCode);
		questionnaireDataBusData.setModuleItemKey(moduleItemKey.toString());
		questionnaireDataBusData.setModuleSubItemKey(subModuleItemKey);
		try {
			Set<String> agreementTypeCodes = agreementService.prepareAgreementTypeHistory(moduleItemKey);
			agreementTypeCodes.add(subModuleCode.toString());
			agreementTypeCodes.forEach(agreementTypeCode ->{
				questionnaireDataBusData.setModuleSubItemCode(Integer.valueOf(agreementTypeCode));
				questionnaireList.addAll(questionnaireService.getQuestionnaireList(questionnaireService.getApplicableQuestionnaire(questionnaireDataBusData)));
			});
			proposalQuestionnaireList = setQuestionnaireList(questionnaireList);
		} catch (Exception e) {
			logger.error("Exception in setQuestionnaireDetails : {}", e.getMessage());
		}
		return proposalQuestionnaireList;
	}

	@SuppressWarnings("unchecked")
	private List<QuestionnairePrintParameter> setQuestionnaireList(List<QuestionnaireDataBus> questionnaireList) {
		List<QuestionnairePrintParameter> questionnaires = new ArrayList<>();
		if (questionnaireList.isEmpty()) {
			questionnaires.add(new QuestionnairePrintParameter("", new ArrayList<>(), null));
		} else {
			for (QuestionnaireDataBus questionnaireDataBus : questionnaireList) {
				if (questionnaireDataBus.getQuestionnaire() != null) {
					List<QuestionAndAnswer> questionAndAnswers = new ArrayList<>();
					String questionnaireHeader = "";
					if (questionnaireDataBus.getQuestionnaireCompleteFlag().equals("Y")) {
						questionnaireHeader = questionnaireHeader + questionnaireDataBus.getQuestionnaireName() + " ("
								+ "Complete" + ")";
					} else {
						questionnaireHeader = questionnaireHeader + questionnaireDataBus.getQuestionnaireName() + " ("
								+ "In Complete" + ")";
					}
					if (!questionnaireDataBus.getQuestionnaire().getQuestions().isEmpty()) {
						for (HashMap<String, Object> question : questionnaireDataBus.getQuestionnaire()
								.getQuestions()) {
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
								else if (question.get(ANSWER_TYPE).equals("Table")) {
									if (answerMap != null & !answerMap.toString().isEmpty()) {
										QuestionAndAnswer questionAndAnswer = new QuestionAndAnswer();
										questionAndAnswer.setQuestion(question.get(QUESTION).toString());
										questionAndAnswer.setAnswer("");
										questionAndAnswers.add(questionAndAnswer);
									}
								} else {
									if (answerMap.get("1") != null) {
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
					questionnaires.add(new QuestionnairePrintParameter(questionnaireHeader, questionAndAnswers, null));
				}
			}
		}
		return questionnaires;
	}

	@Override
	public byte[] getPrintLetterTemplate(String templateTypeCode) {
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
			logger.error("Exception in getLetterTemplate" + e.getMessage());
		}
		return data;
	}

	@Override
	public ResponseEntity<byte[]> generateAgreementSummary(HttpServletResponse response, Integer agreementRequestId,
			String personId, String userName) {
		ResponseEntity<byte[]> attachmentData = null;
		try {
			byte[] bFile = getPrintLetterTemplate(Constants.AGREEMENT_SUMMARY_LETTER_TEMPLATE_TYPE_CODE);
			byte[] mergedOutput = setMergePlaceHoldersOfAgreementSummary(bFile, agreementRequestId, personId, userName);
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
			logger.error("Exception in generateAgreementSummary : {}", e.getMessage());
		}
		return attachmentData;
	}

	@Override
	public byte[] setMergePlaceHoldersOfAgreementSummary(byte[] data, Integer agreementRequestId, String personId, String userName) {
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		try {
			InputStream myInputStream = new ByteArrayInputStream(data);
			IXDocReport report = XDocReportRegistry.getRegistry().loadReport(myInputStream,
					TemplateEngineKind.Velocity);
			FieldsMetadata fieldsMetadata = report.createFieldsMetadata();
			AgreementHeader agreementHeader = agreementDao.getAgreementById(agreementRequestId);

			List<AgreementSponsor> agreementSponsors = agreementService.getAgreementSponsors(agreementRequestId);
			fieldsMetadata.load("agreementSponsors", AgreementPrintParameter.class, true);
			List<AgreementPrintParameter> agreementPrintSponsors = setAgreementSponsors(agreementSponsors);

			List<AgreementPeople> agreementPeoples = agreementService.preparePeopleDetails(agreementDao.getAllAgreementPeople(agreementRequestId));
			fieldsMetadata.load("agreementPeoples", AgreementPrintParameter.class, true);
			List<AgreementPrintParameter> agreementPrintPeoples = setAgreementPeoples(agreementPeoples);

			List<AgreementClausesGroup> agreementClauses = agreementService.prepareAgreementClauses(agreementDao.fetchAgreementClausesBasedOnAgreementId(agreementRequestId));
			fieldsMetadata.load("agreementClauses", AgreementPrintParameter.class, true);
			List<AgreementPrintParameter> agreementPrintClauses = setAgreementClauses(agreementClauses);

			fieldsMetadata.load("questionnaires", QuestionnairePrintParameter.class, true);
			List<QuestionnairePrintParameter> questionnaires = setQuestionnaireDetails(null, personId, Constants.AGREEMENT_MODULE_CODE, agreementRequestId, Integer.parseInt(agreementHeader.getAgreementTypeCode()), Constants.SUBMODULE_ITEM_KEY);

			IContext context = report.createContext();
			context.put("agreementSponsors", agreementPrintSponsors);
			context.put("agreementPeoples", agreementPrintPeoples);
			context.put("agreementClauses", agreementPrintClauses);
			context.put("questionnaires", questionnaires);
			context = setAgreementSummaryPlaceHolderData(context, agreementHeader);

			Options options = Options.getTo(ConverterTypeTo.PDF).via(ConverterTypeVia.XWPF);
			report.convert(context, options, baos);
		} catch (Exception e) {
			logger.error("Exception in mergePlaceHolders : {}", e.getMessage());
		}
		return baos.toByteArray();
	}

	private IContext setAgreementSummaryPlaceHolderData(IContext context, AgreementHeader agreementHeader) {

		context.put("AGREEMENT_REQUEST_ID", agreementHeader.getAgreementRequestId());
		if (agreementHeader.getTitle() != null) {
			context.put("TITLE", agreementHeader.getTitle());
		} else {
			context.put("TITLE", "");
		}
		if (agreementHeader.getRequestorPersonId() != null) {
			context.put("REQUESTOR", agreementHeader.getRequestorName());
		} else {
			context.put("REQUESTOR", "");
		}
		if (agreementHeader.getAgreementStatus() != null) {
			context.put("STATUS", agreementHeader.getAgreementStatus().getDescription());
		} else {
			context.put("STATUS", "");
		}
		if (agreementHeader.getAgreementType() != null) {
			context.put("AREEMENT_TYPE", agreementHeader.getAgreementType().getDescription());
		} else {
			context.put("AREEMENT_TYPE", "");
		}
		if (agreementHeader.getAgreementCategory() != null) {
			context.put("CATEGORY", agreementHeader.getAgreementCategory().getDescription());
		} else {
			context.put("CATEGORY", "");
		}
		if (agreementHeader.getStartDate() != null) {
			String startDate = commonService.convertDateFormatBasedOnTimeZone(agreementHeader.getStartDate().getTime(),
					Constants.DEFAULT_DATE_FORMAT);
			context.put("START_DATE", startDate);
		} else {
			context.put("START_DATE", "");
		}
		if (agreementHeader.getEndDate() != null) {
			String endDate = commonService.convertDateFormatBasedOnTimeZone(agreementHeader.getEndDate().getTime(),
					Constants.DEFAULT_DATE_FORMAT);
			context.put("END_DATE", endDate);
		} else {
			context.put("END_DATE", "");
		}
		if (agreementHeader.getUnitName() != null) {
			context.put("LEAD_UNIT", commonService.getUnitFormatByUnitDetail(agreementHeader.getUnitNumber(), agreementHeader.getUnitName()));
		} else {
			context.put("LEAD_UNIT", "");
		}
		if (agreementHeader.getContractValue() != null) {
			String contractValue = "";
			if (agreementHeader.getCurrency() != null ) {
				contractValue = contractValue.concat(agreementHeader.getCurrency().getCurrencySymbol() + " ");
			}
			contractValue = contractValue.concat(agreementHeader.getContractValue().toString());
			context.put("CONTRACT_VALUE", contractValue);
		} else {
			context.put("CONTRACT_VALUE", "");
		}
		if (agreementHeader.getAmountInWords() != null) {
			context.put("AMOUNT_IN_WORDS", agreementHeader.getAmountInWords());
		} else {
			context.put("AMOUNT_IN_WORDS", "");
		}
		if (agreementHeader.getRemarks() != null) {
			context.put("DESCRIPTION", agreementHeader.getRemarks());
		} else {
			context.put("DESCRIPTION", "");
		}
		return context;
		
	}

	private List<AgreementPrintParameter> setAgreementClauses(List<AgreementClausesGroup> agreementClauses) {
		List<AgreementPrintParameter> agreementPrintParameters = new ArrayList<>();
		agreementClauses.forEach(agreementClause -> {
			AgreementPrintParameter agreementPrintParameter = new AgreementPrintParameter();
			agreementPrintParameter.setClausesGroupName(agreementClause.getClausesGroup().getDescription());
			Map<Integer, String> clauses = agreementClause.getClauses();
			ArrayList<String> valueList = new ArrayList<>(clauses.values());
			StringBuilder subClause = new StringBuilder(0);
			for (String subclauseData : valueList) {
				subClause.append("\n" +subclauseData + "\n");
			}
			agreementPrintParameter.setClauses(subClause.toString());
			agreementPrintParameters.add(agreementPrintParameter);
		});
		return agreementPrintParameters;
	}

	private List<AgreementPrintParameter> setAgreementPeoples(List<AgreementPeople> agreementPeoples) {
		List<AgreementPrintParameter> agreementPrintParameters = new ArrayList<>();
		agreementPeoples.forEach(agreementPeople -> {
			AgreementPrintParameter agreementPrintParameter = new AgreementPrintParameter();
			agreementPrintParameter.setPersonName(agreementPeople.getFullName());
			agreementPrintParameter.setPersonType(agreementPeople.getAgreementPeopleType().getDescription());
			agreementPrintParameter.setPersonEmail(agreementPeople.getEmail());
			agreementPrintParameter.setPersonPhone(agreementPeople.getPhoneNumber());
			agreementPrintParameter.setPersonDepartment(agreementPeople.getDepartment());
			agreementPrintParameters.add(agreementPrintParameter);
		});
		return agreementPrintParameters;
	}

	private List<AgreementPrintParameter> setAgreementSponsors(List<AgreementSponsor> agreementSponsors) {
		List<AgreementPrintParameter> agreementPrintParameters = new ArrayList<>();
		agreementSponsors.forEach(agreementSponsor -> {
			AgreementPrintParameter agreementPrintParameter = new AgreementPrintParameter();
			agreementPrintParameter.setSponsorName(agreementSponsor.getSponsorName());
			if (agreementSponsor.getSponsorRole() != null) {
				agreementPrintParameter.setSponsorRole(agreementSponsor.getSponsorRole().getDescription());
			}
			if (agreementSponsor.getAgreementSponsorType() != null) {
				agreementPrintParameter.setSponsorAgreementType(agreementSponsor.getAgreementSponsorType().getDescription());
			}
			if (agreementSponsor.getSponsor() != null) {
				Sponsor sponsor = agreementSponsor.getSponsor();
				agreementPrintParameter.setSponsorType(sponsor.getSponsorType().getDescription());
				String address = "";
				if (sponsor.getAddressLine1() != null) {
					address = address.concat(sponsor.getAddressLine1());
				}
				if (sponsor.getAddressLine2() != null) {
					address = address.concat(", "+sponsor.getAddressLine2());
				}
				agreementPrintParameter.setSponsorAddress(address);
				agreementPrintParameter.setSponsorLocation(sponsor.getSponsorLocation());
				agreementPrintParameter.setSponsorState(sponsor.getState());
				agreementPrintParameter.setSponsorZip(sponsor.getPostalCode());
				agreementPrintParameter.setSponsorCountry("");
				if (sponsor.getCountry() != null) {
					agreementPrintParameter.setSponsorCountry(sponsor.getCountry().getCountryName());
				}
			}
			if (agreementSponsor.getAgreementSponsorContacts() != null && !agreementSponsor.getAgreementSponsorContacts().isEmpty()) {
				List<AgreementSponsorContactParameter> agreementSponsorContacts = new ArrayList<>();
				agreementSponsor.getAgreementSponsorContacts().forEach(agreementSponsorContact -> {
					AgreementSponsorContactParameter agreementSponsorContactData = new AgreementSponsorContactParameter();
					String name = "";
					if (agreementSponsorContact.getSalutation() != null) {
						name = name.concat(agreementSponsorContact.getSalutation() + " ");
					}
					if (agreementSponsorContact.getContactPersonName() != null) {
						name = name.concat(agreementSponsorContact.getContactPersonName());
					} else {
						name = "";
					}
					agreementSponsorContactData.setContactName(name);
					agreementSponsorContactData.setContactType(agreementSponsorContact.getAgreementSponsorContactType() != null ? agreementSponsorContact.getAgreementSponsorContactType().getDescription() : "");
					agreementSponsorContactData.setContactAddress(agreementSponsorContact.getContactAddressLine() != null ? agreementSponsorContact.getContactAddressLine() : "");
					agreementSponsorContactData.setContactEmail(agreementSponsorContact.getContactEmailId() != null ? agreementSponsorContact.getContactEmailId() : "");
					agreementSponsorContactData.setContactPhone(agreementSponsorContact.getContactPhone() != null ? agreementSponsorContact.getContactPhone() : "");
					agreementSponsorContactData.setContactDesignation(agreementSponsorContact.getDesignation() != null ? agreementSponsorContact.getDesignation() : "");
					agreementSponsorContacts.add(agreementSponsorContactData);
				});
				agreementPrintParameter.getAgreementSponsorContacts().addAll(agreementSponsorContacts);
			}
			agreementPrintParameters.add(agreementPrintParameter);
		});
		return agreementPrintParameters;
	}
}
