package com.polus.fibicomp.questionnaire.service;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import javax.servlet.http.HttpServletResponse;

import com.polus.fibicomp.questionnaire.pojo.QuestTableAnswer;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpHeaders;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.multipart.MultipartHttpServletRequest;

import com.polus.fibicomp.award.service.AwardService;
import com.polus.fibicomp.common.dao.CommonDao;
import com.polus.fibicomp.common.service.CommonService;
import com.polus.fibicomp.constants.Constants;
import com.polus.fibicomp.dbengine.DBEngine;
import com.polus.fibicomp.dbengine.DBEngineConstants;
import com.polus.fibicomp.dbengine.Parameter;
import com.polus.fibicomp.general.dao.GeneralInformationDao;
import com.polus.fibicomp.proposal.dao.ProposalDao;
import com.polus.fibicomp.proposal.pojo.Proposal;
import com.polus.fibicomp.questionnaire.dao.QuestionnaireDAO;
import com.polus.fibicomp.questionnaire.dto.DeleteDto;
import com.polus.fibicomp.questionnaire.dto.QuestionnaireAnswerDto;
import com.polus.fibicomp.questionnaire.dto.QuestionnaireDataBus;
import com.polus.fibicomp.questionnaire.dto.QuestionnaireDto;
import com.polus.fibicomp.questionnaire.pojo.QuestAnswer;
import com.polus.fibicomp.questionnaire.pojo.QuestAnswerAttachment;
import com.polus.fibicomp.questionnaire.pojo.QuestAnswerHeader;
import com.polus.fibicomp.questionnaire.print.service.QuestionnairePrintService;
import com.polus.fibicomp.security.AuthenticatedUser;
import com.polus.fibicomp.vo.LookUp;

@Service(value = "questionnaireService")
public class QuestionnaireServiceImpl implements QuestionnaireService {

	protected static Logger logger = LogManager.getLogger(QuestionnaireServiceImpl.class.getName());

	@Autowired
	public DBEngine dbEngine;

	@Autowired
	public QuestionnaireDAO questionnaireDAO;

	@Autowired
	private QuestionnairePrintService questionnairePrintService;

	@Autowired
	private ProposalDao proposalDao;

	@Autowired
	private CommonDao commonDao;

	@Autowired
	private CommonService commonService;

	@Autowired
	private AwardService awardService;

	@Autowired
	private GeneralInformationDao generalInformationDao;

	private static final String RADIO = "Radio";
	private static final String CHECKBOX = "Checkbox";
	private static final String TEXT = "Text";
	private static final String ATTACHMENT = "Attachment";
	private static final String QUESTION_ID = "QUESTION_ID";
	private static final String AC_TYPE = "AC_TYPE";
	private static final String ANSWER_TYPE = "ANSWER_TYPE";
	private static final String NO_OF_ANSWERS = "NO_OF_ANSWERS";
	private static final String ANSWERS = "ANSWERS";
	private static final String ATTACHMENT_ID = "ATTACHMENT_ID";
	private static final String QUESTIONNAIRE_ANSWER_ATTACHMENT_ID = "<<QUESTIONNAIRE_ANSWER_ATT_ID>>";
	private static final String UPDATE_TIMESTAMP_PARAM = "<<UPDATE_TIMESTAMP>>";
	private static final String UPDATE_USER_PARAM = "<<UPDATE_USER>>";
	private static final String QUESTIONNAIRE_ANSWER_ID_PARAM = "<<QUESTIONNAIRE_ANSWER_ID>>";
	private static final String QUESTIONNAIRE_ANSWER_HEADER_ID_PARAM = "<<QUESTIONNAIRE_ANS_HEADER_ID>>";
	private static final Boolean TRUE = true;
	private static final Boolean FALSE = false;
	private static final String QUESTION_ID_PARAM = "<<QUESTION_ID>>";
	private static final String ANSWER_NUMBER = "ANSWER_NUMBER";
	private static final String GROUP_NAME = "GROUP_NAME";
	private static final String SHOW_QUESTION = "SHOW_QUESTION";
	private static final String QUESTIONNAIRE_COMPLETED_FLAG = "QUESTIONNAIRE_COMPLETED_FLAG";
	private static final String QUESTIONNAIRE_NUMBER = "QUESTIONNAIRE_NUMBER";
	private static final String QUESTIONNAIRE_ID = "QUESTIONNAIRE_ID";
	private static final String ANSWER = "ANSWER";
	private static final String QUESTION_VERSION_NUMBER = "QUESTION_VERSION_NUMBER";
	private static final String PARENT_QUESTION_ID = "PARENT_QUESTION_ID";
	private static final String QUESTIONNAIRE_ANSWER_HEADER_ID = "QUESTIONNAIRE_ANS_HEADER_ID";
	private static final String SORT_ORDER = "SORT_ORDER";
	private static final String UPDATE_USER = "UPDATE_USER";
	private static final String QUESTION_NUMBER = "QUESTION_NUMBER";

	@Override
	public QuestionnaireDataBus getApplicableQuestionnaire(QuestionnaireDataBus questionnaireDataBus) {
		try {
		String moduleItemKey = questionnaireDataBus.getModuleItemKey();
		String moduleSubItemKey = questionnaireDataBus.getModuleSubItemKey();
		Integer moduleItemCode = questionnaireDataBus.getModuleItemCode();
		Integer moduleSubItemCode = questionnaireDataBus.getModuleSubItemCode();
		String actionPersonId = questionnaireDataBus.getActionPersonId();
		String updateUser = AuthenticatedUser.getLoginUserName();
		String questionaireMode = questionnaireDataBus.getQuestionnaireMode();
		logger.info("moduleItemKey : {}", moduleItemKey);
		logger.info("moduleSubItemKey : {}", moduleSubItemKey);
		logger.info("moduleItemCode : {}", moduleItemCode);
		logger.info("moduleSubItemCode : {}", moduleSubItemCode);
		logger.info("actionPersonId : {}", actionPersonId);
		logger.info("updateUser : {}", updateUser);
		List<HashMap<String, Object>> applicableQuestionnaire = questionnaireDAO.getApplicableQuestionnaireData(
				moduleItemKey, moduleSubItemKey, moduleItemCode, moduleSubItemCode, actionPersonId, updateUser, questionaireMode);
		if(questionnaireDataBus.getQuestionnaireNumbers() !=null && !questionnaireDataBus.getQuestionnaireNumbers().isEmpty()){
			applicableQuestionnaire = applicableQuestionnaire.stream().filter(queObjs ->
					questionnaireDataBus.getQuestionnaireNumbers().contains(queObjs.get("QUESTIONNAIRE_NUMBER"))).collect(Collectors.toList());
		}
		questionnaireDataBus.setApplicableQuestionnaire(applicableQuestionnaire);
		return questionnaireDataBus;
		} catch (Exception e) {
			logger.error("Exception in getApplicableQuestionnaire : {}", e.getMessage());
			return null;
		}
	}

	public QuestionnaireDataBus getQuestionnaireDetails(QuestionnaireDataBus questionnaireDataBus) {
		QuestionnaireDto questionnaire = new QuestionnaireDto();
		ArrayList<HashMap<String, Object>> answerList = new ArrayList<>();
		ArrayList<HashMap<String, Object>> questionsList = new ArrayList<>();
		Integer questionnaireAnswerHeaderId = questionnaireDataBus.getQuestionnaireAnswerHeaderId();
		Integer questionnaireId = questionnaireDataBus.getQuestionnaireId();
		Integer newQuestionnaireId = questionnaireDataBus.getNewQuestionnaireId();
		logger.info("questionnaireAnswerHeaderId : {}", questionnaireAnswerHeaderId);
		logger.info("questionnaireId: {}", questionnaireId);
		logger.info("newQuestionnaireId: {}", newQuestionnaireId);
		try {
			if(newQuestionnaireId != null && newQuestionnaireId.equals(questionnaireId)) {
				questionsList = getQuestionnaireQuestions(newQuestionnaireId);
				questionnaireAnswerHeaderId = questionnaireDAO.copyAnswerToNewVersion(questionnaireAnswerHeaderId, newQuestionnaireId);
				questionnaireId = newQuestionnaireId;
				questionnaireDataBus.setNewQuestionnaireId(null);
				questionnaireDataBus.setQuestionnaireId(questionnaireId);
				questionnaireDataBus.setQuestionnaireAnswerHeaderId(questionnaireAnswerHeaderId);
			} else {
				questionsList = getQuestionnaireQuestions(questionnaireId);
			}
			if (questionnaireAnswerHeaderId != null) {
				answerList = getAnswer(questionnaireAnswerHeaderId);
				getTableAnswers(answerList, questionnaireAnswerHeaderId);
			}
			questionsList = setAnswerToQuestionnaireQuestion(questionsList, answerList);
			questionnaire.setQuestions(questionsList);
			List<HashMap<String, Object>> header = questionnaireDAO.getQuestionnaireHeader(questionnaireId, questionnaireAnswerHeaderId);
			List<HashMap<String, Object>> condition = questionnaireDAO.getQuestionnaireCondition(questionnaireId);
			List<HashMap<String, Object>> option = questionnaireDAO.getQuestionnaireOptions(questionnaireId);
			questionnaireDataBus.setHeader(header.get(0));
			questionnaire.setConditions(condition);
			questionnaire.setOptions(option);
		} catch (Exception e) {
			logger.error("Exception in getQuestionnaireDetails : {}", e.getMessage());
		}
		questionnaireDataBus.setQuestionnaire(questionnaire);
		return questionnaireDataBus;
	}

	@Override
	public ResponseEntity<String> saveQuestionnaireAnswers(QuestionnaireDataBus questionnaireDataBus,
			MultipartHttpServletRequest request) {
		Integer moduleCode = questionnaireDataBus.getModuleItemCode();
		Integer moduleSubItemCode = questionnaireDataBus.getModuleSubItemCode();
		String moduleItemKey = questionnaireDataBus.getModuleItemKey();
		String moduleSubItemKey = questionnaireDataBus.getModuleSubItemKey();
		Integer questionnaireId = questionnaireDataBus.getQuestionnaireId();
		String updateUser = AuthenticatedUser.getLoginUserName();
		String acType = questionnaireDataBus.getAcType();
		MultipartFile multipartFile = generateAttachment(questionnaireDataBus, request);
		if (multipartFile != null || questionnaireDataBus.getFileContent() == null) {
			Integer questionnaireAnsHeaderId = questionnaireDataBus.getQuestionnaireAnswerHeaderId();
			logger.info("moduleItemKey : {}", moduleItemKey);
			logger.info("moduleSubItemKey : {}", moduleSubItemKey);
			logger.info("moduleItemCode : {}", moduleCode);
			logger.info("moduleSubItemCode : {}", moduleSubItemCode);
			logger.info("questionnaireId : {}", questionnaireId);
			logger.info("updateUser : {}", updateUser);
			try {
				String questionnaireCompletionFlag = questionnaireDataBus.getQuestionnaireCompleteFlag();
				if (questionnaireAnsHeaderId == null || (acType != null && acType.equals("I"))) {
					questionnaireAnsHeaderId = questionnaireDAO.insertQuestionnaireAnswerHeader(moduleCode, moduleSubItemCode, moduleItemKey, moduleSubItemKey, updateUser, questionnaireId);
					questionnaireDataBus.setQuestionnaireAnswerHeaderId(questionnaireAnsHeaderId);
				}
				List<HashMap<String, Object>> questionlist = questionnaireDataBus.getQuestionnaire().getQuestions();
				for (HashMap<String, Object> question : questionlist) {
					saveAnswer(question, questionnaireDataBus, request, multipartFile);
				}
				updateQuestionnaireCompleteFlag(questionnaireAnsHeaderId, questionnaireCompletionFlag);
				questionnaireDataBus.getHeader().put("ANS_PERSON_FULL_NAME", AuthenticatedUser.getLoginUserFullName());
				questionnaireDataBus.getHeader().put("ANS_UPDATE_TIMESTAMP", commonDao.getCurrentTimestamp());
			} catch (Exception e) {
				logger.error("Exception in saveQuestionnaireAnswers : {}", e.getMessage());
				return new ResponseEntity<>(commonDao.convertObjectToJSON(questionnaireDataBus), HttpStatus.INTERNAL_SERVER_ERROR);
			}
		}
		setDocumentUpdateUserAndTimestamp(moduleCode, Integer.parseInt(moduleItemKey), updateUser);
		return new ResponseEntity<>(commonDao.convertObjectToJSON(questionnaireDataBus), HttpStatus.OK);
	}

	private List<HashMap<String, Object>> getQuestionOptions(Integer questionId, List<HashMap<String, Object>> options) {
		ArrayList<HashMap<String, Object>> questionOption = new ArrayList<>();
		for (HashMap<String, Object> option : options) {
			if (questionId == Integer.parseInt(option.get(QUESTION_ID).toString())) {
				questionOption.add(option);
			}
		}
		return questionOption;
	}

	@SuppressWarnings("unchecked")
	private void saveAnswer(HashMap<String, Object> question, QuestionnaireDataBus questionnaireDataBus, MultipartHttpServletRequest request, MultipartFile multipartFile) throws Exception {
		String updateUser = AuthenticatedUser.getLoginUserName();
		Integer answerHeaderId = questionnaireDataBus.getQuestionnaireAnswerHeaderId();
		HashMap<Integer, String> answers = new HashMap<>();
		List<HashMap<String, Object>> option = questionnaireDataBus.getQuestionnaire().getOptions();
		Integer questionId = (Integer) question.get(QUESTION_ID);
		String acType = null;
		if (questionnaireDataBus.getIsInserted() != null && questionnaireDataBus.getIsInserted()) {
			acType = "I";
		} else {
			acType = (String) question.get(AC_TYPE);
		}
		if (TEXT.equals(question.get(ANSWER_TYPE))) {
			deleteAnswer(answerHeaderId, question);
			Integer noOfAnswer = Integer.parseInt((question.get(NO_OF_ANSWERS).toString()));
			if ((question.get(AC_TYPE) != null ) && !((String) question.get(AC_TYPE)).equals("D")) {
				acType = "I";
				if (question.get(ANSWERS) != null) {
					answers = (HashMap<Integer, String>) question.get(ANSWERS);
				}
				for (Integer index = 1; index <= noOfAnswer; index++) {
					saveAnswer(answerHeaderId, question, acType, answers.get(index.toString()), null, index, updateUser);
				}
			} else if ((question.get(AC_TYPE) != null ) && ((String) question.get(AC_TYPE)).equals("D")) {
				question.put(AC_TYPE, null);
			}
		}
		else if (Constants.SYSTEM_LOOKUP.equals(question.get(ANSWER_TYPE)) || Constants.USER_LOOKUP.equals(question.get(ANSWER_TYPE))) {
			deleteAnswer(answerHeaderId, question);
			HashMap<Integer, HashMap<String, String>> answersLookUp = new HashMap<>();
			if ((question.get(AC_TYPE) != null ) && !((String) question.get(AC_TYPE)).equals("D")) {
				acType = "I";
				if (question.get(ANSWERS) != null) {
					answersLookUp = (HashMap<Integer, HashMap<String, String>>) question.get(ANSWERS);
				}
				for (Integer index = 0; index < answersLookUp.size(); index++) {
					saveAnswer(answerHeaderId, question, acType, (answersLookUp.get(index.toString())).get("description"), (answersLookUp.get(index.toString())).get("code"), index, updateUser);
				}
			} else if ((question.get(AC_TYPE) != null ) && ((String) question.get(AC_TYPE)).equals("D")) {
				question.put(AC_TYPE, null);
			}
		} else if (CHECKBOX.equals(question.get(ANSWER_TYPE))) {
			deleteAnswer(answerHeaderId, question);
			if ((question.get(AC_TYPE) != null) && !((String) question.get(AC_TYPE)).equals("D")) {
				acType = "I";
				Integer noOfAnswer = Integer.parseInt((question.get(NO_OF_ANSWERS).toString()));
				List<HashMap<String, Object>> questionOption = getQuestionOptions(questionId, option);
				answers = (HashMap<Integer, String>) question.get(ANSWERS);
				for (HashMap<String, Object> opt : questionOption) {
					String answer = opt.get("OPTION_LABEL").toString();
					Object answerObj = answers.get(answer);
					/* (boolean) answerObj */
					if (answerObj != null) {
						boolean isAnswered = Boolean.parseBoolean(answerObj.toString());
						if (isAnswered && !question.get(AC_TYPE).equals("D")) {
							saveAnswer(answerHeaderId, question, acType, answer, null, noOfAnswer, updateUser);
						}
					}
				}
			} else if (question.get(AC_TYPE) != null && question.get(AC_TYPE).equals("D")) {
				question.put(AC_TYPE, null);
			}
		} else if (ATTACHMENT.equals(question.get(ANSWER_TYPE))) {
			answers = (HashMap<Integer, String>) question.get(ANSWERS);
			String answer = answers.get("1");
			if ("D".equals(acType)) {
				if (multipartFile != null) {
					saveAttachmentAnswer(question, acType, updateUser, multipartFile);
				} else {
					saveAttachmentAnswer(question, acType, updateUser, request);
				}
				saveAnswer(answerHeaderId, question, acType, answer, null, 1, updateUser);
			} else {
				if (answer != null && !answer.isEmpty()) {
					saveAnswer(answerHeaderId, question, acType, answer, null, 1, updateUser);
					if (multipartFile != null) {
						saveAttachmentAnswer(question, acType, updateUser, multipartFile);
					} else {
						saveAttachmentAnswer(question, acType, updateUser, request);
					}
				}
			}
		} else if (Constants.TABLE.equals(question.get(ANSWER_TYPE))){
			if (question.get(ANSWERS) != null)
			    saveTableAnswer(answerHeaderId, question, (HashMap<String, Object>) question.get(ANSWERS), updateUser);
		}
		else {
			answers = (HashMap<Integer, String>) question.get(ANSWERS);
			String answerLookupCode = question.get("ANSWER_LOOKUP_CODE") != null ? question.get("ANSWER_LOOKUP_CODE").toString(): null;
			String answer = answers.get("1");
			saveAnswer(answerHeaderId, question, acType, answer, answerLookupCode, 1, updateUser);
		}
	}

	private void saveAttachmentAnswer(HashMap<String, Object> question, String acType, String updateUser, MultipartFile multipartFile) throws Exception{
		insertOrUpdateAttachment(question, acType, updateUser, multipartFile);	
	}

	private void saveAttachmentAnswer(HashMap<String, Object> question, String acType, String updateUser, MultipartHttpServletRequest request) throws Exception {
		MultipartFile file = getAttachmentFile(request, question);
		if (file != null) {
			insertOrUpdateAttachment(question, acType, updateUser, file);
		}
	}

	private void insertOrUpdateAttachment(HashMap<String, Object> question, String acType, String updateUser, MultipartFile file)  throws Exception{
		Integer questionnaireAnsAttachmentId = null;
		Object attachmentId = question.get(ATTACHMENT_ID);
		if (attachmentId != null) {
			questionnaireAnsAttachmentId = Integer.parseInt(question.get(ATTACHMENT_ID).toString());
		} else {
			acType = "I";
		}
		if ("D".equals(acType)) {
			if (questionnaireAnsAttachmentId != null) {
				deleteAttachmentAnswer(questionnaireAnsAttachmentId);
			}
		} else if ("U".equals(acType)) {
			if (questionnaireAnsAttachmentId != null) {
				updateAttachmentAnswer(questionnaireAnsAttachmentId, file, updateUser);
			}
		} else if ("I".equals(acType)) {
			insertAttachmentAnswer(question, updateUser, file);
			question.put(AC_TYPE, "U");
		}	
	}

	private MultipartFile getAttachmentFile(MultipartHttpServletRequest request, HashMap<String, Object> question) {
		MultipartFile file = null;
		if (request != null && request.getFileMap() != null) {
			for (String attachmentKey : request.getFileMap().keySet()) {
				if (question.get(QUESTION_ID).toString().equalsIgnoreCase(attachmentKey)) {
					file = request.getFileMap().get(attachmentKey);
				}
			}
		}
		return file;
	}

	private Integer deleteAttachmentAnswer(Integer questionnaireAnsAttachmentId) throws Exception {
		Integer isUpdated = 0;
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter(QUESTIONNAIRE_ANSWER_ATTACHMENT_ID, DBEngineConstants.TYPE_INTEGER,
					questionnaireAnsAttachmentId));
			isUpdated = dbEngine.executeUpdate(inParam, "DELETE_QUESTIONNAIRE_ATTACHMENT_ANSWER");
		} catch (Exception e) {
			logger.error("Exception in deleteQuestionnaireAnswers : {}", e.getMessage());
			throw e;
		}
		return isUpdated;
	}

	private void updateAttachmentAnswer(Integer questionnaireAnsAttachmentId, MultipartFile file, String updateUser) throws Exception {
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<ATTACHMENT>>", DBEngineConstants.TYPE_BLOB, file.getBytes()));
			inParam.add(new Parameter("<<FILE_NAME>>", DBEngineConstants.TYPE_STRING, file.getOriginalFilename()));
			inParam.add(new Parameter("<<CONTENT_TYPE>>", DBEngineConstants.TYPE_STRING, file.getContentType()));
			inParam.add(new Parameter(UPDATE_TIMESTAMP_PARAM, DBEngineConstants.TYPE_TIMESTAMP, commonDao.getCurrentTimestamp()));
			inParam.add(new Parameter(UPDATE_USER_PARAM, DBEngineConstants.TYPE_STRING, updateUser));
			inParam.add(new Parameter(QUESTIONNAIRE_ANSWER_ATTACHMENT_ID, DBEngineConstants.TYPE_INTEGER,
					questionnaireAnsAttachmentId));
			dbEngine.executeUpdate(inParam, "UPDATE_QUESTIONNAIRE_ATTACHMENT_ANSWER");
		} catch (Exception e) {
			logger.error("Exception in updateAttachmentAnswer : {}", e.getMessage());
			throw e;
		}
	}

	private void saveAnswer(Integer answerHeaderId, HashMap<String, Object> question, String acType, String answer,
			String lookUpCode, Integer answerNumber, String updateUser) throws Exception {
		if ("I".equals(acType)) {
			Integer questionId = Integer.parseInt(question.get(QUESTION_ID).toString());
			String explanation = question.get("EXPLANATION")!= null ? question.get("EXPLANATION").toString() : null;
			Integer questionnaireAnswerId = questionnaireDAO.insertQuestionnaireAnswer(answerHeaderId, questionId, answer, lookUpCode, answerNumber, explanation, updateUser);
			question.put(AC_TYPE, "U");
			question.put("QUESTIONNAIRE_ANS_ID", questionnaireAnswerId);
		} else if ("U".equals(acType)) {
			updateAnswer(answerHeaderId, question, answer, lookUpCode, answerNumber, updateUser);
		} else if ("D".equals(acType)) {
			deleteQuestAnswerAttachment(answerHeaderId, question);
			deleteAnswer(answerHeaderId, question);
			question.put(AC_TYPE, null);
		}
	}

	private Integer insertAttachmentAnswer(HashMap<String, Object> question, String updateUser, MultipartFile file) throws Exception {
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			Integer questionnaireAnsAttachmentId = questionnaireDAO.getNextQuestionAnswerAttachId();
//			inParam.add(new Parameter(QUESTIONNAIRE_ANSWER_ATTACHMENT_ID, DBEngineConstants.TYPE_INTEGER, questionnaireAnsAttachmentId));
			inParam.add(new Parameter(QUESTIONNAIRE_ANSWER_ID_PARAM, DBEngineConstants.TYPE_INTEGER, question.get("QUESTIONNAIRE_ANS_ID"))); 
			inParam.add(new Parameter("<<ATTACHMENT>>", DBEngineConstants.TYPE_BLOB, file.getBytes()));
			inParam.add(new Parameter("<<FILE_NAME>>", DBEngineConstants.TYPE_STRING, file.getOriginalFilename()));
			inParam.add(new Parameter("<<CONTENT_TYPE>>", DBEngineConstants.TYPE_STRING, file.getContentType()));
			inParam.add(new Parameter(UPDATE_TIMESTAMP_PARAM, DBEngineConstants.TYPE_TIMESTAMP, commonDao.getCurrentTimestamp()));
			inParam.add(new Parameter(UPDATE_USER_PARAM, DBEngineConstants.TYPE_STRING, updateUser));
			dbEngine.executeUpdate(inParam, "INSERT_QUESTIONNAIRE_ATTACHMENT_ANSWER");
			question.put(ATTACHMENT_ID, questionnaireAnsAttachmentId);
		} catch (Exception e) {
			logger.error("Exception in insertQuestionnaireAnswer : {}", e.getMessage());
			throw e;
		}
		return 1;
	}

	/* Intentionally commented 
	private Integer insertAnswer(Integer answerHeaderId, HashMap<String, Object> question, String answer, String lookUpCode, Integer answerNumber, String updateUser) throws Exception {
		Integer questionnaireAnswerId = questionnaireDAO.getNextQuestionnaireAnswerId();
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter(QUESTIONNAIRE_ANSWER_ID_PARAM, DBEngineConstants.TYPE_INTEGER, questionnaireAnswerId));
			inParam.add(new Parameter(QUESTIONNAIRE_ANSWER_HEADER_ID_PARAM, DBEngineConstants.TYPE_INTEGER, answerHeaderId));
			inParam.add(new Parameter(QUESTION_ID_PARAM, DBEngineConstants.TYPE_INTEGER, question.get(QUESTION_ID)));
			inParam.add(new Parameter("<<ANSWER_NUMBER>>", DBEngineConstants.TYPE_INTEGER, answerNumber));
			inParam.add(new Parameter("<<ANSWER>>", DBEngineConstants.TYPE_STRING, answer));
			inParam.add(new Parameter("<<ANSWER_LOOKUP_CODE>>", DBEngineConstants.TYPE_STRING, lookUpCode));
			inParam.add(new Parameter("<<EXPLANATION>>", DBEngineConstants.TYPE_STRING, question.get("EXPLANATION")));
			inParam.add(new Parameter(UPDATE_TIMESTAMP_PARAM, DBEngineConstants.TYPE_TIMESTAMP, commonDao.getCurrentTimestamp()));
			inParam.add(new Parameter(UPDATE_USER_PARAM, DBEngineConstants.TYPE_STRING, updateUser));
			dbEngine.executeUpdate(inParam, "INSERT_QUESTIONNAIRE_ANSWER");
		} catch (Exception e) {
			logger.error("Exception in insertQuestionnaireAnswer : {}", e.getMessage());
			throw e;
		}
		return questionnaireAnswerId;
	}
*/
	private Integer updateAnswer(Integer answerHeaderId, HashMap<String, Object> question, String answer, String lookUpCode, Integer answerNumber, String updateUser) throws Exception {
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();	
			inParam.add(new Parameter("<<ANSWER>>", DBEngineConstants.TYPE_STRING, answer));
			inParam.add(new Parameter("<<ANSWER_LOOKUP_CODE>>", DBEngineConstants.TYPE_STRING, lookUpCode));
			inParam.add(new Parameter("<<EXPLANATION>>", DBEngineConstants.TYPE_STRING, question.get("EXPLANATION")));
			inParam.add(new Parameter(UPDATE_TIMESTAMP_PARAM, DBEngineConstants.TYPE_TIMESTAMP, commonDao.getCurrentTimestamp()));
			inParam.add(new Parameter(UPDATE_USER_PARAM, DBEngineConstants.TYPE_STRING, updateUser));
			inParam.add(new Parameter(QUESTION_ID_PARAM, DBEngineConstants.TYPE_INTEGER, question.get(QUESTION_ID)));
			inParam.add(new Parameter("<<ANSWER_NUMBER>>", DBEngineConstants.TYPE_INTEGER, answerNumber));
			inParam.add(new Parameter(QUESTIONNAIRE_ANSWER_HEADER_ID_PARAM, DBEngineConstants.TYPE_INTEGER, answerHeaderId));
			dbEngine.executeUpdate(inParam, "UPDATE_QUESTIONNAIRE_ANSWER");
		} catch (Exception e) {
			logger.error("Exception in updateQnrAnswer : {}", e.getMessage());
			throw e;
		}
		return 1;
	}

	public Integer deleteAnswer(Integer answerHeaderId, Map<String, Object> question) throws Exception {
		Integer isUpdated = 0;
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter(QUESTION_ID_PARAM, DBEngineConstants.TYPE_INTEGER, question.get(QUESTION_ID)));
			inParam.add(new Parameter(QUESTIONNAIRE_ANSWER_HEADER_ID_PARAM, DBEngineConstants.TYPE_INTEGER, answerHeaderId));
			isUpdated = dbEngine.executeUpdate(inParam, "DELETE_QUESTIONNAIRE_ANSWER");
		} catch (Exception e) {
			logger.error("Exception in deleteQuestionnaireAnswer : {}", e.getMessage());
			throw e;
		}
		return isUpdated;
	}

	private ArrayList<HashMap<String, Object>> getAnswer(Integer questionnaireAnswerHeaderId) throws Exception {
		try {
			ArrayList<Parameter> inputParam = new ArrayList<>();
			inputParam.add(new Parameter("<<AV_QNR_ANS_HEADER_ID>>", DBEngineConstants.TYPE_INTEGER,
					questionnaireAnswerHeaderId));
			return dbEngine.executeQuery(inputParam, "GET_QUESTIONNAIRE_ANSWER");
		} catch (Exception e) {
			logger.error("Exception in getAnswer : {}", e.getMessage());
			return new ArrayList<>();
		}
	}

	private ArrayList<HashMap<String, Object>> getQuestionnaireQuestions(Integer questionnaireId) throws Exception {
		try {
			ArrayList<Parameter> inputParam = new ArrayList<>();
			inputParam.add(new Parameter("<<AV_QUESTIONNAIRE_ID>>", DBEngineConstants.TYPE_INTEGER, questionnaireId));
			return dbEngine.executeQuery(inputParam, "GET_QUESTIONNAIRE_QUESTIONS");
		} catch (Exception e) {
			logger.error("Exception in getQuestionnaireQuestions : {}", e.getMessage());
			return new ArrayList<>();
		}
	}

	private ArrayList<HashMap<String, Object>> setAnswerToQuestionnaireQuestion(ArrayList<HashMap<String, Object>> questionnaireQuestions, ArrayList<HashMap<String, Object>> answerList) {
		Integer noOfAnswer = null;
		if (questionnaireQuestions != null && !questionnaireQuestions.isEmpty()) {
			for (HashMap<String, Object> question : questionnaireQuestions) {
				boolean isQuestionAnswered = false;
				Integer questionNumber = Integer.parseInt((question.get(QUESTION_NUMBER).toString()));
				HashMap<String, Object> questionAnswer = new HashMap<>();
				for (HashMap<String, Object> answer : answerList) {
					LookUp ans = new LookUp();
					if (Integer.parseInt((answer.get(QUESTION_NUMBER).toString())) == questionNumber) {
						isQuestionAnswered = true;
						if (TEXT.equals(question.get(ANSWER_TYPE))) {
							questionAnswer.put(answer.get(ANSWER_NUMBER).toString(),
									answer.get(ANSWER) == null ? "" : answer.get(ANSWER).toString());
						} else if (CHECKBOX.equals(question.get(ANSWER_TYPE))) {
							questionAnswer.put(answer.get(ANSWER).toString(), "true");
						} 
						else if (Constants.SYSTEM_LOOKUP.equals(question.get(ANSWER_TYPE)) || Constants.USER_LOOKUP.equals(question.get(ANSWER_TYPE))) {
							ans.setCode(question.get("ANSWER_LOOKUP_CODE") != null ? question.get("ANSWER_LOOKUP_CODE").toString() : null);
							ans.setDescription(answer.get(ANSWER).toString());
							questionAnswer.put(answer.get(ANSWER_NUMBER).toString(), ans);
						} 
						else if (RADIO.equals(question.get(ANSWER_TYPE))) {
							questionAnswer.put(answer.get(ANSWER_NUMBER).toString(),
									answer.get(ANSWER) == null ? "" : answer.get(ANSWER).toString());
						} else if (ATTACHMENT.equals(question.get(ANSWER_TYPE))) {
							questionAnswer.put(answer.get(ANSWER_NUMBER).toString(),
									answer.get(ANSWER) == null ? "" : answer.get(ANSWER).toString());
							if (answer.get(ATTACHMENT_ID) != null) {
								question.put(ATTACHMENT_ID, answer.get(ATTACHMENT_ID).toString());
							}
						}
						else if (Constants.TABLE.equals(question.get(ANSWER_TYPE))) {
							questionAnswer.put("1", answer.get(ANSWER));
						}
						else {
							if (answer.get(ANSWER) != null) {
								questionAnswer.put("1", answer.get(ANSWER).toString());
								question.put("ANSWER_LOOKUP_CODE", answer.get("ANSWER_LOOKUP_CODE"));
							}
						}
					}
				}
				if (isQuestionAnswered) {
					question.put(ANSWERS, questionAnswer);
					question.put(AC_TYPE, "U");
				} else {
					if (question.get(NO_OF_ANSWERS) != null) {
						noOfAnswer = Integer.parseInt((question.get(NO_OF_ANSWERS).toString()));
						question.put(ANSWERS, buildAnswerMap(noOfAnswer));
					}
				}
				if ("G0".equals(question.get(GROUP_NAME)) || isQuestionAnswered) {
					question.put(SHOW_QUESTION, true);
				}
			}
		}
		return questionnaireQuestions;
	}

	private HashMap<String, String> buildAnswerMap(Integer noOfAnswers) {
		HashMap<String, String> answerList = new HashMap<>();
		for (Integer index = 1; index <= noOfAnswers; index++) {
			answerList.put(index.toString(), "");
		}
		return answerList;
	}

	/* Intentionally commented
	private void insertQuestionnaireAnswerHeader(Integer moduleCode, Integer moduleSubItemCode, String moduleItemKey,
			String moduleSubItemKey, String updateUser, Integer questionnaireId)
			throws Exception {
		ArrayList<Parameter> inParam = new ArrayList<>();
		try {
			inParam.add(new Parameter("<<QUESTIONNAIRE_ID>>", DBEngineConstants.TYPE_INTEGER, questionnaireId));
			inParam.add(new Parameter("<<MODULE_ITEM_CODE>>", DBEngineConstants.TYPE_INTEGER, moduleCode));
			inParam.add(new Parameter("<<MODULE_SUB_ITEM_CODE>>", DBEngineConstants.TYPE_INTEGER, moduleSubItemCode));
			inParam.add(new Parameter("<<MODULE_ITEM_KEY>>", DBEngineConstants.TYPE_STRING, moduleItemKey));
			inParam.add(new Parameter("<<MODULE_SUB_ITEM_KEY>>", DBEngineConstants.TYPE_STRING, moduleSubItemKey));
			inParam.add(new Parameter("<<QUESTIONNAIRE_COMPLETED_FLAG>>", DBEngineConstants.TYPE_STRING, "N"));
			inParam.add(new Parameter(UPDATE_TIMESTAMP_PARAM, DBEngineConstants.TYPE_TIMESTAMP, commonDao.getCurrentTimestamp()));
			inParam.add(new Parameter(UPDATE_USER_PARAM, DBEngineConstants.TYPE_STRING, updateUser));
			dbEngine.executeUpdate(inParam, "INSERT_QUESTIONNAIRE_ANS_HEADER");
		} catch (Exception e) {
			logger.error("Exception in insertQuestionnaireAnswerHeader : {}", e.getMessage());
			throw e;
		}
	}*/

	private void updateQuestionnaireCompleteFlag(Integer questionnaireAnsHeaderId, String completionFlag)
			throws Exception {
		try {
			ArrayList<Parameter> inputParam = new ArrayList<>();
			inputParam.add(new Parameter("<<QUESTIONNAIRE_COMPLETED_FLAG>>", DBEngineConstants.TYPE_STRING, completionFlag));
			inputParam.add(new Parameter(UPDATE_TIMESTAMP_PARAM, DBEngineConstants.TYPE_TIMESTAMP, commonDao.getCurrentTimestamp()));
			inputParam.add(new Parameter(UPDATE_USER_PARAM, DBEngineConstants.TYPE_STRING, AuthenticatedUser.getLoginUserName()));
			inputParam.add(new Parameter(QUESTIONNAIRE_ANSWER_HEADER_ID_PARAM, DBEngineConstants.TYPE_INTEGER, questionnaireAnsHeaderId));
			dbEngine.executeUpdate(inputParam, "UPDATE_QUESTIONNAIRE_COMPLETE_FLAG");
		} catch (Exception e) {
			logger.error("Exception in updateQuestionnaireCompleteFlag : {}", e.getMessage());
			throw e;
		}
	}
/* Intentionally commented
	private Integer generateQuesAnsHeaderId() {
		Integer questionnaireAnsHeaderId = null;
		try {
			questionnaireAnsHeaderId = questionnaireDAO.getNextQuestionnaireAnswerHeaderId();
		} catch (Exception e) {
			logger.error("Exception in generateQuesAnsHeaderId : {}", e.getMessage());
			throw e;
		}
		return questionnaireAnsHeaderId;
	}*/

	public Integer deleteQuestionnaireAnswer(QuestionnaireAnswerDto questionnaireAnswerDto) throws Exception {
		Integer isUpdated = 0;
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter(QUESTIONNAIRE_ANSWER_ID_PARAM, DBEngineConstants.TYPE_INTEGER,
					questionnaireAnswerDto.getQuestionnaireAnswerId()));
			isUpdated = dbEngine.executeUpdate(inParam, "DELETE_QUESTIONNAIRE_ANSWER");
		} catch (Exception e) {
			logger.error("Exception in deleteQuestionnaireAnswer : {}", e.getMessage());
			throw e;
		}
		return isUpdated;
	}

	@Override
	public boolean isQuestionnaireComplete(QuestionnaireDataBus questionnaireDataBus) {
		try {
		ArrayList<Parameter> inputParam = new ArrayList<>();
		ArrayList<HashMap<String, Object>> result = dbEngine.executeQuerySQL(inputParam, "SELECT QUESTIONNAIRE_COMPLETED_FLAG FROM QUEST_ANSWER_HEADER WHERE QUESTIONNAIRE_ANS_HEADER_ID = "+questionnaireDataBus.getQuestionnaireAnswerHeaderId());
		return  result.get(0).get(QUESTIONNAIRE_COMPLETED_FLAG) != null && result.get(0).get(QUESTIONNAIRE_COMPLETED_FLAG).equals("Y");
		} catch (Exception e) {
			logger.error("Exception in isQuestionnaireComplete : {}", e.getMessage());
			return false;
		}
	}

	@Override
	public QuestionnaireDataBus configureQuestionnaire(QuestionnaireDataBus questionnaireDataBus) {
		try {
		boolean isNewQuestionnaireVersion = questionnaireDataBus.isNewQuestionnaireVersion();
		logger.info("isNewQuestionnaireVersion : {}", isNewQuestionnaireVersion);
		saveQuestionnaireHeader(questionnaireDataBus);
	    saveUsage(questionnaireDataBus);
		questionnaireDataBus = saveQuestion(questionnaireDataBus);
		return questionnaireDataBus;
		} catch (Exception e) {
			logger.error("Exception in configureQuestionnaire : {}", e.getMessage());
			return null;
		}
	}

	private QuestionnaireDataBus saveQuestionnaireHeader(QuestionnaireDataBus questionnaireDataBus) {
		try {
			boolean isNewQuestionnaireVersion = questionnaireDataBus.isNewQuestionnaireVersion();
			if ("I".equals(questionnaireDataBus.getAcType()) || isNewQuestionnaireVersion) {
				setQuestionnaireHeaderInfo(questionnaireDataBus);
			}
			saveHeaderInfo(questionnaireDataBus);
			return questionnaireDataBus;
		} catch (Exception e) {
			logger.error("Exception in saveQuestionnaireHeader : {}", e.getMessage());
			return null;
		}
	}

	private QuestionnaireDataBus setQuestionnaireHeaderInfo(QuestionnaireDataBus questionnaireDataBus) {
		try {
		Integer questionnaireId = questionnaireDAO.getNextQuestionnaireId();
		Integer questionnaireVersionNumber = 0;
		Integer questionnaireNumber = (Integer) questionnaireDataBus.getHeader().get(QUESTIONNAIRE_NUMBER);
		logger.info("questionnaireId : {}", questionnaireId);
		logger.info("questionnaireNumber : {}", questionnaireNumber);
		if (questionnaireDataBus.isNewQuestionnaireVersion()) {
			Integer previousQuestionnaireId = questionnaireDataBus.getQuestionnaireId();
			logger.info("previousQuestionnaireId : {}", previousQuestionnaireId);
			questionnaireVersionNumber = questionnaireDAO.getMaxQuestionnaireVersionNumber(questionnaireNumber);
			logger.info("Previous Questionnaire VersionNumber : {}", questionnaireVersionNumber);
			++questionnaireVersionNumber;
			logger.info("questionnaireVersionNumber : {}", questionnaireVersionNumber);
		} else {
			questionnaireNumber = questionnaireDAO.getNextQuestionnaireNumber();
			questionnaireVersionNumber = 1;
		}
		Map<String, Object> hmHeader = questionnaireDataBus.getHeader();
		hmHeader.put(QUESTIONNAIRE_ID, questionnaireId);
		hmHeader.put(QUESTIONNAIRE_NUMBER, questionnaireNumber);
		hmHeader.put("QUESTIONNAIRE_VERSION", questionnaireVersionNumber);
		questionnaireDataBus.setQuestionnaireId(questionnaireId);
		return questionnaireDataBus;
		} catch (Exception e) {
			logger.error("Exception in setQuestionnaireHeaderInfo : {}", e.getMessage());
			return null;
		}
	}

	private void saveHeaderInfo(QuestionnaireDataBus questionnaireDataBus) {
		if ("I".equals(questionnaireDataBus.getAcType()) || questionnaireDataBus.isNewQuestionnaireVersion()) {
			questionnaireDAO.insertHeader(questionnaireDataBus);
			questionnaireDataBus.setAcType("U");
		} else {
			questionnaireDAO.updateHeader(questionnaireDataBus);
		}
	}

	private QuestionnaireDataBus saveUsage(QuestionnaireDataBus questionnaireDataBus) {
		List<HashMap<String, Object>> usage = questionnaireDataBus.getUsage();
		List<HashMap<String, Object>> updatedlist = new ArrayList<>(usage);
		Collections.copy(updatedlist, usage);
		String updateUser = AuthenticatedUser.getLoginUserName();
		boolean isNewQuestionnaireVersion = questionnaireDataBus.isNewQuestionnaireVersion();
		for (HashMap<String, Object> hmUsage : usage) {
			hmUsage.put("UPDATE_USER", updateUser);
			if ("I".equals(hmUsage.get(AC_TYPE)) || isNewQuestionnaireVersion) {
				Integer questionnaireOptionId = questionnaireDAO.getNextQuestionnaireUsageId();
				hmUsage.put("QUESTIONNAIRE_USAGE_ID", questionnaireOptionId);
				hmUsage.put(QUESTIONNAIRE_ID, questionnaireDataBus.getQuestionnaireId());
				Integer mouduleItemCode = Integer.parseInt(hmUsage.get("MODULE_ITEM_CODE").toString());
				Integer mouduleSubItemCode = Integer.parseInt(hmUsage.get("MODULE_SUB_ITEM_CODE").toString());
				hmUsage.computeIfAbsent(SORT_ORDER, sortOrder -> questionnaireDAO.getNextSortOrder(mouduleItemCode, mouduleSubItemCode));
				questionnaireDAO.insertUsage(hmUsage);
				hmUsage.put(AC_TYPE, "U");
			} else if ("D".equals(hmUsage.get(AC_TYPE))) {
				questionnaireDAO.deleteUsage(hmUsage);
				updatedlist.remove(hmUsage);
			} else {
				questionnaireDAO.updateUsage(hmUsage);
			}
		}
		questionnaireDataBus.getUsage().clear();
		questionnaireDataBus.getUsage().addAll(updatedlist);
		return questionnaireDataBus;
	}

	private QuestionnaireDataBus saveQuestion(QuestionnaireDataBus questionnaireDataBus) {
		HashMap<Integer, Integer> hmQuestionMapping = new HashMap<>();
		List<HashMap<String, Object>> questions = questionnaireDataBus.getQuestionnaire().getQuestions();
		Integer sortOrder = 0;
		Integer maxGroupNumber = 0;
		String updateUser = AuthenticatedUser.getLoginUserName();
		boolean isNewQuestionnaireVersion = questionnaireDataBus.isNewQuestionnaireVersion();
		for (HashMap<String, Object> hmQuestion : questions) {
			hmQuestionMapping.put((Integer) hmQuestion.get(QUESTION_ID), (Integer) hmQuestion.get(QUESTION_ID));
			hmQuestion.put(UPDATE_USER, updateUser);
			hmQuestion.put(SORT_ORDER, ++sortOrder);
			if ("I".equals(hmQuestion.get(AC_TYPE))) {
				Integer questionNumber = questionnaireDAO.getNextQuestionNumber();
				hmQuestion.put("QUESTION_NUMBER", questionNumber);
			}
			if ("I".equals(hmQuestion.get(AC_TYPE)) || isNewQuestionnaireVersion) {
				Integer questionId = questionnaireDAO.getNextQuestionId();
				Integer questionVersionNumber = 1;
				if (isNewQuestionnaireVersion) {
					questionVersionNumber = (Integer) hmQuestion.get(QUESTION_VERSION_NUMBER);
					logger.info("Previous Question VersionNumber : {}", questionVersionNumber);
					++questionVersionNumber;
					logger.info("Latest Question VersionNumber : {}", questionVersionNumber);
				}
				hmQuestionMapping.put((Integer) hmQuestion.get(QUESTION_ID), questionId);
				hmQuestion = setParentQuestionId(hmQuestion, hmQuestionMapping);
				hmQuestion.put(QUESTION_ID, questionId);
				hmQuestion.put(QUESTION_VERSION_NUMBER, questionVersionNumber);
				hmQuestion.put(QUESTIONNAIRE_ID, questionnaireDataBus.getHeader().get(QUESTIONNAIRE_ID));
				questionnaireDAO.insertQuestion(hmQuestion);
				hmQuestion.put(AC_TYPE, "U");
			} else if ("U".equals(hmQuestion.get(AC_TYPE))) {
				questionnaireDAO.updateQuestion(hmQuestion);
			}
			if (hmQuestion.get(GROUP_NAME) != null && Integer.parseInt(hmQuestion.get(GROUP_NAME).toString().substring(1)) > maxGroupNumber) {
				maxGroupNumber = Integer.parseInt(hmQuestion.get(GROUP_NAME).toString().substring(1));
			}
		}
		if (questionnaireDataBus.getQuestionnaire().getDeleteList() != null && !questionnaireDataBus.isNewQuestionnaireVersion()) {
			Set<Integer> deleteList = questionnaireDataBus.getQuestionnaire().getDeleteList().getQuestion();
			if (deleteList != null && !deleteList.isEmpty()) {
				for (Integer id : deleteList) {
					questionnaireDAO.deleteConditionForQuestion(id);
					questionnaireDAO.deleteOptionForQuestion(id);
					questionnaireDAO.deleteQuestion(id);
				}
			}
		}
		questionnaireDataBus.getQuestionnaire().setMaxGroupNumber(maxGroupNumber);
		questionnaireDataBus = saveConditions(questionnaireDataBus, hmQuestionMapping);
		questionnaireDataBus = saveOptions(questionnaireDataBus, hmQuestionMapping);
		return questionnaireDataBus;
	}

	private HashMap<String, Object> setParentQuestionId(HashMap<String, Object> hmQuestion, HashMap<Integer, Integer> hmQuestionMapping) {
		if (hmQuestion.get(PARENT_QUESTION_ID) != null) {
			hmQuestion.put(PARENT_QUESTION_ID, hmQuestionMapping.get((Integer) hmQuestion.get(PARENT_QUESTION_ID)));
		}
		return hmQuestion;
	}

	private Integer getQuestionId(HashMap<String, Object> hmQuestion, HashMap<Integer, Integer> hmQuestionMapping) {
		return hmQuestionMapping.get((Integer) hmQuestion.get(QUESTION_ID));
	}

	private QuestionnaireDataBus saveOptions(QuestionnaireDataBus questionnaireDataBus,
			HashMap<Integer, Integer> hmQuestionMapping) {
		List<HashMap<String, Object>> options = questionnaireDataBus.getQuestionnaire().getOptions();
		boolean isNewQuestionnaireVersion = questionnaireDataBus.isNewQuestionnaireVersion();
		String updateUser = AuthenticatedUser.getLoginUserName();
		for (HashMap<String, Object> hmOption : options) {
			hmOption.put("UPDATE_USER", updateUser);
			if ("I".equals((String) hmOption.get(AC_TYPE)) || isNewQuestionnaireVersion) {
				Integer questionnaireOptionId = questionnaireDAO.getNextQuestionOptionId();
				hmOption.put("QUESTION_OPTION_ID", questionnaireOptionId);				
				if (isNewQuestionnaireVersion) {
					hmOption.put(QUESTION_ID, hmQuestionMapping.get((Integer) hmOption.get(QUESTION_ID)));
				} else {
					hmOption.put(QUESTION_ID, getQuestionId(hmOption, hmQuestionMapping));
				}
				questionnaireDAO.insertOption(hmOption);
				hmOption.put(AC_TYPE, "U");
			} else {
				questionnaireDAO.updateOption(hmOption);
			}
		}
		if (questionnaireDataBus.getQuestionnaire().getDeleteList() != null && !questionnaireDataBus.isNewQuestionnaireVersion()) {
			Set<Integer> deleteList = questionnaireDataBus.getQuestionnaire().getDeleteList().getOption();
			if (deleteList != null && !deleteList.isEmpty()) {
				for (Integer id : deleteList) {
					questionnaireDAO.deleteOption(id);
				}
			}
		}
		return questionnaireDataBus;
	}

	private QuestionnaireDataBus saveConditions(QuestionnaireDataBus questionnaireDataBus,
			HashMap<Integer, Integer> hmQuestionMapping) {
		boolean isNewQuestionnaireVersion = questionnaireDataBus.isNewQuestionnaireVersion();
		String updateUser = AuthenticatedUser.getLoginUserName();
		List<HashMap<String, Object>> conditions = questionnaireDataBus.getQuestionnaire().getConditions();
		for (HashMap<String, Object> hmCondition : conditions) {
			hmCondition.put("UPDATE_USER", updateUser);
			if ("I".equals((String) hmCondition.get(AC_TYPE)) || isNewQuestionnaireVersion) {
				Integer questionnaireConditionId = questionnaireDAO.getNextQuestionConditionId();
				hmCondition.put("QUESTION_CONDITION_ID", questionnaireConditionId);
				hmCondition.put(QUESTION_ID, getQuestionId(hmCondition, hmQuestionMapping));
				questionnaireDAO.insertCondition(hmCondition);
				hmCondition.put(AC_TYPE, "U");
			} else {
				questionnaireDAO.updateCondition(hmCondition);
			}
		}
		if (questionnaireDataBus.getQuestionnaire().getDeleteList() != null && !questionnaireDataBus.isNewQuestionnaireVersion()) {
			Set<Integer> deleteList = questionnaireDataBus.getQuestionnaire().getDeleteList().getCondition();
			if (deleteList != null && !deleteList.isEmpty()) {
				for (Integer id : deleteList) {
					questionnaireDAO.deleteCondition(id);
				}
			}
		}
		return questionnaireDataBus;
	}

	@Override
	public QuestionnaireDataBus showAllQuestionnaire(QuestionnaireDataBus questionnaireDataBus) {
		try {
			List<HashMap<String, Object>> questionnaireList = questionnaireDAO.getAllQuestionnaire();
			List<HashMap<String, Object>> questionnaireGroup = questionnaireDAO.getQuestionnaireGroup();
			questionnaireDataBus.setQuestionnaireList(questionnaireList);
			questionnaireDataBus.setQuestionnaireGroup(questionnaireGroup);
			return questionnaireDataBus;
		} catch (Exception e) {
			logger.error("Exception in showAllQuestionnaire : {}", e.getMessage());
			return null;
		}
	}

	@Override
	public QuestionnaireDataBus createQuestionnaire(QuestionnaireDataBus questionnaireDataBus) {
		try {
			Integer questionnaireId = -1;
			HashMap<String, Object> header = new HashMap<>();
			List<HashMap<String, Object>> questions = questionnaireDAO.getQuestionnaireQuestion(questionnaireId);
			List<HashMap<String, Object>> condition = questionnaireDAO.getQuestionnaireCondition(questionnaireId);
			List<HashMap<String, Object>> option = questionnaireDAO.getQuestionnaireOptions(questionnaireId);
			List<HashMap<String, Object>> questionnaireGroup = questionnaireDAO.getQuestionnaireGroup();
			QuestionnaireDto questionnaire = new QuestionnaireDto();
			header.put(QUESTIONNAIRE_ID, null);
			header.put(QUESTIONNAIRE_NUMBER, null);
			header.put("QUESTIONNAIRE_VERSION", null);
			header.put("QUESTIONNAIRE_NAME", null);
			header.put("QUESTIONNAIRE_DESCRIPTION", null);
			header.put("QUEST_GROUP_TYPE_CODE", null);
			header.put("IS_FINAL", null);
			header.put("UPDATE_USER", null);
			header.put("UPDATE_TIMESTAMP", null);
			questionnaireDataBus.setHeader(header);
			ArrayList<HashMap<String, Object>> usage = new ArrayList<>();
			questionnaireDataBus.setUsage(usage);
			questionnaire.setQuestions(questions);
			questionnaire.setConditions(condition);
			questionnaire.setOptions(option);
			setAdditionalAttributes(questionnaire);
			questionnaireDataBus.setLookUpDetails(commonService.getAllLookUpWindowDetails());
			questionnaireDataBus.setQuestionnaireGroup(questionnaireGroup);
			questionnaireDataBus.setQuestionnaire(questionnaire);
			DeleteDto deleteDto = new DeleteDto();
			questionnaire.setDeleteList(deleteDto);
			questionnaireDataBus.setAcType("I");
			questionnaireDataBus.setQuestionnaireBusinessRules(questionnaireDAO.getBusinessRulesForQuestionnaire());
			return questionnaireDataBus;
		} catch (Exception e) {
			logger.error("Exception in createQuestionnaire : {}", e.getMessage());
			return null;
		}
	}

	@Override
	public QuestionnaireDataBus editQuestionnaire(QuestionnaireDataBus questionnaireDataBus) {
		try {
			Integer questionnaireId = questionnaireDataBus.getQuestionnaireId();
			List<HashMap<String, Object>> header = questionnaireDAO.getQuestionnaireHeader(questionnaireId, null);
			List<HashMap<String, Object>> usage = questionnaireDAO.getQuestionnaireUsage(questionnaireId);
			List<HashMap<String, Object>> questions = questionnaireDAO.getQuestionnaireQuestion(questionnaireId);
			List<HashMap<String, Object>> condition = questionnaireDAO.getQuestionnaireCondition(questionnaireId);
			List<HashMap<String, Object>> option = questionnaireDAO.getQuestionnaireOptions(questionnaireId);
			List<HashMap<String, Object>> questionnaireGroup = questionnaireDAO.getQuestionnaireGroup();
			QuestionnaireDto questionnaire = new QuestionnaireDto();
			questionnaireDataBus.setHeader(header.get(0));
			questionnaireDataBus.setUsage(usage);
			questionnaire.setQuestions(questions);
			questionnaire.setConditions(condition);
			setAdditionalAttributes(questionnaire);
			questionnaire.setOptions(option);
			questionnaireDataBus.setLookUpDetails(commonService.getAllLookUpWindowDetails());
			questionnaireDataBus.setQuestionnaireGroup(questionnaireGroup);
			questionnaireDataBus.setQuestionnaire(questionnaire);
			questionnaireDataBus.setQuestionnaireBusinessRules(questionnaireDAO.getBusinessRulesForQuestionnaire());
			DeleteDto deleteDto = new DeleteDto();
			questionnaire.setDeleteList(deleteDto);
			return questionnaireDataBus;
		} catch (Exception e) {
			logger.error("Exception in editQuestionnaire : {}", e.getMessage());
			return null;
		}
	}

	private QuestionnaireDto setAdditionalAttributes(QuestionnaireDto questionnaire) {
		List<HashMap<String, Object>> output = questionnaire.getQuestions();
		Integer maxGroupNumber = 0;
		if (output != null && !output.isEmpty()) {
			for (HashMap<String, Object> hmResult : output) {
				hmResult.put("HIDE_QUESTION", false);
				hmResult.put(ANSWERS, new HashMap<>());
				if ("G0".equals(hmResult.get(GROUP_NAME))) {
					hmResult.put(SHOW_QUESTION, TRUE);
				} else {
					hmResult.put(SHOW_QUESTION, FALSE);
				}
				hmResult.put(AC_TYPE, Constants.acTypeUpdate);
			}
		}
		List<HashMap<String, Object>> conditions = questionnaire.getConditions();
		if (conditions != null && !conditions.isEmpty()) {
			for (HashMap<String, Object> hmResult : conditions) {
				if (hmResult.get(GROUP_NAME) != null && Integer.parseInt(hmResult.get(GROUP_NAME).toString().substring(1)) > maxGroupNumber) {
					maxGroupNumber = Integer.parseInt(hmResult.get(GROUP_NAME).toString().substring(1));
				}
			}
		}
		questionnaire.setMaxGroupNumber(maxGroupNumber);
		return questionnaire;
	}

	@Override
	public ResponseEntity<byte[]> downloadAttachments(QuestionnaireDataBus questionnaireDataBus,
			HttpServletResponse response) {
		ResponseEntity<byte[]> attachmentData = null;
		try {
			List<HashMap<String, Object>> questionnaireAttachment = questionnaireDAO.getQuestionnaireAttachment(questionnaireDataBus.getQuestionnaireAnsAttachmentId());
			if (questionnaireAttachment.get(0).get("ATTACHMENT") != null) {
				ByteArrayOutputStream byteArrayOutputStream = null;
				byteArrayOutputStream = (ByteArrayOutputStream) questionnaireAttachment.get(0).get("ATTACHMENT");
				byte[] data = byteArrayOutputStream.toByteArray();
				HttpHeaders headers = new HttpHeaders();
				headers.setContentType(MediaType.parseMediaType(questionnaireAttachment.get(0).get("CONTENT_TYPE").toString()));
				String filename = questionnaireAttachment.get(0).get("FILE_NAME").toString();
				headers.setContentDispositionFormData(filename, filename);
				headers.setContentLength(data.length);
				headers.setCacheControl("must-revalidate, post-check=0, pre-check=0");
				headers.setPragma("public");
				attachmentData = new ResponseEntity<>(data, headers, HttpStatus.OK);
			}
		} catch (Exception e) {
			logger.error("Exception in downloadAttachments : {}", e.getMessage());
		}
		return attachmentData;
	}

	@Override
	public List<HashMap<String, Object>> getBusinessRuleForQuestionnaire(QuestionnaireDataBus questionnaireDataBus) {
		try {
			return questionnaireDAO.getBusinessRuleForQuestionnaire(questionnaireDataBus.getModuleItemCode(), questionnaireDataBus.getModuleSubItemCode());
		} catch (Exception e) {
			logger.error("Exception in getBusinessRuleForQuestionnaire : {}", e.getMessage());
			return new ArrayList<>();
		}
	}

	@Override
	public ByteArrayInputStream printQuestionnaire(QuestionnaireDataBus questionnaireDataBus) {
		try {
			questionnaireDataBus = getApplicableQuestionnaire(questionnaireDataBus);
			Proposal proposal = proposalDao
					.fetchProposalById(Integer.parseInt(questionnaireDataBus.getModuleItemKey()));
			List<QuestionnaireDataBus> questionnaireList = getQuestionnaireList(questionnaireDataBus);
			return questionnairePrintService.questionnairePdfReport(questionnaireList, proposal);
		} catch (Exception e) {
			logger.error("Exception in printQuestionnaire : {}", e.getMessage());
			return null;
		}
	}

	@Override
	public List<QuestionnaireDataBus> getQuestionnaireList(QuestionnaireDataBus questionnaireDataBus) {
		List<QuestionnaireDataBus> questionnaireList = new ArrayList<>();
		try {
			if (questionnaireDataBus.getApplicableQuestionnaire() != null && !questionnaireDataBus.getApplicableQuestionnaire().isEmpty()) {
				for (Map<String, Object> applicableQuestionnaire : questionnaireDataBus.getApplicableQuestionnaire()) {
					Integer questionnaireId = (Integer) applicableQuestionnaire.get(QUESTIONNAIRE_ID);
					QuestionnaireDataBus modifiedQuestionnaireDataBus = new QuestionnaireDataBus();
					if (applicableQuestionnaire.get(QUESTIONNAIRE_ANSWER_HEADER_ID) != null) {
						Integer questionnaireAnswerHeaderId = (Integer) applicableQuestionnaire.get(QUESTIONNAIRE_ANSWER_HEADER_ID);
						modifiedQuestionnaireDataBus.setQuestionnaireAnswerHeaderId(questionnaireAnswerHeaderId);
					}
					modifiedQuestionnaireDataBus.setQuestionnaireId(questionnaireId);
					modifiedQuestionnaireDataBus = getQuestionnaireDetails(modifiedQuestionnaireDataBus);
					if (applicableQuestionnaire.get(QUESTIONNAIRE_COMPLETED_FLAG) != null) {
						modifiedQuestionnaireDataBus.setQuestionnaireCompleteFlag(applicableQuestionnaire.get(QUESTIONNAIRE_COMPLETED_FLAG).toString());
					} else {
						modifiedQuestionnaireDataBus.setQuestionnaireCompleteFlag("N");
					}
					modifiedQuestionnaireDataBus.setQuestionnaireName(applicableQuestionnaire.get("QUESTIONNAIRE").toString());
					questionnaireList.add(modifiedQuestionnaireDataBus);
				}
			}
			return questionnaireList;
		} catch (Exception e) {
			logger.error("Exception in getQuestionnaireList : {}", e.getMessage());
			return questionnaireList;
		}
	}

	@Override
	public String activeDeactivateQuestionnaire(QuestionnaireDataBus questionnaireDataBus, boolean isActivate){
		try {
			questionnaireDAO.activeDeactivateQuestionnaire(questionnaireDataBus, (Boolean.TRUE.equals(isActivate) ? "Y" : "N"));
			return "Success";
		} catch (Exception e) {
			logger.error("Exception in activeDeactivateQuestionnaire : {}", e.getMessage());
			return "Failed";
		}
	}

	@Override
	public QuestionnaireDataBus getQuestionnaireBasedOnQuestionnaireId(QuestionnaireDataBus questionnaireDataBus) {
		QuestionnaireDataBus modifiedQuestionnaireDataBus = new QuestionnaireDataBus();
		modifiedQuestionnaireDataBus.setQuestionnaireAnswerHeaderId(questionnaireDataBus.getQuestionnaireAnswerHeaderId());
		if (questionnaireDataBus.getApplicableQuestionnaire() != null && !questionnaireDataBus.getApplicableQuestionnaire().isEmpty()) {
			for (Map<String, Object> applicableQuestionnaire : questionnaireDataBus.getApplicableQuestionnaire()) {
				Integer questionnaireId = (Integer) applicableQuestionnaire.get(QUESTIONNAIRE_ID);
				if (questionnaireId.equals(questionnaireDataBus.getQuestionnaireId())) {
					if (applicableQuestionnaire.get(QUESTIONNAIRE_ANSWER_HEADER_ID) != null) {
						Integer questionnaireAnswerHeaderId = (Integer) applicableQuestionnaire.get(QUESTIONNAIRE_ANSWER_HEADER_ID);
						modifiedQuestionnaireDataBus.setQuestionnaireAnswerHeaderId(questionnaireAnswerHeaderId);
					}
					modifiedQuestionnaireDataBus.setQuestionnaireId(questionnaireId);
					modifiedQuestionnaireDataBus = getQuestionnaireDetails(modifiedQuestionnaireDataBus);
					if (applicableQuestionnaire.get(QUESTIONNAIRE_COMPLETED_FLAG) != null) {
						modifiedQuestionnaireDataBus.setQuestionnaireCompleteFlag(applicableQuestionnaire.get(QUESTIONNAIRE_COMPLETED_FLAG).toString());
					} else {
						modifiedQuestionnaireDataBus.setQuestionnaireCompleteFlag("N");
					}
					modifiedQuestionnaireDataBus.setQuestionnaireName(applicableQuestionnaire.get("QUESTIONNAIRE").toString());
				}
			}
		}
		return modifiedQuestionnaireDataBus;
	}

	@Override
	public MultipartFile generateAttachment(QuestionnaireDataBus questionnaireDataBus, MultipartHttpServletRequest request) {
		MultipartFile multipartFile = null;
		String name = questionnaireDataBus.getFileName();
		String splicedFile = questionnaireDataBus.getFileContent();
		Integer remaining = questionnaireDataBus.getRemaining();
		Integer length = questionnaireDataBus.getLength();
		String userId = questionnaireDataBus.getPersonId();
		String timestamp = questionnaireDataBus.getFileTimestamp();
		if (splicedFile != null) {
			multipartFile = commonService.uploadMedia(splicedFile, name, remaining, length, timestamp, userId, questionnaireDataBus.getContentType());
		}
		return multipartFile;
	}

	@Override
	public QuestionnaireDataBus copyQuestionnaire(QuestionnaireDataBus questionnaireDataBus) {
		questionnaireDataBus = getQuestionnaireDetails(questionnaireDataBus);
		questionnaireDataBus.setAcType(Constants.acTypeInsert);
		questionnaireDataBus.getHeader().replace("QUESTIONNAIRE_NAME", "Copy of "+questionnaireDataBus.getHeader().get("QUESTIONNAIRE_NAME"));
		questionnaireDataBus.getHeader().replace("IS_FINAL", false);
		setAsNewConditions(questionnaireDataBus.getQuestionnaire().getConditions());
		setAsNewQuestions(questionnaireDataBus.getQuestionnaire().getQuestions());
		setAsNewOptions(questionnaireDataBus.getQuestionnaire().getOptions());
		questionnaireDataBus.setUsage(new ArrayList<>());
		configureQuestionnaire(questionnaireDataBus);
		return questionnaireDataBus;
	}

	private void setAsNewConditions(List<HashMap<String, Object>> conditions) {
		if (conditions != null && !conditions.isEmpty()) {
			for (HashMap<String, Object> condition : conditions) {
				condition.put(AC_TYPE, "I");
			}
		}
	}

	private void setAsNewQuestions(List<HashMap<String, Object>> questions) {
		if (questions != null && !questions.isEmpty()) {
			for (HashMap<String, Object> question : questions) {
				question.put(AC_TYPE, "I");
			}
		}
	}

	private void setAsNewOptions(List<HashMap<String, Object>> options) {
		if (options != null && !options.isEmpty()) {
			for (HashMap<String, Object> option : options) {
				option.put(AC_TYPE, "I");
			}
		}
	}

	@Override
	public QuestionnaireDataBus getModuleList(QuestionnaireDataBus questionnaireDataBus) {
		questionnaireDataBus.setModuleList(questionnaireDAO.getModuleList());
		return questionnaireDataBus;
	}

	@Override
	public QuestionnaireDataBus updateQuestionnaireUsage(QuestionnaireDataBus questionnaireDataBus) {
		List<HashMap<String, Object>> questionnaireList = questionnaireDataBus.getQuestionnaireList();
		for (HashMap<String, Object> questionnaireObject : questionnaireList) {
			questionnaireDAO.updateUsageForSortOrder(questionnaireObject, questionnaireDataBus.getModuleItemCode(), questionnaireDataBus.getModuleSubItemCode());
		}
		questionnaireDataBus.setQuestionnaireList(questionnaireList);
		return questionnaireDataBus;
	}

	@Override
	public QuestionnaireDataBus getQuestionnaireListByModule(QuestionnaireDataBus questionnaireDataBus) {
		questionnaireDataBus.setQuestionnaireList(questionnaireDAO.getQuestionnaireListByModule(questionnaireDataBus.getModuleItemCode(), questionnaireDataBus.getModuleSubItemCode()));
		return questionnaireDataBus;
	}

	@Override
	public QuestionnaireDataBus copyQuestionnaireAnswers(QuestionnaireDataBus questionnaireDataBus) {
		try {
			questionnaireDataBus = getQuestionnaireDetails(questionnaireDataBus);
			questionnaireDataBus.setAcType(Constants.acTypeInsert);
			setAsNewAnswers(questionnaireDataBus.getQuestionnaire().getQuestionnaireAnswers());
			MultipartHttpServletRequest multipartFile = null;
			saveQuestionnaireAnswers(questionnaireDataBus, multipartFile);
			return questionnaireDataBus;
		} catch (Exception e) {
			logger.error("Exception in copyQuestionnaireAnswers : {}", e.getMessage());
			return null;
		}
	}

	private void setAsNewAnswers(List<HashMap<String, Object>> answers) {
		if (answers != null && !answers.isEmpty()) {
			for (HashMap<String, Object> answer : answers) {
				answer.put(AC_TYPE, "I");
			}
		}
	}

	private void setDocumentUpdateUserAndTimestamp(Integer moduleCode, Integer moduleItemKey, String updateUser) {
		if (moduleCode.equals(Constants.MODULE_CODE_AWARD)) {
			awardService.updateAwardDocumentUpdateUserAndTimestamp(moduleItemKey, updateUser);
		}
	}

	
	@Override
	public void copyQuestionnaireDatas(Integer moduleItemKey, Integer copiedModuleItemKey, Integer moduleCode, Integer subModuleCode) {
		QuestionnaireDataBus questionnaireDataBus = new QuestionnaireDataBus();
		questionnaireDataBus.setActionPersonId(AuthenticatedUser.getLoginPersonId());
		questionnaireDataBus.setActionUserId(AuthenticatedUser.getLoginUserName());
		questionnaireDataBus.setModuleItemCode(moduleCode);
		questionnaireDataBus.setModuleItemKey(moduleItemKey.toString());
		questionnaireDataBus.setModuleSubItemCode(subModuleCode);
		List<Integer> submoduleCodes = new ArrayList<>();
		submoduleCodes.add(Constants.DEV_PROPOSAL_SUBMODULE_CODE);
		//submoduleCodes.add(Constants.DEV_PROPOSAL_PERSON_CERTIFICATION_SUBMODULE_CODE);
		questionnaireDataBus.getModuleSubItemCodes().addAll(submoduleCodes);
		questionnaireDataBus.setCopyModuleItemKey(copiedModuleItemKey.toString());
		copyQuestionnaireForVersion(questionnaireDataBus);
	}

	@Override
	public void copyQuestionnaireForVersion(QuestionnaireDataBus questionnaireDataBus) {
		Integer moduleCode = questionnaireDataBus.getModuleItemCode();
		List<Integer> moduleSubItemCodes = questionnaireDataBus.getModuleSubItemCodes();
		String moduleItemKey = questionnaireDataBus.getModuleItemKey();
		String moduleSubItemKey = questionnaireDataBus.getModuleSubItemKey();
		String copyModuleItemKey = questionnaireDataBus.getCopyModuleItemKey();
		List<QuestAnswerHeader> answerHeaders = questionnaireDAO.getQuestionanswerHeadersToCopy(moduleCode, moduleSubItemCodes, moduleItemKey, moduleSubItemKey);
		for (QuestAnswerHeader answerHeader : answerHeaders) {
			List<QuestAnswer> copyQuestAnswers = new ArrayList<>();
			QuestAnswerHeader copyAnswerHeader = new QuestAnswerHeader();
			copyAnswerHeader.setModuleItemCode(answerHeader.getModuleItemCode());
			copyAnswerHeader.setModuleSubItemCode(answerHeader.getModuleSubItemCode());
			copyAnswerHeader.setModuleItemKey(copyModuleItemKey);
			copyAnswerHeader.setModuleSubItemKey(answerHeader.getModuleSubItemKey());
			copyAnswerHeader.setQuestCompletedFlag(answerHeader.getQuestCompletedFlag());
			copyAnswerHeader.setQuestionnaireId(answerHeader.getQuestionnaireId());
			copyAnswerHeader.setUpdateUser(answerHeader.getUpdateUser());
			copyAnswerHeader.setUpdateTimeStamp(answerHeader.getUpdateTimeStamp());
			List<QuestAnswer> questAnswers = answerHeader.getQusetAnswers();
			if(questAnswers != null && !questAnswers.isEmpty()) {
				for (QuestAnswer questAnswer : questAnswers) {
					QuestAnswer copyQuestAnswer = new QuestAnswer();
					copyQuestAnswer.setAnswer(questAnswer.getAnswer());
					copyQuestAnswer.setAnswerLookUpCode(questAnswer.getAnswerLookUpCode());
					copyQuestAnswer.setAnswerNumber(questAnswer.getAnswerNumber());
					copyQuestAnswer.setExplanation(questAnswer.getExplanation());
					copyQuestAnswer.setQuestionId(questAnswer.getQuestionId());
					copyQuestAnswer.setOptionNumber(questAnswer.getOptionNumber());
					copyQuestAnswer.setUpdateTimeStamp(questAnswer.getUpdateTimeStamp());
					copyQuestAnswer.setUpdateUser(questAnswer.getUpdateUser());
					copyQuestAnswer.setQuestAnswerHeader(copyAnswerHeader);
					List<QuestAnswerAttachment> copyQuestAnswerAttachments = new ArrayList<>();
					List<QuestAnswerAttachment> questAnswerAttachments = questAnswer.getQuestAnswerAttachment();
					if (questAnswerAttachments != null && !questAnswerAttachments.isEmpty()) {
						for (QuestAnswerAttachment questAnswerAttachment : questAnswerAttachments) {
							QuestAnswerAttachment copyQuestAnswerAttachment = new QuestAnswerAttachment();
							copyQuestAnswerAttachment.setAttachment(questAnswerAttachment.getAttachment());
							copyQuestAnswerAttachment.setContentType(questAnswerAttachment.getContentType());
							copyQuestAnswerAttachment.setFileName(questAnswerAttachment.getFileName());
							copyQuestAnswerAttachment.setUpdateTimeStamp(questAnswerAttachment.getUpdateTimeStamp());
							copyQuestAnswerAttachment.setUpdateUser(questAnswerAttachment.getUpdateUser());
							copyQuestAnswerAttachment.setQuestAnswer(copyQuestAnswer);
							copyQuestAnswerAttachments.add(copyQuestAnswerAttachment);
						}
					}
					copyQuestAnswer.setQuestAnswerAttachment(copyQuestAnswerAttachments);
					copyQuestAnswers.add(copyQuestAnswer);
				}
				 
			}
			copyAnswerHeader.setQusetAnswers(copyQuestAnswers);
			questionnaireDAO.copyQuestionAnswers(copyAnswerHeader);
			copyTableAnswer(answerHeader.getQuestAnsHeaderId(), copyAnswerHeader.getQuestAnsHeaderId());
		}
	}

	private void copyTableAnswer(Integer oldQuestionnaireAnsHeaderId, Integer questionnaireAnsHeaderId) {
		List<QuestTableAnswer> questTableAnswers = questionnaireDAO.getQuestTableAnswers(oldQuestionnaireAnsHeaderId);
		if (questTableAnswers != null && !questTableAnswers.isEmpty()) {
			questTableAnswers.forEach(tableAnswer -> {
				QuestTableAnswer questTableAnswer = new QuestTableAnswer();
				questTableAnswer.setQuestAnsHeaderId(questionnaireAnsHeaderId);
				questTableAnswer.setOrderNumber(tableAnswer.getOrderNumber());
				questTableAnswer.setQuestionId(tableAnswer.getQuestionId());
				questTableAnswer.setUpdateUser(tableAnswer.getUpdateUser());
				questTableAnswer.setUpdateTimeStamp(commonDao.getCurrentTimestamp());
				questTableAnswer.setColumn1(tableAnswer.getColumn1());
				questTableAnswer.setColumn2(tableAnswer.getColumn2());
				questTableAnswer.setColumn3(tableAnswer.getColumn3());
				questTableAnswer.setColumn4(tableAnswer.getColumn4());
				questTableAnswer.setColumn5(tableAnswer.getColumn5());
				questTableAnswer.setColumn6(tableAnswer.getColumn6());
				questTableAnswer.setColumn7(tableAnswer.getColumn7());
				questTableAnswer.setColumn8(tableAnswer.getColumn8());
				questTableAnswer.setColumn9(tableAnswer.getColumn9());
				questTableAnswer.setColumn10(tableAnswer.getColumn10());
				questionnaireDAO.saveQuestTableAnswers(questTableAnswer);
			});
		}
	}

	public Integer deleteQuestAnswerAttachment(Integer answerHeaderId, Map<String, Object> question) throws Exception {
		Integer isUpdated = 0;
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter(QUESTION_ID_PARAM, DBEngineConstants.TYPE_INTEGER, question.get(QUESTION_ID)));
			inParam.add(new Parameter(QUESTIONNAIRE_ANSWER_HEADER_ID_PARAM, DBEngineConstants.TYPE_INTEGER, answerHeaderId));
			isUpdated = dbEngine.executeUpdate(inParam, "DELETE_QUEST_ANSWER_ATTACHMENT");
		} catch (Exception e) {
			logger.error("Exception in deleteQuestionnaireAnswerAttachment : {}", e.getMessage());
			throw e;
		}
		return isUpdated;
	}

	void getTableAnswers(ArrayList<HashMap<String, Object>> answerList, Integer questionnaireAnswerHeaderId) {
		Map<Object, List<HashMap<String, Object>>> data = getTableAnswer(questionnaireAnswerHeaderId)
				.stream().collect(Collectors.groupingBy(obj -> obj.get("QUESTION_NUMBER"), Collectors.toList()));
		data.entrySet().stream().forEach(integerListEntry -> {
			HashMap<String, Object> map = new HashMap<>();
			map.put(QUESTION_NUMBER,integerListEntry.getKey().toString());
			map.put(ANSWER,integerListEntry.getValue());
			answerList.add(map);
		});
	}

	private ArrayList<HashMap<String, Object>> getTableAnswer(Integer questionnaireAnswerHeaderId){
		try {
			ArrayList<Parameter> inputParam = new ArrayList<>();
			inputParam.add(new Parameter("<<AV_QNR_ANS_HEADER_ID>>", DBEngineConstants.TYPE_INTEGER,
					questionnaireAnswerHeaderId));
			return dbEngine.executeQuery(inputParam, "GET_QUESTIONNAIRE_TABLE_ANSWER");
		} catch (Exception e) {
			logger.error("Exception in getTableAnswer : {}", e.getMessage());
			return new ArrayList<>();
		}
	}

	private void saveTableAnswer(Integer answerHeaderId, HashMap<String, Object> question, HashMap<String, Object> answer, String updateUser) {
		try {
			ArrayList<HashMap<String, Object>> data = (ArrayList<HashMap<String, Object>>) answer.get("1");
			for (HashMap<String, Object> tableAnswer : data) {
				if (tableAnswer.get(AC_TYPE) != null && "I".equals(tableAnswer.get(AC_TYPE))) {
					insertTableAnswer(tableAnswer, answerHeaderId, updateUser, (Integer)question.get(QUESTION_ID));
					tableAnswer.put("QUEST_TABLE_ANSWER_ID", getTableAnswerId(answerHeaderId, (Integer)question.get(QUESTION_ID)));
					tableAnswer.put(AC_TYPE, null);
				}
				else if (tableAnswer.get(AC_TYPE) != null && "U".equals(tableAnswer.get(AC_TYPE))) {
					QuestTableAnswer questTableAnswer = new QuestTableAnswer();
					questTableAnswer.setQuestTableAnswerId((Integer)tableAnswer.get("QUEST_TABLE_ANSWER_ID"));
					updateTableAnswer(tableAnswer, updateUser);
					tableAnswer.put(AC_TYPE, null);
				}
				else if (tableAnswer.get(AC_TYPE) != null && "D".equals(tableAnswer.get(AC_TYPE))) {
					deleteTableAnswer((Integer)tableAnswer.get("QUEST_TABLE_ANSWER_ID"));
					tableAnswer.clear();
				}
			}
			data.removeIf(item -> item.isEmpty());
		} catch (Exception e) {
			logger.error("saveTableAnswer {}",e.getMessage());
		}
	}

	private Integer getTableAnswerId(Integer answerHeaderId, Integer questionId) {
		try {
			ArrayList<HashMap<String, Object>> output = new ArrayList<>();
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<QUESTIONNAIRE_ANS_HEADER_ID>>", DBEngineConstants.TYPE_INTEGER, answerHeaderId));
			inParam.add(new Parameter("<<QUESTION_ID>>", DBEngineConstants.TYPE_INTEGER, questionId));
			output = dbEngine.executeQuery(inParam, "GET_TABLE_ANSWER_ID");	
			return Integer.parseInt(output.get(0).get("QUEST_TABLE_ANSWER_ID").toString());
		} catch (Exception e) {
			logger.error("Exception in insertQuestionnaireAnswer : {}", e.getMessage());
			e.printStackTrace();
		}
		return null;
	}

	private void insertTableAnswer(HashMap<String, Object> tableAnswer, Integer answerHeaderId, String updateUser, Integer questionId) {
//		Integer questionnaireAnswerId = questionnaireDAO.getNextQuestionnaireAnswerTableId();
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
//			inParam.add(new Parameter("<<QUEST_TABLE_ANSWER_ID>>", DBEngineConstants.TYPE_INTEGER, questionnaireAnswerId));
			inParam.add(new Parameter("<<QUESTIONNAIRE_ANS_HEADER_ID>>", DBEngineConstants.TYPE_INTEGER, answerHeaderId));
			inParam.add(new Parameter("<<QUESTION_ID>>", DBEngineConstants.TYPE_INTEGER, questionId));
			inParam.add(new Parameter("<<ORDER_NUMBER>>", DBEngineConstants.TYPE_INTEGER, tableAnswer.get("ORDER_NUMBER")));
			inParam.add(new Parameter("<<COLUMN_1>>", DBEngineConstants.TYPE_STRING, tableAnswer.get("COLUMN_1")));
			inParam.add(new Parameter("<<COLUMN_2>>", DBEngineConstants.TYPE_STRING, tableAnswer.get("COLUMN_2")));
			inParam.add(new Parameter("<<COLUMN_3>>", DBEngineConstants.TYPE_STRING, tableAnswer.get("COLUMN_3")));
			inParam.add(new Parameter("<<COLUMN_4>>", DBEngineConstants.TYPE_STRING, tableAnswer.get("COLUMN_4")));
			inParam.add(new Parameter("<<COLUMN_5>>", DBEngineConstants.TYPE_STRING, tableAnswer.get("COLUMN_5")));
			inParam.add(new Parameter("<<COLUMN_6>>", DBEngineConstants.TYPE_STRING, tableAnswer.get("COLUMN_6")));
			inParam.add(new Parameter("<<COLUMN_7>>", DBEngineConstants.TYPE_STRING, tableAnswer.get("COLUMN_7")));
			inParam.add(new Parameter("<<COLUMN_8>>", DBEngineConstants.TYPE_STRING, tableAnswer.get("COLUMN_8")));
			inParam.add(new Parameter("<<COLUMN_9>>", DBEngineConstants.TYPE_STRING, tableAnswer.get("COLUMN_9")));
			inParam.add(new Parameter("<<COLUMN_10>>", DBEngineConstants.TYPE_STRING, tableAnswer.get("COLUMN_10")));
			inParam.add(new Parameter(UPDATE_TIMESTAMP_PARAM, DBEngineConstants.TYPE_TIMESTAMP, commonDao.getCurrentTimestamp()));
			inParam.add(new Parameter(UPDATE_USER_PARAM, DBEngineConstants.TYPE_STRING, updateUser));
			dbEngine.executeUpdate(inParam, "INSERT_QUESTIONNAIRE_TABLE_ANSWER");
		} catch (Exception e) {
			logger.error("Exception in insertQuestionnaireAnswer : {}", e.getMessage());
		}
//		return questionnaireAnswerId;
	}

	private Integer updateTableAnswer(HashMap<String, Object> tableAnswer, String updateUser) {
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<ORDER_NUMBER>>", DBEngineConstants.TYPE_INTEGER, tableAnswer.get("ORDER_NUMBER")));
			inParam.add(new Parameter("<<COLUMN_1>>", DBEngineConstants.TYPE_STRING, tableAnswer.get("COLUMN_1")));
			inParam.add(new Parameter("<<COLUMN_2>>", DBEngineConstants.TYPE_STRING, tableAnswer.get("COLUMN_2")));
			inParam.add(new Parameter("<<COLUMN_3>>", DBEngineConstants.TYPE_STRING, tableAnswer.get("COLUMN_3")));
			inParam.add(new Parameter("<<COLUMN_4>>", DBEngineConstants.TYPE_STRING, tableAnswer.get("COLUMN_4")));
			inParam.add(new Parameter("<<COLUMN_5>>", DBEngineConstants.TYPE_STRING, tableAnswer.get("COLUMN_5")));
			inParam.add(new Parameter("<<COLUMN_6>>", DBEngineConstants.TYPE_STRING, tableAnswer.get("COLUMN_6")));
			inParam.add(new Parameter("<<COLUMN_7>>", DBEngineConstants.TYPE_STRING, tableAnswer.get("COLUMN_7")));
			inParam.add(new Parameter("<<COLUMN_8>>", DBEngineConstants.TYPE_STRING, tableAnswer.get("COLUMN_8")));
			inParam.add(new Parameter("<<COLUMN_9>>", DBEngineConstants.TYPE_STRING, tableAnswer.get("COLUMN_9")));
			inParam.add(new Parameter("<<COLUMN_10>>", DBEngineConstants.TYPE_STRING, tableAnswer.get("COLUMN_10")));
			inParam.add(new Parameter(UPDATE_TIMESTAMP_PARAM, DBEngineConstants.TYPE_TIMESTAMP, commonDao.getCurrentTimestamp()));
			inParam.add(new Parameter(UPDATE_USER_PARAM, DBEngineConstants.TYPE_STRING, updateUser));
			inParam.add(new Parameter("<<QUEST_TABLE_ANSWER_ID>>", DBEngineConstants.TYPE_INTEGER, tableAnswer.get("QUEST_TABLE_ANSWER_ID")));
			dbEngine.executeUpdate(inParam, "UPDATE_QUESTIONNAIRE_TABLE_ANSWER");
		} catch (Exception e) {
			logger.error("Exception in updateQnrTableAnswer : {}", e.getMessage());
		}
		return 1;
	}

	public Integer deleteTableAnswer(Integer answerTableId) {
		Integer isUpdated = 0;
		try {
			ArrayList<Parameter> inParam = new ArrayList<>();
			inParam.add(new Parameter("<<QUEST_TABLE_ANSWER_ID>>", DBEngineConstants.TYPE_INTEGER, answerTableId));
			isUpdated = dbEngine.executeUpdate(inParam, "DELETE_QUESTIONNAIRE_TABLE_ANSWER");
		} catch (Exception e) {
			logger.error("Exception in deleteQuestionnaireTableAnswer : {}", e.getMessage());
		}
		return isUpdated;
	}

	@Override
	public QuestionnaireDataBus ruleEvaluateQuestionnaire(QuestionnaireDataBus questionnaireDataBus) {
		Boolean isrulePassed = generalInformationDao.evaluateRule(questionnaireDataBus.getModuleItemCode(), questionnaireDataBus.getModuleSubItemCode(),
				questionnaireDataBus.getModuleItemKey(), Integer.parseInt(questionnaireDataBus.getRuleId()), AuthenticatedUser.getLoginPersonId(), AuthenticatedUser.getLoginUserName(), questionnaireDataBus.getModuleSubItemKey() != null  && !questionnaireDataBus.getModuleSubItemKey().isEmpty() ? questionnaireDataBus.getModuleSubItemKey() : Constants.SUBMODULE_ITEM_KEY);
		questionnaireDataBus.setRulePassed(isrulePassed);
		return questionnaireDataBus;
	}

}
