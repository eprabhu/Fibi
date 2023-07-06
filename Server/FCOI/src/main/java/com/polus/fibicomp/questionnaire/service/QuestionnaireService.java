package com.polus.fibicomp.questionnaire.service;

import java.io.ByteArrayInputStream;
import java.util.HashMap;
import java.util.List;

import javax.servlet.http.HttpServletResponse;

import org.springframework.http.ResponseEntity;
import org.springframework.web.multipart.MultipartFile;
import org.springframework.web.multipart.MultipartHttpServletRequest;

import com.polus.fibicomp.questionnaire.dto.QuestionnaireDataBus;

public interface QuestionnaireService {

	/**
	 * @param QuestionnaireDataBus
	 * @return QuestionnaireDataBus : Save questionnaire and return back the data
	 * @throws Exception
	 */
	public QuestionnaireDataBus getApplicableQuestionnaire(QuestionnaireDataBus questionnaireDataBus);

	/**
	 * @param QuestionnaireDataBus
	 * @return QuestionnaireDataBus : The list of questionnaire questions,its
	 *         condition,options and its answers
	 * @throws Exception
	 */
	public QuestionnaireDataBus getQuestionnaireDetails(QuestionnaireDataBus questionnaireDataBus);

	/**
	 * @param request
	 * @param QuestionnaireDataBus
	 * @return QuestionnaireDataBus : Save questionnaire and return back the data
	 * @throws Exception
	 */
	public ResponseEntity<String> saveQuestionnaireAnswers(QuestionnaireDataBus questionnaireDataBus, MultipartHttpServletRequest request);

	/**
	 * @param QuestionnaireDataBus
	 * @return Boolean : Check if the questionnaire is complete or not
	 * @throws Exception
	 */
	boolean isQuestionnaireComplete(QuestionnaireDataBus questionnaireDataBus);

	/**
	 * @param QuestionnaireDataBus
	 * @return QuestionnaireDataBus : Save questionnaire and return back the data
	 * @throws Exception
	 */
	public QuestionnaireDataBus configureQuestionnaire(QuestionnaireDataBus questionnaireDataBus);

	/**
	 * @param QuestionnaireDataBus
	 * @return QuestionnaireDataBus : Save questionnaire and return back the data
	 * @throws Exception
	 */
	public QuestionnaireDataBus editQuestionnaire(QuestionnaireDataBus questionnaireDataBus);

	/**
	 * @param QuestionnaireDataBus
	 * @return QuestionnaireDataBus : Save questionnaire and return back the data
	 * @throws Exception
	 */
	public QuestionnaireDataBus showAllQuestionnaire(QuestionnaireDataBus questionnaireDataBus);

	/**
	 * @param QuestionnaireDataBus
	 * @return QuestionnaireDataBus : Save questionnaire and return back the data
	 * @throws Exception
	 */
	public QuestionnaireDataBus createQuestionnaire(QuestionnaireDataBus questionnaireDataBus);

	/**
	 * @param questionnaireDataBus
	 * @param response
	 * @return download attachments
	 */
	public ResponseEntity<byte[]> downloadAttachments(QuestionnaireDataBus questionnaireDataBus, HttpServletResponse response);

	/**
	 * This method will return questionnaire business rule for a module and its sub module
	 * @param questionnaireDataBus
	 * @return List for rule
	 * @throws Exception 
	 */
	public  List<HashMap<String, Object>> getBusinessRuleForQuestionnaire(QuestionnaireDataBus questionnaireDataBus);

	/**
	 * This method will return questionnaire print pdf data
	 * @param questionnaireDataBus
	 * @return List for rule
	 */
	public ByteArrayInputStream printQuestionnaire(QuestionnaireDataBus questionnaireDataBus);

	/**
	 * This method will return questionnaire details
	 * @param questionnaireDataBus
	 * @return List for rule
	 */
	public List<QuestionnaireDataBus> getQuestionnaireList(QuestionnaireDataBus questionnaireDataBus);

	/**
	 * This method will activate or deactivate a questionnaire 
	 * @param questionnaire_id
	 * @param isActivate
	 * @throws Exception 
	 */
	
	public String activeDeactivateQuestionnaire(QuestionnaireDataBus questionnaireDataBus, boolean isActivate);

	/**
	 * This method will return questionnaire details based on questionnaire id
	 * @param questionnaireDataBus
	 * @return questionnaire details
	 */
	public QuestionnaireDataBus getQuestionnaireBasedOnQuestionnaireId(QuestionnaireDataBus questionnaireDataBus);

	/**
	 * This method is used to generate attachment for WAF requests
	 * @param questionnaireDataBus
	 * @param request
	 * @return generated file from multiple requests
	 */
	public MultipartFile generateAttachment(QuestionnaireDataBus questionnaireDataBus, MultipartHttpServletRequest request);

	/**
	 * This method is used to copy questionnaire
	 * @param questionnaireDataBus
	 * @return
	 * @throws Exception 
	 */
	public QuestionnaireDataBus copyQuestionnaire(QuestionnaireDataBus questionnaireDataBus);

	/**
	 * This method is used to get all module list.
	 * @param questionnaireDataBus
	 * @return list of module
	 * @throws Exception 
	 */
	public QuestionnaireDataBus getModuleList(QuestionnaireDataBus questionnaireDataBus);

	/**
	 * This method is used to update header details.
	 * @param questionnaireDataBus
	 * @return 
	 * @throws Exception 
	 */
	public QuestionnaireDataBus updateQuestionnaireUsage(QuestionnaireDataBus questionnaireDataBus);

	/**
	 * This method is used to get the questionnaire list based on given module
	 * @param questionnaireDataBus
	 * @return list of questionnaire
	 */
	public QuestionnaireDataBus getQuestionnaireListByModule(QuestionnaireDataBus questionnaireDataBus);

	/**
	 * This method is used to copy questionnaire answers
	 * @param questionnaireDataBus
	 * @return questionnaireDataBus
	 */
	public QuestionnaireDataBus copyQuestionnaireAnswers(QuestionnaireDataBus questionnaireDataBus);

	/**
	 * This method is used to copy the questionnaire details with answers
	 * @param questionnaireDataBus
	 */
	public void copyQuestionnaireForVersion(QuestionnaireDataBus questionnaireDataBus);

	/**
	 * @param moduleItemKey
	 * @param copiedModuleItemKey
	 * @param moduleCode
	 * @param subModuleCode
	 */
	public void copyQuestionnaireDatas(Integer moduleItemKey, Integer copiedModuleItemKey, Integer moduleCode,
			Integer subModuleCode);

	/**
	 * @param moduleItemKey
	 * @param subModuleItemKey
	 * @param moduleCode
	 * @param subModuleCode
	 */
	public QuestionnaireDataBus ruleEvaluateQuestionnaire(QuestionnaireDataBus questionnaireDataBus);

	/**
	 * This method is used to delete all questionnaire answers
	 * delete table answers
	 * delete attachment answers
	 * delete quest answers
	 * delete quest header
	 * @param questionnaireDataBus
	 */
	void deleteAllQuestionAnswers(QuestionnaireDataBus questionnaireDataBus);
}
