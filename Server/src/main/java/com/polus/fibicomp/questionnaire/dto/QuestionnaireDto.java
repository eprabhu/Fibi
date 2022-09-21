package com.polus.fibicomp.questionnaire.dto;

import java.util.HashMap;
import java.util.List;

import com.polus.fibicomp.vo.LookUp;

public class QuestionnaireDto {

	private Integer maxGroupNumber;

	private List<HashMap<String, Object>> questions;

	private List<HashMap<String, Object>> conditions;

	private List<HashMap<String, Object>> options;

	private DeleteDto deleteList;

	private List<QuestionnaireQuestionDto> questionnaireQuestions;

	private List<HashMap<String, Object>> questionnaireConditions;

	private List<HashMap<String, Object>> questionnaireOptions;

	private List<HashMap<String, Object>> questionnaireAnswers;

	private List<QuestionnaireAttachmentDto> quesAttachmentList;

	private List<LookUp> lookUpDetails;

	public DeleteDto getDeleteList() {
		return deleteList;
	}

	public void setDeleteList(DeleteDto deleteList) {
		this.deleteList = deleteList;
	}

	public Integer getMaxGroupNumber() {
		return maxGroupNumber;
	}

	public void setMaxGroupNumber(Integer maxGroupNumber) {
		this.maxGroupNumber = maxGroupNumber;
	}

	public List<HashMap<String, Object>> getQuestions() {
		return questions;
	}

	public void setQuestions(List<HashMap<String, Object>> questions) {
		this.questions = questions;
	}

	public List<HashMap<String, Object>> getConditions() {
		return conditions;
	}

	public void setConditions(List<HashMap<String, Object>> conditions) {
		this.conditions = conditions;
	}

	public List<HashMap<String, Object>> getOptions() {
		return options;
	}

	public void setOptions(List<HashMap<String, Object>> options) {
		this.options = options;
	}

	public List<HashMap<String, Object>> getQuestionnaireConditions() {
		return questionnaireConditions;
	}

	public void setQuestionnaireConditions(List<HashMap<String, Object>> questionnaireConditions) {
		this.questionnaireConditions = questionnaireConditions;
	}

	public List<HashMap<String, Object>> getQuestionnaireOptions() {
		return questionnaireOptions;
	}

	public void setQuestionnaireOptions(List<HashMap<String, Object>> questionnaireOptions) {
		this.questionnaireOptions = questionnaireOptions;
	}

	public List<HashMap<String, Object>> getQuestionnaireAnswers() {
		return questionnaireAnswers;
	}

	public void setQuestionnaireAnswers(List<HashMap<String, Object>> questionnaireAnswers) {
		this.questionnaireAnswers = questionnaireAnswers;
	}

	public List<QuestionnaireQuestionDto> getQuestionnaireQuestions() {
		return questionnaireQuestions;
	}

	public void setQuestionnaireQuestions(List<QuestionnaireQuestionDto> questionnaireQuestions) {
		this.questionnaireQuestions = questionnaireQuestions;
	}

	public List<QuestionnaireAttachmentDto> getQuesAttachmentList() {
		return quesAttachmentList;
	}

	public void setQuesAttachmentList(List<QuestionnaireAttachmentDto> quesAttachmentList) {
		this.quesAttachmentList = quesAttachmentList;
	}

	public List<LookUp> getLookUpDetails() {
		return lookUpDetails;
	}

	public void setLookUpDetails(List<LookUp> lookUpDetails) {
		this.lookUpDetails = lookUpDetails;
	}
}
