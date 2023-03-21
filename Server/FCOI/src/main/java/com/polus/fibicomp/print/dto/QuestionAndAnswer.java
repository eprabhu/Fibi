package com.polus.fibicomp.print.dto;

import java.util.ArrayList;
import java.util.List;

public class QuestionAndAnswer {

	private String question;
	private String answer;
	private List<String> answersList;
	private Integer maximumNumberOfAnswers;

	public QuestionAndAnswer() {
		answersList = new ArrayList<>();
	}

	public String getQuestion() {
		return question;
	}

	public void setQuestion(String question) {
		this.question = question;
	}

	public String getAnswer() {
		return answer;
	}

	public void setAnswer(String answer) {
		this.answer = answer;
	}

	public List<String> getAnswersList() {
		return answersList;
	}

	public void setAnswersList(List<String> answersList) {
		this.answersList = answersList;
	}

	public Integer getMaximumNumberOfAnswers() {
		return maximumNumberOfAnswers;
	}

	public void setMaximumNumberOfAnswers(Integer maximumNumberOfAnswers) {
		this.maximumNumberOfAnswers = maximumNumberOfAnswers;
	}

}
