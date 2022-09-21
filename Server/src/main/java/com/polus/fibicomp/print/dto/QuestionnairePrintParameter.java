package com.polus.fibicomp.print.dto;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class QuestionnairePrintParameter {

	private String questionnaireName;
	private List<QuestionAndAnswer> questionAndAnswerList;
	private Integer maximumNumberOfAnswers;

	public String getQuestionnaireName() {
		return questionnaireName;
	}

	public void setQuestionnaireName(String questionnaireName) {
		this.questionnaireName = questionnaireName;
	}

	public List<QuestionAndAnswer> getQuestionAndAnswerList() {
		return questionAndAnswerList;
	}

	public void setQuestionAndAnswerList(List<QuestionAndAnswer> questionAndAnswerList) {
		this.questionAndAnswerList = questionAndAnswerList;
	}

	public Integer getMaximumNumberOfAnswers() {
		return maximumNumberOfAnswers;
	}

	public void setMaximumNumberOfAnswers(Integer maximumNumberOfAnswers) {
		this.maximumNumberOfAnswers = maximumNumberOfAnswers;
	}

	public QuestionnairePrintParameter(String questionnaireName, List<QuestionAndAnswer> questionAndAnswerList, Integer maximumNumberOfAnswers) {
		this.questionnaireName = questionnaireName;
		this.maximumNumberOfAnswers = maximumNumberOfAnswers;
		if(questionAndAnswerList != null) {
			this.questionAndAnswerList = new ArrayList<QuestionAndAnswer>(questionAndAnswerList);
			Collections.copy(this.questionAndAnswerList, questionAndAnswerList);
		} else {
			this.questionAndAnswerList = new ArrayList<QuestionAndAnswer>();
		}
	}

}
