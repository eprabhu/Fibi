package com.polus.fibicomp.questionnaire.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.fasterxml.jackson.annotation.JsonManagedReference;

@Entity
@Table(name = "QUEST_ANSWER")
public class QuestAnswer implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "QUESTIONNAIRE_ANSWER_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer questAnswerId;

	@JsonBackReference
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "QUEST_ANSWER_FK1"), name = "QUESTIONNAIRE_ANS_HEADER_ID", referencedColumnName = "QUESTIONNAIRE_ANS_HEADER_ID")
	private QuestAnswerHeader questAnswerHeader;

	@Column(name = "QUESTION_ID")
	private Integer questionId;

	@Column(name = "OPTION_NUMBER")
	private Integer optionNumber;

	@Column(name = "ANSWER_NUMBER")
	private Integer answerNumber;

	@Column(name = "ANSWER")
	private String answer;

	@Column(name = "ANSWER_LOOKUP_CODE")
	private String answerLookUpCode;

	@Column(name = "EXPLANATION")
	private String explanation;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@JsonManagedReference
	@OneToMany(mappedBy = "questAnswer", orphanRemoval = true, cascade = { CascadeType.REMOVE, CascadeType.ALL })
	private List<QuestAnswerAttachment> questAnswerAttachment;

	public Integer getQuestAnswerId() {
		return questAnswerId;
	}

	public void setQuestAnswerId(Integer questAnswerId) {
		this.questAnswerId = questAnswerId;
	}

	public Integer getQuestionId() {
		return questionId;
	}

	public void setQuestionId(Integer questionId) {
		this.questionId = questionId;
	}

	public Integer getOptionNumber() {
		return optionNumber;
	}

	public void setOptionNumber(Integer optionNumber) {
		this.optionNumber = optionNumber;
	}

	public Integer getAnswerNumber() {
		return answerNumber;
	}

	public void setAnswerNumber(Integer answerNumber) {
		this.answerNumber = answerNumber;
	}

	public String getAnswer() {
		return answer;
	}

	public void setAnswer(String answer) {
		this.answer = answer;
	}

	public String getAnswerLookUpCode() {
		return answerLookUpCode;
	}

	public void setAnswerLookUpCode(String answerLookUpCode) {
		this.answerLookUpCode = answerLookUpCode;
	}

	public String getExplanation() {
		return explanation;
	}

	public void setExplanation(String explanation) {
		this.explanation = explanation;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public QuestAnswerHeader getQuestAnswerHeader() {
		return questAnswerHeader;
	}

	public void setQuestAnswerHeader(QuestAnswerHeader questAnswerHeader) {
		this.questAnswerHeader = questAnswerHeader;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}

	public List<QuestAnswerAttachment> getQuestAnswerAttachment() {
		return questAnswerAttachment;
	}

	public void setQuestAnswerAttachment(List<QuestAnswerAttachment> questAnswerAttachment) {
		this.questAnswerAttachment = questAnswerAttachment;
	}

}
