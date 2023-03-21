package com.polus.fibicomp.questionnaire.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonManagedReference;

@Entity
@Table(name = "QUEST_ANSWER_HEADER")
public class QuestAnswerHeader implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "QUESTIONNAIRE_ANS_HEADER_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer questAnsHeaderId;

	@Column(name = "QUESTIONNAIRE_ID")
	private Integer questionnaireId;

	@Column(name = "MODULE_ITEM_CODE")
	private Integer moduleItemCode;

	@Column(name = "MODULE_SUB_ITEM_CODE")
	private Integer moduleSubItemCode;

	@Column(name = "MODULE_ITEM_KEY")
	private String moduleItemKey;

	@Column(name = "MODULE_SUB_ITEM_KEY")
	private String moduleSubItemKey;

	@Column(name = "QUESTIONNAIRE_COMPLETED_FLAG")
	private String questCompletedFlag;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@JsonManagedReference
	@OneToMany(mappedBy = "questAnswerHeader", orphanRemoval = true, cascade = { CascadeType.REMOVE, CascadeType.ALL })
	private List<QuestAnswer> qusetAnswers;

	public Integer getQuestAnsHeaderId() {
		return questAnsHeaderId;
	}

	public void setQuestAnsHeaderId(Integer questAnsHeaderId) {
		this.questAnsHeaderId = questAnsHeaderId;
	}

	public Integer getQuestionnaireId() {
		return questionnaireId;
	}

	public void setQuestionnaireId(Integer questionnaireId) {
		this.questionnaireId = questionnaireId;
	}

	public Integer getModuleItemCode() {
		return moduleItemCode;
	}

	public void setModuleItemCode(Integer moduleItemCode) {
		this.moduleItemCode = moduleItemCode;
	}

	public Integer getModuleSubItemCode() {
		return moduleSubItemCode;
	}

	public void setModuleSubItemCode(Integer moduleSubItemCode) {
		this.moduleSubItemCode = moduleSubItemCode;
	}

	public String getModuleItemKey() {
		return moduleItemKey;
	}

	public void setModuleItemKey(String moduleItemKey) {
		this.moduleItemKey = moduleItemKey;
	}

	public String getModuleSubItemKey() {
		return moduleSubItemKey;
	}

	public void setModuleSubItemKey(String moduleSubItemKey) {
		this.moduleSubItemKey = moduleSubItemKey;
	}

	public String getQuestCompletedFlag() {
		return questCompletedFlag;
	}

	public void setQuestCompletedFlag(String questCompletedFlag) {
		this.questCompletedFlag = questCompletedFlag;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public List<QuestAnswer> getQusetAnswers() {
		return qusetAnswers;
	}

	public void setQusetAnswers(List<QuestAnswer> qusetAnswers) {
		this.qusetAnswers = qusetAnswers;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}
}
