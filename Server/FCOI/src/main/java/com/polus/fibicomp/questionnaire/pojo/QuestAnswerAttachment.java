package com.polus.fibicomp.questionnaire.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonBackReference;

@Entity
@Table(name = "QUEST_ANSWER_ATTACHMENT")
public class QuestAnswerAttachment implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "QUESTIONNAIRE_ANSWER_ATT_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer questAnswerAttachId;

	@JsonBackReference
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "QUEST_ANSWER_ATTACHMENT_FK1"), name = "QUESTIONNAIRE_ANSWER_ID", referencedColumnName = "QUESTIONNAIRE_ANSWER_ID")
	private QuestAnswer questAnswer;

	@Column(name = "ATTACHMENT")
	private byte[] attachment;

	@Column(name = "FILE_NAME")
	private String fileName;

	@Column(name = "CONTENT_TYPE")
	private String contentType;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getQuestAnswerAttachId() {
		return questAnswerAttachId;
	}

	public void setQuestAnswerAttachId(Integer questAnswerAttachId) {
		this.questAnswerAttachId = questAnswerAttachId;
	}

	public QuestAnswer getQuestAnswer() {
		return questAnswer;
	}

	public void setQuestAnswer(QuestAnswer questAnswer) {
		this.questAnswer = questAnswer;
	}

	public byte[] getAttachment() {
		return attachment;
	}

	public void setAttachment(byte[] attachment) {
		this.attachment = attachment;
	}

	public String getFileName() {
		return fileName;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	public String getContentType() {
		return contentType;
	}

	public void setContentType(String contentType) {
		this.contentType = contentType;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

}
