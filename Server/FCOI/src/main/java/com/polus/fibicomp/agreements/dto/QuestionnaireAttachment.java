package com.polus.fibicomp.agreements.dto;

import java.math.BigDecimal;
import java.sql.Timestamp;

public class QuestionnaireAttachment {

	private Integer answerAttachmentId;

	private String fileName;

	private String contentType;

	private String updateUser;

	private Timestamp updateTimestamp;

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

	public Integer getAnswerAttachmentId() {
		return answerAttachmentId;
	}

	public void setAnswerAttachmentId(Integer answerAttachmentId) {
		this.answerAttachmentId = answerAttachmentId;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

}
