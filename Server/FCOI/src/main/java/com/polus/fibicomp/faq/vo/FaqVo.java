package com.polus.fibicomp.faq.vo;

import java.sql.Timestamp;

public class FaqVo {
	private Integer questionId;

	private String question;

	private String answer;

	private Timestamp updateTimestamp;

	private String updateUser;

	private Integer categoryCode;

	private Integer subCategoryCode;
	
	private String url;

	public Integer getQuestionId() {
		return questionId;
	}

	public void setQuestionId(Integer questionId) {
		this.questionId = questionId;
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

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Integer getCategoryCode() {
		return categoryCode;
	}

	public void setCategoryCode(Integer categoryCode) {
		this.categoryCode = categoryCode;
	}

	public Integer getSubCategoryCode() {
		return subCategoryCode;
	}

	public void setSubCategoryCode(Integer subCategoryCode) {
		this.subCategoryCode = subCategoryCode;
	}

	public String getUrl() {
		return url;
	}

	public void setUrl(String url) {
		this.url = url;
	}

}
