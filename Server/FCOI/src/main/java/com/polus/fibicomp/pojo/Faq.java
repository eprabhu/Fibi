package com.polus.fibicomp.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.FetchType;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.OneToMany;
import javax.persistence.Table;

import org.hibernate.annotations.GenericGenerator;
import org.hibernate.annotations.Parameter;

import com.fasterxml.jackson.annotation.JsonManagedReference;
import com.polus.fibicomp.faq.pojo.FaqAttachment;

@Entity
@Table(name = "FAQ")
public class Faq implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GenericGenerator(name = "questionIdGererator", strategy = "increment", parameters = {
			@Parameter(name = "initial_value", value = "1"), @Parameter(name = "increment_size", value = "1") })
	@GeneratedValue(generator = "questionIdGererator")
	@Column(name = "QUESTION_ID")
	private Integer questionId;

	@Column(name = "QUESTION")
	private String question;

	@Column(name = "ANSWER")
	private String answer;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "CATEGORY_CODE")
	private Integer categoryCode;

	@Column(name = "URL")
	private String url;

	@Column(name = "SUB_CATEGORY_CODE")
	private Integer subCategoryCode;

	@JsonManagedReference
	@OneToMany(mappedBy = "faq", orphanRemoval = true, cascade = { CascadeType.ALL }, fetch = FetchType.LAZY)
	private List<FaqAttachment> faqAttachment;

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

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public List<FaqAttachment> getFaqAttachment() {
		return faqAttachment;
	}

	public void setFaqAttachment(List<FaqAttachment> faqAttachment) {
		this.faqAttachment = faqAttachment;
	}

}
