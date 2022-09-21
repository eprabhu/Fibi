package com.polus.fibicomp.faq.vo;

import java.sql.Timestamp;
import java.util.List;

import com.polus.fibicomp.faq.pojo.FaqAttachment;
import com.polus.fibicomp.faq.pojo.FaqCategory;
import com.polus.fibicomp.pojo.Faq;

public class FaqCategoryVo {

	private Integer categoryCode;

	private Integer subCategoryCode;

	private Integer categoryTypeCode;

	private String description;

	private String question;

	private String answer;

	private Timestamp updateTimestamp;

	private String updateUser;

	private Integer questionId;

	private String url;

	private Integer faqAttachmentId;

	private Faq faqdtls;

	private Boolean status;

	private List<FaqCategory> faqCategory;

	private List<Faq> faq;

	private List<FaqAttachment> newFaqAttachment;

	private FaqAttachment faqAttachment;

	private Integer remaining;

	private Integer length;

	private String fileContent;

	private String contentType;

	private String fileName;

	private String fileTimestamp;

	private String personId;

	public Integer getCategoryCode() {
		return categoryCode;
	}

	public void setCategoryCode(Integer categoryCode) {
		this.categoryCode = categoryCode;
	}

	public Integer getCategoryTypeCode() {
		return categoryTypeCode;
	}

	public void setCategoryTypeCode(Integer categoryTypeCode) {
		this.categoryTypeCode = categoryTypeCode;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
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

	public List<Faq> getFaq() {
		return faq;
	}

	public void setFaq(List<Faq> faq) {
		this.faq = faq;
	}

	public Faq getFaqdtls() {
		return faqdtls;
	}

	public void setFaqdtls(Faq faqdtls) {
		this.faqdtls = faqdtls;
	}

	public Integer getSubCategoryCode() {
		return subCategoryCode;
	}

	public void setSubCategoryCode(Integer subCategoryCode) {
		this.subCategoryCode = subCategoryCode;
	}

	public List<FaqCategory> getFaqCategory() {
		return faqCategory;
	}

	public void setFaqCategory(List<FaqCategory> faqCategory) {
		this.faqCategory = faqCategory;
	}

	public FaqAttachment getFaqAttachment() {
		return faqAttachment;
	}

	public void setFaqAttachment(FaqAttachment faqAttachment) {
		this.faqAttachment = faqAttachment;
	}

	public Integer getQuestionId() {
		return questionId;
	}

	public void setQuestionId(Integer questionId) {
		this.questionId = questionId;
	}

	public String getUrl() {
		return url;
	}

	public void setUrl(String url) {
		this.url = url;
	}

	public List<FaqAttachment> getNewFaqAttachment() {
		return newFaqAttachment;
	}

	public void setNewFaqAttachment(List<FaqAttachment> newFaqAttachment) {
		this.newFaqAttachment = newFaqAttachment;
	}

	public Integer getFaqAttachmentId() {
		return faqAttachmentId;
	}

	public void setFaqAttachmentId(Integer faqAttachmentId) {
		this.faqAttachmentId = faqAttachmentId;
	}

	public Boolean getStatus() {
		return status;
	}

	public void setStatus(Boolean status) {
		this.status = status;
	}

	public Integer getRemaining() {
		return remaining;
	}

	public void setRemaining(Integer remaining) {
		this.remaining = remaining;
	}

	public Integer getLength() {
		return length;
	}

	public void setLength(Integer length) {
		this.length = length;
	}

	public String getFileContent() {
		return fileContent;
	}

	public void setFileContent(String fileContent) {
		this.fileContent = fileContent;
	}

	public String getContentType() {
		return contentType;
	}

	public void setContentType(String contentType) {
		this.contentType = contentType;
	}

	public String getFileName() {
		return fileName;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	public String getFileTimestamp() {
		return fileTimestamp;
	}

	public void setFileTimestamp(String fileTimestamp) {
		this.fileTimestamp = fileTimestamp;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}
}
