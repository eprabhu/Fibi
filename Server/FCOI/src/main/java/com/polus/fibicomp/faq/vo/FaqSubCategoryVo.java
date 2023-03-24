package com.polus.fibicomp.faq.vo;

import java.sql.Timestamp;

import com.polus.fibicomp.faq.pojo.FaqCategory;

public class FaqSubCategoryVo {

	private Integer subCategoryCode;

	private Integer categoryCode;

	private String description;

	private Timestamp updateTimestamp;

	private String updateUser;

	private FaqCategory faqCategory;

	public Integer getSubCategoryCode() {
		return subCategoryCode;
	}

	public void setSubCategoryCode(Integer subCategoryCode) {
		this.subCategoryCode = subCategoryCode;
	}

	public Integer getCategoryCode() {
		return categoryCode;
	}

	public void setCategoryCode(Integer categoryCode) {
		this.categoryCode = categoryCode;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
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

	public FaqCategory getFaqCategory() {
		return faqCategory;
	}

	public void setFaqCategory(FaqCategory faqCategory) {
		this.faqCategory = faqCategory;
	}

}
