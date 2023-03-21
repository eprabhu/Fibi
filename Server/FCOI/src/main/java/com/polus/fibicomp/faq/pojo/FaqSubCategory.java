package com.polus.fibicomp.faq.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import org.hibernate.annotations.GenericGenerator;
import org.hibernate.annotations.Parameter;

import com.fasterxml.jackson.annotation.JsonBackReference;

@Entity
@Table(name = "FAQ_SUB_CATEGORY")
public class FaqSubCategory implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GenericGenerator(name = "subCategoryCodeGenerator", strategy = "increment", parameters = {
			@Parameter(name = "initial_value", value = "1"), @Parameter(name = "increment_size", value = "1") })
	@GeneratedValue(generator = "subCategoryCodeGenerator")
	@Column(name = "SUB_CATEGORY_CODE")
	private Integer subCategoryCode;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	public FaqCategory getFaqCategory() {
		return faqCategory;
	}

	public void setFaqCategory(FaqCategory faqCategory) {
		this.faqCategory = faqCategory;
	}

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@JsonBackReference
    @ManyToOne(cascade = { CascadeType.ALL })
    @JoinColumn(foreignKey = @ForeignKey(name = "FAQ_CATEGORY_FK1"), name = "CATEGORY_CODE", referencedColumnName = "CATEGORY_CODE")
	private FaqCategory faqCategory;

	public Integer getSubCategoryCode() {
		return subCategoryCode;
	}

	public void setSubCategoryCode(Integer subCategoryCode) {
		this.subCategoryCode = subCategoryCode;
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

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

}
