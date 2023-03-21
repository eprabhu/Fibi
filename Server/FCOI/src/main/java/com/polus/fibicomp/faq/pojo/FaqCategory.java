package com.polus.fibicomp.faq.pojo;

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

@Entity
@Table(name = "FAQ_CATEGORY")
public class FaqCategory implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@GenericGenerator(name = "categoryCodeGenerator", strategy = "increment", parameters = {
			@Parameter(name = "initial_value", value = "1"), @Parameter(name = "increment_size", value = "1") })
	@GeneratedValue(generator = "categoryCodeGenerator")
	@Column(name = "CATEGORY_CODE")
	private Integer categoryCode;

	@Column(name = "CATEGORY_TYPE_CODE")
	private Integer categoryTypeCode;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	private String faq;

	@JsonManagedReference
	@OneToMany(mappedBy = "faqCategory", orphanRemoval = true, cascade = { CascadeType.REMOVE,CascadeType.ALL }, fetch = FetchType.EAGER)
	private List<FaqSubCategory> faqSubCategory;

	public List<FaqSubCategory> getFaqSubCategory() {
		return faqSubCategory; 
	}

	public void setFaqSubCategory(List<FaqSubCategory> faqSubCategory) {
		this.faqSubCategory = faqSubCategory;
	}

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

	public String getFaq() {
		return faq;
	}

	public void setFaq(String faq) {
		this.faq = faq;
	}

}