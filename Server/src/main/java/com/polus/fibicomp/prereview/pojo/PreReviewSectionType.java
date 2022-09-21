package com.polus.fibicomp.prereview.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "PRE_REVIEW_SECTION_TYPE")
public class PreReviewSectionType implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "PRE_REVIEW_SECTION_TYPE_CODE")
	private String reviewSectionTypeCode;

	@Column(name = "DESCRIPTION")
	private String description;
	
	@Column(name = "PRE_REVIEW_TYPE_CODE")
	private String preReviewTypeCode;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "IS_ACTIVE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isActive;

	public String getReviewSectionTypeCode() {
		return reviewSectionTypeCode;
	}

	public void setReviewSectionTypeCode(String reviewSectionTypeCode) {
		this.reviewSectionTypeCode = reviewSectionTypeCode;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getPreReviewTypeCode() {
		return preReviewTypeCode;
	}

	public void setPreReviewTypeCode(String preReviewTypeCode) {
		this.preReviewTypeCode = preReviewTypeCode;
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

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public Boolean getIsActive() {
		return isActive;
	}

	public void setIsActive(Boolean isActive) {
		this.isActive = isActive;
	}

}
