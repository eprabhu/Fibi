package com.polus.fibicomp.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "SPECIAL_REVIEW")
public class SpecialReviewType implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "SPECIAL_REVIEW_CODE")
	private String specialReviewTypeCode;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "SORT_ID")
	private Integer sortId;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "IS_ACTIVE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isActive;

	@Column(name = "IS_INTEGRATED")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isIntegrated;

	public String getSpecialReviewTypeCode() {
		return specialReviewTypeCode;
	}

	public void setSpecialReviewTypeCode(String specialReviewTypeCode) {
		this.specialReviewTypeCode = specialReviewTypeCode;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public Integer getSortId() {
		return sortId;
	}

	public void setSortId(Integer sortId) {
		this.sortId = sortId;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
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

	public Boolean getIsActive() {
		return isActive;
	}

	public void setIsActive(Boolean isActive) {
		this.isActive = isActive;
	}

	public Boolean getIsIntegrated() {
		return isIntegrated;
	}

	public void setIsIntegrated(Boolean isIntegrated) {
		this.isIntegrated = isIntegrated;
	}
}
