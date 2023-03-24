package com.polus.fibicomp.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Transient;

import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "ACTIVITY_TYPE")
public class ActivityType implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "ACTIVITY_TYPE_CODE")
	private String activityTypeCode;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "HIGHER_EDUCATION_FUNCTION_CODE")
	private String higherEducationFunctionCode;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "IS_ACTIVE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isActive;

	@Transient
	private Integer grantTypeCode;

	@Transient
	private Integer categoryCode;

	public ActivityType() {
		super();
	}

	public ActivityType(String code, String description) {
		super();
		this.activityTypeCode = code;
		this.description = description;
	}

	public String getActivityTypeCode() {
		return activityTypeCode;
	}

	public void setActivityTypeCode(String activityTypeCode) {
		this.activityTypeCode = activityTypeCode;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getHigherEducationFunctionCode() {
		return higherEducationFunctionCode;
	}

	public void setHigherEducationFunctionCode(String higherEducationFunctionCode) {
		this.higherEducationFunctionCode = higherEducationFunctionCode;
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

	public Integer getGrantTypeCode() {
		return grantTypeCode;
	}

	public void setGrantTypeCode(Integer grantTypeCode) {
		this.grantTypeCode = grantTypeCode;
	}

	public Integer getCategoryCode() {
		return categoryCode;
	}

	public void setCategoryCode(Integer categoryCode) {
		this.categoryCode = categoryCode;
	}

	public Boolean getIsActive() {
		return isActive;
	}

	public void setIsActive(Boolean isActive) {
		this.isActive = isActive;
	}

}