package com.polus.fibicomp.award.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "COST_SHARE_TYPE")
public class CostShareType implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "COST_SHARE_TYPE_CODE")
	private Integer costShareTypeCode;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "COMMENTS")
	private String comments;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;
	
	@Column(name = "CAN_INCLUDE_IN_BUDGET")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean canIncludeInBudget= false;

	@Column(name = "IS_ACTIVE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isActive;

	public Integer getCostShareTypeCode() {
		return costShareTypeCode;
	}

	public void setCostShareTypeCode(Integer costShareTypeCode) {
		this.costShareTypeCode = costShareTypeCode;
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

	public String getComments() {
		return comments;
	}

	public void setComments(String comments) {
		this.comments = comments;
	}

	public Boolean getCanIncludeInBudget() {
		return canIncludeInBudget;
	}

	public void setCanIncludeInBudget(Boolean canIncludeInBudget) {
		this.canIncludeInBudget = canIncludeInBudget;
	}

	public Boolean getIsActive() {
		return isActive;
	}

	public void setIsActive(Boolean isActive) {
		this.isActive = isActive;
	}

}
