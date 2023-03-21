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
@Table(name = "AWARD_TRANSACTION_TYPE")
public class AwardTransactionType implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AWARD_TRANSACTION_TYPE_CODE")
	private Integer awardTransactionTypeCode;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "IS_ACTIVE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isActive;

	public Integer getAwardTransactionTypeCode() {
		return awardTransactionTypeCode;
	}

	public void setAwardTransactionTypeCode(Integer awardTransactionTypeCode) {
		this.awardTransactionTypeCode = awardTransactionTypeCode;
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

	public Boolean getIsActive() {
		return isActive;
	}

	public void setIsActive(Boolean isActive) {
		this.isActive = isActive;
	}

//	public String getShowInActionSummary() {
//		return showInActionSummary;
//	}
//
//	public void setShowInActionSummary(String showInActionSummary) {
//		this.showInActionSummary = showInActionSummary;
//	}

}
