package com.polus.fibicomp.budget.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "AWARD_BUDGET_FUND_TYPE")
public class AwardBudgetFundType implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "FUND_TYPE_CODE")
	private String fundTypeCode;

	@Column(name = "FUND_TYPE")
	private String fundType;

	@Column(name = "DESCRIPTION")
	private String decription;

	@Column(name = "IS_DEFAULT")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isDefault;

	@Column(name = "IS_COST_SHARE_ENABLE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isCostShareEnable;

	@Column(name = "REFERENCE_COLUMN")
	private String referanceColumn;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	public String getFundTypeCode() {
		return fundTypeCode;
	}

	public void setFundTypeCode(String fundTypeCode) {
		this.fundTypeCode = fundTypeCode;
	}

	public String getFundType() {
		return fundType;
	}

	public void setFundType(String fundType) {
		this.fundType = fundType;
	}

	public String getDecription() {
		return decription;
	}

	public void setDecription(String decription) {
		this.decription = decription;
	}

	public Boolean getIsDefault() {
		return isDefault;
	}

	public void setIsDefault(Boolean isDefault) {
		this.isDefault = isDefault;
	}

	public Boolean getIsCostShareEnable() {
		return isCostShareEnable;
	}

	public void setIsCostShareEnable(Boolean isCostShareEnable) {
		this.isCostShareEnable = isCostShareEnable;
	}

	public String getReferanceColumn() {
		return referanceColumn;
	}

	public void setReferanceColumn(String referanceColumn) {
		this.referanceColumn = referanceColumn;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

}
