package com.polus.fibicomp.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "FUNDING_SCHEME")
public class FundingScheme implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "FUNDING_SCHEME_CODE")
	private String fundingSchemeCode;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "SCHEME_NAME")
	private String schemeName;

	@Column(name = "ACTIVE_FLAG")
	private String activeFlag;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public String getFundingSchemeCode() {
		return fundingSchemeCode;
	}

	public void setFundingSchemeCode(String fundingSchemeCode) {
		this.fundingSchemeCode = fundingSchemeCode;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getSchemeName() {
		return schemeName;
	}

	public void setSchemeName(String schemeName) {
		this.schemeName = schemeName;
	}

	public String getActiveFlag() {
		return activeFlag;
	}

	public void setActiveFlag(String activeFlag) {
		this.activeFlag = activeFlag;
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

}
