package com.polus.fibicomp.proposal.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "PROPOSAL_FUNDING_STATUS")
public class ProposalFundingStatus implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "FUNDING_STATUS_CODE", updatable = false, nullable = false)
	private Integer fundingStatusCode;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	public Integer getFundingStatusCode() {
		return fundingStatusCode;
	}

	public void setFundingStatusCode(Integer fundingStatusCode) {
		this.fundingStatusCode = fundingStatusCode;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

}
