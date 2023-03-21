package com.polus.fibicomp.ip.dto;

import java.sql.Timestamp;

public class InstituteProposalHistoryDto {


	private Integer proposalId;

	private String proposalNumber;

	private Integer sequenceNumber;

	private String proposalSequenceStatus;

	private String requestType;

	private String createUser;

	private Timestamp createTimestamp;

	private String createUserFullName;

	public Integer getProposalId() {
		return proposalId;
	}

	public void setProposalId(Integer proposalId) {
		this.proposalId = proposalId;
	}

	public String getProposalNumber() {
		return proposalNumber;
	}

	public void setProposalNumber(String proposalNumber) {
		this.proposalNumber = proposalNumber;
	}

	public Integer getSequenceNumber() {
		return sequenceNumber;
	}

	public void setSequenceNumber(Integer sequenceNumber) {
		this.sequenceNumber = sequenceNumber;
	}

	public String getProposalSequenceStatus() {
		return proposalSequenceStatus;
	}

	public void setProposalSequenceStatus(String proposalSequenceStatus) {
		this.proposalSequenceStatus = proposalSequenceStatus;
	}

	public String getRequestType() {
		return requestType;
	}

	public void setRequestType(String requestType) {
		this.requestType = requestType;
	}

	public String getCreateUser() {
		return createUser;
	}

	public void setCreateUser(String createUser) {
		this.createUser = createUser;
	}

	public Timestamp getCreateTimestamp() {
		return createTimestamp;
	}

	public void setCreateTimestamp(Timestamp createTimestamp) {
		this.createTimestamp = createTimestamp;
	}

	public String getCreateUserFullName() {
		return createUserFullName;
	}

	public void setCreateUserFullName(String createUserFullName) {
		this.createUserFullName = createUserFullName;
	}

}
