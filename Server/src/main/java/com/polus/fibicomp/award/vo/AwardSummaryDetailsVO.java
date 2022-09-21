package com.polus.fibicomp.award.vo;

import java.sql.Timestamp;

public class AwardSummaryDetailsVO {

	private Integer awardId;

	private String awardSequenceStatus;

	private String awardNumber;

	private Integer sequenceNumber;

	private String awardVariationTypeCode;

	private String createUser;

	private String title;

	private String awardVariationName;

	private String createUserFullName;

	private Timestamp createTimestamp;

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public Integer getSequenceNumber() {
		return sequenceNumber;
	}

	public void setSequenceNumber(Integer sequenceNumber) {
		this.sequenceNumber = sequenceNumber;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getCreateUser() {
		return createUser;
	}

	public void setCreateUser(String createUser) {
		this.createUser = createUser;
	}

	public String getAwardVariationTypeCode() {
		return awardVariationTypeCode;
	}

	public void setAwardVariationTypeCode(String awardVariationTypeCode) {
		this.awardVariationTypeCode = awardVariationTypeCode;
	}

	public String getAwardVariationName() {
		return awardVariationName;
	}

	public void setAwardVariationName(String awardVariationName) {
		this.awardVariationName = awardVariationName;
	}

	public String getAwardSequenceStatus() {
		return awardSequenceStatus;
	}

	public void setAwardSequenceStatus(String awardSequenceStatus) {
		this.awardSequenceStatus = awardSequenceStatus;
	}

	public String getCreateUserFullName() {
		return createUserFullName;
	}

	public void setCreateUserFullName(String createUserFullName) {
		this.createUserFullName = createUserFullName;
	}

	public Timestamp getCreateTimestamp() {
		return createTimestamp;
	}

	public void setCreateTimestamp(Timestamp createTimestamp) {
		this.createTimestamp = createTimestamp;
	}

}
