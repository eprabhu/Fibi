package com.polus.fibicomp.award.datesandamounts.dto;

public class AwardAmountFNADistributionDTO {

	private Integer awardId;

	private String awardSequenceStatus;

	private String awardNumber;

	private String transactionStatus;

	private Integer sequenceNumber;

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public String getAwardSequenceStatus() {
		return awardSequenceStatus;
	}

	public void setAwardSequenceStatus(String awardSequenceStatus) {
		this.awardSequenceStatus = awardSequenceStatus;
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public String getTransactionStatus() {
		return transactionStatus;
	}

	public void setTransactionStatus(String transactionStatus) {
		this.transactionStatus = transactionStatus;
	}

	public Integer getSequenceNumber() {
		return sequenceNumber;
	}

	public void setSequenceNumber(Integer sequenceNumber) {
		this.sequenceNumber = sequenceNumber;
	}

}
