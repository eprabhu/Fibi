package com.polus.fibicomp.print.dto;

public class AwardDateAndAmountPrintParameter {

	private String transactionType;

	private String proposalNumber;

	private String sourceAwardNumber;

	private String destinationAwardNumber;

	private String noticeDate;

	private String obligationStartDate;

	private String obligationChange;

	private String anticipatedChange;

	private String comment;

	private String obligationEndDate;

	public AwardDateAndAmountPrintParameter(String transactionType, String proposalNumber,
			String sourceAwardNumber, String destinationAwardNumber, String noticeDate, String obligationStartDate,
			String obligationEndDate, String obligationChange, String anticipatedChange, String comment) {
		this.transactionType = transactionType;
		this.proposalNumber = proposalNumber;
		this.sourceAwardNumber = sourceAwardNumber;
		this.destinationAwardNumber = destinationAwardNumber;
		this.noticeDate = noticeDate;
		this.obligationStartDate = obligationStartDate;
		this.obligationEndDate = obligationEndDate;
		this.obligationChange = obligationChange;
		this.anticipatedChange = anticipatedChange;
		this.comment = comment;
	}

	public String getTransactionType() {
		return transactionType;
	}
	public void setTransactionType(String transactionType) {
		this.transactionType = transactionType;
	}
	public String getProposalNumber() {
		return proposalNumber;
	}
	public void setProposalNumber(String proposalNumber) {
		this.proposalNumber = proposalNumber;
	}
	public String getSourceAwardNumber() {
		return sourceAwardNumber;
	}
	public void setSourceAwardNumber(String sourceAwardNumber) {
		this.sourceAwardNumber = sourceAwardNumber;
	}
	public String getDestinationAwardNumber() {
		return destinationAwardNumber;
	}
	public void setDestinationAwardNumber(String destinationAwardNumber) {
		this.destinationAwardNumber = destinationAwardNumber;
	}
	public String getNoticeDate() {
		return noticeDate;
	}
	public void setNoticeDate(String noticeDate) {
		this.noticeDate = noticeDate;
	}
	public String getObligationStartDate() {
		return obligationStartDate;
	}
	public void setObligationStartDate(String obligationStartDate) {
		this.obligationStartDate = obligationStartDate;
	}
	public String getObligationChange() {
		return obligationChange;
	}
	public void setObligationChange(String obligationChange) {
		this.obligationChange = obligationChange;
	}
	public String getAnticipatedChange() {
		return anticipatedChange;
	}
	public void setAnticipatedChange(String anticipatedChange) {
		this.anticipatedChange = anticipatedChange;
	}
	public String getComment() {
		return comment;
	}
	public void setComment(String comment) {
		this.comment = comment;
	}
	public String getObligationEndDate() {
		return obligationEndDate;
	}
	public void setObligationEndDate(String obligationEndDate) {
		this.obligationEndDate = obligationEndDate;
	}

}
