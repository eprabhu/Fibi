package com.polus.fibicomp.award.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;
import javax.persistence.Transient;
import javax.persistence.UniqueConstraint;

import com.polus.fibicomp.award.datesandamounts.pojo.AwardTransactionStatus;

@Entity
@Table(name = "AWARD_AMOUNT_TRANSACTION", 
uniqueConstraints = @UniqueConstraint(name = "AWARD_AMOUNT_TRANSACTION_UK", columnNames = "TRANSACTION_ID"))
public class AwardAmountTransaction implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AWARD_AMOUNT_TRANSACTION_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "AWARD_AMOUNT_TRANSACTION_ID_GENERATOR")
	@SequenceGenerator(name="AWARD_AMOUNT_TRANSACTION_ID_GENERATOR", sequenceName = "AWARD_AMOUNT_TRANSACTION_ID_GENERATOR", allocationSize=1)
	private Integer awardAmountTransactionId;

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	/*@GenericGenerator(name = "transactionIdGenerator", strategy = "increment", parameters = {
			@Parameter(name = "initial_value", value = "1000"), @Parameter(name = "increment_size", value = "1") })
	@GeneratedValue(generator = "transactionIdGenerator")*/
	@Column(name = "TRANSACTION_ID", nullable = false, unique = true)
	private BigDecimal transactionId;

	@Column(name = "TRANSACTION_TYPE_CODE")
	private Integer transactionTypeCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_AMOUNT_TRANSACTION_FK1"), name = "TRANSACTION_TYPE_CODE", referencedColumnName = "AWARD_TRANSACTION_TYPE_CODE", insertable = false, updatable = false)
	private AwardTransactionType awardTransactionType;

	@Column(name = "FUNDED_PROPOSAL_ID")
	private Integer fundedProposalId;

	@Column(name = "NOTICE_DATE")
	private Timestamp noticeDate;

	@Column(name = "COMMENTS")
	private String comments;

	@Column(name = "SOURCE_AWARD_NUMBER")
	private String sourceAwardNumber;

	@Column(name = "DESTINATION_AWARD_NUMBER")
	private String destinationAwardNumber;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "TRANSACTION_STATUS_CODE")
	private String transactionStatusCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_AMOUNT_TRANSACTION_FK2"), name = "TRANSACTION_STATUS_CODE", referencedColumnName = "TRANSACTION_STATUS_CODE", insertable = false, updatable = false)
	private AwardTransactionStatus awardTransactionStatus;

	@Transient
	private String fundingProposalNumber;

	public Integer getAwardAmountTransactionId() {
		return awardAmountTransactionId;
	}

	public void setAwardAmountTransactionId(Integer awardAmountTransactionId) {
		this.awardAmountTransactionId = awardAmountTransactionId;
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public Integer getTransactionTypeCode() {
		return transactionTypeCode;
	}

	public void setTransactionTypeCode(Integer transactionTypeCode) {
		this.transactionTypeCode = transactionTypeCode;
	}

	public Integer getFundedProposalId() {
		return fundedProposalId;
	}

	public void setFundedProposalId(Integer fundedProposalId) {
		this.fundedProposalId = fundedProposalId;
	}

	public Timestamp getNoticeDate() {
		return noticeDate;
	}

	public void setNoticeDate(Timestamp noticeDate) {
		this.noticeDate = noticeDate;
	}

	public String getComments() {
		return comments;
	}

	public void setComments(String comments) {
		this.comments = comments;
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

	public AwardTransactionType getAwardTransactionType() {
		return awardTransactionType;
	}

	public void setAwardTransactionType(AwardTransactionType awardTransactionType) {
		this.awardTransactionType = awardTransactionType;
	}

	public String getFundingProposalNumber() {
		return fundingProposalNumber;
	}

	public void setFundingProposalNumber(String fundingProposalNumber) {
		this.fundingProposalNumber = fundingProposalNumber;
	}

	public BigDecimal getTransactionId() {
		return transactionId;
	}

	public void setTransactionId(BigDecimal transactionId) {
		this.transactionId = transactionId;
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

	public String getTransactionStatusCode() {
		return transactionStatusCode;
	}

	public void setTransactionStatusCode(String transactionStatusCode) {
		this.transactionStatusCode = transactionStatusCode;
	}

	public AwardTransactionStatus getAwardTransactionStatus() {
		return awardTransactionStatus;
	}

	public void setAwardTransactionStatus(AwardTransactionStatus awardTransactionStatus) {
		this.awardTransactionStatus = awardTransactionStatus;
	}

}
