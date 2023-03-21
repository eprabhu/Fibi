package com.polus.fibicomp.award.datesandamounts.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

@Entity
@Table(name = "AWARD_AMT_TRNSCTN_HISTORY")
public class AwardAmountTransactionHistory implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name ="TRNSCTN_HISTORY_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_AWD_AMT_TRNSCTN_HSTRY_ID_GNRTR")
	@SequenceGenerator(name="SEQ_AWD_AMT_TRNSCTN_HSTRY_ID_GNRTR", sequenceName = "SEQ_AWD_AMT_TRNSCTN_HSTRY_ID_GNRTR", allocationSize=1)
	private Integer transactionHistoryId;

	@Column(name = "AWARD_ID") 
	private Integer awardId;

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Column(name = "SEQUENCE_NUMBER")
	private Integer sequenceNumber;

	@Column(name = "TRANSACTION_ID")
	private BigDecimal transactionId;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getTransactionHistoryId() {
		return transactionHistoryId;
	}

	public void setTransactionHistoryId(Integer transactionHistoryId) {
		this.transactionHistoryId = transactionHistoryId;
	}

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

	public BigDecimal getTransactionId() {
		return transactionId;
	}

	public void setTransactionId(BigDecimal transactionId) {
		this.transactionId = transactionId;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

}
