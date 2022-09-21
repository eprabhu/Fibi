package com.polus.fibicomp.award.expense.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

import org.apache.commons.lang3.builder.CompareToBuilder;
import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

@Entity
@Table(name = "AWARD_EXPENSE_DETAILS_EXT")
@EntityListeners(AuditingEntityListener.class)
public class AwardExpenseDetailsExt implements Serializable, Comparable<AwardExpenseDetailsExt> {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AWARD_EXPENSE_DETAILS_EXT_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer awardExpenseDetailsId;

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Column(name = "ACCOUNT_NUMBER")
	private String accountNumber;

	@Column(name = "INTERNAL_ORDER_CODE")
	private String internalOrderCode;

	@Column(name = "COMMITTED_AMOUNT", precision = 12, scale = 2)
	private BigDecimal committedAmount;

	@Column(name = "IS_FROM_SAP")
	private String isFromSap;

	@Column(name = "DESCRIPTION")
	private String description;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public AwardExpenseDetailsExt() {

	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public String getAccountNumber() {
		return accountNumber;
	}

	public void setAccountNumber(String accountNumber) {
		this.accountNumber = accountNumber;
	}

	public String getInternalOrderCode() {
		return internalOrderCode;
	}

	public void setInternalOrderCode(String internalOrderCode) {
		this.internalOrderCode = internalOrderCode;
	}

	public BigDecimal getCommittedAmount() {
		return committedAmount;
	}

	public void setCommittedAmount(BigDecimal committedAmount) {
		this.committedAmount = committedAmount;
	}
    
	public String getIsFromSap() {
		return isFromSap;
	}

	public void setIsFromSap(String isFromSap) {
		this.isFromSap = isFromSap;
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

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	@Override
	public int compareTo(AwardExpenseDetailsExt awardExpenseDetailsExt) {
		return new CompareToBuilder().append(this.awardNumber, awardExpenseDetailsExt.awardNumber)
				.append(this.accountNumber, awardExpenseDetailsExt.accountNumber)
				.append(this.internalOrderCode, awardExpenseDetailsExt.internalOrderCode).toComparison();
	}

	public Integer getAwardExpenseDetailsId() {
		return awardExpenseDetailsId;
	}

	public void setAwardExpenseDetailsId(Integer awardExpenseDetailsId) {
		this.awardExpenseDetailsId = awardExpenseDetailsId;
	}

}
