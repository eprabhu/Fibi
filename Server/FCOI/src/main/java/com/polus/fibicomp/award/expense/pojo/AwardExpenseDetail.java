package com.polus.fibicomp.award.expense.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.IdClass;
import javax.persistence.Table;

import org.apache.commons.lang3.builder.CompareToBuilder;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;

@Entity
@Table(name = "AWARD_EXPENSE_DETAILS")
@IdClass(AwardExpenseDetail.AwardExpenseDetailId.class)

public class AwardExpenseDetail implements Serializable, Comparable<AwardExpenseDetail> {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Id
	@Column(name = "ACCOUNT_NUMBER")
	private String accountNumber;

	@Id
	@Column(name = "INTERNAL_ORDER_CODE")
	private String internalOrderCode;

	@Column(name = "TOTAL_EXPENSE_AMOUNT", precision = 12, scale = 2)
	private BigDecimal totalExpenseAmount;

	@Column(name = "TOTAL_LOGGED_HOURS")
	private BigDecimal totalLoggedHours;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public AwardExpenseDetail() {

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

	public BigDecimal getTotalExpenseAmount() {
		return totalExpenseAmount;
	}

	public void setTotalExpenseAmount(BigDecimal totalExpenseAmount) {
		this.totalExpenseAmount = totalExpenseAmount;
	}

	public BigDecimal getTotalLoggedHours() {
		return totalLoggedHours;
	}

	public void setTotalLoggedHours(BigDecimal totalLoggedHours) {
		this.totalLoggedHours = totalLoggedHours;
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
	public int compareTo(AwardExpenseDetail awardExpenseDetail) {
		return new CompareToBuilder().append(this.awardNumber, awardExpenseDetail.awardNumber)
				.append(this.accountNumber, awardExpenseDetail.accountNumber)
				.append(this.internalOrderCode, awardExpenseDetail.internalOrderCode).toComparison();
	}

	public static final class AwardExpenseDetailId implements Serializable, Comparable<AwardExpenseDetailId> {

		private static final long serialVersionUID = 1L;
		private String awardNumber;
		private String accountNumber;
		private String internalOrderCode;

		public AwardExpenseDetailId() {
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

		public static long getSerialversionuid() {
			return serialVersionUID;
		}

		@Override
		public String toString() {
			return new ToStringBuilder(this).append("awardNumber", this.awardNumber)
					.append("accountNumber", this.accountNumber).append(internalOrderCode, this.internalOrderCode)
					.toString();
		}

		@Override
		public boolean equals(Object other) {
			if (other == null)
				return false;
			if (other == this)
				return true;
			if (other.getClass() != this.getClass())
				return false;
			final AwardExpenseDetailId aed = (AwardExpenseDetailId) other;
			return new EqualsBuilder().append(this.awardNumber, aed.awardNumber)
					.append(this.accountNumber, aed.accountNumber).append(this.internalOrderCode, aed.internalOrderCode)
					.isEquals();
		}

		@Override
		public int hashCode() {
			return new HashCodeBuilder(17, 37).append(this.awardNumber).append(this.accountNumber)
					.append(this.accountNumber).append(this.internalOrderCode).toHashCode();
		}

		@Override
		public int compareTo(AwardExpenseDetailId other) {
			return new CompareToBuilder().append(this.awardNumber, other.awardNumber)
					.append(this.accountNumber, other.accountNumber)
					.append(this.internalOrderCode, other.internalOrderCode).toComparison();
		}

	}

}
