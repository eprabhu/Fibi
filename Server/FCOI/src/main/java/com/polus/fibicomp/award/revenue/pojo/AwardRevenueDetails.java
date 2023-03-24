package com.polus.fibicomp.award.revenue.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.List;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.IdClass;
import javax.persistence.Table;
import javax.persistence.Transient;

import org.apache.commons.lang3.builder.CompareToBuilder;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;

import com.polus.fibicomp.award.revenue.vo.AwardRevenueVO;

@Entity
@Table(name = "AWARD_REVENUE_DETAILS")
@IdClass(AwardRevenueDetails.AwardRevenueDetailId.class)
public class AwardRevenueDetails implements Serializable, Comparable<AwardRevenueDetails>{

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

	@Column(name = "TOTAL_REVENUE_AMOUNT", precision = 12, scale = 2)
	private BigDecimal totalRevenueAmount;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Transient
	private BigDecimal totalAmount;

	@Transient
	private List<AwardRevenueVO> awardRevenueVOs;

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

	public BigDecimal getTotalRevenueAmount() {
		return totalRevenueAmount;
	}

	public void setTotalRevenueAmount(BigDecimal totalRevenueAmount) {
		this.totalRevenueAmount = totalRevenueAmount;
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
	public int compareTo(AwardRevenueDetails awardRevenueDetail) {
		return new CompareToBuilder().append(this.awardNumber, awardRevenueDetail.awardNumber)
				.append(this.accountNumber, awardRevenueDetail.accountNumber)
				.append(this.internalOrderCode, awardRevenueDetail.internalOrderCode).toComparison();
	}

	public static final class AwardRevenueDetailId implements Serializable, Comparable<AwardRevenueDetailId> {

		private static final long serialVersionUID = 1L;
		private String awardNumber;
		private String accountNumber;
		private String internalOrderCode;

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
			final AwardRevenueDetailId ard = (AwardRevenueDetailId) other;
			return new EqualsBuilder().append(this.awardNumber, ard.awardNumber)
					.append(this.accountNumber, ard.accountNumber).append(this.internalOrderCode, ard.internalOrderCode)
					.isEquals();
		}

		@Override
		public int hashCode() {
			return new HashCodeBuilder(17, 37).append(this.awardNumber).append(this.accountNumber)
					.append(this.accountNumber).append(this.internalOrderCode).toHashCode();
		}

		@Override
		public int compareTo(AwardRevenueDetailId other) {
			return new CompareToBuilder().append(this.awardNumber, other.awardNumber)
					.append(this.accountNumber, other.accountNumber)
					.append(this.internalOrderCode, other.internalOrderCode).toComparison();
		}
	}

	public List<AwardRevenueVO> getAwardRevenueVOs() {
		return awardRevenueVOs;
	}

	public void setAwardRevenueVOs(List<AwardRevenueVO> awardRevenueVOs) {
		this.awardRevenueVOs = awardRevenueVOs;
	}

	public BigDecimal getTotalAmount() {
		return totalAmount;
	}

	public void setTotalAmount(BigDecimal totalAmount) {
		this.totalAmount = totalAmount;
	}

}
