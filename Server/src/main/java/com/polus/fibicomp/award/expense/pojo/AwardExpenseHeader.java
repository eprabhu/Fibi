package com.polus.fibicomp.award.expense.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.Collections;
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

import com.polus.fibicomp.award.expense.comparator.AwardExpenseDetailsComparatorBySortOrder;
import com.polus.fibicomp.award.expense.vo.AwardExpenseDetailVO;

@Entity
@Table(name = "AWARD_EXPENSE_HEADER")
@IdClass(AwardExpenseHeader.RateAwardExpenseHeaderId.class)
public class AwardExpenseHeader implements Serializable, Comparable<AwardExpenseHeader> {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Id
	@Column(name = "ACCOUNT_NUMBER")
	private String accountNumber;

	@Column(name = "CREATE_TIMESTAMP")
	private Timestamp createTimestamp;

	@Column(name = "CREATE_USER")
	private String createUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "LAST_SYNCH_TIMESTAMP")
	private Timestamp lastSyncTimestamp;

	@Transient
	private List<AwardExpenseDetailVO> awardExpenseDetailVOs;

	@Transient
	private Boolean manpowerEnabled;
	
	@Transient
	private Boolean budgetAssociatedWithManpower;

	public AwardExpenseHeader() {}

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

	public Timestamp getCreateTimestamp() {
		return createTimestamp;
	}

	public void setCreateTimestamp(Timestamp createTimestamp) {
		this.createTimestamp = createTimestamp;
	}

	public String getCreateUser() {
		return createUser;
	}

	public void setCreateUser(String createUser) {
		this.createUser = createUser;
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

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	@Override
	public int compareTo(AwardExpenseHeader awardExpenseHeader) {
		return new CompareToBuilder().append(this.awardNumber, awardExpenseHeader.awardNumber)
				.append(this.accountNumber, awardExpenseHeader.accountNumber).toComparison();
	}

	public static final class RateAwardExpenseHeaderId implements Serializable, Comparable<RateAwardExpenseHeaderId> {

		private static final long serialVersionUID = 1L;
		private String awardNumber;
		private String accountNumber;

		public RateAwardExpenseHeaderId() {

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

		public static long getSerialversionuid() {
			return serialVersionUID;
		}

		@Override
		public String toString() {
			return new ToStringBuilder(this).append("awardNumber", this.awardNumber)
					.append("accountNumber", this.accountNumber).toString();
		}

		@Override
		public boolean equals(Object other) {
			if (other == null)
				return false;
			if (other == this)
				return true;
			if (other.getClass() != this.getClass())
				return false;
			final RateAwardExpenseHeaderId aeh = (RateAwardExpenseHeaderId) other;
			return new EqualsBuilder().append(this.awardNumber, aeh.awardNumber)
					.append(this.accountNumber, aeh.accountNumber).isEquals();
		}

		@Override
		public int hashCode() {
			return new HashCodeBuilder(17, 37).append(this.awardNumber).append(this.accountNumber).toHashCode();
		}

		@Override
		public int compareTo(RateAwardExpenseHeaderId other) {
			return new CompareToBuilder().append(this.awardNumber, other.awardNumber)
					.append(this.accountNumber, other.accountNumber).toComparison();
		}

	}

	public Timestamp getLastSyncTimestamp() {
		return lastSyncTimestamp;
	}

	public void setLastSyncTimestamp(Timestamp lastSyncTimestamp) {
		this.lastSyncTimestamp = lastSyncTimestamp;
	}

	public List<AwardExpenseDetailVO> getAwardExpenseDetailVOs() {
		if (awardExpenseDetailVOs != null && !awardExpenseDetailVOs.isEmpty()) {
			Collections.sort(awardExpenseDetailVOs, new AwardExpenseDetailsComparatorBySortOrder());
		}
		return awardExpenseDetailVOs;
	}

	public void setAwardExpenseDetailVOs(List<AwardExpenseDetailVO> awardExpenseDetailVOs) {
		this.awardExpenseDetailVOs = awardExpenseDetailVOs;
	}

	public Boolean getManpowerEnabled() {
		return manpowerEnabled;
	}

	public void setManpowerEnabled(Boolean manpowerEnabled) {
		this.manpowerEnabled = manpowerEnabled;
	}

	public Boolean getBudgetAssociatedWithManpower() {
		return budgetAssociatedWithManpower;
	}

	public void setBudgetAssociatedWithManpower(Boolean budgetAssociatedWithManpower) {
		this.budgetAssociatedWithManpower = budgetAssociatedWithManpower;
	}

}
