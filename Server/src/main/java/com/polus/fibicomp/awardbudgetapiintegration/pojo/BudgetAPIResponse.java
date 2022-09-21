package com.polus.fibicomp.awardbudgetapiintegration.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.Id;
import javax.persistence.IdClass;
import javax.persistence.Table;

import org.apache.commons.lang3.builder.CompareToBuilder;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

@Entity
@Table(name = "BUDGET_API_RESPONSE")
@EntityListeners(AuditingEntityListener.class)
@IdClass(BudgetAPIResponse.BudgetAPIResponseId.class)
public class BudgetAPIResponse implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "YEAR")
	private Integer year;
	
	@Id
	@Column(name = "PROJECT_NUMBER")
	private String projectNumber;
	
	@Id
	@Column(name = "TASK_NUMBER")
	private String taskNumber;
	
	@Id
	@Column(name = "EXPENDITURE_TYPE")
	private String expenditureType;
	
	@Column(name = "FINANCE_PROJECT_ACCOUNT_ID")
	private Integer dofPaid;

	@Column(name = "LEDGER")
	private String ledger;

	@Column(name = "OPERATING_UNIT")
	private String ou;

	@Column(name = "PROJECT_NAME")
	private String projectName;

	@Column(name = "LONG_NAME")
	private String longName;

	@Column(name = "TASK_NAME")
	private String taskName;

	@Column(name = "BUDGET")
	private BigDecimal budget;

	@Column(name = "ACTUAL_AMOUNT")
	private BigDecimal actual;

	@Column(name = "COMMITMENT_AMOUNT")
	private BigDecimal commitment;

	@Column(name = "FUND_AVAILABLE")
	private BigDecimal fundAvailable;

	@Column(name = "ORGANIZATION_ID")
	private String orgId;

	@Column(name = "PROJECT_ID")
	private Integer projectId;

	@Column(name = "SHORT_NAME")
	private String shortName;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getDofPaid() {
		return dofPaid;
	}

	public void setDofPaid(Integer dofPaid) {
		this.dofPaid = dofPaid;
	}

	public Integer getYear() {
		return year;
	}

	public void setYear(Integer year) {
		this.year = year;
	}

	public String getLedger() {
		return ledger;
	}

	public void setLedger(String ledger) {
		this.ledger = ledger;
	}

	public String getOu() {
		return ou;
	}

	public void setOu(String ou) {
		this.ou = ou;
	}

	public String getProjectNumber() {
		return projectNumber;
	}

	public void setProjectNumber(String projectNumber) {
		this.projectNumber = projectNumber;
	}

	public String getProjectName() {
		return projectName;
	}

	public void setProjectName(String projectName) {
		this.projectName = projectName;
	}

	public String getLongName() {
		return longName;
	}

	public void setLongName(String longName) {
		this.longName = longName;
	}

	public String getTaskNumber() {
		return taskNumber;
	}

	public void setTaskNumber(String taskNumber) {
		this.taskNumber = taskNumber;
	}

	public String getTaskName() {
		return taskName;
	}

	public void setTaskName(String taskName) {
		this.taskName = taskName;
	}

	public String getExpenditureType() {
		return expenditureType;
	}

	public void setExpenditureType(String expenditureType) {
		this.expenditureType = expenditureType;
	}

	public BigDecimal getBudget() {
		return budget;
	}

	public void setBudget(BigDecimal budget) {
		this.budget = budget;
	}

	public BigDecimal getActual() {
		return actual;
	}

	public void setActual(BigDecimal actual) {
		this.actual = actual;
	}

	public BigDecimal getCommitment() {
		return commitment;
	}

	public void setCommitment(BigDecimal commitment) {
		this.commitment = commitment;
	}

	public BigDecimal getFundAvailable() {
		return fundAvailable;
	}

	public void setFundAvailable(BigDecimal fundAvailable) {
		this.fundAvailable = fundAvailable;
	}

	public String getOrgId() {
		return orgId;
	}

	public void setOrgId(String orgId) {
		this.orgId = orgId;
	}

	public Integer getProjectId() {
		return projectId;
	}

	public void setProjectId(Integer projectId) {
		this.projectId = projectId;
	}

	public String getShortName() {
		return shortName;
	}

	public void setShortName(String shortName) {
		this.shortName = shortName;
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
	public static final class BudgetAPIResponseId implements Serializable, Comparable<BudgetAPIResponseId> {

		private static final long serialVersionUID = 1L;

		private String projectNumber;

		private Integer year;
		
		private String taskNumber;
		
		private String expenditureType;

		@Override
		public String toString() {
			return new ToStringBuilder(this).append("projectNumber", this.projectNumber).append("taskNumber", this.taskNumber).append("expenditureType", this.expenditureType).append("year", this.year).toString();
		}

		@Override
		public boolean equals(Object other) {
			if (other == null)
				return false;
			if (other == this)
				return true;
			if (other.getClass() != this.getClass())
				return false;
			final BudgetAPIResponseId rhs = (BudgetAPIResponseId) other;
			return new EqualsBuilder().append(this.projectNumber, rhs.projectNumber).append(this.taskNumber, rhs.taskNumber).append(this.expenditureType, rhs.expenditureType).append(this.year, rhs.year).isEquals();
		}

		@Override
		public int hashCode() {
			return new HashCodeBuilder(17, 37).append(this.projectNumber).append(this.taskNumber).append(this.expenditureType).append(this.year).toHashCode();
		}

		@Override
		public int compareTo(BudgetAPIResponseId other) {
			return new CompareToBuilder().append(this.projectNumber, other.projectNumber).append(this.taskNumber, other.taskNumber).append(this.expenditureType, other.expenditureType).append(this.year, other.year).toComparison();
		}

		public String getProjectNumber() {
			return projectNumber;
		}

		public void setProjectNumber(String projectNumber) {
			this.projectNumber = projectNumber;
		}

		public Integer getYear() {
			return year;
		}

		public void setYear(Integer year) {
			this.year = year;
		}

		public String getTaskNumber() {
			return taskNumber;
		}

		public void setTaskNumber(String taskNumber) {
			this.taskNumber = taskNumber;
		}

		public String getExpenditureType() {
			return expenditureType;
		}

		public void setExpenditureType(String expenditureType) {
			this.expenditureType = expenditureType;
		}
	}
}
