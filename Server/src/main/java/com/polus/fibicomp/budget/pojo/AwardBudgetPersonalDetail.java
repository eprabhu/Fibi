package com.polus.fibicomp.budget.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import com.fasterxml.jackson.annotation.JsonBackReference;

@Entity
@Table(name = "AWARD_BUDGET_PERSON_DETAIL")
public class AwardBudgetPersonalDetail implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "BUDGET_PERSON_DETAIL_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer budgetPersonDetailId;

	@JsonBackReference
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_BUDGET_PERSONS_FK1"), name = "BUDGET_DETAILS_ID", referencedColumnName = "BUDGET_DETAILS_ID")
	private AwardBudgetDetail budgetDetail;

	@Column(name = "BUDGET_PERSON_ID")
	private Integer budgetPersonId;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_BUDGET_PERSONS_FK2"), name = "BUDGET_PERSON_ID", referencedColumnName = "BUDGET_PERSON_ID", insertable = false, updatable = false)
	private AwardBudgetPerson budgetPerson;

	@Column(name = "LINE_ITEM_NUMBER")
	private Integer lineItemNumber;

	@Column(name = "IO_CODE")
	private String internalOrderCode;

	@Column(name = "UNDERRECOVERY_AMOUNT", precision = 12, scale = 2)
	private BigDecimal underRecoveryAmount;

	@Column(name = "PERCENT_CHARGED", precision = 5, scale = 2)
	private BigDecimal percentageCharged;

	@Column(name = "PERCENT_EFFORT", precision = 5, scale = 2)
	private BigDecimal percentageEffort;

	@Column(name = "COST_SHARING_AMOUNT", precision = 12, scale = 2)
	private BigDecimal costSharingAmount;

	@Column(name = "COST_SHARING_PERCENT", precision = 5, scale = 2)
	private BigDecimal costSharingPercentage;

	@Column(name = "SALARY_REQUESTED", precision = 12, scale = 2)
	private BigDecimal salaryRequested;

	@Column(name = "TOTAL_SALARY", precision = 12, scale = 2)
	private BigDecimal totalSalary;

	@Column(name = "NO_OF_MONTHS", precision = 3, scale = 2)
	private BigDecimal noOfMonths;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "END_DATE")
	private Timestamp endDate;

	@Column(name = "START_DATE")
	private Timestamp startDate;

	public Integer getBudgetPersonDetailId() {
		return budgetPersonDetailId;
	}

	public void setBudgetPersonDetailId(Integer budgetPersonDetailId) {
		this.budgetPersonDetailId = budgetPersonDetailId;
	}

	public Timestamp getEndDate() {
		return endDate;
	}

	public void setEndDate(Timestamp endDate) {
		this.endDate = endDate;
	}

	public Timestamp getStartDate() {
		return startDate;
	}

	public void setStartDate(Timestamp startDate) {
		this.startDate = startDate;
	}

	public BigDecimal getUnderRecoveryAmount() {
		return underRecoveryAmount;
	}

	public void setUnderRecoveryAmount(BigDecimal underRecoveryAmount) {
		this.underRecoveryAmount = underRecoveryAmount;
	}

	public BigDecimal getPercentageCharged() {
		return percentageCharged;
	}

	public void setPercentageCharged(BigDecimal percentageCharged) {
		this.percentageCharged = percentageCharged;
	}

	public BigDecimal getPercentageEffort() {
		return percentageEffort;
	}

	public void setPercentageEffort(BigDecimal percentageEffort) {
		this.percentageEffort = percentageEffort;
	}

	public BigDecimal getCostSharingAmount() {
		return costSharingAmount;
	}

	public void setCostSharingAmount(BigDecimal costSharingAmount) {
		this.costSharingAmount = costSharingAmount;
	}

	public BigDecimal getCostSharingPercentage() {
		return costSharingPercentage;
	}

	public void setCostSharingPercentage(BigDecimal costSharingPercentage) {
		this.costSharingPercentage = costSharingPercentage;
	}

	public BigDecimal getSalaryRequested() {
		return salaryRequested;
	}

	public void setSalaryRequested(BigDecimal salaryRequested) {
		this.salaryRequested = salaryRequested;
	}

	public BigDecimal getTotalSalary() {
		return totalSalary;
	}

	public void setTotalSalary(BigDecimal totalSalary) {
		this.totalSalary = totalSalary;
	}

	public BigDecimal getNoOfMonths() {
		return noOfMonths;
	}

	public void setNoOfMonths(BigDecimal noOfMonths) {
		this.noOfMonths = noOfMonths;
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

	public Integer getBudgetPersonId() {
		return budgetPersonId;
	}

	public void setBudgetPersonId(Integer budgetPersonId) {
		this.budgetPersonId = budgetPersonId;
	}

	public Integer getLineItemNumber() {
		return lineItemNumber;
	}

	public void setLineItemNumber(Integer lineItemNumber) {
		this.lineItemNumber = lineItemNumber;
	}

	public AwardBudgetPerson getBudgetPerson() {
		return budgetPerson;
	}

	public void setBudgetPerson(AwardBudgetPerson budgetPerson) {
		this.budgetPerson = budgetPerson;
	}

	public AwardBudgetDetail getBudgetDetail() {
		return budgetDetail;
	}

	public void setBudgetDetail(AwardBudgetDetail budgetDetail) {
		this.budgetDetail = budgetDetail;
	}

	public String getInternalOrderCode() {
		return internalOrderCode;
	}

	public void setInternalOrderCode(String internalOrderCode) {
		this.internalOrderCode = internalOrderCode;
	}

}
