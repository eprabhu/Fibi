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
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

import org.hibernate.annotations.GenericGenerator;
import org.hibernate.annotations.Parameter;

import com.fasterxml.jackson.annotation.JsonBackReference;

@Entity
@Table(name = "BUDGET_PERSON_DETAIL")
public class BudgetPersonalDetails implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "BUDGET_PERSON_DETAIL_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "BUDGET_PERSON_DETAIL_ID_GENERATOR")
	@SequenceGenerator(name="BUDGET_PERSON_DETAIL_ID_GENERATOR", sequenceName = "BUDGET_PERSON_DETAIL_ID_GENERATOR", allocationSize=1)
	private Integer budgetPersonDetailId;	
		
	@JsonBackReference
    @ManyToOne(optional = false)
    @JoinColumn(foreignKey = @ForeignKey(name = "BUDGET_PERSONS_FK1"), name = "BUDGET_DETAILS_ID", referencedColumnName = "BUDGET_DETAILS_ID")
    private BudgetDetail budgetDetail;

	@Column(name = "PERSON_NAME")
	private String personName;
	
	@Column(name = "PERSON_TYPE") // E - Employee, N - Non Employee, T - To Be Named
	private String personType;
	
	@Column(name = "PERSON_ID")
	private String personId;

	@Column(name = "ROLODEX_ID")
	private Integer rolodexId;
	
	@Column(name = "TBN_ID")
	private String tbnId;
			
	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "BUDGET_PERSONS_FK2"), name = "TBN_ID", referencedColumnName = "TBN_ID", insertable = false, updatable = false)
	private TbnPerson tbnPerson;
	
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

	@Column(name = "BUDGET_PERSON_ID")
	private Integer budgetPersonId;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "BUDGET_PERSONS_FK3"), name = "BUDGET_PERSON_ID", referencedColumnName = "BUDGET_PERSON_ID", insertable = false, updatable = false)
	private BudgetPerson budgetPerson;
	
	@Column(name = "SPONSOR_REQUESTED_AMOUNT", precision = 12, scale = 2)
	private BigDecimal sponsorRequestedAmount;
	
	@Column(name = "SALARY", precision = 12, scale = 2)
	private BigDecimal salary;

	public Integer getBudgetPersonDetailId() {
		return budgetPersonDetailId;
	}

	public void setBudgetPersonDetailId(Integer budgetPersonDetailId) {
		this.budgetPersonDetailId = budgetPersonDetailId;
	}

	public String getPersonName() {
		return personName;
	}

	public void setPersonName(String personName) {
		this.personName = personName;
	}

	public String getPersonType() {
		return personType;
	}

	public void setPersonType(String personType) {
		this.personType = personType;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public Integer getRolodexId() {
		return rolodexId;
	}

	public void setRolodexId(Integer rolodexId) {
		this.rolodexId = rolodexId;
	}

	public String getTbnId() {
		return tbnId;
	}

	public void setTbnId(String tbnId) {
		this.tbnId = tbnId;
	}

	public TbnPerson getTbnPerson() {
		return tbnPerson;
	}

	public void setTbnPerson(TbnPerson tbnPerson) {
		this.tbnPerson = tbnPerson;
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

	public BudgetDetail getBudgetDetail() {
		return budgetDetail;
	}

	public void setBudgetDetail(BudgetDetail budgetDetail) {
		this.budgetDetail = budgetDetail;
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

	public Integer getBudgetPersonId() {
		return budgetPersonId;
	}

	public void setBudgetPersonId(Integer budgetPersonId) {
		this.budgetPersonId = budgetPersonId;
	}

	public BudgetPerson getBudgetPerson() {
		return budgetPerson;
	}

	public void setBudgetPerson(BudgetPerson budgetPerson) {
		this.budgetPerson = budgetPerson;
	}

	public BigDecimal getSponsorRequestedAmount() {
		return sponsorRequestedAmount;
	}

	public void setSponsorRequestedAmount(BigDecimal sponsorRequestedAmount) {
		this.sponsorRequestedAmount = sponsorRequestedAmount;
	}

	public BigDecimal getSalary() {
		return salary;
	}

	public void setSalary(BigDecimal salary) {
		this.salary = salary;
	}
	
}

