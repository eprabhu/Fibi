package com.polus.fibicomp.budget.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.math.RoundingMode;
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
import javax.persistence.Transient;

@Entity
@Table(name = "AWARD_BUDGET_PERSONS")
public class AwardBudgetPerson implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "BUDGET_PERSON_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer budgetPersonId;

	@Column(name = "APPOINTMENT_TYPE_CODE")
	private String appointmentTypeCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_BUDGET_PERS_FK2"), name = "APPOINTMENT_TYPE_CODE", referencedColumnName = "APPOINTMENT_TYPE_CODE", insertable = false, updatable = false)
	private AppointmentType appointmentType;

	@Column(name = "BUDGET_HEADER_ID")
	private Integer budgetHeaderId;

	@Column(name = "TBN_ID")
	private String tbnId;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_BUDGET_PERS_FK3"), name = "TBN_ID", referencedColumnName = "TBN_ID", insertable = false, updatable = false)
	private TbnPerson tbnPerson;

	@Column(name = "PERSON_ID")
	private String personId;

	@Column(name = "ROLODEX_ID")
	private Integer rolodexId;

	@Column(name = "JOB_CODE")
	private String jobCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_BUDGET_PERS_FK1"), name = "JOB_CODE", referencedColumnName = "JOB_CODE", insertable = false, updatable = false)
	private JobCode jobCodes;

	@Column(name = "EFFECTIVE_DATE")
	private Timestamp effectiveDate;

	@Column(name = "CALCULATION_BASE", precision = 12, scale = 2)
	private BigDecimal calculationBase;

	@Column(name = "PERSON_NAME")
	private String personName;

	@Column(name = "PERSON_TYPE") // E - Emplo-yee, N - Non Employee, T - To Be Named
	private String personType;

	@Column(name = "SALARY_ANNIVERSARY_DATE")
	private Timestamp salaryAnniversaryDate;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Transient
	private BigDecimal durationCost = BigDecimal.ZERO;

	@Transient
	private String budgetPersonName;

	public Integer getBudgetPersonId() {
		return budgetPersonId;
	}

	public void setBudgetPersonId(Integer budgetPersonId) {
		this.budgetPersonId = budgetPersonId;
	}

	public String getAppointmentTypeCode() {
		return appointmentTypeCode;
	}

	public void setAppointmentTypeCode(String appointmentTypeCode) {
		this.appointmentTypeCode = appointmentTypeCode;
	}

	public AppointmentType getAppointmentType() {
		return appointmentType;
	}

	public void setAppointmentType(AppointmentType appointmentType) {
		this.appointmentType = appointmentType;
	}

	public Integer getBudgetHeaderId() {
		return budgetHeaderId;
	}

	public void setBudgetHeaderId(Integer budgetHeaderId) {
		this.budgetHeaderId = budgetHeaderId;
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

	public String getJobCode() {
		return jobCode;
	}

	public void setJobCode(String jobCode) {
		this.jobCode = jobCode;
	}

	public JobCode getJobCodes() {
		return jobCodes;
	}

	public void setJobCodes(JobCode jobCodes) {
		this.jobCodes = jobCodes;
	}

	public Timestamp getEffectiveDate() {
		return effectiveDate;
	}

	public void setEffectiveDate(Timestamp effectiveDate) {
		this.effectiveDate = effectiveDate;
	}

	public String getPersonName() {
		return personName;
	}

	public void setPersonName(String personName) {
		this.personName = personName;
	}

	public Timestamp getSalaryAnniversaryDate() {
		return salaryAnniversaryDate;
	}

	public void setSalaryAnniversaryDate(Timestamp salaryAnniversaryDate) {
		this.salaryAnniversaryDate = salaryAnniversaryDate;
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

	public String getPersonType() {
		return personType;
	}

	public void setPersonType(String personType) {
		this.personType = personType;
	}

	public BigDecimal getDurationCost() {
		if (calculationBase != null && calculationBase.compareTo(BigDecimal.ZERO) > 0) {
			durationCost = calculationBase.divide(appointmentType.getDuration(), 4, RoundingMode.HALF_UP);
		}
		return durationCost;
	}

	public BigDecimal getCalculationBase() {
		return calculationBase;
	}

	public void setCalculationBase(BigDecimal calculationBase) {
		this.calculationBase = calculationBase;
	}

	public void setDurationCost(BigDecimal durationCost) {
		this.durationCost = durationCost;
	}

	public String getBudgetPersonName() {
		if (tbnPerson != null) {
			this.budgetPersonName = tbnPerson.getPersonName();
		} else {
			this.budgetPersonName = this.personName;
		}
		return budgetPersonName;
	}

	public void setBudgetPersonName(String budgetPersonName) {
		this.budgetPersonName = budgetPersonName;
	}

}
