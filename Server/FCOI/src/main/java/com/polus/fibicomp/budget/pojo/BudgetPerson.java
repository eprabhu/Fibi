package com.polus.fibicomp.budget.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.sql.Timestamp;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Convert;
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

import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "BUDGET_PERSONS")
public class BudgetPerson implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "BUDGET_PERSON_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "BUDGET_PERSON_ID_GENERATOR")
	@SequenceGenerator(name="BUDGET_PERSON_ID_GENERATOR", sequenceName = "BUDGET_PERSON_ID_GENERATOR", allocationSize=1)
	private Integer budgetPersonId;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "BUDGET_PERS_FK2"), name = "APPOINTMENT_TYPE_CODE", referencedColumnName = "APPOINTMENT_TYPE_CODE", insertable = false, updatable = false)
	private AppointmentType appointmentType;

	@Column(name = "APPOINTMENT_TYPE_CODE")
	private String appointmentTypeCode;

	@Column(name = "BUDGET_HEADER_ID")
	private Integer budgetId;

	@Column(name = "TBN_ID")
	private String tbnId;

	@Column(name = "PERSON_ID")
	private String personId;

	@Column(name = "PERSON_NAME")
	private String personName;

	@Column(name = "ROLODEX_ID")
	private Integer rolodexId;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "BUDGET_PERS_FK1"), name = "JOB_CODE", referencedColumnName = "JOB_CODE", insertable = false, updatable = false)
	private JobCode jobCode;

	@Column(name = "JOB_CODE")
	private String jobCodeType;

	@Column(name = "EFFECTIVE_DATE")
	private Timestamp effectiveDate;

	@Column(name = "CALCULATION_BASE", precision = 12, scale = 2)
	private BigDecimal calculationBase;

	@Column(name = "NON_EMPLOYEE_FLAG")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean nonEmployeeFlag = false;

	@Column(name = "SALARY_ANNIVERSARY_DATE")
	private Timestamp salaryAnniversaryDate;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "BUDGET_PERS_FK3"), name = "TBN_ID", referencedColumnName = "TBN_ID", insertable = false, updatable = false)
	private TbnPerson tbnPerson;

	@Column(name = "PERSON_TYPE") // E - Emplo-yee, N - Non Employee, T - To Be Named
	private String personType;

	@Transient
	private BigDecimal durationCost = BigDecimal.ZERO;

	public String getPersonType() {
		return personType;
	}

	public void setPersonType(String personType) {
		this.personType = personType;
	}

	public TbnPerson getTbnPerson() {
		return tbnPerson;
	}

	public void setTbnPerson(TbnPerson tbnPerson) {
		this.tbnPerson = tbnPerson;
	}

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

	public Integer getBudgetId() {
		return budgetId;
	}

	public void setBudgetId(Integer budgetId) {
		this.budgetId = budgetId;
	}

	public String getPersonName() {
		return personName;
	}

	public void setPersonName(String personName) {
		this.personName = personName;
	}

	public AppointmentType getAppointmentType() {
		return appointmentType;
	}

	public void setAppointmentType(AppointmentType appointmentType) {
		this.appointmentType = appointmentType;
	}

	public JobCode getJobCode() {
		return jobCode;
	}

	public void setJobCode(JobCode jobCode) {
		this.jobCode = jobCode;
	}

	public String getJobCodeType() {
		return jobCodeType;
	}

	public void setJobCodeType(String jobCodeType) {
		this.jobCodeType = jobCodeType;
	}

	public Timestamp getEffectiveDate() {
		return effectiveDate;
	}

	public void setEffectiveDate(Timestamp effectiveDate) {
		this.effectiveDate = effectiveDate;
	}

	public BigDecimal getCalculationBase() {
		return calculationBase;
	}

	public void setCalculationBase(BigDecimal calculationBase) {
		this.calculationBase = calculationBase;
	}

	public Boolean getNonEmployeeFlag() {
		return nonEmployeeFlag;
	}

	public void setNonEmployeeFlag(Boolean nonEmployeeFlag) {
		this.nonEmployeeFlag = nonEmployeeFlag;
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

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public BigDecimal getDurationCost() {
		if (calculationBase != null && calculationBase.compareTo(BigDecimal.ZERO) > 0) {
			durationCost = calculationBase.divide(appointmentType.getDuration(), 4, RoundingMode.HALF_UP);
		}
		return durationCost;
	}

	public void setDurationCost(BigDecimal durationCost) {
		this.durationCost = durationCost;
	}

	public String getTbnId() {
		return tbnId;
	}

	public void setTbnId(String tbnId) {
		this.tbnId = tbnId;
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
}
