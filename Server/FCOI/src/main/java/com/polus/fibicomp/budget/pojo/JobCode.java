package com.polus.fibicomp.budget.pojo;

import java.io.Serializable;
import java.math.BigDecimal;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "JOB_CODE")
public class JobCode implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "JOB_CODE")
	private String jobCode;

	@Column(name = "JOB_TITLE")
	private String jobTitle;

	@Column(name = "MONTHLY_SALARY", precision = 12, scale = 2)
	private BigDecimal monthSalary;

	@Column(name = "IS_ACTIVE")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean isActive;

	public String getJobCode() {
		return jobCode;
	}

	public void setJobCode(String jobCode) {
		this.jobCode = jobCode;
	}

	public String getJobTitle() {
		return jobTitle;
	}

	public void setJobTitle(String jobTitle) {
		this.jobTitle = jobTitle;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public BigDecimal getMonthSalary() {
		return monthSalary;
	}

	public void setMonthSalary(BigDecimal monthSalary) {
		this.monthSalary = monthSalary;
	}

	public Boolean getIsActive() {
		return isActive;
	}

	public void setIsActive(Boolean isActive) {
		this.isActive = isActive;
	}

}
