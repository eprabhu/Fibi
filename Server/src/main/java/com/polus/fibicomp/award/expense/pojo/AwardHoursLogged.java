package com.polus.fibicomp.award.expense.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "AWARD_HOURS_LOGGED")
public class AwardHoursLogged implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "HOURS_LOGGED_ID")
	private String hoursLoggedId;

	@Column(name = "AWARD_NUMBER ")
	private String awardNumber;

	@Column(name = "ACCOUNT_NUMBER ")
	private String accountNumber;

	@Column(name = "INTERNAL_ORDER_CODE ")
	private String internalOrderCode;

	@Column(name = "SUBMITTED_HOURS  ")
	private BigDecimal submittedhours;

	@Column(name = "PAYROLL_HOURS")
	private BigDecimal payrollhours;

	@Column(name = "SUBMITTED_DATE ")
	private Timestamp submittedDate;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public AwardHoursLogged() {

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

	public BigDecimal getSubmittedhours() {
		return submittedhours;
	}

	public void setSubmittedhours(BigDecimal submittedhours) {
		this.submittedhours = submittedhours;
	}

	public BigDecimal getPayrollhours() {
		return payrollhours;
	}

	public void setPayrollhours(BigDecimal payrollhours) {
		this.payrollhours = payrollhours;
	}

	public Timestamp getSubmittedDate() {
		return submittedDate;
	}

	public void setSubmittedDate(Timestamp submittedDate) {
		this.submittedDate = submittedDate;
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

	public String getHoursLoggedId() {
		return hoursLoggedId;
	}

	public void setHoursLoggedId(String hoursLoggedId) {
		this.hoursLoggedId = hoursLoggedId;
	}

}
