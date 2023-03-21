package com.polus.fibicomp.award.expense.vo;

import java.math.BigDecimal;
import java.sql.Timestamp;
import java.util.List;

import com.polus.fibicomp.award.expense.pojo.AwardHoursLogged;

public class AwardHoursLoggedVO {

	private String awardNumber;

	private String accountNumber;

	private String internalOrderCode;

	private BigDecimal submittedhours;

	private BigDecimal payrollhours;

	private Timestamp submittedDate;

	private Timestamp updateTimeStamp;

	private String updateUser;
    
	private List< AwardHoursLogged>  awardHoursLogged;

	private BigDecimal totalSubmittedhours;

	private BigDecimal totalPayrollhours;

	public List<AwardHoursLogged> getAwardHoursLogged() {
		return awardHoursLogged;
	}

	public void setAwardHoursLogged(List<AwardHoursLogged> awardHoursLogged) {
		this.awardHoursLogged = awardHoursLogged;
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

	public BigDecimal getTotalSubmittedhours() {
		return totalSubmittedhours;
	}

	public void setTotalSubmittedhours(BigDecimal totalSubmittedhours) {
		this.totalSubmittedhours = totalSubmittedhours;
	}

	public BigDecimal getTotalPayrollhours() {
		return totalPayrollhours;
	}

	public void setTotalPayrollhours(BigDecimal totalPayrollhours) {
		this.totalPayrollhours = totalPayrollhours;
	}

}
