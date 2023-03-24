package com.polus.fibicomp.integration.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

@Entity
@Table(name = "AWARD_HOURS_LOG_RT")
public class AwardHoursLogRT implements Serializable{

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AWARD_HOURS_LOG_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "AWARD_HOURS_LOG_ID_GENERATOR")
	@SequenceGenerator(name="AWARD_HOURS_LOG_ID_GENERATOR", sequenceName = "AWARD_HOURS_LOG_ID_GENERATOR", allocationSize=1)
	private Integer awardHourLogId;

	@Column(name = "FUND_CODE")
	private String fundCode;

	@Column(name = "FUNDS_CENTER")
	private String fundCenter;

	@Column(name = "IO_CODE")
	private String ioCode;

	@Column(name = "SUBMITTED_HOURS")
	private Integer submittedHour;

	@Column(name = "PAYROLL_HOURS")
	private Integer payrollHour;

	@Column(name = "WORK_DATE")
	private String workDate;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getAwardHourLogId() {
		return awardHourLogId;
	}

	public void setAwardHourLogId(Integer awardHourLogId) {
		this.awardHourLogId = awardHourLogId;
	}

	public String getFundCode() {
		return fundCode;
	}

	public void setFundCode(String fundCode) {
		this.fundCode = fundCode;
	}

	public String getIoCode() {
		return ioCode;
	}

	public void setIoCode(String ioCode) {
		this.ioCode = ioCode;
	}

	public Integer getSubmittedHour() {
		return submittedHour;
	}

	public void setSubmittedHour(Integer submittedHour) {
		this.submittedHour = submittedHour;
	}

	public String getWorkDate() {
		return workDate;
	}

	public void setWorkDate(String workDate) {
		this.workDate = workDate;
	}

	public String getFundCenter() {
		return fundCenter;
	}

	public void setFundCenter(String fundCenter) {
		this.fundCenter = fundCenter;
	}

	public Integer getPayrollHour() {
		return payrollHour;
	}

	public void setPayrollHour(Integer payrollHour) {
		this.payrollHour = payrollHour;
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

}
