package com.polus.fibicomp.award.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

@Entity
@Table(name = "VALID_REPORT_CLASS")
public class ValidReportClass implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "REPORT_CLASS_ID")
	private Integer validReportClassId;

	@Column(name = "REPORT_CLASS_CODE")
	private String reportClassCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "VALID_REPORT_CLASS_FK1"), name = "REPORT_CLASS_CODE", referencedColumnName = "REPORT_CLASS_CODE", insertable = false, updatable = false)
	private ReportClass reportClass;

	@Column(name = "REPORT_CODE")
	private String reportCode;

	@ManyToOne(cascade = { CascadeType.REFRESH })
	@JoinColumn(foreignKey = @ForeignKey(name = "VALID_REPORT_CLASS_FK2"), name = "REPORT_CODE", referencedColumnName = "REPORT_CODE", insertable = false, updatable = false)
	private Report report;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getValidReportClassId() {
		return validReportClassId;
	}

	public void setValidReportClassId(Integer validReportClassId) {
		this.validReportClassId = validReportClassId;
	}

	public String getReportClassCode() {
		return reportClassCode;
	}

	public void setReportClassCode(String reportClassCode) {
		this.reportClassCode = reportClassCode;
	}

	public String getReportCode() {
		return reportCode;
	}

	public void setReportCode(String reportCode) {
		this.reportCode = reportCode;
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

	public ReportClass getReportClass() {
		return reportClass;
	}

	public void setReportClass(ReportClass reportClass) {
		this.reportClass = reportClass;
	}

	public Report getReport() {
		return report;
	}

	public void setReport(Report report) {
		this.report = report;
	}
}
