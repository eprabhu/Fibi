package com.polus.fibicomp.award.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Convert;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

import com.polus.fibicomp.util.JpaCharBooleanConversion;

@Entity
@Table(name = "REPORT")
public class Report implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "REPORT_CODE")
	private String reportCode;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "FINAL_REPORT_FLAG")
	@Convert(converter = JpaCharBooleanConversion.class)
	private Boolean finalReportFlag;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public String getReportCode() {
		return reportCode;
	}

	public void setReportCode(String reportCode) {
		this.reportCode = reportCode;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public Boolean getFinalReportFlag() {
		return finalReportFlag;
	}

	public void setFinalReportFlag(Boolean finalReportFlag) {
		this.finalReportFlag = finalReportFlag;
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
