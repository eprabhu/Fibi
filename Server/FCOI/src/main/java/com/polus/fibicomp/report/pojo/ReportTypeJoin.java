package com.polus.fibicomp.report.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

@Entity
@Table (name = "REPORT_TYPE_JOIN")
public class ReportTypeJoin implements Serializable{

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "TYPE_JOIN_ID")
	private Integer typeJoinId;

	@Column(name = "TYPE_CODE")
	private String typeCode;

	@ManyToOne
	@JoinColumn(foreignKey = @ForeignKey(name = "REPORT_TYPE_JOIN_FK1"), name = "TYPE_CODE", referencedColumnName = "TYPE_CODE", insertable = false, updatable = false)
	private ReportType reportType;

	@Column(name = "JOIN_QUERY")
	private String joinQuery;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	public Integer getTypeJoinId() {
		return typeJoinId;
	}

	public void setTypeJoinId(Integer typeJoinId) {
		this.typeJoinId = typeJoinId;
	}

	public String getTypeCode() {
		return typeCode;
	}

	public void setTypeCode(String typeCode) {
		this.typeCode = typeCode;
	}

	public ReportType getReportType() {
		return reportType;
	}

	public void setReportType(ReportType reportType) {
		this.reportType = reportType;
	}

	public String getJoinQuery() {
		return joinQuery;
	}

	public void setJoinQuery(String joinQuery) {
		this.joinQuery = joinQuery;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

}
