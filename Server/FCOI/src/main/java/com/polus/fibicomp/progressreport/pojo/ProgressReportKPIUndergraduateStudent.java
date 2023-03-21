package com.polus.fibicomp.progressreport.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.Table;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

@Entity
@Table(name = "PROGRESS_REPORT_KPI_UNDERGRADUATE_STUDENT")
@EntityListeners(AuditingEntityListener.class)
public class ProgressReportKPIUndergraduateStudent implements Serializable {
	
	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "KPI_UNDERGRADUATE_STUDENT_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer kpiUnderGraduateStudId;
	
	@Column(name = "KPI_SUMMARY_ID")
	private Integer kpiSummaryId;
	
	@Column(name = "PROGRESS_REPORT_ID")
	private Integer progressReportId;
	
	@Column(name = "KPI_CRITERIA_TYPE_CODE")
	private String kpiCriteriaCode;
	
	@Column(name = "NAME_OF_STUDENT")
	private String nameOfStudent;
	
	@Column(name = "CITIZENSHIP")
	private String citizenship;
	
	@Column(name = "CURRENT_STATUS_CODE")
	private String currentStatusCode;
	
	@Column(name = "DATE_OF_JOINING")
	private Date dateOfJoining;
	
	@Column(name = "DATE_OF_LEAVING")
	private Date dateOfLeaving;
	
	@Column(name = "COMMENTS")
	private String comments;
	
	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;
	
	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;
	
	@ManyToOne
	@JoinColumn(foreignKey = @ForeignKey(name = "KPI_UNDERGRADUATE_STUDENT_FK1"), name = "CURRENT_STATUS_CODE", referencedColumnName = "CURRENT_STATUS_CODE", insertable = false, updatable = false)
	private KPIManpowerDevelopmentCurrentStatus kpiManpowerDevelopmentCurrentStatus;

	public Integer getKpiUnderGraduateStudId() {
		return kpiUnderGraduateStudId;
	}

	public void setKpiUnderGraduateStudId(Integer kpiUnderGraduateStudId) {
		this.kpiUnderGraduateStudId = kpiUnderGraduateStudId;
	}

	public Integer getKpiSummaryId() {
		return kpiSummaryId;
	}

	public void setKpiSummaryId(Integer kpiSummaryId) {
		this.kpiSummaryId = kpiSummaryId;
	}

	public Integer getProgressReportId() {
		return progressReportId;
	}

	public void setProgressReportId(Integer progressReportId) {
		this.progressReportId = progressReportId;
	}

	public String getKpiCriteriaCode() {
		return kpiCriteriaCode;
	}

	public void setKpiCriteriaCode(String kpiCriteriaCode) {
		this.kpiCriteriaCode = kpiCriteriaCode;
	}

	public String getNameOfStudent() {
		return nameOfStudent;
	}

	public void setNameOfStudent(String nameOfStudent) {
		this.nameOfStudent = nameOfStudent;
	}

	public String getCitizenship() {
		return citizenship;
	}

	public void setCitizenship(String citizenship) {
		this.citizenship = citizenship;
	}

	public Date getDateOfJoining() {
		return dateOfJoining;
	}

	public void setDateOfJoining(Date dateOfJoining) {
		this.dateOfJoining = dateOfJoining;
	}

	public Date getDateOfLeaving() {
		return dateOfLeaving;
	}

	public void setDateOfLeaving(Date dateOfLeaving) {
		this.dateOfLeaving = dateOfLeaving;
	}

	public String getComments() {
		return comments;
	}

	public void setComments(String comments) {
		this.comments = comments;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public String getCurrentStatusCode() {
		return currentStatusCode;
	}

	public void setCurrentStatusCode(String currentStatusCode) {
		this.currentStatusCode = currentStatusCode;
	}

	public KPIManpowerDevelopmentCurrentStatus getKpiManpowerDevelopmentCurrentStatus() {
		return kpiManpowerDevelopmentCurrentStatus;
	}

	public void setKpiManpowerDevelopmentCurrentStatus(
			KPIManpowerDevelopmentCurrentStatus kpiManpowerDevelopmentCurrentStatus) {
		this.kpiManpowerDevelopmentCurrentStatus = kpiManpowerDevelopmentCurrentStatus;
	}
}
