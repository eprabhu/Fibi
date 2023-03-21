package com.polus.fibicomp.progressreport.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.EntityListeners;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

@Entity
@Table(name = "PROGRESS_REPORT_KPI_HEALTH_SPECIFIC_OUTCOMES")
@EntityListeners(AuditingEntityListener.class)
public class ProgressReportKPIHealthSpecificOutcomes implements Serializable {
	
	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "KPI_HEALTH_SPECIFIC_OUTCOMES_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer kpiHealthSpecificOutcomeId;
	
	@Column(name = "KPI_SUMMARY_ID")
	private Integer kpiSummaryId;
	
	@Column(name = "PROGRESS_REPORT_ID")
	private Integer progressReportId;
	
	@Column(name = "KPI_CRITERIA_TYPE_CODE")
	private String kpiCriteriaCode;
	
	@Column(name = "TITLE")
	private String title;
	
	@Column(name = "DATE_ESTABLISHED")
	private Date dateEstablished;
	
	@Column(name = "NUMBER_OF_LIFE_YEARS")
	private String numberOfLifeYears;
	
	@Column(name = "COMMENTS")
	private String comments;
	
	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;
	
	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getKpiHealthSpecificOutcomeId() {
		return kpiHealthSpecificOutcomeId;
	}

	public void setKpiHealthSpecificOutcomeId(Integer kpiHealthSpecificOutcomeId) {
		this.kpiHealthSpecificOutcomeId = kpiHealthSpecificOutcomeId;
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

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public Date getDateEstablished() {
		return dateEstablished;
	}

	public void setDateEstablished(Date dateEstablished) {
		this.dateEstablished = dateEstablished;
	}

	public String getNumberOfLifeYears() {
		return numberOfLifeYears;
	}

	public void setNumberOfLifeYears(String numberOfLifeYears) {
		this.numberOfLifeYears = numberOfLifeYears;
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
}
