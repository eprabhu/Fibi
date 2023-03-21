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
@Table(name = "PROGRESS_REPORT_KPI_CONFERENCE_PRESENTATION")
@EntityListeners(AuditingEntityListener.class)
public class ProgressReportKPIConferencePresentation implements Serializable {
	
	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "KPI_CONFERENCE_PRESENTATION_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer kpiConferencePresentationId;
	
	@Column(name = "KPI_SUMMARY_ID")
	private Integer kpiSummaryId;
	
	@Column(name = "PROGRESS_REPORT_ID")
	private Integer progressReportId;
	
	@Column(name = "KPI_CRITERIA_TYPE_CODE")
	private String kpiCriteriaCode;
	
	@Column(name = "NAME_OF_PRESENTER")
	private String nameOfPresenter;
	
	@Column(name = "TITLE")
	private String title;
	
	@Column(name = "CONFERENCE_TITLE")
	private String conferenceTitle;
	
	@Column(name = "ORGANISER")
	private String organiser;
	
	@Column(name = "CONFERENCE_LOCATION")
	private String conferenceLocation;
	
	@Column(name = "DATE")
	private Date date;
	
	@Column(name = "COMMENTS")
	private String comments;
	
	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;
	
	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getKpiConferencePresentationId() {
		return kpiConferencePresentationId;
	}

	public void setKpiConferencePresentationId(Integer kpiConferencePresentationId) {
		this.kpiConferencePresentationId = kpiConferencePresentationId;
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

	public String getNameOfPresenter() {
		return nameOfPresenter;
	}

	public void setNameOfPresenter(String nameOfPresenter) {
		this.nameOfPresenter = nameOfPresenter;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getConferenceTitle() {
		return conferenceTitle;
	}

	public void setConferenceTitle(String conferenceTitle) {
		this.conferenceTitle = conferenceTitle;
	}

	public String getOrganiser() {
		return organiser;
	}

	public void setOrganiser(String organiser) {
		this.organiser = organiser;
	}

	public String getConferenceLocation() {
		return conferenceLocation;
	}

	public void setConferenceLocation(String conferenceLocation) {
		this.conferenceLocation = conferenceLocation;
	}

	public Date getDate() {
		return date;
	}

	public void setDate(Date date) {
		this.date = date;
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
