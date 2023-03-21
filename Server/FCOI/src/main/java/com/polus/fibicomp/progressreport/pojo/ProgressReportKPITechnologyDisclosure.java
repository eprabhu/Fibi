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
@Table(name = "PROGRESS_REPORT_KPI_TECHNOLOGY_DISCLOSURE")
@EntityListeners(AuditingEntityListener.class)
public class ProgressReportKPITechnologyDisclosure implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "KPI_TECHNOLOGY_DISCLOSURE_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer kpiTechnologyDisclosureId;
	
	@Column(name = "KPI_SUMMARY_ID")
	private Integer kpiSummaryId;
	
	@Column(name = "PROGRESS_REPORT_ID")
	private Integer progressReportId;
	
	@Column(name = "KPI_CRITERIA_TYPE_CODE")
	private String kpiCriteriaCode;
	
	@Column(name = "TECHNOLOGY_DISCLOSURE_STATUS_CODE")
	private String technologyDisclosureStatusCode;
	
	@Column(name = "AUTHOR_NAME")
	private String authorName;
	
	@Column(name = "TITLE_OF_PATENT")
	private String titleOfPatent;
	
	@Column(name = "COVERING_COUNTRIES")
	private String coveringCountries;
	
	@Column(name = "FILING_OFFICE")
	private String fillingOffice;
	
	@Column(name = "DATE_OF_FILING")
	private Date dateOffilling;
	
	@Column(name = "DATE_OF_AWARD")
	private Date dateOfAward;
	
	@Column(name = "COMMENTS")
	private String comments;
	
	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;
	
	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;
	
	@ManyToOne
	@JoinColumn(foreignKey = @ForeignKey(name = "KPI_TECHNOLOGY_DISCLOSURE_FK1"), name = "TECHNOLOGY_DISCLOSURE_STATUS_CODE", referencedColumnName = "TECHNOLOGY_DISCLOSURE_STATUS_CODE", insertable = false, updatable = false)
	private KPITechnologyDisclosureStatus kpiTechnologyDisclosureStatus;

	public Integer getKpiTechnologyDisclosureId() {
		return kpiTechnologyDisclosureId;
	}

	public void setKpiTechnologyDisclosureId(Integer kpiTechnologyDisclosureId) {
		this.kpiTechnologyDisclosureId = kpiTechnologyDisclosureId;
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

	public String getTechnologyDisclosureStatusCode() {
		return technologyDisclosureStatusCode;
	}

	public void setTechnologyDisclosureStatusCode(String technologyDisclosureStatusCode) {
		this.technologyDisclosureStatusCode = technologyDisclosureStatusCode;
	}

	public String getAuthorName() {
		return authorName;
	}

	public void setAuthorName(String authorName) {
		this.authorName = authorName;
	}

	public String getTitleOfPatent() {
		return titleOfPatent;
	}

	public void setTitleOfPatent(String titleOfPatent) {
		this.titleOfPatent = titleOfPatent;
	}

	public String getCoveringCountries() {
		return coveringCountries;
	}

	public void setCoveringCountries(String coveringCountries) {
		this.coveringCountries = coveringCountries;
	}

	public String getFillingOffice() {
		return fillingOffice;
	}

	public void setFillingOffice(String fillingOffice) {
		this.fillingOffice = fillingOffice;
	}

	public Date getDateOffilling() {
		return dateOffilling;
	}

	public void setDateOffilling(Date dateOffilling) {
		this.dateOffilling = dateOffilling;
	}

	public Date getDateOfAward() {
		return dateOfAward;
	}

	public void setDateOfAward(Date dateOfAward) {
		this.dateOfAward = dateOfAward;
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

	public KPITechnologyDisclosureStatus getKpiTechnologyDisclosureStatus() {
		return kpiTechnologyDisclosureStatus;
	}

	public void setKpiTechnologyDisclosureStatus(KPITechnologyDisclosureStatus kpiTechnologyDisclosureStatus) {
		this.kpiTechnologyDisclosureStatus = kpiTechnologyDisclosureStatus;
	}		
}
