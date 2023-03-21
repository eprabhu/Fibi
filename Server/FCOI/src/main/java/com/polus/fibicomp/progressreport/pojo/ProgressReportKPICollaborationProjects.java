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

import com.polus.fibicomp.pojo.Country;

@Entity
@Table(name = "PROGRESS_REPORT_KPI_COLLABORATION_PROJECTS")
@EntityListeners(AuditingEntityListener.class)
public class ProgressReportKPICollaborationProjects implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "KPI_COLLABORATION_PROJECTS_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer kpiCollaborationProjectId;
	
	@Column(name = "KPI_SUMMARY_ID")
	private Integer kpiSummaryId;
	
	@Column(name = "PROGRESS_REPORT_ID")
	private Integer progressReportId;
	
	@Column(name = "KPI_CRITERIA_TYPE_CODE")
	private String kpiCriteriaCode;
	
	@Column(name = "PROJECT_TITLE")
	private String projectTitle;
	
	@Column(name = "PROJECT_DESCRIPTION")
	private String projectDescription;
	
	@Column(name = "PROJECT_START_DATE")
	private Date projectStartDate;
	
	@Column(name = "PROJECT_END_DATE")
	private Date projectEndDate;
	
	@Column(name = "COLLABORATING_ORGANIZATION")
	private String collaboratingOrganization;
	
	@Column(name = "COUNTRY_CODE")
	private String countryCode;
	
	@Column(name = "COMPANY_UEN")
	private String companyUen;
	
	@Column(name = "COMMENTS")
	private String comments;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;
	
	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;
	
	@ManyToOne
	@JoinColumn(foreignKey = @ForeignKey(name = "KPI_COLLABORATION_PROJECTS_FK1"), name = "COUNTRY_CODE ", referencedColumnName = "COUNTRY_CODE ", insertable = false, updatable = false)
	private Country country;

	public Integer getKpiCollaborationProjectId() {
		return kpiCollaborationProjectId;
	}

	public void setKpiCollaborationProjectId(Integer kpiCollaborationProjectId) {
		this.kpiCollaborationProjectId = kpiCollaborationProjectId;
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

	public String getProjectTitle() {
		return projectTitle;
	}

	public void setProjectTitle(String projectTitle) {
		this.projectTitle = projectTitle;
	}

	public String getProjectDescription() {
		return projectDescription;
	}

	public void setProjectDescription(String projectDescription) {
		this.projectDescription = projectDescription;
	}

	public Date getProjectStartDate() {
		return projectStartDate;
	}

	public void setProjectStartDate(Date projectStartDate) {
		this.projectStartDate = projectStartDate;
	}

	public Date getProjectEndDate() {
		return projectEndDate;
	}

	public void setProjectEndDate(Date projectEndDate) {
		this.projectEndDate = projectEndDate;
	}

	public String getCollaboratingOrganization() {
		return collaboratingOrganization;
	}

	public void setCollaboratingOrganization(String collaboratingOrganization) {
		this.collaboratingOrganization = collaboratingOrganization;
	}

	public String getCountryCode() {
		return countryCode;
	}

	public void setCountryCode(String countryCode) {
		this.countryCode = countryCode;
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

	public Country getCountry() {
		return country;
	}

	public void setCountry(Country country) {
		this.country = country;
	}

	public String getCompanyUen() {
		return companyUen;
	}

	public void setCompanyUen(String companyUen) {
		this.companyUen = companyUen;
	}
}
