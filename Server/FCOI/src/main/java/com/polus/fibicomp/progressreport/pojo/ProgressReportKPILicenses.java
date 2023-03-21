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
@Table(name = "PROGRESS_REPORT_KPI_LICENSES")
@EntityListeners(AuditingEntityListener.class)
public class ProgressReportKPILicenses implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "KPI_LICENSES_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer kpiLicenseId;
	
	@Column(name = "KPI_SUMMARY_ID")
	private Integer kpiSummaryId;
	
	@Column(name = "PROGRESS_REPORT_ID")
	private Integer progressReportId;
	
	@Column(name = "KPI_CRITERIA_TYPE_CODE")
	private String kpiCriteriaCode;
	
	@Column(name = "NAME_OF_LICENSE")
	private String nameOfLicense;
	
	@Column(name = "START_DATE")
	private Date startDate;
	
	@Column(name = "COMPANY_UEN")
	private String companyUen;
	
	@Column(name = "LICENSING_PERIOD")
	private String licensingPeriod;
	
	@Column(name = "DETAILS_OF_LICENSE")
	private String detailsOfLicense;
	
	@Column(name = "COMMENTS")
	private String comments;
	
	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;
	
	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getKpiLicenseId() {
		return kpiLicenseId;
	}

	public void setKpiLicenseId(Integer kpiLicenseId) {
		this.kpiLicenseId = kpiLicenseId;
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

	public String getNameOfLicense() {
		return nameOfLicense;
	}

	public void setNameOfLicense(String nameOfLicense) {
		this.nameOfLicense = nameOfLicense;
	}

	public Date getStartDate() {
		return startDate;
	}

	public void setStartDate(Date startDate) {
		this.startDate = startDate;
	}

	public String getCompanyUen() {
		return companyUen;
	}

	public void setCompanyUen(String companyUen) {
		this.companyUen = companyUen;
	}

	public String getLicensingPeriod() {
		return licensingPeriod;
	}

	public void setLicensingPeriod(String licensingPeriod) {
		this.licensingPeriod = licensingPeriod;
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

	public String getDetailsOfLicense() {
		return detailsOfLicense;
	}

	public void setDetailsOfLicense(String detailsOfLicense) {
		this.detailsOfLicense = detailsOfLicense;
	}
}
