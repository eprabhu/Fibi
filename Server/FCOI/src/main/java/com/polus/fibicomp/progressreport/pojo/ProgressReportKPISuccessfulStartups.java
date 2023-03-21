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
@Table(name = "PROGRESS_REPORT_KPI_SUCCESSFUL_STARTUPS")
@EntityListeners(AuditingEntityListener.class)
public class ProgressReportKPISuccessfulStartups implements Serializable {
	
	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "KPI_SUCCESSFUL_STARTUPS_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer kpiSuccessfulStartupId;
	
	@Column(name = "KPI_SUMMARY_ID")
	private Integer kpiSummaryId;
	
	@Column(name = "PROGRESS_REPORT_ID")
	private Integer progressReportId;
	
	@Column(name = "KPI_CRITERIA_TYPE_CODE")
	private String kpiCriteriaCode;
	
	@Column(name = "NAME_OF_COMPANY")
	private String nameOfCompany;
	
	@Column(name = "DATE_OF_ESTABLISHMENT")
	private Date dateOfEstablishment;
	
	@Column(name = "DATE_ESTABLISHED")
	private Date dateEstablished;
	
	@Column(name = "COMPANY_UEN")
	private String companyUen;
	
	@Column(name = "EXTERNAL_FUNDING_CRITERIA")
	private String externalFundingCriteria;
	
	@Column(name = "VALUATION_CRITERIA")
	private String valuationCriteria;
	
	@Column(name = "ANNUAL_REVENUE_CRITERIA")
	private String annualRevenueCriteria;
	
	@Column(name = "COMMENTS")
	private String comments;
	
	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;
	
	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getKpiSuccessfulStartupId() {
		return kpiSuccessfulStartupId;
	}

	public void setKpiSuccessfulStartupId(Integer kpiSuccessfulStartupId) {
		this.kpiSuccessfulStartupId = kpiSuccessfulStartupId;
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

	public String getNameOfCompany() {
		return nameOfCompany;
	}

	public void setNameOfCompany(String nameOfCompany) {
		this.nameOfCompany = nameOfCompany;
	}

	public Date getDateOfEstablishment() {
		return dateOfEstablishment;
	}

	public void setDateOfEstablishment(Date dateOfEstablishment) {
		this.dateOfEstablishment = dateOfEstablishment;
	}

	public Date getDateEstablished() {
		return dateEstablished;
	}

	public void setDateEstablished(Date dateEstablished) {
		this.dateEstablished = dateEstablished;
	}

	public String getCompanyUen() {
		return companyUen;
	}

	public void setCompanyUen(String companyUen) {
		this.companyUen = companyUen;
	}

	public String getExternalFundingCriteria() {
		return externalFundingCriteria;
	}

	public void setExternalFundingCriteria(String externalFundingCriteria) {
		this.externalFundingCriteria = externalFundingCriteria;
	}

	public String getValuationCriteria() {
		return valuationCriteria;
	}

	public void setValuationCriteria(String valuationCriteria) {
		this.valuationCriteria = valuationCriteria;
	}

	public String getAnnualRevenueCriteria() {
		return annualRevenueCriteria;
	}

	public void setAnnualRevenueCriteria(String annualRevenueCriteria) {
		this.annualRevenueCriteria = annualRevenueCriteria;
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
