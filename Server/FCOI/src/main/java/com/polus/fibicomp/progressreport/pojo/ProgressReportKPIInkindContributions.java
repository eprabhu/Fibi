package com.polus.fibicomp.progressreport.pojo;

import java.io.Serializable;
import java.math.BigDecimal;
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
@Table(name = "PROGRESS_REPORT_KPI_INKIND_CONTRIBUTIONS")
@EntityListeners(AuditingEntityListener.class)
public class ProgressReportKPIInkindContributions implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "KPI_INKIND_CONTRIBUTIONS_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer kpiInkindContributionId;
	
	@Column(name = "KPI_SUMMARY_ID")
	private Integer kpiSummaryId;
	
	@Column(name = "PROGRESS_REPORT_ID")
	private Integer progressReportId;
	
	@Column(name = "KPI_CRITERIA_TYPE_CODE")
	private String kpiCriteriaCode;
	
	@Column(name = "NAME_OF_COMPANY")
	private String nameOfCompany;
	
	@Column(name = "COUNTRY_CODE")
	private String countryCode;
	
	@Column(name = "DATE_OF_CONTRIBUTION")
	private Date dateOfContribution;
	
	@Column(name = "COMPANY_UEN")
	private String companyUen;
	
	@Column(name = "AMOUNT")
	private BigDecimal amount;
	
	@Column(name = "COMMENTS")
	private String comments;
	
	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;
	
	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;
	
	@ManyToOne
	@JoinColumn(foreignKey = @ForeignKey(name = "KPI_INKIND_CONTRIBUTIONS_FK1"), name = "COUNTRY_CODE", referencedColumnName = "COUNTRY_CODE", insertable = false, updatable = false)
	private Country country;

	public Integer getKpiInkindContributionId() {
		return kpiInkindContributionId;
	}

	public void setKpiInkindContributionId(Integer kpiInkindContributionId) {
		this.kpiInkindContributionId = kpiInkindContributionId;
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

	public String getCountryCode() {
		return countryCode;
	}

	public void setCountryCode(String countryCode) {
		this.countryCode = countryCode;
	}

	public Date getDateOfContribution() {
		return dateOfContribution;
	}

	public void setDateOfContribution(Date dateOfContribution) {
		this.dateOfContribution = dateOfContribution;
	}

	public String getCompanyUen() {
		return companyUen;
	}

	public void setCompanyUen(String companyUen) {
		this.companyUen = companyUen;
	}

	public BigDecimal getAmount() {
		return amount;
	}

	public void setAmount(BigDecimal amount) {
		this.amount = amount;
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
}
