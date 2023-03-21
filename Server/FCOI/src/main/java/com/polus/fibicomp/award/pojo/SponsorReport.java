package com.polus.fibicomp.award.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.ForeignKey;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.ManyToOne;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

import com.polus.fibicomp.pojo.Sponsor;
import com.polus.fibicomp.pojo.SponsorFundingScheme;

@Entity
@Table(name = "SPONSOR_REPORT")
public class SponsorReport implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "SPONSOR_REPORT_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_SPONSOR_REPORT")
	@SequenceGenerator(name="SEQ_SPONSOR_REPORT", sequenceName = "SEQ_SPONSOR_REPORT", allocationSize=1)
	private Integer sponsorReportId;

	@Column(name = "SPONSOR_CODE")
	private String sponsorCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "SPONSOR_REPORT_FK1"), name = "SPONSOR_CODE", referencedColumnName = "SPONSOR_CODE", insertable = false, updatable = false)
	private Sponsor sponsor;

	@Column(name = "REPORT_CLASS_CODE")
	private String reportClassCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "SPONSOR_REPORT_FK3"), name = "REPORT_CLASS_CODE", referencedColumnName = "REPORT_CLASS_CODE", insertable = false, updatable = false)
	private ReportClass reportClass;

	@Column(name = "REPORT_CODE")
	private String reportCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "SPONSOR_REPORT_FK4"), name = "REPORT_CODE", referencedColumnName = "REPORT_CODE", insertable = false, updatable = false)
	private Report report;

	@Column(name = "FREQUENCY_CODE")
	private String frequencyCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "SPONSOR_REPORT_FK5"), name = "FREQUENCY_CODE", referencedColumnName = "FREQUENCY_CODE", insertable = false, updatable = false)
	private Frequency frequency;

	@Column(name = "FREQUENCY_BASE_CODE")
	private String frequencyBaseCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "SPONSOR_REPORT_FK6"), name = "FREQUENCY_BASE_CODE", referencedColumnName = "FREQUENCY_BASE_CODE", insertable = false, updatable = false)
	private FrequencyBase frequencyBase;

	@Column(name = "FUNDING_SCHEME_ID")
	private Integer fundingSchemeId;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "SPONSOR_REPORT_FK2"), name = "FUNDING_SCHEME_ID", referencedColumnName = "FUNDING_SCHEME_ID", insertable = false, updatable = false)
	private SponsorFundingScheme sponsorFundingScheme;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimetamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	public Integer getSponsorReportId() {
		return sponsorReportId;
	}

	public void setSponsorReportId(Integer sponsorReportId) {
		this.sponsorReportId = sponsorReportId;
	}

	public String getSponsorCode() {
		return sponsorCode;
	}

	public void setSponsorCode(String sponsorCode) {
		this.sponsorCode = sponsorCode;
	}

	public Sponsor getSponsor() {
		return sponsor;
	}

	public void setSponsor(Sponsor sponsor) {
		this.sponsor = sponsor;
	}

	public String getReportClassCode() {
		return reportClassCode;
	}

	public void setReportClassCode(String reportClassCode) {
		this.reportClassCode = reportClassCode;
	}

	public ReportClass getReportClass() {
		return reportClass;
	}

	public void setReportClass(ReportClass reportClass) {
		this.reportClass = reportClass;
	}

	public String getReportCode() {
		return reportCode;
	}

	public void setReportCode(String reportCode) {
		this.reportCode = reportCode;
	}

	public Report getReport() {
		return report;
	}

	public void setReport(Report report) {
		this.report = report;
	}

	public String getFrequencyCode() {
		return frequencyCode;
	}

	public void setFrequencyCode(String frequencyCode) {
		this.frequencyCode = frequencyCode;
	}

	public Frequency getFrequency() {
		return frequency;
	}

	public void setFrequency(Frequency frequency) {
		this.frequency = frequency;
	}

	public String getFrequencyBaseCode() {
		return frequencyBaseCode;
	}

	public void setFrequencyBaseCode(String frequencyBaseCode) {
		this.frequencyBaseCode = frequencyBaseCode;
	}

	public FrequencyBase getFrequencyBase() {
		return frequencyBase;
	}

	public void setFrequencyBase(FrequencyBase frequencyBase) {
		this.frequencyBase = frequencyBase;
	}

	public Timestamp getUpdateTimetamp() {
		return updateTimetamp;
	}

	public void setUpdateTimetamp(Timestamp updateTimetamp) {
		this.updateTimetamp = updateTimetamp;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Integer getFundingSchemeId() {
		return fundingSchemeId;
	}

	public void setFundingSchemeId(Integer fundingSchemeId) {
		this.fundingSchemeId = fundingSchemeId;
	}

	public SponsorFundingScheme getSponsorFundingScheme() {
		return sponsorFundingScheme;
	}

	public void setSponsorFundingScheme(SponsorFundingScheme sponsorFundingScheme) {
		this.sponsorFundingScheme = sponsorFundingScheme;
	}

}
