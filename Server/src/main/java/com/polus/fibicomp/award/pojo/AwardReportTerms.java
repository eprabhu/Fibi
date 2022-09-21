package com.polus.fibicomp.award.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.List;

import javax.persistence.*;

import com.fasterxml.jackson.annotation.JsonManagedReference;

@Entity
@Table(name = "AWARD_REPORT_TERMS")
public class AwardReportTerms implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AWARD_REPORT_TERMS_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer awardReportTermsId;

	@Column(name = "AWARD_ID")
	private Integer awardId;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_REPORT_TERMS_FK1"), name = "AWARD_ID", referencedColumnName = "AWARD_ID", insertable = false, updatable = false)
	private Award award;

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Column(name = "SEQUENCE_NUMBER")
	private Integer sequenceNumber;

	@Column(name = "REPORT_CLASS_CODE")
	private String reportClassCode;

	@Column(name = "REPORT_CODE")
	private String reportCode;

	@Column(name = "FREQUENCY_CODE")
	private String frequencyCode;

	@Column(name = "FREQUENCY_BASE_CODE")
	private String frequencyBaseCode;

	@Column(name = "OSP_DISTRIBUTION_CODE")
	private String ospDistributionCode;

	@Column(name = "DUE_DATE")
	private Timestamp dueDate;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "BASE_DATE")
	private Timestamp baseDate;

	@JsonManagedReference
	@OneToMany(mappedBy = "awardReportTerms",  cascade = { CascadeType.REMOVE, CascadeType.ALL }, fetch = FetchType.LAZY)
	private List<AwardReportTermRecipient> awardReportTermRecipient;

	@JsonManagedReference
	@OrderBy("dueDate ASC")
	@OneToMany(mappedBy = "awardReportTerms",  orphanRemoval = true, cascade = {CascadeType.ALL }, fetch = FetchType.LAZY)
	private List<AwardReportTracking> awardReportTracking;

	@Transient
	private ReportClass reportClass;

	@Transient
	private String reportName;

	public AwardReportTerms() {
		awardReportTracking = new ArrayList<AwardReportTracking>();
		awardReportTermRecipient = new ArrayList<AwardReportTermRecipient>();
	}

	public String getReportName() {
		return reportName;
	}

	public void setReportName(String reportName) {
		this.reportName = reportName;
	}

	public ReportClass getReportClass() {
		return reportClass;
	}

	public void setReportClass(ReportClass reportClass) {
		this.reportClass = reportClass;
	}

	public Integer getAwardReportTermsId() {
		return awardReportTermsId;
	}

	public void setAwardReportTermsId(Integer awardReportTermsId) {
		this.awardReportTermsId = awardReportTermsId;
	}

	public Integer getAwardId() {
		return awardId;
	}

	public void setAwardId(Integer awardId) {
		this.awardId = awardId;
	}

	public String getAwardNumber() {
		return awardNumber;
	}

	public void setAwardNumber(String awardNumber) {
		this.awardNumber = awardNumber;
	}

	public Integer getSequenceNumber() {
		return sequenceNumber;
	}

	public void setSequenceNumber(Integer sequenceNumber) {
		this.sequenceNumber = sequenceNumber;
	}

	public String getReportClassCode() {
		return reportClassCode;
	}

	public void setReportClassCode(String reportClassCode) {
		this.reportClassCode = reportClassCode;
	}

	public String getReportCode() {
		return reportCode;
	}

	public void setReportCode(String reportCode) {
		this.reportCode = reportCode;
	}


	public String getOspDistributionCode() {
		return ospDistributionCode;
	}

	public void setOspDistributionCode(String ospDistributionCode) {
		this.ospDistributionCode = ospDistributionCode;
	}

   public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Award getAward() {
		return award;
	}

	public void setAward(Award award) {
		this.award = award;
	}

	public Timestamp getDueDate() {
		return dueDate;
	}

	public void setDueDate(Timestamp dueDate) {
		this.dueDate = dueDate;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public List<AwardReportTermRecipient> getAwardReportTermRecipient() {
		return awardReportTermRecipient;
	}

	public void setAwardReportTermRecipient(List<AwardReportTermRecipient> awardReportTermRecipient) {
		this.awardReportTermRecipient = awardReportTermRecipient;
	}

	public String getFrequencyCode() {
		return frequencyCode;
	}

	public void setFrequencyCode(String frequencyCode) {
		this.frequencyCode = frequencyCode;
	}

	public String getFrequencyBaseCode() {
		return frequencyBaseCode;
	}

	public void setFrequencyBaseCode(String frequencyBaseCode) {
		this.frequencyBaseCode = frequencyBaseCode;
	}

	public List<AwardReportTracking> getAwardReportTracking() {
		return awardReportTracking;
	}

	public void setAwardReportTracking(List<AwardReportTracking> awardReportTracking) {
		this.awardReportTracking = awardReportTracking;
	}

	public Timestamp getBaseDate() {
		return baseDate;
	}

	public void setBaseDate(Timestamp baseDate) {
		this.baseDate = baseDate;
	}

}
