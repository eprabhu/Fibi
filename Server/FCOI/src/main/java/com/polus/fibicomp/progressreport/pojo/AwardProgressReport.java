package com.polus.fibicomp.progressreport.pojo;

import com.fasterxml.jackson.annotation.JsonManagedReference;
import com.polus.fibicomp.award.pojo.Award;
import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import javax.persistence.*;
import java.io.Serializable;
import java.sql.Timestamp;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

@Entity
@Table(name = "AWARD_PROGRESS_REPORT")
@EntityListeners(AuditingEntityListener.class)
public class AwardProgressReport implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "PROGRESS_REPORT_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer progressReportId;
	
	@Column(name = "PROGRESS_REPORT_NUMBER")
	private String progressReportNumber;
	
	@Column(name = "AWARD_ID")
	private Integer awardId;
	
	@Column(name = "AWARD_NUMBER")
	private String awardNumber;
	
	@Column(name = "SEQUENCE_NUMBER")
	private Integer sequenceNumber;
	
	@Column(name = "PROGRESS_REPORT_STATUS_CODE")
	private String progressReportStatusCode;
	
	@Column(name  = "DUE_DATE")
	private Date dueDate;

	@Column(name = "AWARD_REPORT_TRACKING_ID")
	private Integer awardReportTrackingId;

	@Column(name = "REPORT_CLASS_CODE")
	private String reportClassCode;

	@CreatedBy
	@Column(name = "CREATE_USER")
	private String createUser;

	@CreatedDate
	@Column(name = "CREATE_TIMESTAMP")
	private Timestamp createTimeStamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "SUBMIT_USER")
	private String submitUser;

	@Column(name = "SUBMISSION_DATE")
	private Timestamp submissionDate;
	
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_PROGRESS_REPORT_FK1"), name = "PROGRESS_REPORT_STATUS_CODE", referencedColumnName = "PROGRESS_REPORT_STATUS_CODE", insertable = false, updatable = false)
	private ProgressReportStatus progressReportStatus;
	
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_PROGRESS_REPORT_FK2"), name = "AWARD_ID", referencedColumnName = "AWARD_ID", insertable = false, updatable = false)
	private Award award;
	
	@JsonManagedReference
	@OneToMany(mappedBy = "awardProgressReport", orphanRemoval = true, cascade = { CascadeType.ALL })
	@OrderBy("updateTimeStamp DESC")
	private List<AwardProgressReportMilestone> awardProgressReportMilestones;
	
	@JsonManagedReference
	@OneToMany(mappedBy = "awardProgressReport", orphanRemoval = true, cascade = { CascadeType.ALL })
	@OrderBy("achievementTypeCode ASC")
	private List<AwardProgressReportAchievement> awardProgressReportAchievements;
	
	@JsonManagedReference
	@OneToMany(mappedBy = "awardProgressReport", orphanRemoval = true, cascade = { CascadeType.ALL })
	@OrderBy("kpiCategoryTypeCode ASC")
	private List<AwardProgressReportKPISummary> awardProgressReportKPISummarys;

	@Column(name = "FUNDER_APPROVAL_DATE")
	private Timestamp funderApprovalDate;

	@Column(name = "REPORT_START_DATE")
	private Timestamp reportStartDate;

	@Column(name = "REPORT_END_DATE")
	private Timestamp reportEndDate;

	@Column(name = "TITLE")
	private String title;

	@Transient
	private String createdPersonId;

	@Transient
	private String createdPersonName;

	@Transient
	private String submitUserFullName;

	@Transient
	private String principalInvestigator;

	@Transient
	private String updatedPersonName;
	
	@Transient
	private String reportClassDescription;
	
	@Transient
	private String reportTypeDescription;
	
	@Transient
	private String reportTypeCode;
	
	@Transient
	private String progressReportStatusDescription;

	@Transient
	private Timestamp lastReportEndDate;

	public String getCreatedPersonId() {
		return createdPersonId;
	}

	public void setCreatedPersonId(String createdPersonId) {
		this.createdPersonId = createdPersonId;
	}

	public String getReportClassDescription() {
		return reportClassDescription;
	}

	public void setReportClassDescription(String reportClassDescription) {
		this.reportClassDescription = reportClassDescription;
	}

	public String getReportTypeDescription() {
		return reportTypeDescription;
	}

	public void setReportTypeDescription(String reportTypeDescription) {
		this.reportTypeDescription = reportTypeDescription;
	}

	public String getReportTypeCode() {
		return reportTypeCode;
	}

	public void setReportTypeCode(String reportTypeCode) {
		this.reportTypeCode = reportTypeCode;
	}

	public String getProgressReportStatusDescription() {
		return progressReportStatusDescription;
	}

	public void setProgressReportStatusDescription(String progressReportStatusDescription) {
		this.progressReportStatusDescription = progressReportStatusDescription;
	}

	public Integer getProgressReportId() {
		return progressReportId;
	}

	public void setProgressReportId(Integer progressReportId) {
		this.progressReportId = progressReportId;
	}

	public String getProgressReportNumber() {
		return progressReportNumber;
	}

	public void setProgressReportNumber(String progressReportNumber) {
		this.progressReportNumber = progressReportNumber;
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

	public String getProgressReportStatusCode() {
		return progressReportStatusCode;
	}

	public void setProgressReportStatusCode(String progressReportStatusCode) {
		this.progressReportStatusCode = progressReportStatusCode;
	}

	public Date getDueDate() {
		return dueDate;
	}

	public void setDueDate(Date dueDate) {
		this.dueDate = dueDate;
	}

	public String getCreateUser() {
		return createUser;
	}

	public void setCreateUser(String createUser) {
		this.createUser = createUser;
	}

	public Timestamp getCreateTimeStamp() {
		return createTimeStamp;
	}

	public void setCreateTimeStamp(Timestamp createTimeStamp) {
		this.createTimeStamp = createTimeStamp;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}

	public ProgressReportStatus getProgressReportStatus() {
		return progressReportStatus;
	}

	public void setProgressReportStatus(ProgressReportStatus progressReportStatus) {
		this.progressReportStatus = progressReportStatus;
	}

	public Award getAward() {
		return award;
	}

	public void setAward(Award award) {
		this.award = award;
	}

	public List<AwardProgressReportMilestone> getAwardProgressReportMilestones() {
		return awardProgressReportMilestones;
	}

	public void setAwardProgressReportMilestones(List<AwardProgressReportMilestone> awardProgressReportMilestones) {
		this.awardProgressReportMilestones = awardProgressReportMilestones;
	}

	public String getCreatedPersonName() {
		return createdPersonName;
	}

	public void setCreatedPersonName(String createdPersonName) {
		this.createdPersonName = createdPersonName;
	}

	public String getSubmitUserFullName() {
		return submitUserFullName;
	}

	public void setSubmitUserFullName(String submitUserFullName) {
		this.submitUserFullName = submitUserFullName;
	}

	public String getPrincipalInvestigator() {
		return principalInvestigator;
	}

	public void setPrincipalInvestigator(String principalInvestigator) {
		this.principalInvestigator = principalInvestigator;
	}

	public List<AwardProgressReportAchievement> getAwardProgressReportAchievements() {
		return awardProgressReportAchievements;
	}

	public void setAwardProgressReportAchievements(List<AwardProgressReportAchievement> awardProgressReportAchievements) {
		this.awardProgressReportAchievements = awardProgressReportAchievements;
	}

	public List<AwardProgressReportKPISummary> getAwardProgressReportKPISummarys() {
		return awardProgressReportKPISummarys;
	}

	public void setAwardProgressReportKPISummarys(List<AwardProgressReportKPISummary> awardProgressReportKPISummarys) {
		this.awardProgressReportKPISummarys = awardProgressReportKPISummarys;
	}

	public Integer getAwardReportTrackingId() {
		return awardReportTrackingId;
	}

	public void setAwardReportTrackingId(Integer awardReportTrackingId) {
		this.awardReportTrackingId = awardReportTrackingId;
	}

	public String getReportClassCode() {
		return reportClassCode;
	}

	public void setReportClassCode(String reportClassCode) {
		this.reportClassCode = reportClassCode;
	}

	public AwardProgressReport() {
		super();
		this.awardProgressReportKPISummarys = new ArrayList<>();
		this.awardProgressReportMilestones = new ArrayList<>();
		this.awardProgressReportAchievements = new ArrayList<>();
	}
	
	public AwardProgressReport(Integer awardReportTrackingId, String reportClassDescription, String reportTypeDescription, Integer progressReportId, String reportClassCode, String reportTypeCode,
			Integer awardId, String progressReportNumber, String awardNumber, Integer sequenceNumber,
			String progressReportStatusCode, String progressReportStatusDescription, Date dueDate, String createUser,
			Date createTimeStamp, String updateUser, Date updateTimeStamp, Date reportStartDate, Date reportEndDate) {
		super();
		this.awardReportTrackingId = awardReportTrackingId;
		this.reportClassDescription = reportClassDescription;
		this.reportTypeDescription = reportTypeDescription;
		this.progressReportId = progressReportId;
		this.reportClassCode = reportClassCode;
		this.reportTypeCode = reportTypeCode;
		this.awardId = awardId;
		this.progressReportNumber = progressReportNumber;
		this.awardNumber = awardNumber;
		this.sequenceNumber = sequenceNumber;
		this.progressReportStatusCode = progressReportStatusCode;
		this.progressReportStatusDescription = progressReportStatusDescription;
		this.dueDate = dueDate;
		this.createUser = createUser;
		this.createTimeStamp = createTimeStamp != null ? new Timestamp(createTimeStamp.getTime()) : null;
		this.updateUser = updateUser;
		this.updateTimeStamp = updateTimeStamp != null ? new Timestamp(updateTimeStamp.getTime()) : null;
		this.reportStartDate = reportStartDate != null ? new Timestamp(reportStartDate.getTime()) : null;
		this.reportEndDate = reportEndDate != null ? new Timestamp(reportEndDate.getTime()) : null;
	}

	public AwardProgressReport(Integer progressReportId, String progressReportNumber, String progressReportStatusCode,
			Date dueDate, String createUser, Timestamp createTimeStamp, ProgressReportStatus progressReportStatus, Timestamp updateTimeStamp) {
		super();
		this.progressReportId = progressReportId;
		this.progressReportNumber = progressReportNumber;
		this.progressReportStatusCode = progressReportStatusCode;
		this.dueDate = dueDate;
		this.createUser = createUser;
		this.createTimeStamp = createTimeStamp;
		this.progressReportStatus = progressReportStatus;
		this.updateTimeStamp = updateTimeStamp;
	}

	public String getUpdatedPersonName() {
		return updatedPersonName;
	}

	public void setUpdatedPersonName(String updatedPersonName) {
		this.updatedPersonName = updatedPersonName;
	}

	public Timestamp getFunderApprovalDate() {
		return funderApprovalDate;
	}

	public void setFunderApprovalDate(Timestamp funderApprovalDate) {
		this.funderApprovalDate = funderApprovalDate;
	}

	public Timestamp getReportStartDate() {
		return reportStartDate;
	}
	public void setReportStartDate(Timestamp reportStartDate) {
		this.reportStartDate = reportStartDate;
	}

	public Timestamp getReportEndDate() {
		return reportEndDate;
	}

	public void setReportEndDate(Timestamp reportEndDate) {
		this.reportEndDate = reportEndDate;
	}

	public Timestamp getLastReportEndDate() {
		return lastReportEndDate;
	}

	public void setLastReportEndDate(Timestamp lastReportEndDate) {
		this.lastReportEndDate = lastReportEndDate;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getSubmitUser() {
		return submitUser;
	}

	public void setSubmitUser(String submitUser) {
		this.submitUser = submitUser;
	}

	public Timestamp getSubmissionDate() {
		return submissionDate;
	}

	public void setSubmissionDate(Timestamp submissionDate) {
		this.submissionDate = submissionDate;
	}

}
