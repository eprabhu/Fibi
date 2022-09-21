package com.polus.fibicomp.award.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

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
import javax.persistence.Transient;

import org.springframework.data.annotation.CreatedBy;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.polus.fibicomp.progressreport.pojo.AwardProgressReport;

@Entity
@Table(name = "AWARD_REPORT_TRACKING")
@EntityListeners(AuditingEntityListener.class)
public class AwardReportTracking implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AWARD_REPORT_TRACKING_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer awardReportTrackingId;
	/*
	 * @Column(name = "AWARD_REPORT_TERMS_ID") private Integer awardReportTermsId;
	 */
	@JsonBackReference
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_REPORT_TRACKING_FK1"), name = "AWARD_REPORT_TERMS_ID", referencedColumnName = "AWARD_REPORT_TERMS_ID")
	private AwardReportTerms awardReportTerms;

	@Column(name = "AWARD_ID")
	private Integer awardId;

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Column(name = "SEQUENCE_NUMBER")
	private Integer sequenceNumber;

	@Column(name = "STATUS_CODE")
	private String statusCode;

	@Column(name = "ACTIVITY_DATE")
	private Timestamp activityDate;

	@Column(name = "COMMENTS")
	private String comments;

	@Column(name = "PREPARER_ID")
	private String preparerId;

	@CreatedBy
	@Column(name = "CREATE_USER")
	private String createUser;

	@CreatedDate
	@Column(name = "CREATE_DATE")
	private Timestamp createDate;

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "DUE_DATE")
	private Timestamp dueDate;

	@Column(name = "PREPARER_NAME")
	private String preparerName;
	
	@Column(name = "PROGRESS_REPORT_ID")
	private Integer progressReportId;

	@Transient
	private AwardReportTrackingFile awardReportTrackingFile;

	@Transient
	private AwardProgressReport awardProgressReport;

	@Transient
	private Integer awardReportTermsId;

	public Integer getAwardReportTrackingId() {
		return awardReportTrackingId;
	}

	public void setAwardReportTrackingId(Integer awardReportTrackingId) {
		this.awardReportTrackingId = awardReportTrackingId;
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

	public String getStatusCode() {
		return statusCode;
	}

	public void setStatusCode(String statusCode) {
		this.statusCode = statusCode;
	}

	public Timestamp getActivityDate() {
		return activityDate;
	}

	public void setActivityDate(Timestamp activityDate) {
		this.activityDate = activityDate;
	}

	public String getComments() {
		return comments;
	}

	public void setComments(String comments) {
		this.comments = comments;
	}

	public String getPreparerId() {
		return preparerId;
	}

	public void setPreparerId(String preparerId) {
		this.preparerId = preparerId;
	}

	public String getCreateUser() {
		return createUser;
	}

	public void setCreateUser(String createUser) {
		this.createUser = createUser;
	}

	public Timestamp getCreateDate() {
		return createDate;
	}

	public void setCreateDate(Timestamp createDate) {
		this.createDate = createDate;
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

	public AwardReportTerms getAwardReportTerms() {
		return awardReportTerms;
	}

	public void setAwardReportTerms(AwardReportTerms awardReportTerms) {
		this.awardReportTerms = awardReportTerms;
	}

	public Timestamp getDueDate() {
		return dueDate;
	}

	public void setDueDate(Timestamp dueDate) {
		this.dueDate = dueDate;
	}

	public String getPreparerName() {
		return preparerName;
	}

	public void setPreparerName(String preparerName) {
		this.preparerName = preparerName;
	}

	public AwardReportTrackingFile getAwardReportTrackingFile() {
		return awardReportTrackingFile;
	}

	public void setAwardReportTrackingFile(AwardReportTrackingFile awardReportTrackingFile) {
		this.awardReportTrackingFile = awardReportTrackingFile;
	}

	public AwardProgressReport getAwardProgressReport() {
		return awardProgressReport;
	}

	public void setAwardProgressReport(AwardProgressReport awardProgressReport) {
		this.awardProgressReport = awardProgressReport;
	}

	public Integer getAwardReportTermsId() {
		return awardReportTermsId;
	}

	public void setAwardReportTermsId(Integer awardReportTermsId) {
		this.awardReportTermsId = awardReportTermsId;
	}

	public Integer getProgressReportId() {
		return progressReportId;
	}

	public void setProgressReportId(Integer progressReportId) {
		this.progressReportId = progressReportId;
	}

}
