package com.polus.fibicomp.view;

import org.hibernate.annotations.Immutable;

import javax.persistence.*;
import java.io.Serializable;
import java.sql.Timestamp;
import java.util.Date;

@Entity
@Immutable
@Table(name = "AWARD_MV")
public class AwardView implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AWARD_ID")
	private Integer awardId;

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Column(name = "SEQUENCE_NUMBER")
	private Integer sequenceNumber;

	@Column(name = "DOCUMENT_NUMBER")
	private String documentNumber;

	@Column(name = "ACCOUNT_NUMBER")
	private String accountNumber;

	@Column(name = "TITLE")
	private String title;

	@Column(name = "STATUS")
	private String status;

	@Column(name = "SPONSOR")
	private String sponsor;

	@Column(name = "SPONSOR_CODE")
	private String sponsorCode;

	@Column(name = "UNIT_NUMBER")
	private String unitNumber;

	@Column(name = "UNIT_NAME")
	private String unitName;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "FULL_NAME")
	private String fullName;

	@Column(name = "PERSON_ID")
	private String personId;

	@Transient
	private String total_cost;

	@Transient
	private String awardDocumentType;

	@Transient
	private String grantCallTitle;

	@Transient
	private String awardType;

	@Transient
	private String awardSequenceStatus;

	@Transient
	private String workflowAwardStatusCode;

	@Transient
	private String awardWorkflowStatus;

	@Transient
	private String awardVariationType;

	@Transient
	private String sponsorAwardNumber;
	
	private transient Integer claimId;

	private transient String claimNumber;

	private transient Date claimSubmissionDate;

	private transient String createUserName;

	private transient String updateUserName;

	private transient Date claimUpdateTimeStamp;

	private transient Date awardStartDate;

	private transient Date awardEndDate;

	private transient Date lastClaimEndDate;
	
	private transient String progressReportNumber;
	
	private transient Integer progressReportId;
	
	private transient String progressReportStatus;

	private transient String reportTrackingId;

	private transient String reportClassCode;

	private transient Date dueDate;

	private transient String financeOfficer;

	private transient String reportClassDescription;

	private transient String claimStatus;
	
	private transient Date submittedDate;

	private transient String reportType;
	
	private transient String claimStatusCode;

	public String getReportClassDescription() {
		return reportClassDescription;
	}

	public void setReportClassDescription(String reportClassDescription) {
		this.reportClassDescription = reportClassDescription;
	}

	public String getReportTrackingId() {
		return reportTrackingId;
	}

	public void setReportTrackingId(String reportTrackingId) {
		this.reportTrackingId = reportTrackingId;
	}

	public String getReportClassCode() {
		return reportClassCode;
	}

	public void setReportClassCode(String reportClassCode) {
		this.reportClassCode = reportClassCode;
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

	public String getDocumentNumber() {
		return documentNumber;
	}

	public void setDocumentNumber(String documentNumber) {
		this.documentNumber = documentNumber;
	}

	public String getAccountNumber() {
		return accountNumber;
	}

	public void setAccountNumber(String accountNumber) {
		this.accountNumber = accountNumber;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
	}

	public String getSponsor() {
		return sponsor;
	}

	public void setSponsor(String sponsor) {
		this.sponsor = sponsor;
	}

	public String getSponsorCode() {
		return sponsorCode;
	}

	public void setSponsorCode(String sponsorCode) {
		this.sponsorCode = sponsorCode;
	}

	public String getUnitNumber() {
		return unitNumber;
	}

	public void setUnitNumber(String unitNumber) {
		this.unitNumber = unitNumber;
	}

	public String getUnitName() {
		return unitName;
	}

	public void setUnitName(String unitName) {
		this.unitName = unitName;
	}

	public Timestamp getUpdateTimeStamp() {
		return updateTimeStamp;
	}

	public void setUpdateTimeStamp(Timestamp updateTimeStamp) {
		this.updateTimeStamp = updateTimeStamp;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public static long getSerialversionuid() {
		return serialVersionUID;
	}

	public String getFullName() {
		return fullName;
	}

	public void setFullName(String fullName) {
		this.fullName = fullName;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getTotal_cost() {
		return total_cost;
	}

	public void setTotal_cost(String total_cost) {
		this.total_cost = total_cost;
	}

	public String getGrantCallTitle() {
		return grantCallTitle;
	}

	public void setGrantCallTitle(String grantCallTitle) {
		this.grantCallTitle = grantCallTitle;
	}

	public String getAwardType() {
		return awardType;
	}

	public void setAwardType(String awardType) {
		this.awardType = awardType;
	}

	public String getAwardDocumentType() {
		return awardDocumentType;
	}

	public void setAwardDocumentType(String awardDocumentType) {
		this.awardDocumentType = awardDocumentType;
	}

	public String getAwardSequenceStatus() {
		return awardSequenceStatus;
	}

	public void setAwardSequenceStatus(String awardSequenceStatus) {
		this.awardSequenceStatus = awardSequenceStatus;
	}

	public String getWorkflowAwardStatusCode() {
		return workflowAwardStatusCode;
	}

	public void setWorkflowAwardStatusCode(String workflowAwardStatusCode) {
		this.workflowAwardStatusCode = workflowAwardStatusCode;
	}

	public String getAwardWorkflowStatus() {
		return awardWorkflowStatus;
	}

	public void setAwardWorkflowStatus(String awardWorkflowStatus) {
		this.awardWorkflowStatus = awardWorkflowStatus;
	}

	public String getAwardVariationType() {
		return awardVariationType;
	}

	public void setAwardVariationType(String awardVariationType) {
		this.awardVariationType = awardVariationType;
	}

	public String getSponsorAwardNumber() {
		return sponsorAwardNumber;
	}

	public void setSponsorAwardNumber(String sponsorAwardNumber) {
		this.sponsorAwardNumber = sponsorAwardNumber;
	}

	public Integer getClaimId() {
		return claimId;
	}

	public void setClaimId(Integer claimId) {
		this.claimId = claimId;
	}

	public Date getClaimSubmissionDate() {
		return claimSubmissionDate;
	}

	public void setClaimSubmissionDate(Date claimSubmissionDate) {
		this.claimSubmissionDate = claimSubmissionDate;
	}

	public String getCreateUserName() {
		return createUserName;
	}

	public void setCreateUserName(String createUserName) {
		this.createUserName = createUserName;
	}

	public String getUpdateUserName() {
		return updateUserName;
	}

	public void setUpdateUserName(String updateUserName) {
		this.updateUserName = updateUserName;
	}

	public Date getClaimUpdateTimeStamp() {
		return claimUpdateTimeStamp;
	}

	public void setClaimUpdateTimeStamp(Date claimUpdateTimeStamp) {
		this.claimUpdateTimeStamp = claimUpdateTimeStamp;
	}

	public Date getAwardStartDate() {
		return awardStartDate;
	}

	public void setAwardStartDate(Date awardStartDate) {
		this.awardStartDate = awardStartDate;
	}

	public Date getAwardEndDate() {
		return awardEndDate;
	}

	public void setAwardEndDate(Date awardEndDate) {
		this.awardEndDate = awardEndDate;
	}

	public Date getLastClaimEndDate() {
		return lastClaimEndDate;
	}

	public void setLastClaimEndDate(Date lastClaimEndDate) {
		this.lastClaimEndDate = lastClaimEndDate;
	}

	public String getClaimNumber() {
		return claimNumber;
	}

	public void setClaimNumber(String claimNumber) {
		this.claimNumber = claimNumber;
	}

	public String getProgressReportNumber() {
		return progressReportNumber;
	}

	public void setProgressReportNumber(String progressReportNumber) {
		this.progressReportNumber = progressReportNumber;
	}

	public Integer getProgressReportId() {
		return progressReportId;
	}

	public void setProgressReportId(Integer progressReportId) {
		this.progressReportId = progressReportId;
	}

	public String getProgressReportStatus() {
		return progressReportStatus;
	}

	public void setProgressReportStatus(String progressReportStatus) {
		this.progressReportStatus = progressReportStatus;
	}

	public Date getDueDate() {
		return dueDate;
	}

	public void setDueDate(Date dueDate) {
		this.dueDate = dueDate;
	}

	public String getFinanceOfficer() {
		return financeOfficer;
	}

	public void setFinanceOfficer(String financeOfficer) {
		this.financeOfficer = financeOfficer;
	}

	public String getClaimStatus() {
		return claimStatus;
	}

	public void setClaimStatus(String claimStatus) {
		this.claimStatus = claimStatus;
	}

	public Date getSubmittedDate() {
		return submittedDate;
	}

	public void setSubmittedDate(Date submittedDate) {
		this.submittedDate = submittedDate;
	}

	public String getReportType() {
		return reportType;
	}

	public void setReportType(String reportType) {
		this.reportType = reportType;
	}

	public String getClaimStatusCode() {
		return claimStatusCode;
	}

	public void setClaimStatusCode(String claimStatusCode) {
		this.claimStatusCode = claimStatusCode;
	}
}
