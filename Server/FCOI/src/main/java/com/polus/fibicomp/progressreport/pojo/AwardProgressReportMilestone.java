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

import com.fasterxml.jackson.annotation.JsonBackReference;
import com.polus.fibicomp.award.pojo.AwardMileStone;
import com.polus.fibicomp.award.pojo.MilestoneStatus;

@Entity
@Table(name = "AWARD_PROGRESS_REPORT_MILESTONE")
@EntityListeners(AuditingEntityListener.class)
public class AwardProgressReportMilestone implements Serializable {
	
	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "PROGRESS_REPORT_MILESTONE_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer progressReportMilestoneId;
	
	@Column(name = "AWARD_ID")
	private Integer awardId;
	
	@Column(name = "AWARD_NUMBER")
	private String awardNumber;
	
	@Column(name = "SEQUENCE_NUMBER")
	private Integer sequenceNumber;
	
	@Column(name = "MILESTONE_NUMBER")
	private String milestoneNumber;	

	@Column(name = "ACTUAL_START_MONTH")
	private Date actualStartMonth;
	
	@Column(name = "ACTUAL_END_MONTH")
	private Date actualEndMonth;
	
	@Column(name = "MILESTONE_STATUS_CODE")
	private String milestoneStatusCode;		
	
	@Column(name = "REMARK")
	private String remark;
	
	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;
	
	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;
	
	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_MILESTONE_FK2"), name = "MILESTONE_STATUS_CODE", referencedColumnName = "MILESTONE_STATUS_CODE", insertable = false, updatable = false)
	private MilestoneStatus milestoneStatus;
	
	@JsonBackReference
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "PROGRESS_REPRT_MILESTONE_FK2"), name = "PROGRESS_REPORT_ID", referencedColumnName = "PROGRESS_REPORT_ID")
	private AwardProgressReport awardProgressReport;
	
	private transient AwardMileStone awardMileStone;

	private transient Timestamp committedStartDate;

	private transient Timestamp committedEndDate;

	private transient String milestone;
	
	private transient String status;

	public Integer getProgressReportMilestoneId() {
		return progressReportMilestoneId;
	}

	public void setProgressReportMilestoneId(Integer progressReportMilestoneId) {
		this.progressReportMilestoneId = progressReportMilestoneId;
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

	public String getMilestoneNumber() {
		return milestoneNumber;
	}

	public void setMilestoneNumber(String milestoneNumber) {
		this.milestoneNumber = milestoneNumber;
	}

	public Date getActualStartMonth() {
		return actualStartMonth;
	}

	public void setActualStartMonth(Date actualStartMonth) {
		this.actualStartMonth = actualStartMonth;
	}

	public Date getActualEndMonth() {
		return actualEndMonth;
	}

	public void setActualEndMonth(Date actualEndMonth) {
		this.actualEndMonth = actualEndMonth;
	}

	public String getMilestoneStatusCode() {
		return milestoneStatusCode;
	}

	public void setMilestoneStatusCode(String milestoneStatusCode) {
		this.milestoneStatusCode = milestoneStatusCode;
	}

	public String getRemark() {
		return remark;
	}

	public void setRemark(String remark) {
		this.remark = remark;
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

	public AwardMileStone getAwardMileStone() {
		return awardMileStone;
	}

	public void setAwardMileStone(AwardMileStone awardMileStone) {
		this.awardMileStone = awardMileStone;
	}

	public AwardProgressReport getAwardProgressReport() {
		return awardProgressReport;
	}

	public void setAwardProgressReport(AwardProgressReport awardProgressReport) {
		this.awardProgressReport = awardProgressReport;
	}

	public Timestamp getCommittedStartDate() {
		return committedStartDate;
	}

	public void setCommittedStartDate(Timestamp committedStartDate) {
		this.committedStartDate = committedStartDate;
	}

	public Timestamp getCommittedEndDate() {
		return committedEndDate;
	}

	public void setCommittedEndDate(Timestamp committedEndDate) {
		this.committedEndDate = committedEndDate;
	}

	public String getMilestone() {
		return milestone;
	}

	public void setMilestone(String milestone) {
		this.milestone = milestone;
	}

	public String getStatus() {
		return status;
	}

	public void setStatus(String status) {
		this.status = status;
	}

	public MilestoneStatus getMilestoneStatus() {
		return milestoneStatus;
	}

	public void setMilestoneStatus(MilestoneStatus milestoneStatus) {
		this.milestoneStatus = milestoneStatus;
	}

}
