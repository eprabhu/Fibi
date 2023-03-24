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
import javax.persistence.Table;
import javax.persistence.Transient;

@Entity
@Table(name = "AWARD_MILESTONE")
public class AwardMileStone implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "AWARD_MILESTONE_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer awardMilestoneId;

	@Column(name = "AWARD_ID")
	private Integer awardId;

	@Column(name = "AWARD_NUMBER")
	private String awardNumber;

	@Column(name = "SEQUENCE_NUMBER")
	private Integer sequenceNumber;

	@Column(name = "MILESTONE")
	private String milestone;

	@Column(name = "START_DATE")
	private Timestamp startDate;

	@Column(name = "END_DATE")
	private Timestamp endDate;
	
	@Column(name = "DURATION")
	private String duration;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimeStamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;
	
	@Column(name = "MILESTONE_NUMBER")
	private String milestoneNumber;

	@Column(name = "MILESTONE_STATUS_CODE")
	private String milestoneStatusCode;

	@ManyToOne(optional = true)
	@JoinColumn(foreignKey = @ForeignKey(name = "AWARD_MILESTONE_FK1"), name = "MILESTONE_STATUS_CODE", referencedColumnName = "MILESTONE_STATUS_CODE", insertable = false, updatable = false)
	private MilestoneStatus milestoneStatus;

	@Column(name = "COMMENT")
	private String comment;

	@Transient
	private String updateUserFullName;

	public Integer getAwardMilestoneId() {
		return awardMilestoneId;
	}

	public void setAwardMilestoneId(Integer awardMilestoneId) {
		this.awardMilestoneId = awardMilestoneId;
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

	public String getMilestone() {
		return milestone;
	}

	public void setMilestone(String milestone) {
		this.milestone = milestone;
	}

	public Timestamp getStartDate() {
		return startDate;
	}

	public void setStartDate(Timestamp startDate) {
		this.startDate = startDate;
	}

	public Timestamp getEndDate() {
		return endDate;
	}

	public void setEndDate(Timestamp endDate) {
		this.endDate = endDate;
	}

	public String getDuration() {
		return duration;
	}

	public void setDuration(String duration) {
		this.duration = duration;
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

	public String getMilestoneNumber() {
		return milestoneNumber;
	}

	public void setMilestoneNumber(String milestoneNumber) {
		this.milestoneNumber = milestoneNumber;
	}

	public String getComment() {
		return comment;
	}

	public void setComment(String comment) {
		this.comment = comment;
	}

	public String getMilestoneStatusCode() {
		return milestoneStatusCode;
	}

	public void setMilestoneStatusCode(String milestoneStatusCode) {
		this.milestoneStatusCode = milestoneStatusCode;
	}

	public MilestoneStatus getMilestoneStatus() {
		return milestoneStatus;
	}

	public void setMilestoneStatus(MilestoneStatus milestoneStatus) {
		this.milestoneStatus = milestoneStatus;
	}

	public String getUpdateUserFullName() {
		return updateUserFullName;
	}

	public void setUpdateUserFullName(String updateUserFullName) {
		this.updateUserFullName = updateUserFullName;
	}

}
