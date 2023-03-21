package com.polus.fibicomp.progressreport.pojo;

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

import org.springframework.data.annotation.LastModifiedBy;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.jpa.domain.support.AuditingEntityListener;

import com.fasterxml.jackson.annotation.JsonBackReference;

@Entity
@Table(name = "AWARD_PROGRESS_REPORT_ACHIEVEMENT")
@EntityListeners(AuditingEntityListener.class)
public class AwardProgressReportAchievement implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "PROGRESS_REPORT_ACHIEVEMENT_ID")
	@GeneratedValue(strategy = GenerationType.IDENTITY)
	private Integer progressReportAchievementId;

	@Column(name = "ACHIEVEMENT_TYPE_CODE")
	private String achievementTypeCode;
	
	@Column(name = "DESCRIPTION")
	private String description;	

	@LastModifiedDate
	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;
	
	@LastModifiedBy
	@Column(name = "UPDATE_USER")
	private String updateUser;
	
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "PROGRESS_REPRT_ACHIEVEMENT_FK1"), name = "ACHIEVEMENT_TYPE_CODE", referencedColumnName = "ACHIEVEMENT_TYPE_CODE", insertable = false, updatable = false)
	private ProgressReportAchievementType progressReportAchievementType;
	
	@JsonBackReference
	@ManyToOne(optional = false)
	@JoinColumn(foreignKey = @ForeignKey(name = "PROGRESS_REPRT_ACHIEVEMENT_FK2"), name = "PROGRESS_REPORT_ID", referencedColumnName = "PROGRESS_REPORT_ID")
	private AwardProgressReport awardProgressReport;

	public Integer getProgressReportAchievementId() {
		return progressReportAchievementId;
	}

	public void setProgressReportAchievementId(Integer progressReportAchievementId) {
		this.progressReportAchievementId = progressReportAchievementId;
	}
	
	public String getAchievementTypeCode() {
		return achievementTypeCode;
	}

	public void setAchievementTypeCode(String achievementTypeCode) {
		this.achievementTypeCode = achievementTypeCode;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
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

	public ProgressReportAchievementType getProgressReportAchievementType() {
		return progressReportAchievementType;
	}

	public void setProgressReportAchievementType(ProgressReportAchievementType progressReportAchievementType) {
		this.progressReportAchievementType = progressReportAchievementType;
	}

	public AwardProgressReport getAwardProgressReport() {
		return awardProgressReport;
	}

	public void setAwardProgressReport(AwardProgressReport awardProgressReport) {
		this.awardProgressReport = awardProgressReport;
	}
}
