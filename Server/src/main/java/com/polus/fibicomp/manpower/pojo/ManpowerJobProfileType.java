package com.polus.fibicomp.manpower.pojo;

import java.io.Serializable;
import java.sql.Timestamp;
import java.util.Date;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "MANPOWER_JOB_PROFILE_TYPE")
public class ManpowerJobProfileType implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "JOB_PROFILE_TYPE_CODE")
	private String jobProfileTypeCode;

	@Column(name = "DESCRIPTION")
	private String description;

	@Column(name = "IS_ACTIVE")
	private String isActive;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "JOB_PROFILE_CODE")
	private String jobProfileCode;

	@Column(name = "DEFAULT_JOB_TITLE")
	private String defaultJobTitle;

	@Column(name = "JOB_CATEGORY")
	private String jobCategory;

	@Column(name = "JOB_CATEGORY_ID")
	private String jobCategoryId;

	@Column(name = "EFFECTIVE_DATE")
	private Date effectiveDate;

	@Column(name = "WORK_SHIFT_REQUIRED")
	private String workShiftRequired;

	@Column(name = "PUBLIC_JOB")
	private String publicJob;

	@Column(name = "IS_WORKDAY_ACTIVE")
	private String isWorkdayActive;

	@Column(name = "SORT_ORDER")
	private Integer sortOrder;

	@Column(name = "JOB_FAMILY")
	private String jobFamily;

	@Column(name = "JOB_FAMILY_ID")
	private String jobFamilyId;

	@Column(name = "JOB_FAMILY_GROUP")
	private String jobFamilyGroup;

	public ManpowerJobProfileType() {

	}

	public ManpowerJobProfileType(String jobProfileTypeCode, String description, String jobFamily) {
		this.description = description;
		this.jobProfileTypeCode = jobProfileTypeCode;
		this.jobFamily = jobFamily;
	}

	public String getJobFamily() {
		return jobFamily;
	}

	public void setJobFamily(String jobFamily) {
		this.jobFamily = jobFamily;
	}

	public String getJobFamilyId() {
		return jobFamilyId;
	}

	public void setJobFamilyId(String jobFamilyId) {
		this.jobFamilyId = jobFamilyId;
	}

	public String getJobFamilyGroup() {
		return jobFamilyGroup;
	}

	public void setJobFamilyGroup(String jobFamilyGroup) {
		this.jobFamilyGroup = jobFamilyGroup;
	}

	public String getJobProfileTypeCode() {
		return jobProfileTypeCode;
	}

	public void setJobProfileTypeCode(String jobProfileTypeCode) {
		this.jobProfileTypeCode = jobProfileTypeCode;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getIsActive() {
		return isActive;
	}

	public void setIsActive(String isActive) {
		this.isActive = isActive;
	}

	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

	public String getJobProfileCode() {
		return jobProfileCode;
	}

	public void setJobProfileCode(String jobProfileCode) {
		this.jobProfileCode = jobProfileCode;
	}

	public String getDefaultJobTitle() {
		return defaultJobTitle;
	}

	public void setDefaultJobTitle(String defaultJobTitle) {
		this.defaultJobTitle = defaultJobTitle;
	}

	public String getJobCategory() {
		return jobCategory;
	}

	public void setJobCategory(String jobCategory) {
		this.jobCategory = jobCategory;
	}

	public String getJobCategoryId() {
		return jobCategoryId;
	}

	public void setJobCategoryId(String jobCategoryId) {
		this.jobCategoryId = jobCategoryId;
	}

	public String getWorkShiftRequired() {
		return workShiftRequired;
	}

	public void setWorkShiftRequired(String workShiftRequired) {
		this.workShiftRequired = workShiftRequired;
	}

	public String getPublicJob() {
		return publicJob;
	}

	public void setPublicJob(String publicJob) {
		this.publicJob = publicJob;
	}

	public String getIsWorkdayActive() {
		return isWorkdayActive;
	}

	public void setIsWorkdayActive(String isWorkdayActive) {
		this.isWorkdayActive = isWorkdayActive;
	}

	public Date getEffectiveDate() {
		return effectiveDate;
	}

	public void setEffectiveDate(Date effectiveDate) {
		this.effectiveDate = effectiveDate;
	}

	public Integer getSortOrder() {
		return sortOrder;
	}

	public void setSortOrder(Integer sortOrder) {
		this.sortOrder = sortOrder;
	}

}
