package com.polus.fibicomp.manpowerintegration.dto.jobprofile;

import java.util.List;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
    "EFFECTIVE_DATE",
    "JOB_PROFILE_ID",
    "INACTIVE",
    "Job_Family_group",
    "PUBLIC_JOB",
    "JOB_PROFILE_NAME",
    "WORK_SHIFT_REQUIRED",
    "JOB_PROFILE_CODE",
    "DEFAULT_JOB_TITLE",
    "Job_Category",
    "JOB_CATEGORY_ID"
})
public class JobProfileReportEntry {

	@JsonProperty("EFFECTIVE_DATE")
    private String effectiveDate;
    @JsonProperty("JOB_PROFILE_ID")
    private String jobProfileId;
    @JsonProperty("INACTIVE")
    private String inactive;
    @JsonProperty("Job_Family_group")
    private List<JobFamilyGroup> jobFamilyGroups = null;
    @JsonProperty("PUBLIC_JOB")
    private String publicJob;
    @JsonProperty("JOB_PROFILE_NAME")
    private String jobProfileName;
    @JsonProperty("WORK_SHIFT_REQUIRED")
    private String workShiftRequired;
    @JsonProperty("JOB_PROFILE_CODE")
    private String jobProfileCode;
    @JsonProperty("DEFAULT_JOB_TITLE")
    private String defaultJobTitle;
    @JsonProperty("Job_Category")
    private String jobCategory;
    @JsonProperty("JOB_CATEGORY_ID")
    private String jobCategoryId;

    @JsonProperty("EFFECTIVE_DATE")
	public String getEffectiveDate() {
		return effectiveDate;
	}

    @JsonProperty("EFFECTIVE_DATE")
	public void setEffectiveDate(String effectiveDate) {
		this.effectiveDate = effectiveDate;
	}

    @JsonProperty("JOB_PROFILE_ID")
	public String getJobProfileId() {
		return jobProfileId;
	}

    @JsonProperty("JOB_PROFILE_ID")
	public void setJobProfileId(String jobProfileId) {
		this.jobProfileId = jobProfileId;
	}

    @JsonProperty("INACTIVE")
	public String getInactive() {
		return inactive;
	}

    @JsonProperty("INACTIVE")
	public void setInactive(String inactive) {
		this.inactive = inactive;
	}

    @JsonProperty("Job_Family_group")
	public List<JobFamilyGroup> getJobFamilyGroups() {
		return jobFamilyGroups;
	}

    @JsonProperty("Job_Family_group")
	public void setJobFamilyGroups(List<JobFamilyGroup> jobFamilyGroups) {
		this.jobFamilyGroups = jobFamilyGroups;
	}

    @JsonProperty("PUBLIC_JOB")
	public String getPublicJob() {
		return publicJob;
	}

    @JsonProperty("PUBLIC_JOB")
	public void setPublicJob(String publicJob) {
		this.publicJob = publicJob;
	}

    @JsonProperty("JOB_PROFILE_NAME")
	public String getJobProfileName() {
		return jobProfileName;
	}

    @JsonProperty("JOB_PROFILE_NAME")
	public void setJobProfileName(String jobProfileName) {
		this.jobProfileName = jobProfileName;
	}

    @JsonProperty("WORK_SHIFT_REQUIRED")
	public String getWorkShiftRequired() {
		return workShiftRequired;
	}

    @JsonProperty("WORK_SHIFT_REQUIRED")
	public void setWorkShiftRequired(String workShiftRequired) {
		this.workShiftRequired = workShiftRequired;
	}

    @JsonProperty("JOB_PROFILE_CODE")
	public String getJobProfileCode() {
		return jobProfileCode;
	}

    @JsonProperty("JOB_PROFILE_CODE")
	public void setJobProfileCode(String jobProfileCode) {
		this.jobProfileCode = jobProfileCode;
	}

    @JsonProperty("DEFAULT_JOB_TITLE")
	public String getDefaultJobTitle() {
		return defaultJobTitle;
	}

    @JsonProperty("DEFAULT_JOB_TITLE")
	public void setDefaultJobTitle(String defaultJobTitle) {
		this.defaultJobTitle = defaultJobTitle;
	}

    @JsonProperty("Job_Category")
	public String getJobCategory() {
		return jobCategory;
	}

    @JsonProperty("Job_Category")
	public void setJobCategory(String jobCategory) {
		this.jobCategory = jobCategory;
	}

    @JsonProperty("JOB_CATEGORY_ID")
	public String getJobCategoryId() {
		return jobCategoryId;
	}

    @JsonProperty("JOB_CATEGORY_ID")
	public void setJobCategoryId(String jobCategoryId) {
		this.jobCategoryId = jobCategoryId;
	}

}
