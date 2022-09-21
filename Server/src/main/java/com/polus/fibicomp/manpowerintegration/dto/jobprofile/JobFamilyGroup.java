package com.polus.fibicomp.manpowerintegration.dto.jobprofile;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
    "Job_Family",
    "JOB_FAMILY_ID",
    "Job_Family_Group"
})
public class JobFamilyGroup {

	@JsonProperty("Job_Family")
    private String jobFamily;
    @JsonProperty("JOB_FAMILY_ID")
    private String jobFamilyId;
    @JsonProperty("Job_Family_Group")
    private String jobFamilyGroup;

    @JsonProperty("Job_Family")
	public String getJobFamily() {
		return jobFamily;
	}

    @JsonProperty("Job_Family")
	public void setJobFamily(String jobFamily) {
		this.jobFamily = jobFamily;
	}

    @JsonProperty("JOB_FAMILY_ID")
	public String getJobFamilyId() {
		return jobFamilyId;
	}

    @JsonProperty("JOB_FAMILY_ID")
	public void setJobFamilyId(String jobFamilyId) {
		this.jobFamilyId = jobFamilyId;
	}

    @JsonProperty("Job_Family_Group")
	public String getJobFamilyGroup() {
		return jobFamilyGroup;
	}

    @JsonProperty("Job_Family_Group")
	public void setJobFamilyGroup(String jobFamilyGroup) {
		this.jobFamilyGroup = jobFamilyGroup;
	}

}
