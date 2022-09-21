package com.polus.fibicomp.manpowerintegration.dto.terminations;

import java.util.HashMap;
import java.util.Map;

import com.fasterxml.jackson.annotation.JsonAnyGetter;
import com.fasterxml.jackson.annotation.JsonAnySetter;
import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonPropertyOrder;

@JsonInclude(JsonInclude.Include.NON_NULL)
@JsonPropertyOrder({
    "Job_Family",
    "Termination_Event_Status",
    "Overall_Event_Status",
    "Employee_ID",
    "Title",
    "NTU_Department",
    "Overall_Event_Initiation_Date",
    "Termination_Approve_Date",
    "Name",
    "AU",
    "businessTitle",
    "Job_Family_Group",
    "Business_Process_Name",
    "Job_Profile",
    "Recommended_System_Termination_Date",
    "Supervisory_Organization",
    "Worker_Proposed_Termination_Date",
    "Worker_Resignation_Notification_Date",
    "Worker_Proposed_Resignation_Date",
    "Resignation_Approve_Date",
    "Notice_Period",
    "Resignation_Event_Status"
})
public class TerminationsReportEntry {

	@JsonProperty("Job_Family")
    private String jobFamily;
    @JsonProperty("Termination_Event_Status")
    private String terminationEventStatus;
    @JsonProperty("Overall_Event_Status")
    private String overallEventStatus;
    @JsonProperty("Employee_ID")
    private String employeeID;
    @JsonProperty("Title")
    private String title;
    @JsonProperty("NTU_Department")
    private String nTUDepartment;
    @JsonProperty("Overall_Event_Initiation_Date")
    private String overallEventInitiationDate;
    @JsonProperty("Termination_Approve_Date")
    private String terminationApproveDate;
    @JsonProperty("Name")
    private String name;
    @JsonProperty("AU")
    private String aU;
    @JsonProperty("businessTitle")
    private String businessTitle;
    @JsonProperty("Job_Family_Group")
    private String jobFamilyGroup;
    @JsonProperty("Business_Process_Name")
    private String businessProcessName;
    @JsonProperty("Job_Profile")
    private String jobProfile;
    @JsonProperty("Recommended_System_Termination_Date")
    private String recommendedSystemTerminationDate;
    @JsonProperty("Supervisory_Organization")
    private String supervisoryOrganization;
    @JsonProperty("Worker_Proposed_Termination_Date")
    private String workerProposedTerminationDate;
    @JsonProperty("Worker_Resignation_Notification_Date")
    private String workerResignationNotificationDate;
    @JsonProperty("Worker_Proposed_Resignation_Date")
    private String workerProposedResignationnDate;
    @JsonProperty("Resignation_Approve_Date")
    private String resignationApproveDate;
    @JsonProperty("Notice_Period")
    private String noticePeriod;
    @JsonProperty("Resignation_Event_Status")
    private String resignationEventStatus;
    @JsonIgnore
    private Map<String, Object> additionalProperties = new HashMap<String, Object>();

    @JsonProperty("Job_Family")
	public String getJobFamily() {
		return jobFamily;
	}

    @JsonProperty("Job_Family")
	public void setJobFamily(String jobFamily) {
		this.jobFamily = jobFamily;
	}

    @JsonProperty("Termination_Event_Status")
	public String getTerminationEventStatus() {
		return terminationEventStatus;
	}

    @JsonProperty("Termination_Event_Status")
	public void setTerminationEventStatus(String terminationEventStatus) {
		this.terminationEventStatus = terminationEventStatus;
	}

    @JsonProperty("Overall_Event_Status")
	public String getOverallEventStatus() {
		return overallEventStatus;
	}

    @JsonProperty("Overall_Event_Status")
	public void setOverallEventStatus(String overallEventStatus) {
		this.overallEventStatus = overallEventStatus;
	}

    @JsonProperty("Employee_ID")
	public String getEmployeeID() {
		return employeeID;
	}

    @JsonProperty("Employee_ID")
	public void setEmployeeID(String employeeID) {
		this.employeeID = employeeID;
	}

    @JsonProperty("Title")
	public String getTitle() {
		return title;
	}

    @JsonProperty("Title")
	public void setTitle(String title) {
		this.title = title;
	}

    @JsonProperty("NTU_Department")
	public String getnTUDepartment() {
		return nTUDepartment;
	}

    @JsonProperty("NTU_Department")
	public void setnTUDepartment(String nTUDepartment) {
		this.nTUDepartment = nTUDepartment;
	}

    @JsonProperty("Overall_Event_Initiation_Date")
	public String getOverallEventInitiationDate() {
		return overallEventInitiationDate;
	}

    @JsonProperty("Overall_Event_Initiation_Date")
	public void setOverallEventInitiationDate(String overallEventInitiationDate) {
		this.overallEventInitiationDate = overallEventInitiationDate;
	}

    @JsonProperty("Termination_Approve_Date")
	public String getTerminationApproveDate() {
		return terminationApproveDate;
	}

    @JsonProperty("Termination_Approve_Date")
	public void setTerminationApproveDate(String terminationApproveDate) {
		this.terminationApproveDate = terminationApproveDate;
	}

    @JsonProperty("Name")
	public String getName() {
		return name;
	}

    @JsonProperty("Name")
	public void setName(String name) {
		this.name = name;
	}

    @JsonProperty("AU")
	public String getaU() {
		return aU;
	}

    @JsonProperty("AU")
	public void setaU(String aU) {
		this.aU = aU;
	}

    @JsonProperty("businessTitle")
	public String getBusinessTitle() {
		return businessTitle;
	}

    @JsonProperty("businessTitle")
	public void setBusinessTitle(String businessTitle) {
		this.businessTitle = businessTitle;
	}

    @JsonProperty("Job_Family_Group")
	public String getJobFamilyGroup() {
		return jobFamilyGroup;
	}

    @JsonProperty("Job_Family_Group")
	public void setJobFamilyGroup(String jobFamilyGroup) {
		this.jobFamilyGroup = jobFamilyGroup;
	}

    @JsonProperty("Business_Process_Name")
	public String getBusinessProcessName() {
		return businessProcessName;
	}

    @JsonProperty("Business_Process_Name")
	public void setBusinessProcessName(String businessProcessName) {
		this.businessProcessName = businessProcessName;
	}

    @JsonProperty("Job_Profile")
	public String getJobProfile() {
		return jobProfile;
	}

    @JsonProperty("Job_Profile")
	public void setJobProfile(String jobProfile) {
		this.jobProfile = jobProfile;
	}

    @JsonProperty("Recommended_System_Termination_Date")
	public String getRecommendedSystemTerminationDate() {
		return recommendedSystemTerminationDate;
	}

    @JsonProperty("Recommended_System_Termination_Date")
	public void setRecommendedSystemTerminationDate(String recommendedSystemTerminationDate) {
		this.recommendedSystemTerminationDate = recommendedSystemTerminationDate;
	}

    @JsonProperty("Supervisory_Organization")
	public String getSupervisoryOrganization() {
		return supervisoryOrganization;
	}

    @JsonProperty("Supervisory_Organization")
	public void setSupervisoryOrganization(String supervisoryOrganization) {
		this.supervisoryOrganization = supervisoryOrganization;
	}

    @JsonProperty("Worker_Proposed_Termination_Date")
	public String getWorkerProposedTerminationDate() {
		return workerProposedTerminationDate;
	}

    @JsonProperty("Worker_Proposed_Termination_Date")
	public void setWorkerProposedTerminationDate(String workerProposedTerminationDate) {
		this.workerProposedTerminationDate = workerProposedTerminationDate;
	}

    @JsonProperty("Worker_Resignation_Notification_Date")
	public String getWorkerResignationNotificationDate() {
		return workerResignationNotificationDate;
	}

    @JsonProperty("Worker_Resignation_Notification_Date")
	public void setWorkerResignationNotificationDate(String workerResignationNotificationDate) {
		this.workerResignationNotificationDate = workerResignationNotificationDate;
	}

    @JsonProperty("Worker_Proposed_Resignation_Date")
	public String getWorkerProposedResignationnDate() {
		return workerProposedResignationnDate;
	}

    @JsonProperty("Worker_Proposed_Resignation_Date")
	public void setWorkerProposedResignationnDate(String workerProposedResignationnDate) {
		this.workerProposedResignationnDate = workerProposedResignationnDate;
	}

    @JsonProperty("Resignation_Approve_Date")
	public String getResignationApproveDate() {
		return resignationApproveDate;
	}

    @JsonProperty("Resignation_Approve_Date")
	public void setResignationApproveDate(String resignationApproveDate) {
		this.resignationApproveDate = resignationApproveDate;
	}

    @JsonProperty("Notice_Period")
	public String getNoticePeriod() {
		return noticePeriod;
	}

    @JsonProperty("Notice_Period")
	public void setNoticePeriod(String noticePeriod) {
		this.noticePeriod = noticePeriod;
	}

    @JsonProperty("Resignation_Event_Status")
	public String getResignationEventStatus() {
		return resignationEventStatus;
	}

    @JsonProperty("Resignation_Event_Status")
	public void setResignationEventStatus(String resignationEventStatus) {
		this.resignationEventStatus = resignationEventStatus;
	}

    @JsonAnyGetter
    public Map<String, Object> getAdditionalProperties() {
        return this.additionalProperties;
    }

    @JsonAnySetter
    public void setAdditionalProperty(String name, Object value) {
        this.additionalProperties.put(name, value);
    }

}
