package com.polus.fibicomp.manpowerintegration.dto.longleave;

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
    "First_Day_of_Leave",
    "initiated",
    "Title",
    "NTU_Department",
    "Last_Day_of_Leave_-_Estimated",
    "Name",
    "Last_Day_of_Leave_-_Actual",
    "Approval_Date",
    "AU",
    "businessTitle",
    "Job_Family_Group",
    "Job_Profile",
    "Leave_Type__Excluding_Family_",
    "Supervisory_Organization",
    "status",
    "Employee_ID"
})
public class LongLeaveReportEntry {

	@JsonProperty("Job_Family")
    private String jobFamily;
    @JsonProperty("First_Day_of_Leave")
    private String firstDayOfLeave;
    @JsonProperty("initiated")
    private String initiated;
    @JsonProperty("Title")
    private String title;
    @JsonProperty("NTU_Department")
    private String nTUDepartment;
    @JsonProperty("Last_Day_of_Leave_-_Estimated")
    private String lastDayOfLeaveEstimated;
    @JsonProperty("Name")
    private String name;
    @JsonProperty("Last_Day_of_Leave_-_Actual")
    private String lastDayOfLeaveActual;
    @JsonProperty("Approval_Date")
    private String approvalDate;
    @JsonProperty("AU")
    private String aU;
    @JsonProperty("businessTitle")
    private String businessTitle;
    @JsonProperty("Job_Family_Group")
    private String jobFamilyGroup;
    @JsonProperty("Job_Profile")
    private String jobProfile;
    @JsonProperty("Leave_Type__Excluding_Family_")
    private String leaveTypeExcludingFamily;
    @JsonProperty("Supervisory_Organization")
    private String supervisoryOrganization;
    @JsonProperty("status")
    private String status;
    @JsonProperty("Employee_ID")
    private String employeeId;
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

    @JsonProperty("First_Day_of_Leave")
	public String getFirstDayOfLeave() {
		return firstDayOfLeave;
	}

    @JsonProperty("First_Day_of_Leave")
	public void setFirstDayOfLeave(String firstDayOfLeave) {
		this.firstDayOfLeave = firstDayOfLeave;
	}

    @JsonProperty("initiated")
	public String getInitiated() {
		return initiated;
	}

    @JsonProperty("initiated")
	public void setInitiated(String initiated) {
		this.initiated = initiated;
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

    @JsonProperty("Last_Day_of_Leave_-_Estimated")
	public String getLastDayOfLeaveEstimated() {
		return lastDayOfLeaveEstimated;
	}

    @JsonProperty("Last_Day_of_Leave_-_Estimated")
	public void setLastDayOfLeaveEstimated(String lastDayOfLeaveEstimated) {
		this.lastDayOfLeaveEstimated = lastDayOfLeaveEstimated;
	}

    @JsonProperty("Name")
	public String getName() {
		return name;
	}

    @JsonProperty("Name")
	public void setName(String name) {
		this.name = name;
	}

    @JsonProperty("Last_Day_of_Leave_-_Actual")
	public String getLastDayOfLeaveActual() {
		return lastDayOfLeaveActual;
	}

    @JsonProperty("Last_Day_of_Leave_-_Actual")
	public void setLastDayOfLeaveActual(String lastDayOfLeaveActual) {
		this.lastDayOfLeaveActual = lastDayOfLeaveActual;
	}

    @JsonProperty("Approval_Date")
	public String getApprovalDate() {
		return approvalDate;
	}

    @JsonProperty("Approval_Date")
	public void setApprovalDate(String approvalDate) {
		this.approvalDate = approvalDate;
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

    @JsonProperty("Job_Profile")
	public String getJobProfile() {
		return jobProfile;
	}

    @JsonProperty("Job_Profile")
	public void setJobProfile(String jobProfile) {
		this.jobProfile = jobProfile;
	}

    @JsonProperty("Leave_Type__Excluding_Family_")
	public String getLeaveTypeExcludingFamily() {
		return leaveTypeExcludingFamily;
	}

    @JsonProperty("Leave_Type__Excluding_Family_")
	public void setLeaveTypeExcludingFamily(String leaveTypeExcludingFamily) {
		this.leaveTypeExcludingFamily = leaveTypeExcludingFamily;
	}

    @JsonProperty("Supervisory_Organization")
	public String getSupervisoryOrganization() {
		return supervisoryOrganization;
	}

    @JsonProperty("Supervisory_Organization")
	public void setSupervisoryOrganization(String supervisoryOrganization) {
		this.supervisoryOrganization = supervisoryOrganization;
	}

    @JsonProperty("status")
	public String getStatus() {
		return status;
	}

    @JsonProperty("status")
	public void setStatus(String status) {
		this.status = status;
	}

    @JsonProperty("Employee_ID")
	public String getEmployeeId() {
		return employeeId;
	}

    @JsonProperty("Employee_ID")
	public void setEmployeeId(String employeeId) {
		this.employeeId = employeeId;
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
