package com.polus.fibicomp.manpowerintegration.pojo;

import java.io.Serializable;
import java.sql.Timestamp;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.SequenceGenerator;
import javax.persistence.Table;

@Entity
@Table(name = "WORKDAY_TERMINATION_DETAILS")
public class WorkdayTerminationDetails implements Serializable {

	private static final long serialVersionUID = 1L;

	@Id
	@Column(name = "WORKDAY_TERMINATION_ID")
	@GeneratedValue(strategy = GenerationType.SEQUENCE, generator = "SEQ_WORKDAY_TERMINATION_ID_GENERATOR")
	@SequenceGenerator(name = "SEQ_WORKDAY_TERMINATION_ID_GENERATOR", sequenceName = "SEQ_WORKDAY_TERMINATION_ID_GENERATOR", allocationSize = 1)
	private Integer workdayTerminationId;
	
	@Column(name = "PERSON_ID")
	private String personId;

	@Column(name = "NAME")
	private String name;

	@Column(name = "JOB_FAMILY")
	private String jobFamily;

	@Column(name = "TERMINATION_EVENT_STATUS")
	private String terminationEventStatus;

	@Column(name = "OVERALL_EVENT_STATUS")
	private String overallEventStatus;

	@Column(name = "TITLE")
	private String title;

	@Column(name = "NTU_DEPARTMENT")
	private String nTUDepartment;

	@Column(name = "OVERALL_EVENT_INITIATION_DATE")
	private Timestamp overallEventInitiationDate;

	@Column(name = "TERMINATION_APPROVE_DATE")
	private Timestamp terminationApproveDate;

	@Column(name = "AU")
	private String aU;

	@Column(name = "BUSINESS_TITLE")
	private String businessTitle;

	@Column(name = "JOB_FAMILY_GROUP")
	private String jobFamilyGroup;

	@Column(name = "BUSINESS_PROCESS_NAME")
	private String businessProcessName;

	@Column(name = "JOB_PROFILE")
	private String jobProfile;

	@Column(name = "RECOMMENDED_SYSTEM_TERMINATION_DATE")
	private Timestamp recommendedSystemTerminationDate;

	@Column(name = "SUPERVISORY_ORGANIZATION")
	private String supervisoryOrganization;

	@Column(name = "WORKER_PROPOSED_TERMINATION_DATE")
	private Timestamp workerProposedTerminationDate;

	@Column(name = "WORKER_RESIGNATION_NOTIFICATION_DATE")
	private Timestamp workerResignationNotificationDate;

	@Column(name = "WORKER_PROPOSED_RESIGNATIONN_DATE")
	private Timestamp workerProposedResignationnDate;

	@Column(name = "RESIGNATION_APPROVE_DATE")
	private Timestamp resignationApproveDate;

	@Column(name = "NOTICE_PERIOD")
	private String noticePeriod;

	@Column(name = "RESIGNATION_EVENT_STATUS")
	private String resignationEventStatus;
	
	@Column(name = "UNIQUE_EVENT_INITIATION_DATE")
	private String uniqueEventInitiationDate;

	@Column(name = "UPDATE_TIMESTAMP")
	private Timestamp updateTimestamp;

	@Column(name = "UPDATE_USER")
	private String updateUser;

	@Column(name = "TRIGGER_DATE")
	private Timestamp triggerDate;

	public String getUniqueEventInitiationDate() {
		return uniqueEventInitiationDate;
	}

	public void setUniqueEventInitiationDate(String uniqueEventInitiationDate) {
		this.uniqueEventInitiationDate = uniqueEventInitiationDate;
	}
	
	public Timestamp getOverallEventInitiationDate() {
		return overallEventInitiationDate;
	}

	public void setOverallEventInitiationDate(Timestamp overallEventInitiationDate) {
		this.overallEventInitiationDate = overallEventInitiationDate;
	}

	public String getPersonId() {
		return personId;
	}

	public void setPersonId(String personId) {
		this.personId = personId;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getJobFamily() {
		return jobFamily;
	}

	public void setJobFamily(String jobFamily) {
		this.jobFamily = jobFamily;
	}

	public String getTerminationEventStatus() {
		return terminationEventStatus;
	}

	public void setTerminationEventStatus(String terminationEventStatus) {
		this.terminationEventStatus = terminationEventStatus;
	}

	public String getOverallEventStatus() {
		return overallEventStatus;
	}

	public void setOverallEventStatus(String overallEventStatus) {
		this.overallEventStatus = overallEventStatus;
	}

	public String getTitle() {
		return title;
	}

	public void setTitle(String title) {
		this.title = title;
	}

	public String getnTUDepartment() {
		return nTUDepartment;
	}

	public void setnTUDepartment(String nTUDepartment) {
		this.nTUDepartment = nTUDepartment;
	}

	public Timestamp getTerminationApproveDate() {
		return terminationApproveDate;
	}

	public void setTerminationApproveDate(Timestamp terminationApproveDate) {
		this.terminationApproveDate = terminationApproveDate;
	}

	public String getaU() {
		return aU;
	}

	public void setaU(String aU) {
		this.aU = aU;
	}

	public String getBusinessTitle() {
		return businessTitle;
	}

	public void setBusinessTitle(String businessTitle) {
		this.businessTitle = businessTitle;
	}

	public String getJobFamilyGroup() {
		return jobFamilyGroup;
	}

	public void setJobFamilyGroup(String jobFamilyGroup) {
		this.jobFamilyGroup = jobFamilyGroup;
	}

	public String getBusinessProcessName() {
		return businessProcessName;
	}

	public void setBusinessProcessName(String businessProcessName) {
		this.businessProcessName = businessProcessName;
	}

	public String getJobProfile() {
		return jobProfile;
	}

	public void setJobProfile(String jobProfile) {
		this.jobProfile = jobProfile;
	}

	public Timestamp getRecommendedSystemTerminationDate() {
		return recommendedSystemTerminationDate;
	}

	public void setRecommendedSystemTerminationDate(Timestamp recommendedSystemTerminationDate) {
		this.recommendedSystemTerminationDate = recommendedSystemTerminationDate;
	}

	public String getSupervisoryOrganization() {
		return supervisoryOrganization;
	}

	public void setSupervisoryOrganization(String supervisoryOrganization) {
		this.supervisoryOrganization = supervisoryOrganization;
	}

	public Timestamp getWorkerProposedTerminationDate() {
		return workerProposedTerminationDate;
	}

	public void setWorkerProposedTerminationDate(Timestamp workerProposedTerminationDate) {
		this.workerProposedTerminationDate = workerProposedTerminationDate;
	}

	public Timestamp getWorkerResignationNotificationDate() {
		return workerResignationNotificationDate;
	}

	public void setWorkerResignationNotificationDate(Timestamp workerResignationNotificationDate) {
		this.workerResignationNotificationDate = workerResignationNotificationDate;
	}

	public Timestamp getWorkerProposedResignationnDate() {
		return workerProposedResignationnDate;
	}

	public void setWorkerProposedResignationnDate(Timestamp workerProposedResignationnDate) {
		this.workerProposedResignationnDate = workerProposedResignationnDate;
	}

	public Timestamp getResignationApproveDate() {
		return resignationApproveDate;
	}

	public void setResignationApproveDate(Timestamp resignationApproveDate) {
		this.resignationApproveDate = resignationApproveDate;
	}

	public String getNoticePeriod() {
		return noticePeriod;
	}

	public void setNoticePeriod(String noticePeriod) {
		this.noticePeriod = noticePeriod;
	}

	public String getResignationEventStatus() {
		return resignationEventStatus;
	}

	public void setResignationEventStatus(String resignationEventStatus) {
		this.resignationEventStatus = resignationEventStatus;
	}
	
	public String getUpdateUser() {
		return updateUser;
	}

	public void setUpdateUser(String updateUser) {
		this.updateUser = updateUser;
	}

	public Timestamp getTriggerDate() {
		return triggerDate;
	}

	public void setTriggerDate(Timestamp triggerDate) {
		this.triggerDate = triggerDate;
	}

	public Integer getWorkdayTerminationId() {
		return workdayTerminationId;
	}

	public void setWorkdayTerminationId(Integer workdayTerminationId) {
		this.workdayTerminationId = workdayTerminationId;
	}

	public Timestamp getUpdateTimestamp() {
		return updateTimestamp;
	}

	public void setUpdateTimestamp(Timestamp updateTimestamp) {
		this.updateTimestamp = updateTimestamp;
	}

}
